#' @title CompBoost Classification Learner
#'
#' @name mlr_learners_classif.compboost
#'
#' @description
#' Componentwise boosting
#' Calls [compboost::Compboost()] from package \CRANpkg{compboost}.
#'
#' @template section_dictionary_learner
#' @templateVar id classif.compboost
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifCompboost = R6Class("LearnerClassifCompboost",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Create a `LearnerClassifCompboost` object.
    initialize = function () {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "df", default = 4, lower = 1),
          ParamDbl$new(id = "df_cat", default = 4, lower = 1),
          ParamInt$new(id = "iters_max_univariat", default = 10000L, lower = 1L),
          ParamInt$new(id = "iters_max_interactions", default = 10000L, lower = 1L),
          ParamDbl$new(id = "learning_rate_univariate", default = 0.01, lower = 0),
          ParamDbl$new(id = "learning_rate_interactions", default = 0.1, lower = 0),
          ParamDbl$new(id = "n_knots_univariat", default = 20L, lower = 4),
          ParamDbl$new(id = "n_knots_interactions", default = 10L, lower = 4),
          ParamInt$new(id = "ncores", default = 1L, lower = 1L, upper = parallel::detectCores() - 1L),
          ParamDbl$new(id = "stop_epsylon_for_break", default = 0.00001, lower = 0, upper = 1),
          ParamDbl$new(id = "stop_patience", default = 10L, lower = 1L),
          ParamDbl$new(id = "val_fraction", default = 0.33, lower = 0, upper = 1),
          ParamDbl$new(id = "top_interactions", default = 0.02, lower = 0.01, upper = 1),
          ParamLgl$new(id = "use_early_stopping", default = TRUE),
          ParamLgl$new(id = "show_output", default = FALSE),
          ParamLgl$new(id = "just_univariat", default = FALSE),
          ParamLgl$new(id = "add_rf", default = FALSE),
          ParamInt$new(id = "train_time_total", default = 0, lower = 0)
        ))
      ps$values = list(df = 6, show_output = FALSE, top_interactions = 0.02, learning_rate_univariate = 0.01,
        learning_rate_interactions = 0.05, train_time_total = 10, iters_max_univariat = 50000L,
        iters_max_interactions = 50000L, n_knots_univariat = 15, n_knots_interactions = 8,
        use_early_stopping = TRUE, stop_patience = 10L, stop_epsylon_for_break = 1e-6)

      super$initialize(
        id = "classif.compboost",
        packages = "compboost",
        feature_types = c("numeric", "factor", "integer", "character"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass")
      )
    }
  ),

  private = list(
    .train = function(task) {
      time0 = proc.time()

      ### Set params with default and user defined ones:
      pdefaults = self$param_set$default
      pars      = self$param_set$values

      self$param_set$values = mlr3misc::insert_named(pdefaults, pars)

      ### Get stop arguments and train/test indices:
      stop_args = list(eps_for_break = self$param_set$values$stop_epsylon_for_break,
        patience = self$param_set$values$stop_patience)
      if (self$param_set$values$use_early_stopping) {
        test_idx  = as.integer(sample(seq_len(task$nrow), trunc(task$nrow * self$param_set$values$val_fraction)))
      } else {
        test_idx = NULL
      }
      train_idx = setdiff(seq_len(task$nrow), test_idx)

      optimizer = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)

      ### Define compboost model for univariate features:
      loss = compboost::LossBinomial$new()
      cboost_uni = Compboost$new(data = task$data(), target = task$target_names, loss = loss,
        learning_rate = self$param_set$values$learning_rate_univariate, optimizer = optimizer, test_idx = test_idx,
        stop_args = stop_args, use_early_stopping = self$param_set$values$use_early_stopping)

      ### If a maximum time is given, the logger is used for stopping. Otherwise the time is just logged:
      if (self$param_set$values$train_time_total > 0) {
        cboost_uni$addLogger(LoggerTime, TRUE, "minutes", self$param_set$values$train_time_total, "minutes")
      } else {
        cboost_uni$addLogger(LoggerTime, FALSE, "minutes", 0, "minutes")
      }

      ### Add base-learner/components (linear + centered spline):
      nuisance = lapply(task$feature_names, function(nm) {
        if (is.numeric(task$data()[[nm]])) {
          cboost_uni$addComponents(nm, n_knots = self$param_set$values$n_knots_univariat, df = self$param_set$values$df)
        } else {
          cboost_uni$addBaselearner(nm, "category", BaselearnerCategoricalRidge, df = self$param_set$values$df_cat)
        }
      })

      ### Train model:
      if (self$param_set$values$show_output) {
        cboost_uni$train(self$param_set$values$iters_max_univariat)
      } else {
        nuisance = capture.output(cboost_uni$train(self$param_set$values$iters_max_univariat))
      }

      ### Check if model was early stopped, if so, set it to iters - patience - 1 to set the
      ### optimal stopping iter:
      ld = cboost_uni$getLoggerData()
      was_early_stopped = (max(ld[["_iterations"]]) < self$param_set$values[["iters_max_univariat"]]) &&
        (max(ld[["minutes"]]) < self$param_set$values[["train_time_total"]])
      if (was_early_stopped && (cboost_uni$getCurrentIteration() > self$param_set$values$stop_patience + 2))
        cboost_uni$train(cboost_uni$getCurrentIteration() - self$param_set$values$stop_patience - 1)

      out = list()
      out[["univariat"]] = cboost_uni

      ### Create new task for interaction detection:
      df_new = task$data()
      df_new[[task$target_names]] = NULL

      # Access predictions:
      pred_inbag = cboost_uni$response$getPrediction()
      if (self$param_set$values$use_early_stopping) {
        pred_oob   = cboost_uni$response_oob$getPrediction()
      }

      # Calculate pseudo residuals from the fitted univariate model:
      res1 = as.vector(loss$calculatePseudoResiduals(cboost_uni$response$getResponse(), pred_inbag))
      if (self$param_set$values$use_early_stopping) {
        res2 = as.vector(loss$calculatePseudoResiduals(cboost_uni$response_oob$getResponse(), pred_oob))
      } else {
        res2 = NULL
      }
      # Reconstruct the order of the original data to correctly add pseudo residuals:
      idx_re = order(c(train_idx, test_idx))
      res = c(res1, res2)[idx_re]

      # Define new task with 'residuals' as target
      df_new$residuals = res
      tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")

      if (! self$param_set$values$just_univariat) {

        ### Extract interactions based on random forest:
        extracted_interactions = na.omit(po("extract_interactions", degree = 2)$train(list(tsk_new))$output)

        ninteractions = nrow(extracted_interactions)
        ntopinteractions = ceiling(ninteractions * self$param_set$values$top_interactions)
        if (ntopinteractions > 0) {
          ### Just use the top interactions (defined by the user, too much interactions makes the model too slow):
          top_interactionss = seq_len(ntopinteractions)

          ### Define optimizer and loss with predictions as offset to continue training instead of
          ### start from all over again:
          optimizer_int  = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)
          loss_int_inbag = compboost::LossBinomial$new(pred_inbag, TRUE)
          if (self$param_set$values$use_early_stopping) {
            loss_int_oob   = compboost::LossBinomial$new(pred_oob, TRUE)
          } else {
            loss_int_oob = compboost::LossBinomial$new()
          }

          ### Define interaction model based on tensor splines:
          cboost_int = Compboost$new(data = task$data(), target = task$target_names, loss = loss_int_inbag,
            learning_rate = self$param_set$values$learning_rate_interactions, optimizer = optimizer_int, test_idx = test_idx,
            stop_args = c(stop_args, loss_oob = loss_int_oob), use_early_stopping = self$param_set$values$use_early_stopping)

          #browser()
          ### Add tensor splines. (FIXME: Atm just for numeric-numeric interactions):
          nuisance = lapply(top_interactionss, function(i) {
             #Check if numeric! Interactions between ridge and spline needs to be tested first!
            e = try({
              cboost_int$addTensor(extracted_interactions$feat1[i], extracted_interactions$feat2[i],
                n_knots = self$param_set$values$n_knots_interactions, df = self$param_set$values$df)
            }, silent = TRUE)
          })

          ### Check if train is used as logging. If so calculate remaining budget:
          if (self$param_set$values$train_time_total > 0) {
            tint = proc.time() - time0
          } else {
            tint = Inf
          }

          if (self$param_set$values$train_time_total > (tint[3] / 60)) {
            if (self$param_set$values$train_time_total > 0) {
              tintuse = ceiling(tint[3] / 60)
              cboost_int$addLogger(LoggerTime, TRUE, "minutes", self$param_set$values$train_time_total - tintuse, "minutes")
            } else {
              cboost_int$addLogger(LoggerTime, FALSE, "minutes", 0, "minutes")
              tintuse = Inf
            }

            ### Train interaction model:
            if (self$param_set$values$show_output) {
              cboost_int$train(self$param_set$values$iters_max_interactions)
            } else {
              nuisance = capture.output(cboost_int$train(self$param_set$values$iters_max_interactions))
            }

            ld = cboost_int$getLoggerData()
            was_early_stopped = (max(ld[["_iterations"]]) < self$param_set$values[["iters_max_interactions"]]) &&
              (max(ld[["minutes"]]) < tintuse)
            if (was_early_stopped && (cboost_int$getCurrentIteration() > (self$param_set$values$stop_patience + 2)))
              cboost_int$train(cboost_int$getCurrentIteration() - self$param_set$values$stop_patience - 1)

            ### Post check if model was really trained on the same data::
            ch1 = all.equal(cboost_uni$data, cboost_int$data)
            if (!ch1) stop("Check failed! Data for both models is not equal!")

            if (self$param_set$values$use_early_stopping) {
              ch2 = all.equal(cboost_uni$response_oob$getResponse(), cboost_int$response_oob$getResponse())
              if (!ch2) stop("Check failed! Response for both models is not equal!")
            }
            out[["interactions"]] = cboost_int

            if (self$param_set$values$add_rf) {
              # Access predictions:
              pred_inbag = cboost_int$response$getPrediction()
              if (self$param_set$values$use_early_stopping) {
                pred_oob   = cboost_int$response_oob$getPrediction()
              }
              # Calculate pseudo residuals from the fitted univariate model:
              res1 = as.vector(loss$calculatePseudoResiduals(cboost_int$response$getResponse(), pred_inbag))
              if (self$param_set$values$use_early_stopping) {
                res2 = as.vector(loss$calculatePseudoResiduals(cboost_int$response_oob$getResponse(), pred_oob))
              }
              # Reconstruct the order of the original data to correctly add pseudo residuals:
              idx_re = order(c(train_idx, test_idx))
              res = c(res1, res2)[idx_re]

              # Define new task with 'residuals' as target
              df_new$residuals = res
              tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")

              lrn = lrn("regr.ranger")
              lrn$train(tsk_new, row_ids = train_idx)

              if (self$param_set$values$use_early_stopping) {
                rf_pred = lrn$predict(tsk_new, row_ids = test_idx)
                oob_loss_final = mean(log(1 + exp(-2 * cboost_int$response_oob$getResponse() * pred_oob + cbind(rf_pred$response))))
              } else {
                rf_pred = NULL
                oob_loss_final = NULL
              }
              out[["rf"]] = list(lrn = lrn, oob_pred = rf_pred, oob_loss_final = oob_loss_final)
            }
          }
        }
      }
      return(out)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      lin_pred = self$model$univariat$predict(newdata)
      if (! is.null(self$model$interactions))
        lin_pred = lin_pred + self$model$interactions$predict(newdata)
      if (! is.null(self$model$rf)) {
        df_new = task$data()
        df_new$residuals = 0
        tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")
        lin_pred = lin_pred + self$model$rf$lrn$predict(tsk_new)$response
      }

      probs = 1 / (1 + exp(-lin_pred))

      pos = self$model$univariat$response$getPositiveClass()
      neg = setdiff(names(self$model$univariat$response$getClassTable()), pos)
      pmat = matrix(c(probs, 1 - probs), ncol = 2L, nrow = length(probs))
      colnames(pmat) = c(pos, neg)
      if (self$predict_type == "prob") {
        list(prob = pmat)
      }
      if (self$predict_type == "response") {
        list(response = ifelse(probs > self$model$univariat$response$getThreshold(), pos, neg))
      } else {
        list(prob = pmat)
      }
    }
  )
)



if (FALSE) {

devtools::load_all()

library(ggplot2)
library(dplyr)
library(tidyr)
library(mlr3extralearners)
#devtools::install_github("zeehio/facetscales")

cboost_pars = list("classif.compboost",
  predict_type = "prob", df = 6, show_output = TRUE, top_interactions = 0.02,
  learning_rate_univariate = 0.01, learning_rate_interactions = 0.05,
  train_time_total = 5,
  iters_max_univariat = 50000L, iters_max_interactions = 50000L,
  n_knots_univariat = 10, n_knots_interactions = 10,
  use_early_stopping = TRUE, stop_patience = 10L, stop_epsylon_for_break = 1e-6)

#lr = do.call(lrn, c(cboost_pars, id = "cboost"))
#lr$train(tsk("sonar"))
#lr$predict(tsk("sonar"))
#microbenchmark::microbenchmark(
  #compboost = compboost::boostSplines(tsk("sonar")$data(), target = "Class", loss = LossBinomial$new(), iterations = 1000, bin_root = 2L,
    #n_knots = 10L, cache_type = "cholesky", df = 4L),
  #times = 10L
#)

lr_wrf = do.call(lrn, c(cboost_pars, add_rf = TRUE, id = "cboost with rf"))
#lr_wrf$train(tsk("sonar"))
#lr_wrf$predict(tsk("sonar"))

lr_uni = do.call(lrn, c(cboost_pars, just_univariat = TRUE, id = "cboost univariat"))

options("mlr3.debug" = TRUE)

task = tsk("spam")
grid2 = benchmark_grid(task,
  list(
    lr_uni, lr, lr_wrf,
    lrn("classif.ranger", predict_type = "prob", id = "ranger"),
    lrn("classif.log_reg", predict_type = "prob", id = "logistic regression"),
    lrn("classif.gamboost", predict_type = "prob", mstop = 5000, id = "gamboost"),
    lrn("classif.cv_glmnet", predict_type = "prob", id = "glmnet")
  ), rsmp("cv", folds = 10L))
bm = benchmark(grid2)
scr = bm$score(msrs(c("classif.auc", "time_train")))


scales_y = list(
  `time_train` = scale_y_continuous(trans = "log2"),
  `classif.auc` = scale_y_continuous()
)
gg = scr %>% select(learner_id, classif.auc, time_train) %>%
  pivot_longer(cols = c("classif.auc", "time_train"), names_to = "Measure", values_to = "value") %>%
  ggplot(aes(x = learner_id, y = value, color = learner_id, fill = learner_id)) +
    geom_boxplot(alpha = 0.2) +
    ggsci::scale_color_uchicago() +
    ggsci::scale_fill_uchicago() +
    #scale_color_brewer(palette = "Set1") +
    #scale_fill_brewer(palette = "Set1") +
    labs(color = "Learner", fill = "Learner") +
    xlab("") +
    ylab("") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    facetscales::facet_grid_sc(vars(Measure), scales = list(y = scales_y))

ggsave(gg, filename = paste0(here::here(), "/temp/fig-test-bmr.pdf"))
}
