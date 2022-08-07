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
    initialize = function() {
      cores_max = parallel::detectCores()
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "df", default = 4, lower = 1, tags = "train"),
          ParamDbl$new(id = "df_cat", default = 4, lower = 1, tags = "train"),
          ParamInt$new(id = "iters_max_univariate", default = 10000L, lower = 1L, tags = "train"),
          ParamInt$new(id = "iters_max_interactions", default = 10000L, lower = 1L, tags = "train"),
          # ParamDbl$new(id = "learning_rate_univariate", default = 0.01, lower = 0),
          # ParamDbl$new(id = "learning_rate_interactions", default = 0.1, lower = 0),
          ParamDbl$new(id = "n_knots_univariate", default = 20L, lower = 4, tags = "train"),
          ParamDbl$new(id = "n_knots_interactions", default = 10L, lower = 4, tags = "train"),
          ParamLgl$new(id = "use_components", default = TRUE, tags = "train"),
          ParamInt$new(id = "ncores", default = 1L, lower = 1L, upper = parallel::detectCores() - 1L, tags = c("train", "test")),
          ParamDbl$new(id = "stop_epsylon_for_break", default = 0.00001, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new(id = "stop_patience", default = 10L, lower = 1L, tags = "train"),
          ParamDbl$new(id = "val_fraction", default = 0.33, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new(id = "top_interactions", default = 0.02, lower = 0.01, upper = 1, tags = "train"),
          ParamDbl$new(id = "n_min_interactions", default = 10L, lower = 0, tags = "train"),
          ParamLgl$new(id = "use_early_stopping", default = TRUE, tags = "train"),
          ParamLgl$new(id = "show_output", default = FALSE, tags = "train"),
          ParamLgl$new(id = "just_univariate", default = FALSE, tags = "train"),
          ParamLgl$new(id = "add_deeper_interactions", default = TRUE, tags = "train"),
          ParamInt$new(id = "iters_deeper_interactions", default = 500, lower = 0, tags = "train"),
          #ParamDbl$new(id = "learning_rate_deeper_interactions", default = 0.2, lower = 0, upper = 1),
          ParamInt$new(id = "train_time_total", default = 0, lower = 0, tags = "train"),
          ParamInt$new(id = "n_threshold_binning", default = 4900, lower = 0, tags = "train"),
          ParamDbl$new(id = "learning_rate", default = 0.01, lower = 0, tags = "train")
        ))
      ps$values = list(
        # General pars:
        df = 6,
        n_threshold_binning = 4000L,
        use_components = TRUE,
        show_output = FALSE,
        learning_rate = 0.01,
        ncores = parallel::detectCores() - 1L,

        # Univariate model:
        #learning_rate_univariate = 0.1,
        n_knots_univariate = 15,
        iters_max_univariate = 10000L,

        # Interaction model (tensor splines):
        just_univariate = FALSE,
        top_interactions = 0.02,
        #learning_rate_interactions = 0.15,
        n_knots_interactions = 8,
        iters_max_interactions = 10000L,

        # Control deeper interactions (trees):
        add_deeper_interactions = TRUE,
        # iters_deeper_interactions = 500L,
        #learning_rate_deeper_interactions = 0.15,

        # Control early stopping:
        use_early_stopping = TRUE,
        stop_patience = 10L,
        stop_epsylon_for_break = 1e-6,
        train_time_total = 120
      ) # Restrict the training to 2 hours

      super$initialize(
        id = "classif.compboost",
        packages = "compboost",
        feature_types = c("numeric", "factor", "integer", "character"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass")
      )
    },

    getRiskStages = function(log_entry = "train_risk") {
      if (is.null(self$model))
        stop("Train learner first to extract risk values.")

      out = data.frame(stage = c("featureless", "univariate", "pairwise-interactions", "deep-interactions"),
        value = rep(NA_real_, 4L), explained = rep(NA_real_, 4L), percentage = rep(NA_real_, 4L),
        iterations = rep(NA_integer_, 4L))

      if ("univariate" %in% names(self$model)) {
        logs = self$model$univariate$getLoggerData()
        if (! log_entry %in% names(logs))
          stop("No log entry", log_entry, "found in compboost log.")
        out$value[1] = logs[[log_entry]][logs$baselearner == "intercept"]
        out$value[2] = tail(logs[[log_entry]], 1)
        out$iterations[1] = 0
        out$iterations[2] = max(logs[["_iterations"]])
      } else {
        stop("At least univariate model must be given!")
      }
      if ("interactions" %in% names(self$model)) {
        logs = self$model$interactions$getLoggerData()
        if (! log_entry %in% names(logs))
          stop("No log entry", log_entry, "found in compboost log.")
        out$value[3] = tail(logs[[log_entry]], 1)
        out$iterations[3] = max(logs[["_iterations"]])
      }
      if ("deeper_interactions" %in% names(self$model)) {
        vals = vapply(X = self$model$deeper_interactions$trees, FUN = function(tree) {
          if (! log_entry %in% names(tree))
            stop("Cannot find log entry", log_entry, "in tree.")
          return(tree[[log_entry]])
        }, FUN.VALUE = numeric(1L))
        if (length(vals) == 0)
          out$value[4] = NA_real_
        else
          out$value[4] = tail(vals, 1L)

        out$iterations[4] = length(vals)
      }
      out$explained = c(0, -diff(out$value))
      out$percentage = out$explained / sum(out$explained, na.rm = TRUE)
      return(out)
    }
  ),

  private = list(
    .train = function(task) {

      time0 = proc.time()
      logLoss = function(truth, pred) log(1 + exp(-2 * truth * pred))
      logRisk = function(truth, pred) mean(logLoss(truth, pred))

      ### Set params with default and user defined ones:
      pdefaults = self$param_set$default
      pars      = self$param_set$values

      self$param_set$values = mlr3misc::insert_named(pdefaults, pars)

      if (task$nrow >= self$param_set$values$n_threshold_binning)
        bin_roots = c(seq(2, 1, length.out = 4)[-4], 0)
      else
        bin_roots = 0L


      ### Get stop arguments and train/test indices:
      stop_args = list(eps_for_break = self$param_set$values$stop_epsylon_for_break,
        patience = self$param_set$values$stop_patience)

      if (self$param_set$values$use_early_stopping)
        test_idx  = as.integer(sample(seq_len(task$nrow), trunc(task$nrow * self$param_set$values$val_fraction)))
      else
        test_idx = NULL

      train_idx = setdiff(seq_len(task$nrow), test_idx)

      for (bin_root in bin_roots) {

        optimizer = OptimizerCoordinateDescent$new(self$param_set$values$ncores)

        ### Define compboost model for univariate features:
        loss = compboost::LossBinomial$new()

        cboost_uni = Compboost$new(data = task$data(), target = task$target_names,
          loss = loss, learning_rate = self$param_set$values$learning_rate,
          #loss = loss, learning_rate = self$param_set$values$learning_rate_univariate,
          optimizer = optimizer, test_idx = test_idx, stop_args = stop_args,
          use_early_stopping = self$param_set$values$use_early_stopping
        )

        ### If a maximum time is given, the logger is used for stopping. Otherwise the time is just logged:
        if (self$param_set$values$train_time_total > 0)
          cboost_uni$addLogger(LoggerTime, TRUE, "minutes", self$param_set$values$train_time_total, "minutes")
        else
          cboost_uni$addLogger(LoggerTime, FALSE, "minutes", 0, "minutes")

        ### Add base-learner/components (linear + centered spline):
        e = try({
          nuisance = lapply(task$feature_names, function(nm) {
            if (is.numeric(task$data()[[nm]])) {
              if (self$param_set$values$use_components) {
                cboost_uni$addComponents(nm, n_knots = self$param_set$values$n_knots_univariat,
                  df = self$param_set$values$df, bin_root = bin_root)
              } else {
                cboost_uni$addBaselearner(nm, "spline", BaselearnerPSpline,
                  n_knots = self$param_set$values$n_knots_univariat,
                  df = self$param_set$values$df, bin_root = bin_root)
              }
            } else {
              cboost_uni$addBaselearner(nm, "category", BaselearnerCategoricalRidge,
                df = self$param_set$values$df_cat)
            }
          })
          ### Train model:
          if (self$param_set$values$show_output)
            cboost_uni$train(self$param_set$values$iters_max_univariate)
          else
            nuisance = capture.output(cboost_uni$train(self$param_set$values$iters_max_univariate))
        }, silent = TRUE)

        if (class(e) == "try-error") {
          if (grepl("chol()", attr(e, "condition")) && (bin_root > 0)) {
            i = which(bin_root == bin_roots)
            msg = paste0("Trying to catch Cholesky decomposition error.",
              "This may appear due to too aggressive binning with a root of ",
              round(bin_root, 2))
            if (i == (length(bin_roots) - 1)) {
              msg = paste0(msg, ". Trying to fit model without binning.")
            } else {
              msg = paste0(msg, ". Now trying with a smaller root of ", round(bin_roots[i+1]), ".")
            }
            warning(msg)
          } else {
            stop(e)
          }
        } else {
          break
        }
      }

      out = list()
      out[["univariate"]] = cboost_uni

      if (self$param_set$values$just_univariate) return(out)

      ### Create new task for interaction detection:
      df_new = task$data()
      df_new[[task$target_names]] = NULL

      # Access predictions:
      bin_response = ifelse(task$data()[[task$target_names]] == cboost_uni$response$getPositiveClass(), 1, -1)
      pred_uni     = cboost_uni$predict(task$data())
      pseudo_uni   = as.vector(loss$calculatePseudoResiduals(cbind(bin_response), pred_uni))

      # Define new task with 'residuals' as target
      df_new$residuals = pseudo_uni
      tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")

      ### Extract interactions based on random forest:
      extracted_interactions = na.omit(po("extract_interactions", degree = 2)$train(list(tsk_new))$output)

      ninteractions = nrow(extracted_interactions)
      ntopinteractions = ceiling(ninteractions * self$param_set$values$top_interactions)
      if (ntopinteractions < self$param_set$values$n_min_interactions)
        ntopinteractions = min(self$param_set$values$n_min_interactions, ninteractions)

      ### STOP if no interactions were detected:
      if (ntopinteractions == 0) {
        if (self$param_set$values$show_output)
          warning("No interactions were selected! Algorithm is now stopped!")
        return(out)
      }

      ### Just use the top interactions (defined by the user, too much interactions makes the model too slow):
      top_interactions = seq_len(ntopinteractions)

      ### Define optimizer and loss with predictions as offset to continue training instead of
      ### start from all over again:
      optimizer_int  = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)
      loss_int_inbag = compboost::LossBinomial$new(cbind(pred_uni[train_idx]), TRUE)

      if (self$param_set$values$use_early_stopping)
        loss_int_oob = compboost::LossBinomial$new(cbind(pred_uni[test_idx]), TRUE)
      else
        loss_int_oob = compboost::LossBinomial$new()

      ### Define interaction model based on tensor splines:
      cboost_int = Compboost$new(data = task$data(), target = task$target_names, loss = loss_int_inbag,
        #learning_rate = self$param_set$values$learning_rate_interactions, optimizer = optimizer_int,
        learning_rate = self$param_set$values$learning_rate, optimizer = optimizer_int,
        test_idx = test_idx, stop_args = c(stop_args, loss_oob = loss_int_oob),
        use_early_stopping = self$param_set$values$use_early_stopping)

      ### Add tensor splines. (FIXME: Atm just for numeric-numeric interactions):
      nuisance = lapply(top_interactions, function(i) {
         #Check if numeric! Interactions between ridge and spline needs to be tested first!
        e = try({
          cboost_int$addTensor(extracted_interactions$feat1[i], extracted_interactions$feat2[i],
            n_knots = self$param_set$values$n_knots_interactions, df = self$param_set$values$df)
        }, silent = TRUE)
      })

      ### Check if train is used as logging. If so calculate remaining budget:
      if (self$param_set$values$train_time_total > 0)
        tint = proc.time() - time0
      else
        tint = -Inf

      ### STOP if time is exhausted:
      if (self$param_set$values$train_time_total <= (tint[3] / 60)) return(out)

      if (self$param_set$values$train_time_total > 0) {
        tintuse = ceiling(tint[3] / 60)
        cboost_int$addLogger(LoggerTime, TRUE, "minutes", self$param_set$values$train_time_total - tintuse, "minutes")
      } else {
        cboost_int$addLogger(LoggerTime, FALSE, "minutes", 0, "minutes")
        tintuse = Inf
      }

      if (length(cboost_int$getBaselearnerNames()) > 0) {
        ### Train interaction model:
        if (self$param_set$values$show_output)
          cboost_int$train(self$param_set$values$iters_max_interactions)
        else
          nuisance = capture.output(cboost_int$train(self$param_set$values$iters_max_interactions))

        ### Post check if model was really trained on the same data::
        ch1 = all.equal(cboost_uni$data, cboost_int$data)
        if (!ch1) stop("Check failed! Data for both models is not equal!")

        if (self$param_set$values$use_early_stopping) {
          ch2 = all.equal(cboost_uni$response_oob$getResponse(), cboost_int$response_oob$getResponse())
          if (!ch2) stop("Check failed! Response for both models is not equal!")
        }
        out[["interactions"]] = cboost_int
      } else {
        warning("No interactions were included! Cannot train interaction model.")
      }

      ### STOP if no deeper interactions are specified:
      if (! self$param_set$values$add_deeper_interactions) return(out)

      # Calculate pseudo residuals from the fitted univariate model:
      pred_int = cboost_uni$predict(task$data())
      if (! is.null(cboost_int$model))
        pred_int = pred_int + cboost_int$predict(task$data())

      pseudo_int = as.vector(loss$calculatePseudoResiduals(cbind(bin_response), pred_int))

      # Define new task with 'residuals' as target
      df_new$residuals = pseudo_int
      tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")

      out[["deeper_interactions"]] = distilledRF(task = tsk_new,
        max_time = self$param_set$values$train_time_total - (proc.time() - time0)[3] / 60
      )

      #residual_booster = boostRpart(tsk_new, lr = self$param_set$values$learning_rate_deeper_interactions,
      # residual_booster = boostRpart(tsk_new, lr = self$param_set$values$learning_rate,
        # iters = self$param_set$values$iters_deeper_interactions,
        # patience = stop_args$patience, eps_for_break = stop_args$eps_for_break,
        # use_es = self$param_set$values$use_early_stopping, idx_train = train_idx,
        # idx_test = test_idx, logRisk = logRisk, binary_response = bin_response,
        # prediction_offset = pred_int, pseudoResiduals = loss$calculatePseudoResiduals,
        # max_time = self$param_set$values$train_time_total - (proc.time() - time0)[3] / 60)
#
      # out[["deeper_interactions"]] = residual_booster
      # browser()
      return(out)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      lin_pred = self$model$univariate$predict(newdata)
      if (! is.null(self$model$interactions))
        lin_pred = lin_pred + self$model$interactions$predict(newdata)
      # browser()
      if (! is.null(self$model$deeper_interactions)) {
        df_new = task$data()
        df_new$residuals = 0
        tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")
        lin_pred = lin_pred + predict(self$model$deeper_interactions$model, tsk_new$data(cols = tsk_new$feature_names))
        # lin_pred = lin_pred + self$model$deeper_interactions$trees$model$predict(tsk_new)$response
      }

      probs = 1 / (1 + exp(-lin_pred))

      pos = self$model$univariate$response$getPositiveClass()
      neg = setdiff(names(self$model$univariate$response$getClassTable()), pos)
      pmat = matrix(c(probs, 1 - probs), ncol = 2L, nrow = length(probs))
      colnames(pmat) = c(pos, neg)

      if (self$predict_type == "prob")
        list(prob = pmat)

      if (self$predict_type == "response")
        list(response = ifelse(probs > self$model$univariate$response$getThreshold(), pos, neg))
      else
        list(prob = pmat)
    }
  )
)



if (FALSE) {

devtools::load_all()

#library(mlr3extralearners)
#devtools::install_github("zeehio/facetscales")
#devtools::install("~/repos/compboost")

cboost_pars = list("classif.compboost",
  predict_type = "prob", df = 6, show_output = TRUE, top_interactions = 0.02,
  learning_rate_univariate = 0.01, learning_rate_interactions = 0.05,
  train_time_total = 5,
  iters_max_univariate = 2500L, iters_max_interactions = 2500L,
  n_knots_univariate = 10, n_knots_interactions = 10,
  use_early_stopping = TRUE, stop_patience = 10L, stop_epsylon_for_break = 1e-6,
  ncores = 4)

task = tsk("spam")

lr = lrn("classif.compboost", id = "cboost",
  predict_type = "prob", df = 6, show_output = TRUE, top_interactions = 0.02,
  learning_rate_univariate = 0.01, learning_rate_interactions = 0.05,
  train_time_total = 5,
  iters_max_univariate = 50000L, iters_max_interactions = 50000L,
  n_knots_univariate = 10, n_knots_interactions = 10,
  use_early_stopping = TRUE, stop_patience = 6L, stop_epsylon_for_break = 1e-6,
  ncores = 6)
lr$train(task)

lr = do.call(lrn, c(cboost_pars, id = "cboost"))
lr_wrf = do.call(lrn, c(cboost_pars, add_deeper_interactions = TRUE, id = "cboost with rf"))
lr_uni = do.call(lrn, c(cboost_pars, just_univariate = TRUE, id = "cboost univariate"))

options("mlr3.debug" = TRUE)

#task = tsk("oml", task_id = 359994)
grid2 = benchmark_grid(task,
  list(
    lr_uni, lr, lr_wrf,
    lrn("classif.ranger", predict_type = "prob", id = "ranger"),
    lrn("classif.log_reg", predict_type = "prob", id = "logistic regression"),
    lrn("classif.gamboost", predict_type = "prob", mstop = 5000, id = "gamboost"),
    lrn("classif.cv_glmnet", predict_type = "prob", id = "glmnet")
  ), rsmp("cv", folds = 10L))
bm = benchmark(grid2)


vizBMR = function(bm) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)

  scales_y = list(
    `time_train` = scale_y_continuous(trans = "log2"),
    `classif.auc` = scale_y_continuous()
  )
  bm$score(msrs(c("classif.auc", "time_train"))) %>%
    select(learner_id, classif.auc, time_train) %>%
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
}

ggsave(vizBMR(bm), filename = paste0(here::here(), "/temp/fig-test-bmr.pdf"))




devtools::load_all()

cboost_pars = list("classif.compboost",
  predict_type = "prob", df = 6, show_output = TRUE, top_interactions = 0.02,
  n_min_interactions = 5L, train_time_total = 5, use_components = FALSE,
  learning_rate_univariate = 0.01, learning_rate_interactions = 0.05,
  iters_max_univariat = 50000L, iters_max_interactions = 50000L,
  n_knots_univariat = 10, n_knots_interactions = 10,
  use_early_stopping = TRUE, stop_patience = 10L, stop_epsylon_for_break = 1e-8)

task = tsk("sonar")

#lr_wrf = do.call(lrn, c(cboost_pars, add_deeper_interactions = TRUE, id = "cboost with rf"))
#lr_wrf$train(task)

mstop = 5000L
lrc = lrn("classif.compboost", id = "cboost components", predict_type = "prob", just_univariat = TRUE,
  use_early_stopping = FALSE, iters_max_univariat = mstop, df = 4, n_knots_univariat = 20L, show_output = TRUE,
  learning_rate_univariate = 0.1, ncores = 1)
#lrc$train(task)

lrc_mc = lrn("classif.compboost", id = "cboost components multicore", predict_type = "prob", just_univariat = TRUE,
  use_early_stopping = FALSE, iters_max_univariat = mstop, df = 4, n_knots_univariat = 20L, show_output = TRUE,
  learning_rate_univariate = 0.1, ncores = 7)
lrc_mc$train(task)

lrs = lrn("classif.compboost", id = "cboost splines", predict_type = "prob", use_components = FALSE,
  just_univariat = TRUE, use_early_stopping = FALSE, iters_max_univariat = mstop, df = 4,
  n_knots_univariat = 20L, show_output = TRUE, learning_rate_univariate = 0.1, ncores = 1)
lrs$train(task)

lrs_mc = lrn("classif.compboost", id = "cboost splines multicore", predict_type = "prob", use_components = FALSE,
  just_univariat = TRUE, use_early_stopping = FALSE, iters_max_univariat = mstop, df = 4,
  n_knots_univariat = 20L, show_output = TRUE, learning_rate_univariate = 0.1, ncores = 7)
lrs_mc$train(task)

library(mlr3extralearners)
lr_gb = lrn("classif.gamboost", predict_type = "prob", mstop = mstop, id = "gamboost")

options("mlr3.debug" = TRUE)
grid = benchmark_grid(task, list(lrc, lrc_mc, lrs, lrs_mc, lr_gb), rsmp("subsampling", repeats = 3, ratio = 0.9))
bm = benchmark(grid)
vizBMR(bm)



devtools::load_all()

lr_wrf = lrn("classif.compboost", id = "cboost",
  predict_type = "prob", df = 6, show_output = TRUE, top_interactions = 0.02,
  learning_rate_univariate = 0.01, learning_rate_interactions = 0.01,
  train_time_total = 5, add_deeper_interactions = TRUE,
  iters_max_univariat = 50000L, iters_max_interactions = 50000L,
  n_knots_univariat = 10, n_knots_interactions = 8,
  use_early_stopping = TRUE, stop_patience = 4L, stop_epsylon_for_break = 1e-7,
  ncores = 4, just_univariat = FALSE, learning_rate_deeper_interactions = 0.01,
  iters_deeper_interactions = 1000L, n_threshold_binning = 100)

task = tsk("sonar")
lr_wrf$train(task)
pred = lr_wrf$predict(task)
pred$score(msr("classif.auc"))

inbag1 = lr_wrf$model$univariat$getInbagRisk()
inbag2 = lr_wrf$model$interactions$getInbagRisk()
inbag3 = sapply(lr_wrf$model$deeper_interactions$trees, function(x) x$train_risk)
if (length(inbag3) == 0) inbag3 = NA

oob1   = lr_wrf$model$univariat$getLoggerData()$oob_risk
oob2   = lr_wrf$model$interactions$getLoggerData()$oob_risk
oob3 = sapply(lr_wrf$model$deeper_interactions$trees, function(x) x$test_risk)
if (length(oob3) == 0) oob3 = NA

cutoff = length(inbag1)
cutoff2 = cutoff + length(inbag2)

rall = na.omit(c(inbag1, inbag2, inbag3, oob1, oob2, oob3))
yrg = c(min(rall), max(rall))
xrg = c(0, length(c(oob1, oob2, oob3)))

plot(c(inbag1, inbag2, inbag3), type = "l", col = "red", ylim = yrg, xlim = xrg)
lines(c(oob1, oob2, oob3), col = "blue")
abline(v = c(cutoff, cutoff2), lty = 2, col = "dark grey")
legend("bottomleft", lty = 1, col = c("red", "blue"), legend = c("Train risk", "Validation risk"))
text(x = c(cutoff, cutoff2) + 0.01 * (length(c(inbag1, inbag2))), y = max(c(inbag1, oob1)) - 0.05 * (yrg[2] - yrg[1]),
  labels = c("Switch from univariate\nto interaction model", "Switch to deep\ninteractions"), adj = c(0, 0))


q()
R
#devtools::install("~/repos/compboost")
devtools::load_all()

#task = tsk("spam")
task = tsk("oml", task_id = 359994)

mstop = 100L

lr_bin = lrn("classif.compboost", predict_type = "prob", just_univariate = TRUE,
  use_components = TRUE, n_threshold_binning = 0, show_output = TRUE,
  iters_max_univariate = mstop, use_early_stopping = FALSE, ncores = 4)
#lr$train(task)

lr_nobin = lrn("classif.compboost", predict_type = "prob", just_univariate = TRUE,
  use_components = TRUE, n_threshold_binning = 40000000, show_output = TRUE,
  iters_max_univariate = mstop, use_early_stopping = FALSE, ncores = 4)
#lr0$train(task)

mb = microbenchmark::microbenchmark(lr_bin$train(task), lr_nobin$train(task), times = 3L)
mb

}
