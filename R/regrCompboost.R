LearnerRegrCompboost = R6Class("LearnerRegrCompboost",
  inherit = LearnerRegr,
  public = list(

    #' @description
    #' Create a `LearnerRegrCompboost` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "df", default = 4, lower = 1),
          ParamDbl$new(id = "df_cat", default = 4, lower = 1),
          ParamInt$new(id = "iters_max_univariat", default = 10000L, lower = 1L),
          ParamInt$new(id = "iters_max_interactions", default = 10000L, lower = 1L),
          ParamDbl$new(id = "learning_rate_univariat", default = 0.01, lower = 0),
          ParamDbl$new(id = "learning_rate_interactions", default = 0.1, lower = 0),
          ParamDbl$new(id = "n_knots_univariat", default = 20L, lower = 4),
          ParamDbl$new(id = "n_knots_interactions", default = 10L, lower = 4),
          ParamInt$new(id = "ncores", default = 1L, lower = 1L, upper = parallel::detectCores() - 1L),
          ParamDbl$new(id = "stop_epsylon_for_break", default = 0.00001, lower = 0, upper = 1),
          ParamDbl$new(id = "stop_patience", default = 10L, lower = 1L),
          ParamDbl$new(id = "val_fraction", default = 0.33, lower = 0, upper = 1),
          ParamDbl$new(id = "top_interaction", default = 0.02, lower = 0.01, upper = 1),
          ParamLgl$new(id = "use_early_stopping", default = TRUE),
          ParamLgl$new(id = "show_output", default = FALSE),
          ParamLgl$new(id = "just_univariat", default = FALSE),
          ParamLgl$new(id = "add_rf", default = FALSE),
          ParamInt$new(id = "train_time_total", default = 0, lower = 0)
        ))
      ps$values = list(df = 6, show_output = FALSE, top_interaction = 0.02, learning_rate_univariat = 0.01,
        learning_rate_interactions = 0.05, train_time_total = 10, iters_max_univariat = 50000L,
        iters_max_interactions = 50000L, n_knots_univariat = 15, n_knots_interactions = 8,
        use_early_stopping = TRUE, stop_patience = 10L, stop_epsylon_for_break = 1e-6)

      super$initialize(
        id = "regr.compboost",
        packages = "compboost",
        feature_types = c("numeric", "factor", "integer", "character"),
        predict_types = "response",
        param_set = ps
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
      loss = compboost::LossQuadratic$new()
      cboost_uni = Compboost$new(data = task$data(), target = task$target_names, loss = loss,
        learning_rate = self$param_set$values$learning_rate_univariat, optimizer = optimizer, test_idx = test_idx,
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
        ntopinteractions = ceiling(ninteractions * self$param_set$values$top_interaction)
        if (ntopinteractions > 0) {
          ### Just use the top interactions (defined by the user, too much interactions makes the model too slow):
          top_interactions = seq_len(ntopinteractions)

          ### Define optimizer and loss with predictions as offset to continue training instead of
          ### start from all over again:
          optimizer_int  = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)
          loss_int_inbag = compboost::LossQuadratic$new(pred_inbag, TRUE)
          if (self$param_set$values$use_early_stopping) {
            loss_int_oob   = compboost::LossQuadratic$new(pred_oob, TRUE)
          } else {
            loss_int_oob = compboost::LossQuadratic$new()
          }

          ### Define interaction model based on tensor splines:
          cboost_int = Compboost$new(data = task$data(), target = task$target_names, loss = loss_int_inbag,
            learning_rate = self$param_set$values$learning_rate_interactions, optimizer = optimizer_int, test_idx = test_idx,
            stop_args = c(stop_args, loss_oob = loss_int_oob), use_early_stopping = self$param_set$values$use_early_stopping)

          #browser()
          ### Add tensor splines. (FIXME: Atm just for numeric-numeric interactions):
          nuisance = lapply(top_interactions, function(i) {
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

      return(list(response = lin_pred))
    }
  )
)
mlr_learners$add("regr.compboost", LearnerRegrCompboost)


if (FALSE) {

devtools::load_all()

lr1 = lrn("regr.compboost",
  df = 4, show_output = TRUE, top_interaction = 0.02,
  learning_rate_univariat = 0.01, learning_rate_interactions = 0.01,
  train_time_total = 5,
  iters_max_univariat = 50000L, iters_max_interactions = 50000L,
  n_knots_univariat = 10, n_knots_interactions = 10,
  use_early_stopping = TRUE, stop_patience = 10L, stop_epsylon_for_break = 1e-7)

task = tsk("boston_housing")
lr1$train(task)
pred = lr1$predict(task)
pred$score(msrs(c("regr.mse", "regr.mae")))

lr1$model$univariat$plotBlearnerTraces(n_legend = 20L)
lr1$model$interactions$plotBlearnerTraces()

inbag1 = lr1$model$univariat$getInbagRisk()
inbag2 = lr1$model$interactions$getInbagRisk()
oob1   = lr1$model$univariat$getLoggerData()$oob_risk
oob2   = lr1$model$interactions$getLoggerData()$oob_risk
cutoff = lr1$model$univariat$getCurrentIteration()


yrg = c(min(inbag2, oob2), max(inbag1, oob1))
plot(c(inbag1, inbag2), type = "l", col = "red", ylim = yrg)
lines(c(oob1, oob2), col = "blue")
abline(v = cutoff, lty = 2, col = "dark grey")
legend("topright", lty = 1, col = c("red", "blue"), legend = c("Train risk", "Validation risk"))
text(x = cutoff + 0.01 * (length(c(inbag1, inbag2))), y = max(c(inbag1, oob1)) - 0.05 * (yrg[2] - yrg[1]),
  labels = "Switch from univariate\nto interaction model", adj = c(0, 0))
}
