LearnerClassifCompboost = R6Class("LearnerClassifCompboost",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Create a `LearnerClassifCompboost` object.
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
          ParamDbl$new(id = "ntop_interaction", default = 10, lower = 1),
          ParamLgl$new(id = "use_early_stopping", default = TRUE),
          ParamLgl$new(id = "show_output", default = FALSE),
          ParamInt$new(id = "max_minutes_univariat", default = 0, lower = 0),
          ParamInt$new(id = "max_minutes_interaction", default = 0, lower = 0)
        )
      )
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
      ### Set params with default and user defined ones:
      pdefaults = self$param_set$default
      pars      = self$param_set$values

      self$param_set$values = mlr3misc::insert_named(pdefaults, pars)

      ### Get stop arguments and train/test indices:
      stop_args = list(eps_for_break = self$param_set$values$stop_epsylon_for_break,
        patience = self$param_set$values$stop_patience)
      test_idx  = as.integer(sample(seq_len(task$nrow), trunc(task$nrow * self$param_set$values$val_fraction)))
      train_idx = setdiff(seq_len(task$nrow), test_idx)

      optimizer = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)

      ### Define compboost model for univariate features:
      loss = compboost::LossBinomial$new()
      cboost_uni = Compboost$new(data = task$data(), target = task$target_names, loss = loss,
        learning_rate = self$param_set$values$learning_rate_univariat, optimizer = optimizer, test_idx = test_idx,
        stop_args = stop_args, use_early_stopping = self$param_set$values$use_early_stopping)

      ### If a maximum time is given, the logger is used for stopping. Otherwise the time is just logged:
      if (self$param_set$values$max_minutes_univariat > 0) {
        cboost_uni$addLogger(LoggerTime, TRUE, "minutes", self$param_set$values$max_minutes_univariat, "minutes")
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
        (max(ld[["minutes"]]) < self$param_set$values[["max_minutes_univariat"]])
      if (was_early_stopped)
        cboost_uni$train(cboost_uni$getCurrentIteration() - self$param_set$values$stop_patience - 1)

      out = list()
      out[["univariat"]] = cboost_uni

      ### Create new task for interaction detection:
      df_new = task$data()
      df_new[[task$target_names]] = NULL

      # Access predictions:
      pred_inbag = cboost_uni$response$getPrediction()
      pred_oob   = cboost_uni$response_oob$getPrediction()

      # Calculate pseudo residuals from the fitted univariate model:
      res1 = as.vector(loss$calculatePseudoResiduals(cboost_uni$response$getResponse(), pred_inbag))
      res2 = as.vector(loss$calculatePseudoResiduals(cboost_uni$response_oob$getResponse(), pred_oob))

      # Reconstruct the order of the original data to correctly add pseudo residuals:
      idx_re = order(c(train_idx, test_idx))
      res = c(res1, res2)[idx_re]

      # Define new task with 'residuals' as target
      df_new$residuals = res
      tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")

      ### Extract interactions based on random forest:
      extracted_interactions = na.omit(po("extract_interactions", degree = 2)$train(list(tsk_new))$output)

      if (nrow(extracted_interactions) > 0) {
        ### Just use the top interactions (defined by the user, too much interactions makes the model too slow):
        top_interactions = seq_len(self$param_set$values$ntop_interaction)

        ### Define optimizer and loss with predictions as offset to continue training instead of
        ### start from all over again:
        optimizer_int  = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)
        loss_int_inbag = compboost::LossBinomial$new(pred_inbag, TRUE)
        loss_int_oob   = compboost::LossBinomial$new(pred_oob, TRUE)

        ### Define interaction model based on tensor splines:
        cboost_int = Compboost$new(data = task$data(), target = task$target_names, loss = loss_int_inbag,
          learning_rate = self$param_set$values$learning_rate_interactions, optimizer = optimizer_int, test_idx = test_idx,
          stop_args = c(stop_args, loss_oob = loss_int_oob), use_early_stopping = self$param_set$values$use_early_stopping)

        ### If a maximum time is given, the logger is used for stopping. Otherwise the time is just logged:
        if (self$param_set$values$max_minutes_interaction > 0) {
          cboost_int$addLogger(LoggerTime, TRUE, "minutes", self$param_set$values$max_minutes_interaction, "minutes")
        } else {
          cboost_int$addLogger(LoggerTime, FALSE, "minutes", 0, "minutes")
        }

        ### Add tensor splines. (FIXME: Atm just for numeric-numeric interactions):
        nuisance = lapply(top_interactions, function(i) {
           #Check if numeric! Interactions between ridge and spline needs to be tested first!
          e = try({
            cboost_int$addTensor(extracted_interactions$feat1[i], extracted_interactions$feat2[i], "tensor",
              n_knots = self$param_set$values$n_knots_interactions, df = self$param_set$values$df)
          }, silent = TRUE)
        })

        ### Train interaction model:
        if (self$param_set$values$show_output) {
          cboost_int$train(self$param_set$values$iters_max_interactions)
        } else {
          nuisance = capture.output(cboost_int$train(self$param_set$values$iters_max_interactions))
        }

        ### Post check if model was really trained on the same data::
        ch1 = all.equal(cboost_uni$data, cboost_int$data)
        ch2 = all.equal(cboost_uni$response_oob$getResponse(), cboost_int$response_oob$getResponse())
        if (!ch1) stop("Check failed! Data for both models is not equal!")
        if (!ch2) stop("Check failed! Response for both models is not equal!")

        out[["interactions"]] = cboost_int
      }
      return(out)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      lin_pred = self$model$univariat$predict(newdata)
      if (! is.null(self$model$interactions))
        lin_pred = lin_pred + self$model$interactions$predict(newdata)

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
mlr_learners$add("classif.compboost", LearnerClassifCompboost)


#devtools::load_all()
##remotes::install_github("schalkdaniel/compboost@tensors")


#lr1 = lrn("classif.compboost", stop_patience = 10L, stop_epsylon_for_break = 1e-5, learning_rate_univariat = 0.01,
  #learning_rate_interactions = 0.1, max_minutes_univariat = 2L, max_minutes_interaction = 2L, show_output = TRUE,
  #predict_type = "prob", ntop_interaction = 20L, iters_max_univariat = 20000L,  iters_max_interactions = 20000L)

#lr1$train(tsk("sonar"))
#pred = lr1$predict(tsk("sonar"))
#pred$confusion
#pred$score(msr("classif.auc"))

#inbag1 = lr1$model$univariat$getInbagRisk()
#inbag2 = lr1$model$interactions$getInbagRisk()
#oob1   = lr1$model$univariat$getLoggerData()$oob_risk
#oob2   = lr1$model$interactions$getLoggerData()$oob_risk
#cutoff = lr1$model$univariat$getCurrentIteration()

#lr1$model$univariat$plotBlearnerTraces(n_legend = 20L)
#lr1$model$interactions$plotBlearnerTraces()

#yrg = c(min(inbag2, oob2), max(inbag1, oob1))
#plot(c(inbag1, inbag2), type = "l", col = "red", ylim = yrg)
#lines(c(oob1, oob2), col = "blue")
#abline(v = cutoff, lty = 2, col = "dark grey")
#legend("topright", lty = 1, col = c("red", "blue"), legend = c("Train risk", "Validation risk"))
#text(x = cutoff + 0.01 * (length(c(inbag1, inbag2))), y = max(c(inbag1, oob1)) - 0.05 * (yrg[2] - yrg[1]),
  #labels = "Switch from univariate\nto interaction model", adj = c(0, 0))
