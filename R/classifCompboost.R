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
          ParamDbl$new(id = "learning_rate", default = 0.1, lower = 0),
          ParamDbl$new(id = "n_knots", default = 20L, lower = 4),
          ParamInt$new(id = "ncores", default = 1L, lower = 1L, upper = parallel::detectCores() - 1L),
          ParamDbl$new(id = "stop_epsylon_for_break", default = 0.00001, lower = 0, upper = 1),
          ParamDbl$new(id = "stop_patience", default = 10L, lower = 1L),
          ParamDbl$new(id = "val_fraction", default = 0.33, lower = 0, upper = 1),
          ParamDbl$new(id = "ntop_interaction", default = 10, lower = 1),
          ParamLgl$new(id = "use_early_stopping", default = TRUE),
          ParamLgl$new(id = "show_output", default = FALSE)
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
      pdefaults = self$param_set$default
      pars = self$param_set$values
      for (id in self$param_set$ids()) {
        if (is.null(pars[[id]])) pars[[id]] = pdefaults[[id]]
      }
      self$param_set$values = pars

      stop_args = list(eps_for_break = self$param_set$values$stop_epsylon_for_break,
        patience = self$param_set$values$stop_patience)
      test_idx  = as.integer(sample(seq_len(task$nrow), trunc(task$nrow * self$param_set$values$val_fraction)))
      train_idx = setdiff(seq_len(task$nrow), test_idx)

      optimizer = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)

      cboost_uni = Compboost$new(data = task$data(), target = task$target_names, loss = compboost::LossBinomial$new(),
        learning_rate = self$param_set$values$learning_rate, optimizer = optimizer, test_idx = test_idx,
        stop_args = stop_args, use_early_stopping = self$param_set$values$use_early_stopping)

      nuisance = lapply(task$feature_names, function(nm) {
        if (is.numeric(task$data()[[nm]])) {
          cboost_uni$addComponents(nm, n_knots = self$param_set$values$n_knots, df = self$param_set$values$df)
        } else {
          cboost_uni$addBaselearner(nm, "category", BaselearnerCategoricalRidge, df = self$param_set$values$df_cat)
        }
      })
      if (self$param_set$values$show_output) {
        cboost_uni$train(self$param_set$values$iters_max_univariat)
      } else {
        nuisance = capture.output(cboost_uni$train(self$param_set$values$iters_max_univariat))
      }

      out = list()
      out[["univariat"]] = cboost_uni

      df_new = task$data()
      df_new[[task$target_names]] = NULL

      pred_inbag = cboost_uni$response$getPrediction()
      pred_oob   = cboost_uni$response_oob$getPrediction()

      res1 = as.vector(cboost_uni$response$getResponse() - pred_inbag)
      res2 = as.vector(cboost_uni$response_oob$getResponse() - pred_oob)

      idx_re = order(c(train_idx, test_idx))
      res = c(res1, res2)[idx_re]

      ### Check if data was processed correctly:
      #ch_data = rbind(cboost_uni$data, cboost_uni$data_oob)[idx_re, ]
      #ch_datao = as.data.frame(task$data())
      #all.equal(ch_data, ch_datao[, task$feature_names])

      df_new$residuals = res
      tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")
      extracted_interactions = na.omit(po("extract_interactions", degree = 2)$train(list(tsk_new))$output)

      if (nrow(extracted_interactions) > 0) {
        #int_filter = cumsum(extracted_interactions$count) / sum(extracted_interactions$count)
        top_interactions = seq_len(self$param_set$values$ntop_interaction)

        optimizer_int  = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)
        loss_int_inbag = compboost::LossBinomial$new(pred_inbag, TRUE)
        loss_int_oob   = compboost::LossBinomial$new(pred_oob, TRUE)

        cboost_int = Compboost$new(data = task$data(), target = task$target_names, loss = loss_int_inbag,
          learning_rate = self$param_set$values$learning_rate, optimizer = optimizer_int, test_idx = test_idx,
          stop_args = c(stop_args, loss_oob = loss_int_oob), use_early_stopping = self$param_set$values$use_early_stopping)

        nuisance = lapply(top_interactions, function(i) {
           #Check if numeric! Interactions between ridge and spline needs to be tested first!
          e = try({
            cboost_int$addTensor(extracted_interactions$feat1[i], extracted_interactions$feat2[i], "tensor",
              n_knots = self$param_set$values$n_knots, df = self$param_set$values$df)
          }, silent = TRUE)
        })
        if (self$param_set$values$show_output) {
          cboost_int$train(self$param_set$values$iters_max_interactions)
        } else {
          nuisance = capture.output(cboost_int$train(self$param_set$values$iters_max_interactions))
        }
        ### Post check:
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

#lr1 = lrn("classif.compboost", n_knots = 10L, stop_patience = 8L, show_output = TRUE, predict_type = "prob")
#lr1$train(tsk("sonar"))
#pred = lr1$predict(tsk("sonar"))
#pred$confusion
#pred$score(msr("classif.auc"))
