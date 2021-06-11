LearnerClassifCompboost = R6Class("LearnerClassifCompboost",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Create a `LearnerClassifCompboost` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "df", default = 5, lower = 1),
          ParamDbl$new(id = "df_cat", default = 1, lower = 1),
          ParamInt$new(id = "iters_univariat", default = 100L, lower = 1L),
          ParamInt$new(id = "iters_interactions", default = 100L, lower = 1L),
          ParamDbl$new(id = "learning_rate", default = 0.05, lower = 0),
          ParamDbl$new(id = "n_knots", default = 20L, lower = 4),
          ParamInt$new(id = "ncores", default = 1L, lower = 1L, upper = parallel::detectCores())
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

      browser()

      pdefaults = self$param_set$default
      pars = self$param_set$values
      for (id in self$param_set$ids()) {
        if (is.null(pars[[id]])) pars[[id]] = pdefaults[[id]]
      }
      self$param_set$values = pars

      optimizer = compboost::OptimizerCoordinateDescent$new(self$param_set$values$ncores)

      cboost_uni = Compboost$new(data = task$data(), target = task$target_names, loss = compboost::LossBinomial$new(),
        learning_rate = self$param_set$values$learning_rate, optimizer = optimizer)

      nuisance = lapply(names(task$data()), function(nm) {
        if (is.numeric(task$data()[[nm]])) {
          cboost_uni$addComponent(nm, n_knots = self$param_set$values$n_knots, df = self$param_set$values$df)
        } else {
          model$addBaselearner(nm, "category", BaselearnerCategoricalRidge, df = self$param_set$values$df)
        }
      })
      nuisance = capture.output({ cboost_uni$train(self$param_set$values$iters_univariat) })

      df_new = task$data()
      df_new[[task$target_names]] = NULL
      df_new$residuals = cboost_uni$response$getResponse() - cboost_uni$response$getPrediction()
      tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")

      extracted_interactions = po("extract_interactions", degree = 2)$train(list(task))$output
      loss_interactions = compboost::LossBinomial$new(cboost_uni$predict(), TRUE)

      ### TODO: second compboost object:
      #cboost_int = Compboost$new(data = task$data(), target = task$target_names, loss = loss_interactions,
        #learning_rate = self$param_set$values$learning_rate, optimizer = optimizer)
      #nuisance = lapply(extracted_interactions, function(nm) {
        # Check if numeric! Interactions between ridge and spline needs to be tested first!
        #cboost_uni$addTensor(nm[1], nm[2], "tensor", n_knots = self$param_set$values$n_knots, df = self$param_set$values$df)
      #})
      #nuisance = capture.output({ cboost_uni$train(self$param_set$values$iters_interactions) })

       return(list(cboost_uni, cboost_int))
    },

    .predict = function(task) {
      #browser()
      newdata = task$data(cols = task$feature_names)

      lin_pred = self$model$cboost_uni$predict(newdata) + self$model$cboost_int$predict(newdata)
      probs = 1 / (1 + exp(-lin_pred))

      pos = self$model$cboost_uni$response$getPositiveClass()
      neg = setdiff(names(self$model$cboost$response$getClassTable()), pos)
      pmat = matrix(c(probs, 1 - probs), ncol = 2L, nrow = length(probs))
      colnames(pmat) = c(pos, neg)
      if (self$predict_type == "prob") {
        list(prob = pmat)
      }
      if (self$predict_type == "response") {
        list(response = ifelse(probs > self$model$cboost$response$getThreshold(), pos, neg))
      } else {
        list(prob = pmat)
      }
    }
  )
)
mlr_learners$add("classif.compboost", LearnerClassifCompboost)


devtools::load_all()

lr1 = lrn("classif.compboost")
lr1$train(tsk("sonar"))

