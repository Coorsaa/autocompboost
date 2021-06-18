#' @title AutoCompBoostBase
#'
#' @description
#' Base class for AutoCompBoost. Has subclasses for Classification and Regression.
#'
#' @section Internals:
#' The AutoCompBoostBase class uses [mlr3pipelines] to create a machine learning pipeline. \cr
#' This pipeline contains multiple preprocessing steps wrapped in a [GraphLearner][mlr3pipelines::GraphLearner]. \cr
#' This [GraphLearner][mlr3pipelines::GraphLearner] is wrapped in an [AutoTuner][mlr3tuning::AutoTuner] for Hyperparameter Optimization and proper resampling. \cr
#' Tuning is performed using Bayesian Optimization.
#'
#' @section Construction:
#' Objects should be created using the [AutoCompBoost][autocompboost::AutoCompBoost] interface function.
#' ```
#' model = AutoCompBoost(task, resampling, measure, tuning_budget, tuning_iters, final_model)
#' ```
#'
#' @param task ([`Task`][mlr3::Task]) \cr
#' Contains the task to be solved. Currently [`TaskClassif`][mlr3::TaskClassif] and [`TaskRegr`][mlr3::TaskRegr] are supported.
#' @param resampling ([Resampling][mlr3::Resampling]) \cr
#' Contains the resampling method to be used for hyper-parameter optimization.
#' Defaults to [ResamplingCV][mlr3::ResamplingCV] with 3 folds.
#' @param param_values (`list()`) \cr
#' Parameter values which are pass on to the learner.
#' @param measure ([Measure][mlr3::Measure]) \cr
#' Contains the performance measure, for which we optimize during training. \cr
#' Defaults to [Accuracy][mlr3measures::acc] for classification and [RMSE][mlr3measures::rmse] for regression.
#' @param tuning_method (`character(1)`) \cr
#' Tuning method. Possible choices are `"mbo"`, `"hyperband"` or `"smash"`¸ Default is `"mbo"`.
#' @param tuning_time (`integer(1)`) \cr
#' Termination criterium. Number of seconds for which to run the optimization. Does *not* include training time of the final model. \cr
#' Default is set to `3600`, i.e. one hour. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param tuning_iters (`integer(1)`) \cr
#' Termination criterium. Number of MBO iterations for which to run the optimization. \cr
#' Default is set to `150` iterations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param final_model (`logical(1)`) \cr
#' Whether or not to return the final model trained on the whole dataset at the end.
#'
#' @field task ([`Task`][mlr3::Task]) \cr
#' Contains the task to be solved.
#' @field learner ([AutoTuner][mlr3tuning::AutoTuner]) \cr
#' The ML pipeline at the core of mlr3automl is an [AutoTuner][mlr3tuning::AutoTuner] containing a [GraphLearner][mlr3pipelines::GraphLearner].
#' @field param_values (`list()`) \cr
#' Parameter values which are pass on to the learner.
#' @field resampling ([Resampling][mlr3::Resampling]) \cr
#' Contains the resampling method to be used for hyper-parameter optimization.
#' @field measure ([Measure][mlr3::Measure]) \cr
#' Contains the performance measure, for which we optimize during training. \cr
#' @field tuning_method (`character(1)`) \cr
#' Tuning method. Possible choices are `"mbo"`, `"hyperband"` or `"smash"`¸ Default is `"mbo"`.
#' @field tuning_time (`integer(1)`) \cr
#' Termination criterium. Number of seconds for which to run the optimization. Does *not* include training time of the final model. \cr
#' Default is set to `60`, i.e. one minuet. Tuning is terminated depending on the first termination criteria fulfilled.
#' @field tuning_iters (`integer(1)`) \cr
#' Termination criterium. Number of MBO iterations for which to run the optimization. \cr
#' Default is set to `150` iterations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @field final_model (`logical(1)`) \cr
#' Whether or not to return the final model trained on the whole dataset at the end.
#' @field tuner ([TunerInterMBO][mlrintermbo::TunerInterMBO]) \cr
#' Tuning is performed using [TunerInterMBO][mlrintermbo::TunerInterMBO].
#' @field tuning_terminator ([Terminator][bbotk::Terminator]) \cr
#' Contains an termination criterion for model tuning. \cr
#'
#' @rawNamespace import(mlr3, except = c(lrn, lrns))
#' @import compboost
#' @import mlr3misc
#' @import mlr3oml
#' @import mlr3pipelines
#' @import mlrintermbo
#' @import mlr3tuning
#' @import paradox
#' @import checkmate
#' @import testthat
#' @importFrom R6 R6Class
#' @import data.table
AutoCompBoostBase = R6::R6Class("CompBoostBase",
  public = list(
    task = NULL,
    learner = NULL,
    param_values = NULL,
    resampling = NULL,
    measure = NULL,
    tuning_method = NULL,
    tuning_time = NULL,
    tuning_iters = NULL,
    final_model = NULL,
    tuner = NULL,
    tuning_terminator = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @return [AutoCompBoostBase][autocompboost::AutoCompBoostBase]
    initialize = function(task, resampling = NULL, param_values = NULL, measure = NULL, tuning_method = "mbo",
      tuning_time = 60L, tuning_iters = 150L, final_model = TRUE) { # FIXME possibly add: , stratify = TRUE, tune_threshold = TRUE) {

      if (!is.null(resampling)) assert_resampling(resampling)
      if (!is.null(measure)) assert_measure(measure)

      self$task = assert_task(task)
      self$resampling = resampling %??% rsmp("cv", folds = 3)
      self$param_values = assert_list(param_values)
      self$measure = assert_measure(measure)
      self$tuning_time = assert_number(tuning_time, lower = 0)
      self$tuning_iters = assert_number(tuning_iters, lower = 0)
      check_subset(tuning_method, choices = c("mbo", "hyperband", "smash"))
      self$tuning_method = assert_character(tuning_method, len = 1)

      self$tuning_terminator = trm("combo", list(
        trm("run_time", secs = self$tuning_time),
        trm("evals", n_evals = self$tuning_iters)
        ), any = TRUE
      )

      if (tuning_method == "mbo") {
        self$tuner = tnr("intermbo")
      } else if (tuning_method == "hyperband") {
        self$tuner = tnr("hyperband", eta = 3L)
      } else if (tuning_method == "smash") {
        # imputepl = po("imputeoor", offset = 1, multiplier = 10) %>>% po("fixfactors") %>>% po("imputesample")
        # learnerlist = list(
        #   ranger = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.ranger", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
        #   knn = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))
        # )
        # self$tuner = tnr("smash",
        #   budget_log_step = log(7),
        #   survival_fraction = 0.45,
        #   filter_algorithm = "progressive",
        #   surrogate_learner = learnerlist$knn,
        #   filter_with_max_budget = TRUE,
        #   filter_factor_first = 50,  # keine ahnung wie wichtig das ist
        #   filter_factor_first.end = 1000,
        #   filter_factor_last = 10,
        #   filter_factor_last.end = 25,
        #   random_interleave_fraction = 0.5,
        #   random_interleave_fraction.end = 0.8,
        #   random_interleave_random = TRUE  # scheint relativ egal zu sein
        # )
        stopf("This tuning method is currently not supported") # FIXME
      }
      self$learner = private$.create_learner(param_values)
      self$final_model = assert_logical(final_model)
    },

    #' @description
    #' Trains the AutoML system.
    #' @param row_ids (`integer()`)\cr
    #' Vector of training indices.
    train = function(row_ids = NULL) {
      self$learner$train(self$task, row_ids)
      if (length(self$learner$learner$errors) > 0) {
        warning("An error occured during training. Fallback learner was used!")
        print(self$learner$learner$errors)
      }
      if (self$final_model)
        private$.final_model = self$learner$learner$model[[paste0(self$task$task_type, ".compboost")]]$model # FIXME: change to compboost when implemented
    },

    #' @description
    #' Returns a [Prediction][mlr3::Prediction] object for the given data based on the trained model.
    #' @param data ([data.frame] | [data.table] | [Task][mlr3::Task]) \cr
    #' New observations to be predicted. If `NULL`, defaults to the task the model
    #' was trained on.
    #' @param row_ids (`integer()`) \cr
    #' Vector of training indices.
    #' @return [`PredictionClassif`][mlr3::PredictionClassif] | [`PredictionRegr`][mlr3::PredictionRegr]
    predict = function(data = NULL, row_ids = NULL) {
      if (is.null(data)) {
        return(self$learner$predict(self$task, row_ids))
      } else {
        return(self$learner$predict(data, row_ids))
      }
    },

    #' @description
    #' Performs nested resampling. [`ResamplingHoldout`][mlr3::ResamplingHoldout] is used for the outer resampling.
    #' @return [`ResampleResult`][mlr3::ResampleResult]
    resample = function() {
      outer_resampling = rsmp("holdout")
      resample_result = mlr3::resample(self$task, self$learner, outer_resampling)
      self$learner = resample_result$learners[[1]]
      if (length(self$learner$learner$errors) > 0) {
        warning("An error occured during training. Fallback learner was used!")
        print(self$learner$learner$errors)
      }
      return(resample_result)
    },

    #' @description
    #' Helper to extract the best hyperparameters from a tuned model.
    #' @return [`data.table`][data.table::data.table]
    tuned_params = function() {
      if (is.null(self$learner$tuning_instance$archive)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else {
        return(self$learner$tuning_instance$archive$best())
      }
    },

    #' @description
    #' Returns the model summary
    #' @return [`data.table`][data.table::data.table]
    summary = function() {
      if (is.null(self$learner$model)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else {
        return(self$learner$model)
      }
    },

    #' @description
    #' Returns the trained model if `final_model` is set to TRUE.
    #' @return [`Compboost`][compboost::Compboost]
    model = function() {
      if (is.null(self$learner$model)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else if (self$final_model == FALSE) {
        warning("Argument `final_model` has been set to `FALSE`. No final Model trained.")
      } else {
        return(private$.final_model)
      }
    },

    #' @description
    #' Returns the selected base learners by the final model.
    #' @return (`character()`)
    getSelectedBaselearner = function() {
      if (is.null(self$learner$model)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else if (self$final_model == FALSE) {
        warning("Argument `final_model` has been set to `FALSE`. No final Model trained.")
      } else {
        return(private$.final_model$getSelectedBaselearner())
      }
    },

    #' @description
    #' Plot function to plot a single spline.
    #' @param spline (`character(1L)`) \cr
    #' Name of spline to plot.
    #' @return [`ggplot`][ggplot2::ggplot]
    plotSpline = function(spline = character(1L)) {
      assert_character(spline, len = 1L)
      if (is.null(self$learner$model)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else if (self$final_model == FALSE) {
        warning("Argument `final_model` has been set to `FALSE`. No final Model trained.")
      } else {
        return(private$.final_model$getSelectedBaselearner() + ggplot2::theme_bw())
      }
    },

    #' @description
    #' Plot function to plot the feature importance.
    #' @return [`ggplot`][ggplot2::ggplot]
    plotFeatureImportance = function() {
      if (is.null(self$learner$model)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else if (self$final_model == FALSE) {
        warning("Argument `final_model` has been set to `FALSE`. No final Model trained.")
      } else {
        return(private$.final_model$plotFeatureImportance() + ggplot2::theme_bw())
      }
    },

    #' @description
    #' Plot function to plot the learner traces.
    #' @return [`ggplot`][ggplot2::ggplot]
    plotBlearnerTraces = function() {
      if (is.null(self$learner$model)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else if (self$final_model == FALSE) {
        warning("Argument `final_model` has been set to `FALSE`. No final Model trained.")
      } else {
        return(private$.final_model$plotBlearnerTraces() + ggplot2::theme_bw())
      }
    }
  ),

  private = list(
    .final_model = NULL,
    .create_learner = function(param_values = NULL) {
      # get preproc pipeline
      if (self$task$task_type == "classif") {
        pipeline = autocompboost_preproc_pipeline(self$task, max_cardinality = 1000) %>>% po("subsample", stratify = TRUE)
      } else {
        pipeline = autocompboost_preproc_pipeline(self$task, max_cardinality = 1000) %>>% po("subsample")
      }

      # compboost learner
      learner = lrn(paste0(self$task$task_type, ".compboost"), show_output = TRUE)
      if (!is.null(self$param_values)) {
        learner$param_set$values = insert_named(learner$param_set$values, self$param_values)
      }
      if (self$task$task_type == "classif") {
        learner$predict_type = "prob"
      }
      if (self$task$task_type == "classif" && length(self$task$class_names) > 2) {
          # pipelne for multiclass
          # can be removed when compoboost supports multiclass classification
          pipeline = pipeline %>>%
            pipeline_ovr(learner)
      } else {
        pipeline = pipeline %>>%
          learner
      }

      # create graphlearner
      graph_learner = as_learner(pipeline)

      # fallback learner is featureless learner for classification / regression
      # graph_learner$fallback = lrn(paste0(self$task$task_type, ".featureless"))
      # use callr encapsulation so we are able to kill model training, if it
      # takes too long
      # graph_learner$encapsulate = c(train = "callr", predict = "callr")

      param_set = autocompboost_default_params(self$task$task_type)

      # FIXME: use hard timeout from mlr3automl here?
      # if (is.finite(self$tuning_time)) {
      #  tuner = TunerWrapperHardTimeout$new(
      #    tuner,
      #    timeout = self$tuning_time
      #  )
      # }

      at = AutoTuner$new(
        learner = graph_learner,
        resampling = self$resampling,
        measure = self$measure,
        search_space = param_set,
        terminator = self$tuning_terminator,
        tuner = self$tuner
      )
      at$id = paste0(self$task$task_type, ".autocompboost")
      return(at)
    }
  )
)
