#' @title AutoCompBoostRegr
#'
#' @description
#' Class for Automated Regression in autocompboost. Subclass of [AutoCompBoostBase][autocompboost::AutoCompBoostBase]
#'
#' @section Construction:
#' Objects should be created using the [AutoCompBoost][autocompboost::AutoCompBoost] interface function.
#' ```
#' regression_model = AutoCompBoost(regression_task, resampling, measure,
#' tuning_time, tuning_iters, final_model)
#' ```
#'
#' @param task ([`Task`][mlr3::Task]) \cr
#' [`TaskRegr`][mlr3::TaskRegr] to be solved.
#' @param resampling ([Resampling][mlr3::Resampling]) \cr
#' Contains the resampling method to be used for hyper-parameter optimization.
#' Defaults to [ResamplingCV][mlr3::ResamplingCV] with 3 folds.
#' @param param_values (`list()`) \cr
#' Parameter values which are pass on to the learner.
#' @param measure ([Measure][mlr3::Measure]) \cr
#' Contains the performance measure, for which we optimize during training. \cr
#' Defaults to [Accuracy][mlr3measures::acc] for classification and [RMSE][mlr3measures::rmse] for regression.
#' @param tuning_method (`character(1)`) \cr
#' Tuning method. Possible choices are `"mbo"`, `"hyperband"` or `"smashy"`¸ Default is `"smashy"`.
#' @param tuning_time (`integer(1)`) \cr
#' Termination criterium. Number of seconds for which to run the optimization. Does *not* include training time of the final model. \cr
#' Default is set to `3600`, i.e. one hour. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param tuning_iters (`integer(1)`) \cr
#' Termination criterium. Number of MBO iterations for which to run the optimization. \cr
#' Default is set to `150` iterations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param tuning_generations (`integer(1)`) \cr
#' Termination criterium for tuning method `smashy`. Number of generations for which to run the optimization. \cr
#' Default is set to `3` generations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param enable_tuning (`logical(1)`) \cr
#' Whether or not to perform hyperparameter optimization. Default is `TRUE`.
#' @param final_model (`logical(1)`) \cr
#' Whether or not to return the final model trained on the whole dataset at the end.
#'
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(autocompboost)
#'
#' regression_model = AutoCompBoost(tsk("boston_housing"))
#' regression_model$train()
#' }
AutoCompBoostRegr = R6Class(
  "AutoCompBoostRegr",
  inherit = AutoCompBoostBase,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @return [AutoCompBoostRegr][autocompboost::AutoCompBoostRegr]
    initialize = function(task, resampling, param_values, measure, tuning_method,
      tuning_time, tuning_iters, tuning_generations, enable_tuning, final_model) {
      checkmate::assert_r6(task, "TaskRegr")
      assert_number(tuning_iters)
      assert_number(tuning_time)
      self$measure = measure %??% mlr_measures$get("regr.rmse")

      super$initialize(task = task, resampling = resampling, param_values = param_values,
        measure = self$measure, tuning_method = tuning_method, tuning_time = tuning_time,
        tuning_iters = tuning_iters, enable_tuning = enable_tuning, final_model = final_model)
    }
  )
)
