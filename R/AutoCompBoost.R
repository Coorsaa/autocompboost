#' @title Interface function for autocompboost
#'
#' @description
#' Creates an instance of [AutoCompBoostClassif][autocompboost::AutoCompBoostClassif] or [AutoCompBoostRegr][autocompboost::AutoCompBoostRegr].
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
#' Default is set to `60`, i.e. one minute. Tuning is terminated depending on the first termination criteria fulfilled.
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
#' @return ([AutoCompBoostClassif][autocompboost::AutoCompBoostClassif] | [AutoCompBoostRegr][autocompboost::AutoCompBoostRegr]) \cr
#' Returned class depends on the type of task.
#' @export
#'
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(autocompboost)
#'
#' model = AutoCompBoost(tsk("sonar"))
#' model$train()
#' }
AutoCompBoost = function(task, resampling = NULL, param_values = NULL, measure = NULL, tuning_method = "hyperband",
  tuning_time = 60L, tuning_iters = 150L, tuning_generations = 3L, enable_tuning = TRUE, final_model = TRUE) {
  if (task$task_type == "classif") {
    # stratify target variable so that every target lable appears in all folds while resampling
    target_is_factor = task$col_info[task$col_info$id == task$target_names, ]$type == "factor"
    if (length(target_is_factor) == 1 && target_is_factor) {
      task$col_roles$stratum = task$target_names
    }
    return(AutoCompBoostClassif$new(task = task, resampling = resampling, param_values = param_values,
      measure = measure, tuning_method = tuning_method, tuning_time = tuning_time, tuning_iters = tuning_iters,
      enable_tuning = enable_tuning, final_model = final_model))
  } else if (task$task_type == "regr") {
    return(AutoCompBoostRegr$new(task = task, resampling = resampling, param_values = param_values,
      measure = measure, tuning_method = tuning_method, tuning_time = tuning_time, tuning_iters = tuning_iters,
      enable_tuning = enable_tuning, final_model = final_model))
  } else {
    stop("autocompboost only supports classification and regression tasks for now")
  }
}
