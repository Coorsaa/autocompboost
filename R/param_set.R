#' @title autocompboost_default_params
#' @description
#' Default tuning search space for autocompboost.
#' @param task_type (`character(1L)`) \cr
#' Task type. `"classif"` for classification or `"regr"` for regression.
#' @return [`ParamSet`][paradox::ParamSet]
#' @export
autocompboost_default_params = function(task_type) {
  if (task_type == "classif") {
    return(
      ps(
        classif.compboost.learning_rate_interactions = p_dbl(lower = 0.1, upper = 0.9)
      )
    )
  } else if (task_type == "regr") {
    return(
      ps(
        regr.compboost.learning_rate_interactions = p_dbl(lower = 0.1, upper = 0.9)
      )
    )
  }
}
