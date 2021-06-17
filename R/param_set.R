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
        classif.compboost.learning_rate_univariate = p_dbl(lower = 1e-4, upper = 0.5, tags = "budget"),
        classif.compboost.learning_rate_interactions = p_dbl(lower = 1e-4, upper = 0.5),
        classif.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2)
      )
    )
  } else if (task_type == "regr") {
    return(
      ps(
        regr.compboost.learning_rate_univariate = p_dbl(lower = 1e-4, upper = 0.5, tags = "budget"),
        regr.compboost.learning_rate_interactions = p_dbl(lower = 1e-4, upper = 0.5),
        regr.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2)
      )
    )
  }
}
