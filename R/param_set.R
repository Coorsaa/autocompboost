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
        classif.rpart.cp = p_dbl(lower = 0, upper = 1),
        classif.rpart.maxdepth = p_int(lower = 1, upper = 30, tag = "budget")
      )
    )
  } else if (task_type == "regr") {
    return(
      ps(
        regr.rpart.cp = p_dbl(lower = 0, upper = 1),
        regr.rpart.maxdepth = p_int(lower = 1, upper = 30, tag = "budget")
      )
    )
  }
}
