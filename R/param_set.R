#' @title autocompboost_default_params
#' @description
#' Default tuning search space for autocompboost.
#' @param task_type (`character(1L)`) \cr
#' Task type. `"classif"` for classification or `"regr"` for regression.
#' @param tuning_method (`character(1L)`) \cr
#' Tuning method, choice of `"mbo"` `"smashy"` and `"hyperband"`. This is necessary since `"hyperband"` and `"smashy"` need a budget parameter.
#' @return [`ParamSet`][paradox::ParamSet]
#' @export
autocompboost_default_params = function(task_type, tuning_method) {
  if (tuning_method %in% c("hyperband", "smashy")) {
    if (task_type == "classif") {
      return(
        ps(
          classif.compboost.learning_rate = p_int(lower = -14, upper = -1, trafo = function(x) 2^x),
          classif.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2),
          subsample.frac = p_dbl(lower = 0.5, upper = 1, tags = "budget")
        )
      )
    } else if (task_type == "regr") {
      return(
        ps(
          regr.compboost.learning_rate = p_int(lower = -14, upper = -1, trafo = function(x) 2^x),
          regr.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2),
          subsample.frac = p_dbl(lower = 0.5, upper = 1, tags = "budget")
        )
      )
    }
  } else if (tuning_method == "mbo") {
    if (task_type == "classif") {
      return(
        ps(
          classif.compboost.learning_rate = p_int(lower = -14, upper = -1, trafo = function(x) 2^x),
          classif.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2)
        )
      )
    } else if (task_type == "regr") {
      return(
        ps(
          regr.compboost.learning_rate = p_int(lower = -14, upper = -1, trafo = function(x) 2^x),
          regr.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2)
        )
      )
    }
  }
}
