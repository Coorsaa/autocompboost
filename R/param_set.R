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
      par_set = ps(
        classif.compboost.learning_rate = p_int(lower = -13, upper = -1, trafo = function(x) 2^x),
        classif.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2),
        classif.compboost.just_univariate = p_lgl(),
        classif.compboost.add_deeper_interactions = p_lgl(),
        subsample.frac = p_dbl(lower = 0.5, upper = 1, tags = "budget")
      )
      par_set$trafo = function(x, param_set) {
        x$classif.compboost.just_univariate = x$classif.compboost.just_univariate
        return(x)
      }
      return(par_set)
    } else if (task_type == "regr") {
      par_set = ps(
        regr.compboost.learning_rate = p_int(lower = -13, upper = -1, trafo = function(x) 2^x),
        regr.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2),
        regr.compboost.just_univariate = p_lgl(),
        regr.compboost.add_deeper_interactions = p_lgl(),
        subsample.frac = p_dbl(lower = 0.5, upper = 1, tags = "budget")
      )
      par_set$trafo = function(x, param_set) {
        x$regr.compboost.just_univariate = x$regr.compboost.just_univariate
        return(x)
      }
      return(par_set)
    }
  } else if (tuning_method == "mbo") {
    if (task_type == "classif") {
      return(
        ps(
          classif.compboost.learning_rate = p_int(lower = -13, upper = -1, trafo = function(x) 2^x),
          classif.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2)
        )
      )
    } else if (task_type == "regr") {
      return(
        ps(
          regr.compboost.learning_rate = p_int(lower = -13, upper = -1, trafo = function(x) 2^x),
          regr.compboost.top_interactions = p_dbl(lower = 0.01, upper = 0.2)
        )
      )
    }
  }
}
