#' @title autocompboost_default_params
#' @description
#' Default tuning search space for autocompboost.
#' @return [`ParamSet`][paradox::ParamSet]
#' @export
autocompboost_default_params = function() {
  return(
    ps(
      classif.rpart.cp = p_dbl(lower = 0, upper = 1),
      classif.rpart.maxdepth = p_int(lower = 1, upper = 30)
    )
  )
}
