#' @title autocompboost_preproc_pipeline

#' @description
#' AutoML pipeline of autocompboost.
#'
#' @param task ([`Task`][mlr3::Task]) \cr
#' Contains the task to be solved. Currently [`TaskClassif`][mlr3::TaskClassif] and [`TaskRegr`][mlr3::TaskRegr] are supported.
#' @param max_cardinality `integer(1)` \cr
#' Maximum number of factor levels allowed. Factors with a cardinality > max_cardinality are
#' collapsed using [`PipeOpCollapseFactors`][mlr3pipelines::PipeOpCollapseFactors]. Default is set to 1000.
#' @return [`Graph`][mlr3pipelines::Graph]
#' @export
autocompboost_preproc_pipeline = function(task, max_cardinality = 100) {
  has_type_feats = function(types, if_null = TRUE) {
    if (is.null(task)) if_null else any(types %in% task$feature_types$type)
  }

  if (!is.null(task)) assert_task(task)

  pos = list()
  pos = c(pos, po("removeconstants"))

  # recode hidden missings
  pos = c(pos, po("colapply", id = "recode_hidden_missings", param_vals = list(affect_columns = selector_type(c("numeric", "integer")),
    applicator = function(x) {
      x[x == -999] = NA
      return(x)
    }
  )))

  # remove outliers
  # pos = c(pos, po("colapply", id = "remove_outliers", param_vals = list(affect_columns = selector_type(c("numeric", "integer")),
  #   applicator = function(x) {
  #     x[abs(x) > 3 * sd(x)] = NA
  #     return(x)
  #   }
  # )))


  if (has_type_feats("character")) {
    pos = c(pos, po("colapply", id = "char_to_fct", param_vals = list(affect_columns = selector_type("character"), applicator = function(x) as.factor(x))))
  }

  if (has_type_feats("logical")) {
    pos = c(pos, po("colapply", id = "lgl_to_fct", param_vals = list(affect_columns = selector_type("logical"), applicator = function(x) as.factor(x))))
  }

  # if (has_type_feats("integer")) {
    # pos = c(pos, po("colapply", id = "int_to_dbl", param_vals = list(affect_columns = selector_type("integer"), applicator = function(x) as.numeric(x))))
  # }

  if (has_type_feats("POSIXct")) {
    pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))
  }

  if (has_type_feats(c("numeric", "integer"))) {
    pos = c(pos,
      gunion(list(
        po("imputehist"),
        po("missind", param_vals = list(affect_columns = selector_type(c("numeric", "integer")), type = "factor")))) %>>%
      po("featureunion"))
  }

  # Impute factors
  if (has_type_feats(c("factor", "ordered", "character"))) {
    pos = c(pos, po("imputesample", affect_columns = selector_type(c("factor", "ordered", "character"))))
  }

  # Fix extra factor levels
  if (has_type_feats(c("factor", "ordered"))) {
    pos = c(pos, po("fixfactors"))
  }

  # Ensure all factor levels are encoded during predict FIXME: should we sample those or drop features with NA ratio above x%?
  if (has_type_feats(c("factor", "ordered", "character"))) {
    pos = c(pos, po("imputesample", affect_columns = selector_type(c("factor", "ordered", "character"))))
  }

  # Collapse factors over 1000 levels
  # FIXME: Can be improved after #330 is solved
  if (is.null(task)) {
    pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  } else {
    if (any(mlr3misc::map_lgl(task$levels(task$feature_types$id[task$feature_types$type == "factor"]), function(x) length(x) > max_cardinality))) {
      pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
    }
  }
  pos = c(pos, po("removeconstants", id = "removeconstants_end"))

  as_graph(Reduce(`%>>%`, pos))
}
