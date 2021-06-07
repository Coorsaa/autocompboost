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
autocompboost_preproc_pipeline = function(task, max_cardinality = 1000) {
  has_type_feats = function(types, if_null = TRUE) {
    if (is.null(task)) if_null else any(types %in% task$feature_types$type)
  }

  if (!is.null(task)) assert_task(task)

  pos = list()

  if (has_type_feats("character")) {
    pos = c(pos, po("colapply", id = "char_to_fct", param_vals = list(affect_columns = selector_type("character"), applicator = function(x) as.factor(x))))
  }

  if (has_type_feats("POSIXct")) {
    pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))
  }

  if (sum(task$missings()) > 0 && has_type_feats(c("numeric", "integer"))) {
    pos = c(pos,
      gunion(list(
        po("imputehist"),
        po("missind", param_vals = list(affect_columns = selector_type(c("numeric", "integer")), type = "factor")))) %>>%
      po("featureunion"))
  }

  # Impute factors
  if (sum(task$missings()) > 0 && has_type_feats(c("factor", "ordered", "character"))) {
    pos = c(pos, po("imputemode"))
  }

  # Fix extra factor levels
  if (has_type_feats(c("factor", "ordered"))) {
    pos = c(pos, po("fixfactors"))
  }

  # Ensure all factor levels are encoded during predict FIXME: should we sample those or drop features with NA ratio above x%?
  if (sum(task$missings()) > 0 && has_type_feats(c("factor", "ordered", "character"))) {
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

  pos = c(pos, po("removeconstants"))
  as_graph(Reduce(`%>>%`, pos))
}
