#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @import mlr3
#' @import mlr3pipelines
#' @import checkmate
#' @importFrom R6 R6Class
"_PACKAGE"

register_mlr3 = function () {
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  x$add("classif.compboost", LearnerClassifCompboost)
  x$add("regr.compboost", LearnerRegrCompboost)
}

register_mlr3pipelines = function() {
  x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")

  x$add("extract_interactions", PipeOpExtractInteractions)
}

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  register_mlr3()
  if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
    register_mlr3pipelines()
  }

  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(), action = "append")
  backports::import(pkgname)
} # nocov end


.onUnload = function(libpath) { # nolint
  # nocov start
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "autocompboost"], action = "replace")

  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "autocompboost"], action = "replace")

  library.dynam.unload("autocompboost", libpath)
} # nocov end

leanify_package()
