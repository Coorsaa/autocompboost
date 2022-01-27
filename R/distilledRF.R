#' @title RF to CART distiller
#'
#' @description
#' Calls [rpart::rpart()] to train a decision tree on cross-validated predictions of a
#' random forest using [ranger::ranger()] from package \CRANpkg{ranger}.
#' Cross-validated predictions are obtained by [mlr3pipelines::PipeOpLearnerCV].
#' @export
distilledRF = function(task, max_time) {
  browser()
  assert_task(task)

  time0 = proc.time()

  lrn = as_learner(
    po("cv_prediction_target", lrn(paste0(task$task_type, ".ranger")), resampling.folds = 5) %>>%
      po("fixfactors") %>>%
      lrn(paste0(task$task_type, ".rpart"), minsplit = 1L, minbucket = 1L, cp = 1e-8)
  )

  ttrain = (proc.time() - time0)[3] / 60
  if (ttrain > max_time) break

  lin = list()
  lin[["model"]] = lrn$train(task)

  ttrain = (proc.time() - time0)[3] / 60
  lin[["time_train"]] = ttrain
  cat("Tree trained in: ", round(ttrain, 2), " Min.", sep = "")

  out = list(trees = lin)
  class(out) = "distilledTree"
  return(out)
}