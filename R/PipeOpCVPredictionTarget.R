#' @title Wrap a Learner into a PipeOp with Cross-validated Predictions as Features
#'
#' @name mlr_pipeops_cv_prediction_target
#' @format [`R6Class`] object inheriting from [`mlr3pipelines::PipeOpTaskPreproc`]/[`mlr3pipelines::PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`mlr3pipelines::PipeOp`].
#' @section Methods:
#' Methods inherited from [`mlr3pipelines::PipeOpTaskPreproc`]/[`mlr3pipelines::PipeOp`].
#'
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @import mlr3pipelines
#' @export
PipeOpCVPredictionTarget = R6Class("PipeOpCVPredictionTarget",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      private$.learner$param_set$set_id = ""
      id = id %??% private$.learner$id
      # FIXME: can be changed when mlr-org/mlr3#470 has an answer
      task_type = mlr_reflections$task_types[get("type") == private$.learner$task_type][order(get("package"))][1L]$task

      private$.crossval_param_set = ParamSet$new(params = list(
        ParamFct$new("method", levels = c("cv", "insample"), tags = c("train", "required")),
        ParamInt$new("folds", lower = 2L, upper = Inf, tags = c("train", "required"))
      ))
      private$.crossval_param_set$values = list(method = "cv", folds = 3)
      private$.crossval_param_set$set_id = "resampling"
      # Dependencies in paradox have been broken from the start and this is known since at least a year:
      # https://github.com/mlr-org/paradox/issues/216
      # The following would make it _impossible_ to set "method" to "insample", because then "folds"
      # is both _required_ (required tag above) and at the same time must be unset (because of this
      # dependency). We will opt for the least annoying behaviour here and just not use dependencies
      # in PipeOp ParamSets.
      # private$.crossval_param_set$add_dep("folds", "method", CondEqual$new("cv"))  # don't do this.

      super$initialize(id, alist(private$.crossval_param_set, private$.learner$param_set), param_vals = param_vals, can_subset_cols = TRUE, task_type = task_type, tags = c("learner", "ensemble"))
    }

  ),
  active = list(
    learner = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      private$.learner
    },
    learner_model = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner_model is read-only.")
        }
      }
      if (is.null(self$state) || is_noop(self$state)) {
        private$.learner
      } else {
        multiplicity_recurse(self$state, clone_with_state, learner = private$.learner)
      }
    }
  ),
  private = list(
    .train_task = function(task) {
      on.exit({private$.learner$state = NULL})

      # Train a learner for predicting
      self$state = private$.learner$train(task)$state
      pv = private$.crossval_param_set$values

      # Compute CV Predictions
      if (pv$method != "insample") {
        rdesc = mlr_resamplings$get(pv$method)
        if (pv$method == "cv") rdesc$param_set$values = list(folds = pv$folds)
        rr = resample(task, private$.learner, rdesc)
        prds = as.data.table(rr$prediction(predict_sets = "test"))
      } else {
        prds = as.data.table(private$.learner$predict(task))
      }

      private$pred_to_task(prds, task)
    },

    .predict_task = function(task) {
      on.exit({private$.learner$state = NULL})
      private$.learner$state = self$state
      prediction = as.data.table(private$.learner$predict(task))
      private$pred_to_task(prediction, task)
    },

    pred_to_task = function(prds, task) {
      if (!is.null(prds$truth)) prds[, truth := NULL]
      renaming = setdiff(colnames(prds), c("row_id", "row_ids"))
      setnames(prds, renaming, sprintf("%s.%s", self$id, renaming))

      # This can be simplified for mlr3 >= 0.11.0;
      # will be always "row_ids"
      row_id_col = intersect(colnames(prds), c("row_id", "row_ids"))
      setnames(prds, old = row_id_col, new = task$backend$primary_key)
      task$cbind(prds)
      convert_task(
        intask = task,
        target = sprintf("%s.%s", self$id, renaming),
        drop_original_target = TRUE
      )
    },
    .crossval_param_set = NULL,
    .learner = NULL
  )
)
