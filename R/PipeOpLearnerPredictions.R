#' @title Wrap a Learner into a PipeOp with which returns a task with the predictions as new target.
#'
#' @usage NULL
#' @name mlr_pipeops_learner_predictions
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc]/[`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'
#' Returns a [`Task`][mlr3::Task] where the original target is replaced by the response obtained on the training set during training.
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' [`PipeOpLearnerPredictions`] can be used to create "destillation" [`Graph`]s that use the output of one [`Learner`][mlr3::Learner]
#' as new target for another [`Learner`][mlr3::Learner].
#'
#' @section Construction:
#' ```
#' PipeOpLearnerPredictions$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] \cr
#'   [`Learner`][mlr3::Learner] to use for training, or a string identifying a
#'   [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#'  This argument is always cloned; to access the [`Learner`][mlr3::Learner] inside `PipeOpLearnerPredictions` by-reference, use `$learner`.\cr
#' * `id` :: `character(1)`
#'   Identifier of the resulting object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpLearnerPredictions`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpLearnerPredictions`] has one output channel named `"output"`, producing a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' The output is a task with the same features, with the original target replaced by predictions made by the [`Learner`][mlr3::Learner].
#' During training, this prediction is the response based on the training set.
#'
#' @section State:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object, together with the `$state` elements inherited from the
#' [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc]. It is a named `list` with the inherited members, as well as:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$.train()` function.
#' * `train_log` :: [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during training.
#' * `train_time` :: `numeric(1)`\cr
#'   Training time, in seconds.
#' * `predict_log` :: `NULL` | [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during prediction.
#' * `predict_time` :: `NULL` | `numeric(1)`
#'   Prediction time, in seconds.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc], as well as the parameters of the [`Learner`][mlr3::Learner] wrapped by this object.
#'
#' @section Internals:
#' The `$state` is currently not updated by prediction, so the `$state$predict_log` and `$state$predict_time` will always be `NULL`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#' * `learner_model` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. This learner contains the model if the `PipeOp` is trained. Read-only.
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc]/[`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @family Pipeops
#' @family Meta PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @import mlr3pipelines
#' @export
#' @examples
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#'
#' lrnpred_po = po("learner_predictions", learner)
#'
#' lrnpred_po$train(list(task))
PipeOpLearnerPredictions = R6Class("PipeOpLearnerPredictions",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      private$.learner$param_set$set_id = ""
      id = id %??% private$.learner$id
      # FIXME: can be changed when mlr-org/mlr3#470 has an answer
      task_type = mlr_reflections$task_types[get("type") == private$.learner$task_type][order(get("package"))][1L]$task

      super$initialize(id, private$.learner$param_set, param_vals = param_vals, can_subset_cols = TRUE, task_type = task_type, tags = c("learner", "ensemble"))
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

      # Compute CV Predictions
      prds = as.data.table(private$.learner$predict(task))

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
    .learner = NULL
  )
)
