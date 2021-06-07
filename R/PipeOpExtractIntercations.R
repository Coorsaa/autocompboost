#' @title PipeOpExtractInteractions
#'
#' @usage NULL
#' @name mlr_pipeops_extract_interactions
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @description
#' PipeOperator to extract interactions gained by an `rpart` model.
#'
#' @section Construction:
#' ```
#' PipeOpExtractInteractions$new(id = "extract_interactions", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' Based on the input [`Task`][mlr3::Task], a tree model is trained.
#' The output contains interactions as `list()`.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOp`][mlr3pipelines::PipeOp], as well as:
#' * `degree`  :: `integer(1)` \cr
#'   Degree of interactions. Initialized to `2L`.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @family PipeOps
#' @export
PipeOpExtractInteractions = R6Class("PipeOpExtractInteractions",

  inherit = mlr3pipelines::PipeOp,

  public = list(
    initialize = function(id = "extract_interactions", param_vals = list(),
      packages = character(0), task_type = "Task", tags = NULL, feature_types = mlr_reflections$task_feature_types) {
      param_set = ps(
        degree = p_int(lower = 2L, upper = Inf, tags = c("train", "predict"))
      )
      param_set$values = list(degree = 2L)
      super$initialize(id = id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "list", predict = "Task"),
        packages = "rpart", tags = "meta"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      degree = self$param_set$values$degree

      # define learner
      if (intask$task_type == "classif")
        learner = lrn("classif.rpart", maxdepth = degree)
      if (intask$task_type == "regr")
        learner = lrn("regr.rpart", maxdepth = degree)

      # train forest
      rr = resample(intask, learner, rsmp("cv"), store_models = TRUE)

      # extract chosen features from models
      vars = lapply(rr$learners, function(l) {
          nodes = which(l$model$frame$var == "<leaf>")
          paths = rpart::path.rpart(l$model, nodes)
        }
      )

      self$state$interactions = vars

      list(vars)
    },

    .predict = function(inputs) {
      return(inputs)
    }

  )
)




