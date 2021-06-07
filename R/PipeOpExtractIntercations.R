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
        output = data.table(name = "output", train = "data.table", predict = "NULL"),
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

      self$state$interactions = private$.extractInteractionVars(feats = intask$feature_names, vars = vars)
      return(list(self$state$interactions))
    },

    .predict = function(inputs) {
      return(NULL)
    },

    .extractInteractionVars = function(feats, vars) {
      fs_all = lapply(vars, function(var) {
        fs = lapply(var, function(v) {
          vtemp = v["root" != v]
          fs = sub("<.*|>.*", "", vtemp)

          ## Check if all extracted features are real features and no
          ## artifacts are extracted:
          nuisance = lapply(fs, function(f) {
            if (! f %in% feats)
              stop("Extracted feature ", f, " not found in given features.")
          })
          return(fs)
        })
        fs = fs[vapply(fs, length, integer(1L)) > 1]
        return(unique(fs))
      })

      fs_str = vapply(do.call(c, fs_all), function(f) paste(sort(f), collapse = "<x>"), character(1L))
      fst = table(fs_str)

      out = data.table(
        feat1 = sub("<x>.*", "", names(fst)),
        feat2 = sub(".*<x>", "", names(fst)),
        count = as.integer(fst))

      ## Another check if all final feats are included in
      ## the given features:
      nuisance = lapply(out[, c("feat1", "feat2")], function(fs) {
        if (any(! fs %in% feats))
          stop("Extracted features ", paste(fs[! fs %in% feats], collapse = ", "), " are not included in given features.")
      })
      return(out[order(out$count, decreasing = TRUE), ])
    }
  )
)




