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
        degree = p_int(lower = 2L, upper = 3L, tags = c("train", "predict"))
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
        learner = lrn("classif.ranger", max.depth = degree)
      if (intask$task_type == "regr")
        learner = lrn("regr.ranger", max.depth = degree)

      # train forest
      learner$train(intask)
      mod = learner$model

      self$state$interactions = private$.extractInteractions(mod, degree)

      return(list(self$state$interactions))
    },

    .predict = function(inputs) {
      return(NULL)
    },

    .extractInteractions = function(mod, degree) {
      fs = lapply(1:mod$num.trees, function(x) {
        na.omit(treeInfo(mod, x)[, 1:6])
      })

      if (degree == 2) {
        fs_all = lapply(fs, function(x) {
          list(
            unique(c(
              x[1, "splitvarName"],
              x[x$nodeID == x[1, "leftChild"], "splitvarName"]
            )),
            unique(c(
              x[1, "splitvarName"],
              x[x$nodeID == x[1, "rightChild"], "splitvarName"]
            ))
          )
        })
      } else if (degree == 3) {
        fs_all = lapply(fs, function(x) {
          list(
            unique(c(
              x[1, "splitvarName"],
              x[x$nodeID == x[1, "leftChild"], "splitvarName"],
              x[x$nodeID == x[x$nodeID == x[1, "leftChild"], "leftChild"], "splitvarName"]
            )),
            unique(c(
              x[1, "splitvarName"],
              x[x$nodeID == x[1, "leftChild"], "splitvarName"],
              x[x$nodeID == x[x$nodeID == x[1, "leftChild"], "rightChild"], "splitvarName"]
            )),
            unique(c(
              x[1, "splitvarName"],
              x[x$nodeID == x[1, "rightChild"], "splitvarName"],
              x[x$nodeID == x[x$nodeID == x[1, "rightChild"], "leftChild"], "splitvarName"]
            )),
            unique(c(
              x[1, "splitvarName"],
              x[x$nodeID == x[1, "rightChild"], "splitvarName"],
              x[x$nodeID == x[x$nodeID == x[1, "rightChild"], "rightChild"], "splitvarName"]
            ))
          )
        })
      }

      fs_all = unique(fs_all[vapply(fs_all, length, integer(1L)) > 1])

      fs_str = vapply(do.call(c, fs_all), function(f) paste(sort(f), collapse = "<x>"), character(1L))
      fst = table(fs_str)

      out = data.table(
        feat1 = sapply(names(fst), function(s) {strsplit(s, "<x>")[[1]][1]}),
        feat2 = sapply(names(fst), function(s) {strsplit(s, "<x>")[[1]][2]}),
        feat3 = sapply(names(fst), function(s) {strsplit(s, "<x>")[[1]][3]}),
        count = as.integer(fst))

      out = out[, !apply(out, 2 , function(j) all(is.na(j))), with = FALSE]
      ## Another check if all final feats are included in
      ## the given features:
      if (degree == 2) {
        out = na.omit(out)
        nuisance = lapply(out[, -ncol(out), with = FALSE], function(fs) {
          if (any(! fs %in% feats))
            stop("Extracted features ", paste(fs[! fs %in% feats], collapse = ", "), " are not included in given features.")
        })
      }
      return(out[order(out$count, decreasing = TRUE), ])
    }
  )
)




