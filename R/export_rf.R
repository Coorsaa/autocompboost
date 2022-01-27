# edited from https://github.com/talegari/solitude/blob/master/R/terminal_nodes.R
NodesDepthPerTree = function(treelike){

  treelike  = data.table::as.data.table(treelike)
  melted    = data.table::melt(
    treelike[, c("nodeID", "leftChild", "rightChild")],
      id.vars      = "nodeID",
      measure.vars = c("leftChild", "rightChild"),
      na.rm        = TRUE
    )

  # create graph in 1-index mode
  edgeMat     = as.matrix(melted[ , c("nodeID", "value")]) + 1L
  treegraph   = igraph::graph_from_edgelist(edgeMat, directed = TRUE)

  # get depths of terminal nodes
  tnValues    = treelike[["nodeID"]]
  depths      = igraph::distances(treegraph,
    v    = 1L,
    to   = tnValues + 1L, # to 1-index mode,
    mode = "out"
  )
  dim(depths) = NULL

  res = data.table::data.table(nodeID = tnValues, depth = depths)
  return(res)
}


########################################
library(mlr3)
library(mlr3learners)
library(data.table)


task = tsk("iris")

nbtrees = 50
learner = lrn("classif.ranger", num.trees = nbtrees, max.depth = 5)

learner$train(task)
model = learner$model

library(ranger)
ord = c(1, 7, 2, 3, 4, 6, 8)
forest = lapply(seq_len(nbtrees), function(t) {
  treeInfo(obj = model, t)[, ord]
})

tree_depths = lapply(forest, NodesDepthPerTree)
max_depth = max(data.table::rbindlist(tree_depths)$depth)

capture.output(
  writeLines(paste("DATASET_NAME:", task$id,
    "\nENSEMBLE: RF",
    "\nNB_TREES:", nbtrees,
    "\nNB_FEATURES:", length(task$feature_names),
    "\nNB_CLASSES:", length(task$class_names),
    "\nMAX_TREE_DEPTH:", max_depth
  )),
  for (i in seq_along(forest)) {
    # tree = t[[i]]
    # tree[is.na(tree)] =-1
    tree = data.frame(forest[[i]][, 1:6], depth = tree_depths[[i]]$depth, prediction = forest[[i]]$prediction)
    tree$prediction = as.integer(tree$prediction) - 1
    tree[is.na(tree)] = -1
    writeLines(
      paste0(
        ifelse(i == 1, paste0("Format: ", capture.output(write.table(tree, sep = " ", row.names = FALSE))[1], "\n\n"), "\n"),
        "[TREE ", i - 1, "]\n",
        "NB_NODES: ", max(tree$nodeID) + 1
      )
    )
    writeLines(capture.output(write.table(tree, sep = " ", row.names = FALSE))[-1])
    # writeLines(capture.output(print(tree, width = 200, row.names = FALSE))[-1])
  },
  file = "iris_forest.txt"
)





# learner2 = lrn("classif.rpart")
# learner2$train(task)
# tm = learner2$model
# tm

# library(partykit)

# sp_o <- partysplit(1L, index = 1:3)
# sp_h <- partysplit(3L, breaks = 75)
# sp_w <- partysplit(4L, index = 1:2)

# pn <- partynode(1L, split = sp_o, kids = list(
#   partynode(2L, split = sp_h, kids = list(
#   partynode(3L, info = "yes"),
#   partynode(4L, info = "no"))),
#   partynode(5L, info = "yes"),
#   partynode(6L, split = sp_w, kids = list(
#   partynode(7L, info = "yes"),
#   partynode(8L, info = "no")))))