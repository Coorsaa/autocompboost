boostRpart = function(task, lr, iters, patience, eps_for_break, use_es,
  idx_train, idx_test, logRisk, binary_response, prediction_offset,
  pseudoResiduals, max_time, ...) {

  time0 = proc.time()

  ll_trees = list()
  if (inherits(task, "TaskRegr"))
    largs = c("regr.rpart", list(...))
  else
    stop("Please use just regression tasks!")

  dat = task$data()
  target = dat[[task$target_names]]
  #dat[[task$target_names]] = NULL
  k_stop = 0
  pred_old = prediction_offset

  train_risk_old = logRisk(binary_response[idx_train], prediction_offset[idx_train])
  test_risk_old = logRisk(binary_response[idx_test], prediction_offset[idx_test])
  es_was_hit = FALSE
  cat("Start risk:", train_risk_old, " test:", test_risk_old, "\n")

  for (i in seq_len(iters)) {
    ttrain = (proc.time() - time0)[3] / 60
    if (ttrain > max_time) break

    dat[[task$target_names]] = target
    t0 = TaskRegr$new(
      id = paste0(task$id, "boost", i),
      backend = dat,
      target = task$target_names)

    lin = list()
    lin[["model"]] = do.call(lrn, largs)$train(t0, row_ids = idx_train)
    lin[["prediction"]] = lin[["model"]]$predict(t0)$response

    pred_new = lin[["prediction"]]
    pred_old = pred_old + lr * pred_new

    ll_trees[[i]] = lin

    if ((length(idx_test) > 0) && use_es) {
      ll_trees[[i]][["test_risk"]] = logRisk(binary_response[idx_test], pred_old[idx_test])
      ll_trees[[i]][["train_risk"]] = logRisk(binary_response[idx_train], pred_old[idx_train])

      enhancement = (test_risk_old - ll_trees[[i]][["test_risk"]]) / test_risk_old
      if (enhancement < eps_for_break)
        k_stop = k_stop + 1
      else
        k_stop = 0

      if (k_stop == patience) {
        es_was_hit = TRUE
        break
      }
    }
    target = as.vector(pseudoResiduals(cbind(binary_response),
      cbind(pred_old)))
    train_risk_old = ll_trees[[i]][["train_risk"]]
    test_risk_old = ll_trees[[i]][["test_risk"]]
    cat("Tree ", i, ": train risk: ", round(train_risk_old, 4), " test risk: ",
      round(test_risk_old, 4), " time: ", round(ttrain, 2), " Min.\n", sep = "")
  }
  if (es_was_hit) {
    ll_trees[i:(i - patience)] = NULL
  }
  out = list(trees = ll_trees, lr = lr)
  class(out) = "residualBooster"
  return(out)
}

predict.residualBooster = function(obj, newdata) {
  if (length(obj$trees) == 0) return(rep(0, newdata$nrow))
  predmat = do.call(cbind, lapply(obj$trees, function(l) obj$lr * l$model$predict(newdata)$response))
  rowSums(predmat)
}