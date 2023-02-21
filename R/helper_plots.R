#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend = function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

plotIndividualContributionAC = function(cboost, newdata) {

  checkmate::assertR6(cboost, "Learner")
  checkmate::assertR6(cboost$model$univariate, "Compboost")
  if (! checkmate::testR6(cboost$model$interactions, "Compboost")) {
    warning("No interaction model detected, falling back to `plotIndividualContribution(cboost$model$univariate, newdata)`.")
    return(plotIndividualContribution(cboost$model$univariate, newdata, offset = FALSE))
  }
  checkmate::assertDataFrame(newdata, nrows = 1)

  p_uni = cboost$model$univariate$predictIndividual(newdata)
  p_int = cboost$model$interactions$predictIndividual(newdata)
  p_int$offset = NULL

  df_new = newdata
  df_new$residuals = 0
  tsk_new = TaskRegr$new(id = "residuals", backend = df_new, target = "residuals")
  p_trees = predict.residualBooster(cboost$model$deeper_interactions, tsk_new)

  p = c(p_uni, p_int, other = p_trees)

  df_plt = data.frame(blearner = names(p), value = unlist(p),
    stage = rep(c("Univariate", "Pairwise interactions", "Deep trees"), times = c(length(p_uni), length(p_int), 1)))

  df_plt$blearner = factor(df_plt$blearner, levels = rev(df_plt$blearner))
  df_plt$stage = factor(df_plt$stage, levels = c("Univariate", "Pairwise interactions", "Deep trees"))
  df_plt$bl_num = rev(seq_along(df_plt$blearner))

  fnames = c("offset", "other", task$feature_names)
  fidx = t(do.call(rbind, lapply(fnames, function(feat) grepl(feat, df_plt$blearner))))

  flab = character(nrow(fidx))
  for (i in seq_len(nrow(fidx))) {
    feats = fnames[fidx[i, ]]

    if (length(feats) > 1) {
      df_char = data.frame(f = feats, n = nchar(feats))
      df_char = df_char[order(df_char$n, decreasing = TRUE), ]
      df_char$count = sapply(df_char$f, function(fn) sum(grepl(fn, df_char$f)))
      feats = df_char$f[df_char$count == 1]
    }
    feats = vapply(feats, function(f) {
      if (f %in% c("other", "offset")) return(f)

      fval = newdata[[f]]
      if (is.numeric(fval))
        return(paste0(f, " (", round(fval, 2), ")"))
      else
        return(paste0(f, " (", fval, ")"))
    }, character(1L))

    if (length(feats) == 2)
      flab[i] = paste0("Interaction: ", paste(feats, collapse = ", "))
    else
      flab[i] = feats
  }
  flab[flab == "other"] = "Deep trees"
  df_plt$feat = flab
  df_plt0 = df_plt %>%
    group_by(feat) %>%
    summarize(value = sum(value), stage = stage[1]) %>%
    ungroup() %>%
    arrange(desc(stage)) %>%
    mutate(bl_num = seq_along(value))

  pred = sum(df_plt0$value)
  prob = 1 / (1 + exp(-pred))
  plabel = ifelse(prob > 0.5, task$positive, task$negative)

  title = "Prediction"
  subtitle = paste0("Score: ", round(pred, 2), " Probability: ", round(prob, 2), " Predicted label: ", plabel)

  ggplot(df_plt0, aes(x = value, y = bl_num, color = stage, fill = stage)) +
    geom_vline(xintercept = 0, color = "dark grey", alpha = 0.6) +
    geom_segment(aes(xend = 0, yend = bl_num)) +
    geom_point() +
    ylab("") +
    xlab("Contribution to predicted value") +
    labs(color = "", fill = "") +
    ggsci::scale_fill_uchicago() +
    ggsci::scale_color_uchicago() +
    #scale_y_reverse() +
    scale_y_continuous(labels = df_plt0$feat, breaks = df_plt0$bl_num) +
    theme_minimal() +
    ggtitle(title, subtitle)
}



### Need to put this directly in compboost:
predict.compboostExtract = function(object, newdata) {
  feats = names(object)
  feats = feats[feats != "offset"]
  out = lapply(feats, function(ft) {
    if (! ft %in% names(newdata)) {
      warning("New data should contain all features used for building the model! Feature ", ft, " is missing.")
      return(NULL)
    } else {
      eff = object[[ft]]$predict(newdata[[ft]])
      pe = eff$linear + eff$nonlinear
      return(list(pe = pe, effects = eff, src = newdata[[ft]]))
    }
  })
  names(out) = feats
  pred = rowSums(do.call(cbind, lapply(out, function(x) x$pe))) + object$offset
  return(list(pred = pred, pe = out, offset = object$offset))
}

plotCategorical = function(feats, pe, cnums = NULL) {
  plt_dat = do.call(rbind, lapply(feats, function(feat) {
    if (! any(grepl(feat, names(pe))))
      stop("Feature ", feat, " not in partial effects data.")
    fidx = grep(feat, names(pe))
    data.frame(cat = cnums, est = pe[[fidx]], feature = feat)
  }))

  if (is.null(cnums[1]))
    cnums = rownames(pe[[fidx]])
  plt_dat$cat = factor(plt_dat$cat, levels = cnums)

  ggplot(data = plt_dat, aes(x = cat, y = est)) +
    geom_boxplot() +
    ylab("Contribution to\nprediction") +
    xlab("") +
    labs(color = "") +
    theme(legend.position = "bottom") +
    facet_wrap(. ~ feature, ncol = 3, scales = "free")
}

plotNumeric = function(feats, pe) {
  plt_dat = do.call(rbind, lapply(feats, function(feat) {
    if (! feat %in% names(pe$pe))
      stop("Feature ", feat, " not in partial effects data.")
    p = pe$pe[[feat]]
    data.frame(x = p$src, pe = p$pe, linear = p$effects$linear,
      nonlinear = p$effects$nonlinear, feature = feat)
  }))
  plt_dat$feat = factor(plt_dat$feat, levels = feats)
  ggplot(data = plt_dat, aes(x = x)) +
    geom_line(aes(y = linear, color = "Linear part"), alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = nonlinear, color = "Non-linear part"), alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = pe, color = "Partial effect"), size = 1.3) +
    geom_rug() +
    ylab("Contribution to\nprediction") +
    xlab("") +
    labs(color = "") +
    theme(legend.position = "bottom") +
    facet_wrap(. ~ feature, ncol = 3, scales = "free")
}

predictTensor = function(dat, dato, coef, tnames, n_knots, degree) {
  xo1 = dato[[tnames[1]]]
  xo2 = dato[[tnames[2]]]

  x1 = dat[[tnames[1]]]
  x2 = dat[[tnames[2]]]

  knots1 = compboostSplines::createKnots(values = xo1, n_knots = n_knots, degree = degree)
  basis1 = compboostSplines::createSplineBasis(values = x1, degree = degree, knots = knots1)

  knots2 = compboostSplines::createKnots(values = xo2, n_knots = n_knots, degree = degree)
  basis2 = compboostSplines::createSplineBasis(values = x2, degree = degree, knots = knots2)

  tensor = compboostSplines::rowWiseTensor(basis1, basis2)
  return(tensor %*% coef)
}
