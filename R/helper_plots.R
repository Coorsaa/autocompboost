#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend = function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

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