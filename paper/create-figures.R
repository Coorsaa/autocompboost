#q()
#R
#devtools::install("~/repos/compboost")

devtools::load_all()
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)

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

# Use adult task from OpenML:
#task = tsk("oml", task_id = 7592)
task = tsk("sonar")

# Remove rows with missings:
task$filter(which(complete.cases(task$data())))

# Train compboost learner:
set.seed(3141)
cboost = lrn("classif.compboost", predict_type = "prob", show_output = TRUE,
  learning_rate = 0.01, add_deeper_interactions = TRUE,
  stop_epsylon_for_break = 0, stop_patience = 3L, df = 4)
cboost$train(task)


font = "TeX Gyre Bonum"

sysfonts::font_add(font,
    #regular = paste0(base_dir, "/paper-figures/gyre-bonum/texgyrebonum-regular.ttf"),
    #bold = paste0(base_dir, "/paper-figures/gyre-bonum/texgyrebonum-bold.ttf"))
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
#showtext::showtext_auto()
extrafont::font_import(paths = "~/repos/autocompboost/paper/misc/gyre-bonum", prompt = FALSE)
extrafont::loadfonts()


theme_set(
  theme_bw(base_family = font) +
  #theme_minimal() +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
)

dinA4width = 121.99854

scaleFUN = function(x) sprintf("%.2f", x)

#### Required model complexity
#### =======================================

rstages = cboost$getRiskStages()
knitr::kable(rstages)

rstages = rstages[-1, ]
rstages$stage = factor(rstages$stage, levels = rev(rstages$stage))

#gg_explained_per_stage = ggplot(rstages, aes(x = "", y = percentage, fill = stage)) +
  #geom_bar(stat = "identity") +
  #theme(legend.position = "bottom") +
  #coord_flip() +
  scale_y_reverse() +
  #xlab("") +
  #ylab("Percentage of explained risk") +
  ggtitle("Explained risk per stage") +
  #labs(fill = "") +
  #ggsci::scale_fill_uchicago()


log_uni = cboost$model$univariate$getLoggerData()
log_uni$stage = "univariate"
log_int = cboost$model$interactions$getLoggerData()
log_int$stage = "pairwise interactions"

log_deep = do.call(rbind, lapply(cboost$model$deeper_interactions$trees, function(x) {
  data.frame(oob_risk = x$test_risk, train_risk = x$train_risk, stage = "deep interactions")
}))
cols_risk = c("train_risk", "oob_risk", "stage")
log = rbind(log_uni[, cols_risk], log_int[, cols_risk], log_deep[, cols_risk])
log$iters = seq_len(nrow(log))

log$stage[log$stage == "deep interactions"] = "deep trees"

df_stages = rstages
df_stages$auc_end = rstages$value
df_stages$auc_start = c(max(log$train_risk), rstages$value[-3])
df_stages$stage = c("univariate", "pairwise interactions", "deep trees")

df_text = data.frame(text = paste0(as.character(round(rstages$percentage, 3) * 100), " %"),
  auc = df_stages$auc_end - diff(c(df_stages$auc_start, df_stages$auc_end[3])) / 2,
  stage = c("univariate", "pairwise interactions", "deep trees"))

gg_risk = ggplot(log, aes(x = iters, color = stage)) +
  geom_line(aes(y = train_risk, linetype = "Train risk")) +
  geom_line(aes(y = oob_risk, linetype = "Validation risk")) +
  geom_segment(data = df_stages, mapping = aes(x = 1, y = auc_start, xend = 1, yend = auc_end)) +
  geom_label(data = df_text, mapping = aes(x = 70, y = auc, label = text, fill = stage),
    color = "white", fontface = "bold", show.legend = FALSE, size = 2) +
  ylab("Risk") +
  xlab("Iteration") +
  labs(linetype = "", color = "Stage") +
  ggsci::scale_color_uchicago() +
  ggsci::scale_fill_uchicago() +
  ggtitle("Risk traces")

ggsave(
  plot = gg_risk,
  filename = "fig-complexity.pdf",
  width = dinA4width,
  height = dinA4width * 0.35,
  units = "mm")


#mylegend = g_legend(gg_risk)

#gt1 = ggplot_gtable(ggplot_build(gg_risk + theme(legend.position = "none")))
#gt2 = ggplot_gtable(ggplot_build(gg_explained_per_stage + theme(legend.position = "none")))

#gt2$widths = gt1$width
#dev.off()

#p3 = grid.arrange(
  #arrangeGrob(
    #gt1,
    #gt2,
    #nrow = 2,
    #heights = c(4, 2)),
  #mylegend, nrow = 1, widths = c(5, 3))
#dev.off()

#ggsave(
  #plot = p3,
  #filename = "figures/fig-complexity.pdf",
  #width = dinA4width,
  #height = dinA4width * 0.4,
  #units = "mm")



#### Variable importance and PE
#### =======================================

cboost$model$univariate$calculateFeatureImportance()
vip = cboost$model$univariate$calculateFeatureImportance(aggregate_bl_by_feat = TRUE)

vip$stage = "VIP: Uni. effects"
vip$fnum = rev(seq_len(nrow(vip)))

gg_vip_uni = ggplot(vip, aes(x = risk_reduction, y = fnum)) +
  geom_vline(xintercept = 0, color = "dark grey", alpha = 0.6) +
  geom_segment(aes(xend = 0, yend = fnum)) +
  geom_point() +
  ylab("") +
  xlab("Risk reduction") +
  labs(color = "", fill = "") +
  scale_y_continuous(labels = vip$feature, breaks = vip$fnum) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  facet_grid(. ~ stage)

vip_int = cboost$model$interactions$calculateFeatureImportance()
top_interaction = vip_int$baselearner[1]
extractInt = function(x) {
  x = gsub("_tensor", "", x)
  gsub("_", " ", x)
}

vip_int$stage = "VIP: Pairwise int."
vip_int$fnum = rev(seq_len(nrow(vip_int)))
vip_int$baselearner = c("age cap.gain", "cap.loss marital", "cap.gain hours (week)", "cap.gain educ.num", "cap.loss relation", "age educ.num")

gg_vip_int = ggplot(vip_int, aes(x = risk_reduction, y = fnum)) +
  geom_vline(xintercept = 0, color = "dark grey", alpha = 0.6) +
  geom_segment(aes(xend = 0, yend = fnum)) +
  geom_point() +
  ylab("") +
  xlab("Risk reduction") +
  labs(color = "", fill = "") +
  scale_y_continuous(labels = vip_int$baselearner, breaks = vip_int$fnum) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3), limits = c(0, 0.004)) +
  facet_grid(. ~ stage)


#p3 = grid.arrange(gg_vip_uni, gg_vip_int, nrow = 1, widths = c(3, 4))
#dev.off()

#ggsave(
  #plot = p3,
  #filename = "figures/fig-vip.pdf",
  #width = dinA4width,
  #height = dinA4width * 0.3,
  #units = "mm")


#### Explaining the models decision making
#### =======================================

coefs = cboost$model$univariate$getEstimatedCoef()
offset = coefs$offset

# numeric
extract = cboost$model$univariate$extractComponents()
pe_numeric = predict(extract, newdata = task$data())

# categorical
pe_cat = coefs[grepl("Categorical", vapply(coefs, function(cf) {
    atr = attr(cf, "blclass")
    if (is.null(atr))
      return("Offset")
    else
      return(atr)
  }, character(1L)))]

# Visualize top vars:

# Visualize categorical features:
#gg_cat = plotCategorical(c("marital.status", "relationship", "occupation"), pe_cat) +
cnums = c("Div.", "Marr.-AFs", "Marr.-civs", "Marr.-abs.s", "Never-marr.", "Sep.", "Wid.")
.
gg_cat = plotCategorical("marital.status", pe_cat, cnums) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize numerical features:
gg_num = plotNumeric(c("education.num", "age"), pe_numeric) +
  ggsci::scale_color_uchicago() +
  theme(legend.position = "right")


coefs_int = cboost$model$interactions$getEstimatedCoef()[[top_interaction]]

dat0 = cboost$model$univariate$data

dat = expand.grid(age = seq(min(dat0$age), max(dat0$age), length.out = 100),
  capital.gain = seq(min(dat0$capital.gain), max(dat0$capital.gain), length.out = 100))

n_knots = 8
degree = 3

dat$y = predictTensor(dat, dat0, coefs_int, c("age", "capital.gain"), n_knots, degree)
dat0$y = predictTensor(dat0, dat0, coefs_int, c("age", "capital.gain"), n_knots, degree)

dat$feat1 = "age"
dat0$feat1 = "age"
dat$feat2 = "capital.gain"
dat0$feat2 = "capital.gain"

gg_int = ggplot() +
  geom_point(data = dat0, aes(x = age, y = capital.gain, color = y), size = 0) +
  colorspace::scale_color_continuous_sequential(palette = "Viridis", rev = FALSE) +
  geom_contour_filled(data = dat, aes(x = age, y = capital.gain, z = y), bins = 15, show.legend = FALSE) +
  geom_rug(data = dat0, aes(x = age, y = capital.gain)) +
  labs(color = "Contribution\nto prediction") +
  xlab("") +
  ylab("") +
  facet_grid(feat2 ~ feat1)




### Patchworking all figures together:
### ------------------------------------------------

library(patchwork)

gg_empty = ggplot() + theme_void()

p3 = ((gg_vip_uni / gg_vip_int) | gg_empty | (gg_num / (gg_cat + gg_int))) +
    plot_layout(ncol = 3, widths = c(3, 1, 8))

scale_factor_text = 1.6
theme_set(
  theme_bw(base_family = font) +
  #theme_minimal() +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = scale_factor_text*6),
    axis.text = element_text(size = scale_factor_text*6),
    axis.title = element_text(size = scale_factor_text*8),
    plot.title = element_text(size = scale_factor_text*9),
    plot.subtitle = element_text(size = scale_factor_text*8),
    legend.title = element_text(size = scale_factor_text*8),
    legend.text = element_text(size = scale_factor_text*7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
)

scale_factor_fig = 2
ggsave(
  plot = p3,
  filename = "figures/fig-pe.pdf",
  width = scale_factor_fig * dinA4width,
  height = scale_factor_fig * dinA4width * 0.45,
  units = "mm")



## Prediction decomposition

theme_set(
  theme_bw(base_family = font) +
  #theme_minimal() +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
)



# "new observation"
newdata = task$data()[1234,]
newdata$class = NULL
knitr::kable(t(newdata))

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

gg_dec = ggplot(df_plt0, aes(x = value, y = bl_num, color = stage, fill = stage)) +
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
  ggtitle(title, subtitle)

ggsave(
  plot = gg_dec,
  filename = "figures/fig-dec.pdf",
  width = dinA4width * 1.3,
  height = dinA4width * 0.6,
  units = "mm")









library(ggplot2)
library(dplyr)
library(facetscales)
library(ggh4x)
library(ggthemes)


getBest = function(score, type) {
  minmax = min
  if (type[1] == "auc")
    minmax = max
  ifelse(score == minmax(score, na.rm = TRUE), "best", "other")
}
getDiffToBest = function(score, type) {
  minmax = min
  other = max
  if (type[1] == "auc") {
    minmax = max
    other = min
  }
  idx = minmax(score, na.rm = TRUE) == score
  idx_w = other(score, na.rm = TRUE) == score
  score_best = score[idx]
  abs(score -  score[idx_w]) / abs(score_best - score[idx_w])
}

dat = read.csv("final_scores.csv")
dat = dat %>% filter(! task_id %in% c(9981, 168332, 168910, 168868, 34539, 189354, 189356, 7593))

fmap = c("AutoCWB (no HPO)", "AutoCWB (Deep, no HPO)", "glmnet", "AutoCWB", "AutoCWB (Deep)",
  "Featureless", "RF", "RF (HPO)", "auto-sklearn", "H2O-AutoML", "TPOT", "Auto-Weka")
names(fmap) = unique(dat$framework)
dat$framework = fmap[dat$framework]

funique = unique(dat$framework)
funique[3:5] = funique[4:6]
funique[6] = "glmnet"
ftab = seq_along(funique)
names(ftab) = funique




idx_binary = dat$type == "auc"

tunique1 = as.character(unique(dat$task_id[idx_binary]))
tunique2 = as.character(unique(dat$task_id[! idx_binary]))

ttab1 = seq_along(tunique1)
ttab2 = seq_along(tunique2)
ttab = c(ttab1, ttab2)
names(ttab) = c(tunique1, tunique2)

dat_plt = dat %>%
  mutate(task = as.character(task_id)) %>%
  group_by(task, framework) %>%
  summarize(score = mean(score, na.rm = TRUE), type = type[1]) %>%
  group_by(task) %>%
  mutate(best = getBest(score, type), sdiff = getDiffToBest(score, type)) %>%
  group_by(type) %>%
  mutate(task_num = ttab[task], framework_num = ftab[framework])

scales_x = list(
  "Binary classification" = scale_x_continuous(breaks = ttab1, labels = tunique1),
  "Multiclass classification" = scale_x_continuous(breaks = ttab2, labels = tunique2)
)

dat_plt$ptype = ifelse(dat_plt$type == "auc", "Binary classification", "Multiclass classification")
dat_plt = dat_plt %>% filter(is.finite(sdiff), ! is.nan(sdiff))

#fmap = c(
  #"autocompboost_no_tuning" = "AutoCWB (two stages, no HPO)",
  #"autocompboost.with_trees_no_tuning" = "AutoCWB (three stages, no HPO)",
  #"constantpredictor" = "Featureless",
  #"randomforest" = "Random Forest",
  #"tunedrandomforest" = "Random Forest (HPO)",
  #"autosklearn" = "auto-sklearn",
  #"h2oautoml" = "H2O-AutoML",
  #"tpot" = "TPOT",
  #"autoweka" = "Auto-WEKA"
#)

dat_all = rbind(expand.grid(x = ttab1, y = ftab, ptype = "Binary classification"),
  expand.grid(x = ttab2, y = ftab, ptype = "Multiclass classification"))

gg_bm = ggplot() +
  geom_tile(data = dat_all, aes(x = x, y = y), fill = "gray", width = 0.95) +
  geom_text(data = dat_all, aes(x = x, y = y), label = "NA", size = 0.95) +
  geom_tile(data = dat_plt, aes(x = task_num, y = framework_num, fill = sdiff), width = 0.95) +
  geom_tile(data = dat_plt %>% filter(best == "best"), aes(y = framework_num, x = task_num), fill = "#EE9A00", width = 0.95) +
  #geom_text(data = dat_plt, aes(x = task_num, y = framework_num, label = round(score, 2)), size = 1.5) +
  #geom_text(data = dat_plt %>% filter(best == "best"), aes(x = task_num, y = framework_num, label = round(score, 2)),
    #color = "black", fontface = "bold", size = 1.5) +
  scale_y_continuous(breaks = unique(dat_plt$framework_num), labels = unique(dat_plt$framework)) +
  xlab("") +
  ylab("") +
  labs(fill = "Relative performance\n(1 = best, 0 = worst)") +
  colorspace::scale_fill_continuous_sequential(palette = "Greens", rev = TRUE) +
  #colorspace::scale_fill_continuous_sequential(palette = "Viridis") +
  facetscales::facet_grid_sc(cols = vars(ptype), scales = list(x = scales_x)) +
  ggh4x::force_panelsizes(cols = c(1, 0.75)) +
  theme_minimal(base_family = font) +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 8),
    legend.title = element_text(size = 7, vjust = 1, hjust = 1),
    legend.text = element_text(size = 7),
    legend.box.margin = margin(-20,-10,-10,-120),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
    legend.position = "bottom"
  )
gg_bm
ggsave(
  plot = gg_bm,
  filename = "figures/fig-bm.pdf",
  width = 1*dinA4width,
  height = dinA4width * 0.5,
  units = "mm")

