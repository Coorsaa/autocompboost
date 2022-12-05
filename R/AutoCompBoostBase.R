#' @title AutoCompBoostBase
#'
#' @description
#' Base class for AutoCompBoost. Has subclasses for Classification and Regression.
#'
#' @section Internals:
#' The AutoCompBoostBase class uses [mlr3pipelines] to create a machine learning pipeline. \cr
#' This pipeline contains multiple preprocessing steps wrapped in a [GraphLearner][mlr3pipelines::GraphLearner]. \cr
#' This [GraphLearner][mlr3pipelines::GraphLearner] is wrapped in an [AutoTuner][mlr3tuning::AutoTuner] for Hyperparameter Optimization and proper resampling. \cr
#' Tuning is performed using Bayesian Optimization.
#'
#' @section Construction:
#' Objects should be created using the [AutoCompBoost][autocompboost::AutoCompBoost] interface function.
#' ```
#' model = AutoCompBoost(task, resampling, measure, tuning_budget, tuning_iters, final_model)
#' ```
#'
#' @param task ([`Task`][mlr3::Task]) \cr
#' Contains the task to be solved. Currently [`TaskClassif`][mlr3::TaskClassif] and [`TaskRegr`][mlr3::TaskRegr] are supported.
#' @param resampling ([Resampling][mlr3::Resampling]) \cr
#' Contains the resampling method to be used for hyper-parameter optimization.
#' Defaults to [ResamplingCV][mlr3::ResamplingCV] with 3 folds.
#' @param param_values (`list()`) \cr
#' Parameter values which are pass on to the learner.
#' @param measure ([Measure][mlr3::Measure]) \cr
#' Contains the performance measure, for which we optimize during training. \cr
#' Defaults to [Accuracy][mlr3measures::acc] for classification and [RMSE][mlr3measures::rmse] for regression.
#' @param tuning_method (`character(1)`) \cr
#' Tuning method. Possible choices are `"mbo"`, `"hyperband"` or `"smashy"`¸ Default is `"mbo"`.
#' @param tuning_time (`integer(1)`) \cr
#' Termination criterium. Number of seconds for which to run the optimization. Does *not* include training time of the final model. \cr
#' Default is set to `3600`, i.e. one hour. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param tuning_iters (`integer(1)`) \cr
#' Termination criterium. Number of MBO iterations for which to run the optimization. \cr
#' Default is set to `150` iterations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param tuning_generations (`integer(1)`) \cr
#' Termination criterium for tuning method `smashy`. Number of generations for which to run the optimization. \cr
#' Default is set to `3` generations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @param enable_tuning (`logical(1)`) \cr
#' Whether or not to perform hyperparameter optimization. Default is `TRUE`.
#' @param final_model (`logical(1)`) \cr
#' Whether or not to return the final model trained on the whole dataset at the end.
#'
#' @field task ([`Task`][mlr3::Task]) \cr
#' Contains the task to be solved.
#' @field learner ([AutoTuner][mlr3tuning::AutoTuner]) \cr
#' The ML pipeline at the core of mlr3automl is an [AutoTuner][mlr3tuning::AutoTuner] containing a [GraphLearner][mlr3pipelines::GraphLearner].
#' @field param_values (`list()`) \cr
#' Parameter values which are pass on to the learner.
#' @field resampling ([Resampling][mlr3::Resampling]) \cr
#' Contains the resampling method to be used for hyper-parameter optimization.
#' @field measure ([Measure][mlr3::Measure]) \cr
#' Contains the performance measure, for which we optimize during training. \cr
#' @field tuning_method (`character(1)`) \cr
#' Tuning method. Possible choices are `"mbo"`, `"hyperband"` or `"smashy"`¸ Default is `"smashy"`.
#' @field tuning_time (`integer(1)`) \cr
#' Termination criterium. Number of seconds for which to run the optimization. Does *not* include training time of the final model. \cr
#' Default is set to `60`, i.e. one minuet. Tuning is terminated depending on the first termination criteria fulfilled.
#' @field tuning_iters (`integer(1)`) \cr
#' Termination criterium. Number of MBO iterations for which to run the optimization. \cr
#' Default is set to `150` iterations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @field tuning_generations (`integer(1)`) \cr
#' Termination criterium for tuning method `smashy`. Number of generations for which to run the optimization. \cr
#' Default is set to `3` generations. Tuning is terminated depending on the first termination criteria fulfilled.
#' @field enable_tuning (`logical(1)`) \cr
#' Whether or not to perform hyperparameter optimization. Default is `TRUE`.
#' @field final_model (`logical(1)`) \cr
#' Whether or not to return the final model trained on the whole dataset at the end.
#' @field tuner ([TunerInterMBO][mlrintermbo::TunerInterMBO]) \cr
#' Tuning is performed using [TunerInterMBO][mlrintermbo::TunerInterMBO].
#' @field tuning_terminator ([Terminator][bbotk::Terminator]) \cr
#' Contains an termination criterion for model tuning. \cr
#'
#' @rawNamespace import(mlr3, except = c(lrn, lrns))
#' @import compboost
#' @import rpart
#' @import ranger
#' @import mlr3misc
#' @import mlr3oml
#' @import mlr3pipelines
#' @import mlrintermbo
#' @import mlr3tuning
#' @import mlr3hyperband
#' @import mlr3learners
#' @import paradox
#' @import ggplot2
#' @import checkmate
#' @import testthat
#' @importFrom R6 R6Class
#' @import data.table
AutoCompBoostBase = R6::R6Class("CompBoostBase",
  public = list(
    task = NULL,
    learner = NULL,
    param_values = NULL,
    resampling = NULL,
    measure = NULL,
    tuning_method = NULL,
    tuning_time = NULL,
    tuning_iters = NULL,
    enable_tuning = TRUE,
    final_model = NULL,
    tuner = NULL,
    tuning_terminator = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @return [AutoCompBoostBase][autocompboost::AutoCompBoostBase]
    initialize = function(task, resampling, param_values, measure, tuning_method,
      tuning_time, tuning_iters, tuning_generations, enable_tuning, final_model) { # FIXME possibly add: , stratify = TRUE, tune_threshold = TRUE) {

      if (!is.null(resampling)) assert_resampling(resampling)
      if (!is.null(measure)) assert_measure(measure)

      self$task = assert_task(task)
      self$resampling = resampling %??% rsmp("cv", folds = 3)
      self$param_values = assert_list(param_values, null.ok = TRUE)
      self$measure = assert_measure(measure)
      self$enable_tuning = assert_logical(enable_tuning)
      self$tuning_time = assert_number(tuning_time, lower = 0)
      self$tuning_iters = assert_number(tuning_iters, lower = 0)
      check_subset(tuning_method, choices = c("mbo", "hyperband"))
      self$tuning_method = assert_character(tuning_method, len = 1)
      if (self$tuning_method == "hyperband") {
        self$tuning_terminator = trm("none")
      } else {
        self$tuning_terminator = trm("run_time", secs = self$tuning_time)
      }
      # self$tuning_terminator = trm("combo", list(
      #   trm("run_time", secs = self$tuning_time),
      #   trm("evals", n_evals = self$tuning_iters)
      #   ), any = TRUE
      # )

      if (tuning_method == "mbo") {
        self$tuner = tnr("intermbo")
      } else if (tuning_method == "hyperband") {
        self$tuner = tnr("hyperband", eta = 2, repetitions = 3)
      }# else if (tuning_method == "smashy") {
      #   self$tuner = tnr("smashy", fidelity_steps = 3, # FIXME: change after fix in smashy
      #     ftr("maybe", p = 0.5, filtor = ftr("surprog",
      #       surrogate_learner = lrn("regr.ranger"),
      #       filter.pool_factor = 10)),
      #     mu = 20, survival_fraction = 0.5
      #   )
      #   self$tuning_terminator = trm("gens", generations = tuning_generations)
      # }
      self$learner = private$.create_learner(param_values)
      self$final_model = assert_logical(final_model)
    },

    #' @description
    #' Trains the AutoML system.
    #' @param row_ids (`integer()`)\cr
    #' Vector of training indices.
    train = function(row_ids = NULL) {
      self$learner$train(self$task, row_ids)
      if (self$enable_tuning) {
        if (length(self$learner$learner$errors) > 0) {
          warning("An error occured during training. Fallback learner was used!")
          print(self$learner$learner$errors)
        }
        if (self$final_model)
        private$.final_model = self$learner$learner$model[[paste0(self$task$task_type, ".compboost")]]$model
      } else {
        if (length(self$learner$errors) > 0) {
          warning("An error occured during training. Fallback learner was used!")
          print(self$learner$errors)
        }
        if (self$final_model) {
          if ("multiclass" %in% self$task$properties) {
            private$.final_model = self$learner$model[[paste0(self$task$task_type, ".compboost")]]
          } else {
            private$.final_model = self$learner$model[[paste0(self$task$task_type, ".compboost")]]$model
          }

        }

      }
    },

    #' @description
    #' Returns a [Prediction][mlr3::Prediction] object for the given data based on the trained model.
    #' @param data ([data.frame] | [data.table] | [Task][mlr3::Task]) \cr
    #' New observations to be predicted. If `NULL`, defaults to the task the model
    #' was trained on.
    #' @param row_ids (`integer()`) \cr
    #' Vector of training indices.
    #' @return [`PredictionClassif`][mlr3::PredictionClassif] | [`PredictionRegr`][mlr3::PredictionRegr]
    predict = function(data = NULL, row_ids = NULL) {
      if (is.null(data)) {
        return(self$learner$predict(self$task, row_ids))
      } else {
        return(self$learner$predict(data, row_ids))
      }
    },

    #' @description
    #' Performs nested resampling if `enable_tuning` equals `TRUE`.
    #' @param outer_resampling ([`Resampling`][mlr3::Resampling]) \cr
    #' Resampling strategy. Default is [`ResamplingHoldout`][mlr3::ResamplingHoldout] for the outer resampling.
    #' @return [`ResampleResult`][mlr3::ResampleResult]
    resample = function(outer_resampling = NULL) {
      outer_resampling = outer_resampling %??% rsmp("holdout")
      assert_resampling(outer_resampling)

      private$.resample_result = mlr3::resample(self$task, self$learner, outer_resampling)
      self$learner = private$.resample_result$learners[[1]]
      if (self$enable_tuning) {
        if (length(self$learner$learner$errors) > 0) {
          warning("An error occured during training. Fallback learner was used!")
          print(self$learner$learner$errors)
        }
      } else {
        if (length(self$learner$errors) > 0) {
          warning("An error occured during training. Fallback learner was used!")
          print(self$learner$errors)
        }
      }
      return(private$.resample_result)
    },

    #' @description
    #' Helper to extract the best hyperparameters from a tuned model.
    #' @return [data.table]
    tuned_params = function() {
      if (is.null(self$learner$tuning_instance$archive)) {
        warning("Model has not been trained. Run the $train() method first.")
      } else {
        return(self$learner$tuning_instance$archive$best())
      }
    },

    #' @description
    #' Returns the trained model if `final_model` is set to TRUE.
    #' @return [`Compboost`][compboost::Compboost]
    model = function() {
      private$.check_trained()
      return(private$.final_model)
    },

    #' @description
    #' Returns the resample result of method `resample()`.
    #' @return [`mlr3`][mlr3::ResampleResult]
    resample_result = function() {
      if (is.null(private$.resample_result)) {
        warning("Model has not been resampled yet. Run the $resample() method first.")
      } else {
        return(private$.resample_result)
      }
    },

    #' @description
    #' Returns the scores of the `resample_result`.
    #' @return [`data.table`][data.table::`data.table`]
    score = function(measure = NULL) {
      if (is.null(private$.resample_result)) {
        warning("Model has not been resampled yet. Run the $resample() method first.")
      } else {
        return(private$.resample_result$score(measure))
      }
    },

    #' @description
    #' Returns the aggregated score of the `resample_result`.
    #' @return (`numeric`)
    aggregate = function(measure = NULL) {
      if (is.null(private$.resample_result)) {
        warning("Model has not been resampled yet. Run the $resample() method first.")
      } else {
        return(private$.resample_result$aggregate(measure))
      }
    },

    #' @description
    #' Returns the risk stages of the final model.
    #' @return (`list`)
    getRiskStages = function(log_entry = "train_risk") {
      private$.check_trained()
      out = data.frame(stage = c("featureless", "univariate", "pairwise interactions", "deep interactions"),
        value = rep(NA_real_, 4L), explained = rep(NA_real_, 4L), percentage = rep(NA_real_, 4L),
        iterations = rep(NA_integer_, 4L))

      if ("univariate" %in% names(private$.final_model)) {
        logs = private$.final_model$univariate$getLoggerData()
        if (! log_entry %in% names(logs))
          stop("No log entry", log_entry, "found in compboost log.")
        out$value[1] = logs[[log_entry]][logs$baselearner == "intercept"]
        out$value[2] = tail(logs[[log_entry]], 1)
        out$iterations[1] = 0
        out$iterations[2] = max(logs[["_iterations"]])
      } else {
        stop("At least univariate model must be given!")
      }
      if ("interactions" %in% names(private$.final_model)) {
        logs = private$.final_model$interactions$getLoggerData()
        if (! log_entry %in% names(logs))
          stop("No log entry", log_entry, "found in compboost log.")
        out$value[3] = tail(logs[[log_entry]], 1)
        out$iterations[3] = max(logs[["_iterations"]])
      }
      if ("deeper_interactions" %in% names(private$.final_model)) {
        vals = vapply(X = private$.final_model$deeper_interactions$trees, FUN = function(tree) {
          if (! log_entry %in% names(tree))
            stop("Cannot find log entry", log_entry, "in tree.")
          return(tree[[log_entry]])
        }, FUN.VALUE = numeric(1L))
        if (length(vals) == 0)
          out$value[4] = NA_real_
        else
          out$value[4] = tail(vals, 1L)

        out$iterations[4] = length(vals)
      }
      out$explained = c(0, -diff(out$value))
      out$percentage = out$explained / sum(out$explained, na.rm = TRUE)
      return(out)
    },

    #' @description
    #' Plot function to plot the final model's risk stages.
    #' @return [`patchwork`][patchwork::wrap_plots]
    plotRiskStages = function(log_entry = "train_risk") {
      private$.check_trained()
      if (is.null(private$.risk_plot)) {
        rstages = self$getRiskStages(log_entry)[-1, ]
        rstages$stage = factor(rstages$stage, levels = rstages$stage)

        log_uni = private$.final_model$univariate$getLoggerData()
        log_uni$stage = "univariate"
        log_int = private$.final_model$interactions$getLoggerData()
        log_int$stage = "pairwise interactions"

        log_deep = do.call(rbind, lapply(private$.final_model$deeper_interactions$trees, function(x) {
          data.frame(oob_risk = x$test_risk, train_risk = x$train_risk, stage = "deep interactions")
        }))
        cols_risk = c("train_risk", "oob_risk", "stage")
        log = rbind(log_uni[, cols_risk], log_int[, cols_risk], log_deep[, cols_risk])
        log$iters = seq_len(nrow(log))
        log$stage = factor(log$stage, levels = levels(rstages$stage))

        df_stages = rstages
        df_stages$loss_end = rstages$value
        df_stages$loss_start = c(max(log$train_risk), rstages$value[-3])
        df_stages$stage = c("univariate", "pairwise interactions", "deep interactions")
        df_stages$stage = factor(df_stages$stage, levels = levels(rstages$stage))
        df_stages[is.na(df_stages$loss_end), "loss_end"] = df_stages[is.na(df_stages$loss_end), "loss_start"]

        df_text = data.frame(text = paste0(as.character(round(rstages$percentage, 3) * 100), " %"),
          loss = df_stages$loss_end - diff(c(df_stages$loss_start, df_stages$loss_end[3])) / 2,
          stage = c("univariate", "pairwise interactions", "deep trees"))
        df_text$stage = factor(df_text$stage, levels = levels(rstages$stage))
        # df_text$height = df_stages$percentage / 2


        sp = ggplot(df_stages, aes(x = "", y = percentage * 100, fill = stage)) +
          geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
          theme(legend.position = "bottom") +
          # geom_label(data = df_text, mapping = aes(x = "", y = 100 - (height * 100), label = text, fill = stage),
            # color = "white", fontface = "bold", show.legend = FALSE, size = 3) +
          scale_y_reverse() +
          xlab("") +
          ylab("Percentage of explained risk per stage") +
          labs(fill = "") +
          theme_minimal() +
          ggsci::scale_fill_uchicago() +
          theme(legend.position = "none")

        tp = ggplot(log, aes(x = iters, color = stage)) +
          geom_line(aes(y = train_risk, linetype = "Train risk")) +
          geom_line(aes(y = oob_risk, linetype = "Validation risk")) +
          geom_label(data = df_text, mapping = aes(x = 70, y = loss, label = text, fill = stage),
            color = "white", fontface = "bold", show.legend = FALSE, size = 4) +
          ylab("Risk") +
          xlab("Iteration") +
          labs(linetype = "", color = "Stage") +
          ggsci::scale_color_uchicago() +
          ggsci::scale_fill_uchicago() +
          theme_minimal() +
          ggtitle("Risk traces")
        private$.risk_plot = patchwork::wrap_plots(sp, tp, ncol = 2, widths = c(0.1, 0.9))
      }


      return(private$.risk_plot)
    },

    #' @description
    #' Plot function to plot the feature importance.
    #' @return [`patchwork`][patchwork::wrap_plots]
    getFeatureImportance = function() {
      private$.check_trained()
      if (is.null(private$.fi_data)) {
        fi_uni = private$.final_model$univariate$calculateFeatureImportance(aggregate_bl_by_feat = TRUE)
        fi_uni$stage = "Univariate Effects"
        fi_uni$fnum = rev(seq_len(nrow(fi_uni)))

        fi_int = private$.final_model$interactions$calculateFeatureImportance()
        # top_interaction = fi_int$baselearner[1]

        fi_int$stage = "Pairwise Interactions"
        fi_int$fnum = rev(seq_len(nrow(fi_int)))
        fi_int$baselearner = sapply(
          stringi::stri_split_fixed(fi_int$baselearner, pattern = "_"),
          function(x) paste(x[1], x[2], sep = " x ")
        )
        private$.fi_data = list(univariate = fi_uni, interactions = fi_int)
      }
      return(private$.fi_data)
    },

    #' @description
    #' Plot function to plot the feature importance.
    #' @return [`patchwork`][patchwork::wrap_plots]
    plotFeatureImportance = function() {
      private$.check_trained()
      if (is.null(private$.fi_plot)) {
        fi_data = self$getFeatureImportance()
        fi_uni = fi_data$univariate
        gg_fi_uni = ggplot(fi_uni, aes(x = risk_reduction, y = fnum)) +
          geom_vline(xintercept = 0, color = "dark grey", alpha = 0.6) +
          geom_segment(aes(xend = 0, yend = fnum)) +
          geom_point() +
          ylab("") +
          xlab("Risk reduction") +
          ggtitle("Feature importance: Univariate Effects") +
          labs(color = "", fill = "") +
          scale_y_continuous(labels = fi_uni$feature, breaks = fi_uni$fnum) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
          theme_minimal()

        fi_int = fi_data$interactions
        gg_fi_int = ggplot(fi_int, aes(x = risk_reduction, y = fnum)) +
          geom_vline(xintercept = 0, color = "dark grey", alpha = 0.6) +
          geom_segment(aes(xend = 0, yend = fnum)) +
          geom_point() +
          ylab("") +
          xlab("Risk reduction") +
          ggtitle("Feature Importance: Pairwise Interactions") +
          labs(color = "", fill = "") +
          scale_y_continuous(labels = fi_int$baselearner, breaks = fi_int$fnum) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
          theme_minimal()

        private$.fi_plot = patchwork::wrap_plots(gg_fi_uni, gg_fi_int)
      }
      return(private$.fi_plot)
    },

    #' @description
    #' Plot function to plot the univariate effects.
    #' @return [`patchwork`][patchwork::wrap_plots]
    plotUnivariateEffects = function(top_n_effects = 3L) {
      private$.check_trained()
      if (is.null(private$.uni_effects_plot)) {
        feats = self$getFeatureImportance()$univariate$feature
        uni_effects = lapply(
          feats[1L:min(length(feats), top_n_effects)], function(f) {
            plotPEUni(private$.final_model$univariate, feat = f)
          }
        )
        private$.uni_effects_plot = patchwork::wrap_plots(uni_effects)
      }
      return(private$.uni_effects_plot)
    },

    #' @description
    #' Plot function to plot the interaction effects.
    #' @return [`patchwork`][patchwork::wrap_plots]
    plotInteractionEffects = function(top_n_effects = 3L) {
      private$.check_trained()
      if (is.null(private$.int_effects_plot)) {
        feats = private$.final_model$interactions$calculateFeatureImportance()$baselearner
        int_effects = lapply(
          feats[1L:min(length(feats), top_n_effects)], function(f) {
            plotTensor(private$.final_model$interactions, f)
          }
        )
        if(!is.null(int_effects))
          private$.uni_effects_plot = patchwork::wrap_plots(int_effects)
      }
      return(private$.uni_effects_plot)
    },

    # #' @description
    # #' Returns the model summary
    # #' @return [`data.table`][data.table::data.table]
    # summary = function() {
    #   if (is.null(self$learner$model)) {
    #     warning("Model has not been trained. Run the $train() method first.")
    #   } else {
    #     return(self$.final_model)
    #   }
    # },

    #' @description
    #' Returns the selected base learners by the final model.
    #' @return (`character()`)
    getSelectedBaselearner = function() {
      private$.check_trained()
      return(private$.final_model$getSelectedBaselearner())
    },

    #' @description
    #' Plot function to plot a single spline.
    #' @param spline (`character(1L)`) \cr
    #' Name of spline to plot.
    #' @return [`ggplot`][ggplot2::ggplot]
    plotSpline = function(spline = character(1L)) {
      assert_character(spline, len = 1L)
      private$.check_trained()
      return(private$.final_model$getSelectedBaselearner() + ggplot2::theme_bw())
    },

    #' @description
    #' Plot function to plot the learner traces.
    #' @return [`ggplot`][ggplot2::ggplot]
    plotBlearnerTraces = function() {
      private$.check_trained()
      return(private$.final_model$plotBlearnerTraces() + ggplot2::theme_bw())
    }
  ),

  private = list(
    .final_model = NULL,
    .resample_result = NULL,
    .risk_plot = NULL,
    .fi_data = NULL,
    .fi_plot = NULL,
    .uni_effects_plot = NULL,
    .int_effects_plot = NULL,
    .check_trained = function() {
      if (is.null(private$.final_model) | self$final_model == FALSE) {
        stop("Argument `final_model` has been set to `FALSE` or model has not been trained yet. Run the $train() method first.")
      }
    },
    .create_learner = function(param_values = NULL) {
      # get preproc pipeline
      if(self$tuning_method %in% c("hyperband", "smashy")) {
        if (self$task$task_type == "classif") {
          pipeline = autocompboost_preproc_pipeline(self$task, max_cardinality = 1000) %>>% po("subsample", stratify = TRUE)
        } else {
          pipeline = autocompboost_preproc_pipeline(self$task, max_cardinality = 1000) %>>% po("subsample")
        }
      } else {
          pipeline = autocompboost_preproc_pipeline(self$task, max_cardinality = 1000)
      }

      # compboost learner
      learner = lrn(paste0(self$task$task_type, ".compboost"), show_output = TRUE)
      if (!is.null(self$param_values)) {
        learner$param_set$values = insert_named(learner$param_set$values, self$param_values)
      }
      if (self$task$task_type == "classif") {
        learner$predict_type = "prob"
      }
      if (self$task$task_type == "classif" && length(self$task$class_names) > 2) {
          # pipelne for multiclass
          # can be removed when compoboost supports multiclass classification
          pipeline = pipeline %>>%
            pipeline_ovr(learner)
      } else {
        pipeline = pipeline %>>%
          learner
      }

      # create graphlearner
      graph_learner = as_learner(pipeline)

      if (!self$enable_tuning) {
        graph_learner$id = paste0(self$task$task_type, ".autocompboost")
        return(graph_learner)
      } else {
        # fallback learner is featureless learner for classification / regression
        # graph_learner$fallback = lrn(paste0(self$task$task_type, ".featureless"))
        # use callr encapsulation so we are able to kill model training, if it
        # takes too long
        # graph_learner$encapsulate = c(train = "callr", predict = "callr")

        param_set = autocompboost_default_params(self$task$task_type, self$tuning_method)

        # FIXME: use hard timeout from mlr3automl here?
        # if (is.finite(self$tuning_time)) {
        #  tuner = TunerWrapperHardTimeout$new(
        #    tuner,
        #    timeout = self$tuning_time
        #  )
        # }

        at = AutoTuner$new(
          learner = graph_learner,
          resampling = self$resampling,
          measure = self$measure,
          search_space = param_set,
          terminator = self$tuning_terminator,
          tuner = self$tuner
        )
        at$id = paste0(self$task$task_type, ".autocompboost")
        return(at)
      }
    }
  )
)
