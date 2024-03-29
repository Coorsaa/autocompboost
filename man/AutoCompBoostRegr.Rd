% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoCompBoostRegr.R
\name{AutoCompBoostRegr}
\alias{AutoCompBoostRegr}
\title{AutoCompBoostRegr}
\arguments{
\item{task}{(\code{\link[mlr3:Task]{Task}}) \cr
\code{\link[mlr3:TaskRegr]{TaskRegr}} to be solved.}

\item{resampling}{(\link[mlr3:Resampling]{Resampling}) \cr
Contains the resampling method to be used for hyper-parameter optimization.
Defaults to \link[mlr3:mlr_resamplings_cv]{ResamplingCV} with 3 folds.}

\item{param_values}{(\code{list()}) \cr
Parameter values which are pass on to the learner.}

\item{measure}{(\link[mlr3:Measure]{Measure}) \cr
Contains the performance measure, for which we optimize during training. \cr
Defaults to \link[mlr3measures:acc]{Accuracy} for classification and \link[mlr3measures:rmse]{RMSE} for regression.}

\item{tuning_method}{(\code{character(1)}) \cr
Tuning method. Possible choices are \code{"mbo"}, \code{"hyperband"} or \code{"smashy"}¸ Default is \code{"smashy"}.}

\item{tuning_time}{(\code{integer(1)}) \cr
Termination criterium. Number of seconds for which to run the optimization. Does \emph{not} include training time of the final model. \cr
Default is set to \code{3600}, i.e. one hour. Tuning is terminated depending on the first termination criteria fulfilled.}

\item{tuning_iters}{(\code{integer(1)}) \cr
Termination criterium. Number of MBO iterations for which to run the optimization. \cr
Default is set to \code{150} iterations. Tuning is terminated depending on the first termination criteria fulfilled.}

\item{tuning_generations}{(\code{integer(1)}) \cr
Termination criterium for tuning method \code{smashy}. Number of generations for which to run the optimization. \cr
Default is set to \code{3} generations. Tuning is terminated depending on the first termination criteria fulfilled.}

\item{enable_tuning}{(\code{logical(1)}) \cr
Whether or not to perform hyperparameter optimization. Default is \code{TRUE}.}

\item{final_model}{(\code{logical(1)}) \cr
Whether or not to return the final model trained on the whole dataset at the end.}
}
\value{
\link[=AutoCompBoostRegr]{AutoCompBoostRegr}
}
\description{
Class for Automated Regression in autocompboost. Subclass of \link[=AutoCompBoostBase]{AutoCompBoostBase}

Creates a new instance of this \link[R6:R6Class]{R6} class.
}
\section{Construction}{

Objects should be created using the \link[=AutoCompBoost]{AutoCompBoost} interface function.\preformatted{regression_model = AutoCompBoost(regression_task, resampling, measure,
tuning_time, tuning_iters, final_model)
}
}

\examples{
\dontrun{
library(mlr3)
library(autocompboost)

regression_model = AutoCompBoost(tsk("boston_housing"))
regression_model$train()
}
}
