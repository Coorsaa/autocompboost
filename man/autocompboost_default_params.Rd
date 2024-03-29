% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/param_set.R
\name{autocompboost_default_params}
\alias{autocompboost_default_params}
\title{autocompboost_default_params}
\usage{
autocompboost_default_params(task_type, tuning_method)
}
\arguments{
\item{task_type}{(\code{character(1L)}) \cr
Task type. \code{"classif"} for classification or \code{"regr"} for regression.}

\item{tuning_method}{(\code{character(1L)}) \cr
Tuning method, choice of \code{"mbo"} \code{"smashy"} and \code{"hyperband"}. This is necessary since \code{"hyperband"} and \code{"smashy"} need a budget parameter.}
}
\value{
\code{\link[paradox:ParamSet]{ParamSet}}
}
\description{
Default tuning search space for autocompboost.
}
