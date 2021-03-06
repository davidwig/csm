% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/best_model.R
\name{best_model}
\alias{best_model}
\title{"Best Model" Procedure, OLS Subset Selection}
\usage{
best_model(y, x, crit = "BIC", quick = FALSE)
}
\arguments{
\item{y}{A one-dimensional dataframe with the desired dependent variable}

\item{x}{A multi-dimensional dataframe with desired indepdent variables to be considered}

\item{crit}{Which criterion should the model optimize?}

\item{quick}{If TRUE, uses exhaustive search. If FALSE, uses stepwise search.}
}
\value{
An lm object of the "best model"
}
\description{
This function fits an OLS model that optimizes BIC or AIC
based on provided data.
}
