% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify_model.R
\name{specify_model}
\alias{specify_model}
\title{Specifies the model and sets the variables
Specify the type of analysis, the predictor and response variables, variance
structure, and whether to test a null hypothesis or generate a confidence
interval. Currently, only two predictor variables are allowed in the case of
a regression model and maximum one of the predictor variables can be a
categorical variable. The categorical variable can only have two levels.}
\usage{
specify_model(
  df,
  test,
  procedure,
  cont1 = NULL,
  cont2 = NULL,
  cat1 = NULL,
  cat2 = NULL,
  resp = NULL,
  error_cat = NULL,
  error_cont1 = 1,
  het_cont1 = 1,
  error_cont2 = 1,
  het_cont2 = 1,
  success
)
}
\description{
Specifies the model and sets the variables
Specify the type of analysis, the predictor and response variables, variance
structure, and whether to test a null hypothesis or generate a confidence
interval. Currently, only two predictor variables are allowed in the case of
a regression model and maximum one of the predictor variables can be a
categorical variable. The categorical variable can only have two levels.
}
