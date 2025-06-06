% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response_curve.R
\name{response_curve}
\alias{response_curve}
\title{Response Curve Function}
\usage{
response_curve(x, params, type = "gompertz")
}
\arguments{
\item{x}{A numeric vector representing the input values.}

\item{params}{A list containing the parameters for the model. It should include:
\itemize{
 \item b: The slope parameter (numeric).
 \item c: The lower asymptote (numeric).
 \item d: The upper asymptote (numeric).
 \item e: The half-saturation (numeric).
 }}

\item{type}{A character string specifying the model type. It can be either "gompertz" or "richards".}
}
\value{
A numeric vector representing the response values computed by the model.
}
\description{
This function computes the response curve based on the specified model type.
It supports the Gompertz model and the Richards model.
}
