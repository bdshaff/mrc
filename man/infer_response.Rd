% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infer_response.R
\name{infer_response}
\alias{infer_response}
\title{Infer response from a fitted model}
\usage{
infer_response(rc_fit, xrange = NULL)
}
\arguments{
\item{rc_fit}{A fitted model object.}

\item{xrange}{A numeric vector of length 2 specifying the range of x values for prediction. Default is NULL, which uses the range of x in the data.}
}
\value{
A data frame containing the predicted response values and the model response.
}
\description{
This function infers the response from a fitted model object.
}
\details{
The function infers the response from the fitted model object and returns a data frame with the predicted response values and the model response.
}
