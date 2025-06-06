% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dispatch_response_model.R
\name{dispatch_response_model}
\alias{dispatch_response_model}
\title{Dispatch Response Model}
\usage{
dispatch_response_model(type)
}
\arguments{
\item{type}{A string indicating the type of response model to dispatch.}
}
\value{
A function corresponding to the specified response model.
}
\description{
This function dispatches the appropriate response model based on the type provided.
}
\details{
The function takes a string input `type` and returns the corresponding response model function.
The available response models are:
- "logistic"
- "log_logistic"
- "gompertz"
- "reflected_gompertz"
- "weibull"
- "reflected_weibull"
}
