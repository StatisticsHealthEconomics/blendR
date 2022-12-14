% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_extdata.R
\name{transform_extdata}
\alias{transform_extdata}
\title{Transform external data from risk set format}
\usage{
transform_extdata(extdat, tmax = 100, n = 100)
}
\arguments{
\item{extdat}{Long-term data typically from elicitation or
external sources; data frame}

\item{tmax}{Maximum survival time}

\item{n}{Number of samples}
}
\value{
Survival probabilities
}
\description{
Transform external data from risk set format
}
\examples{
extdat <- data.frame(start = c(5, 10, 15, 20),
                     stop =  c(10, 15, 20, 25),
                     n = c(100, 100, 100, 100),
                     r = c(50, 40, 30, 20))
transform_extdata(extdat, tmax = 100)

}
