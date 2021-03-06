% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion.R
\name{pr_conversion}
\alias{pr_conversion}
\alias{pr_conversion_simple}
\title{Convert monthly downscaled preciptation from kg/m2/s to mm/month}
\usage{
pr_conversion(input)

pr_conversion_simple(input)
}
\arguments{
\item{input}{a 2d array of monhtly downscaled preciptation data in kg/m2/s to convert to mm/month, row.names of input must correspond to a time formatted as YYYYMM}
}
\value{
a matrix of monthly precipitation data in mm/month
}
\description{
A kg/m2 of water is approximately one millimeter of depth (because at standard temperature
1 cc of water masses 1 gram).  So, to convert to depth per unit time, you just multiply
by the number of seconds in the time unit you want.
}
\details{
The difference between the regular and simplified version is that the simplified version
doesn't attempt to correct for the fact that months are not all the same length; it just
treats every month as 1/12 of a tropical year.
}
\section{Functions}{
\itemize{
\item \code{pr_conversion_simple}: Simplified conversion of monthly downscaled preciptation from kg/m2/s to mm/month
}}

