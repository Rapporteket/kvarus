% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataOps.R
\name{getFirstRegistrations}
\alias{getFirstRegistrations}
\title{Filter out the first measurement point per patient}
\usage{
getFirstRegistrations(timepointData)
}
\value{
timepointData data frame
}
\description{
This function sorts the rows for each patient according to the registration time
and keeps the first registration
}
