% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/getJob.R
\name{getJob}
\alias{getJob}
\title{Get job from registry by id.}
\usage{
getJob(reg, id, check.id = TRUE)
}
\arguments{
\item{reg}{[\code{\link{Registry}}]\cr
Registry.}

\item{id}{[\code{integer(1)}]\cr
Id of job.}

\item{check.id}{[\code{logical(1)}]\cr
Check the job id?
Default is \code{TRUE}.}
}
\value{
[\code{\link{Job}}].
}
\description{
Get job from registry by id.
}

