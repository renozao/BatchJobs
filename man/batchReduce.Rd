% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/batchReduce.R
\name{batchReduce}
\alias{batchReduce}
\title{Reduces via a binary function over a list adding jobs to a registry.}
\usage{
batchReduce(reg, fun, xs, init, block.size, more.args = list())
}
\arguments{
\item{reg}{[\code{\link{Registry}}]\cr
Empty Registry.}

\item{fun}{[\code{function(aggr, x, ...)}]\cr
Function to reduce \code{xs} with.}

\item{xs}{[\code{vector}]\cr
Vector to reduce.}

\item{init}{[any]\cr
Initial object for reducing.}

\item{block.size}{[\code{integer(1)}]\cr
Number of elements of \code{xs} reduced in one job.}

\item{more.args}{[\code{list}]\cr
A list of other arguments passed to \code{fun}.
Default is empty list.}
}
\value{
Vector of type \code{integer} with job ids.
}
\description{
Each jobs reduces a certain number of elements on one slave.
You can then submit these jobs to the batch system.
}
\examples{
# define function to reduce on slave, we want to sum a vector
f = function(aggr, x) aggr + x
reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)

# sum 20 numbers on each slave process, i.e. 5 jobs
batchReduce(reg, fun = f, 1:100, init = 0, block.size = 5)
submitJobs(reg)
waitForJobs(reg)

# now reduce one final time on master
reduceResults(reg, fun = function(aggr,job,res) f(aggr, res))
}

