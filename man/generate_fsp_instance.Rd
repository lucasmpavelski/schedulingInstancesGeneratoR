% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_fsp_instance.R
\name{generate_fsp_instance}
\alias{generate_fsp_instance}
\title{Generate flowshop instances with given attributes}
\usage{
generate_fsp_instance(
  no_jobs,
  no_machines,
  distribution_type = DISTRIBUTION_TYPES$UNIFORM,
  correlation_type = CORRELATION_TYPES$JOB_CORRELATED,
  correlation = 0,
  ...,
  seed = NA_integer_
)
}
\arguments{
\item{no_jobs}{number of jobs}

\item{no_machines}{number of machines}

\item{distribution_type}{processing times distribution}

\item{correlation_type}{processing times correlation type}

\item{correlation}{correlation value}

\item{...}{additional parameters (not used)}

\item{seed}{optional random number generator seed}
}
\value{
Instance objects with generated processing times
}
\description{
Generate flowshop instances with given attributes
}
\examples{
generate_fsp_instance(20, 5, "erlang", "machine-correlated", 0.95)
}
