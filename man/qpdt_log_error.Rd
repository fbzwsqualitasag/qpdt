% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logger.R
\name{qpdt_log_error}
\alias{qpdt_log_error}
\title{Wrapper for log4r error}
\usage{
qpdt_log_error(plogger, ps_caller, ps_msg)
}
\arguments{
\item{plogger}{log4r logger object}

\item{ps_caller}{function from which we are called}

\item{ps_msg}{logging message}
}
\description{
Wrapper for log4r error
}
\examples{
\dontrun{
qpdt_logger <- get_qpdt_logger()
qpdt_log_level(qpdt_logger, 'ERROR')
qpdt_log_error(qpdt_logger, 'Examples', 'test message')
}
}
