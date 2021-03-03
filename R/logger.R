### #
### #
### #
### #   Purpose:   Functions Related to Logging
### #   started:   2019-10-03 (pvr)
### #
### # ############################################## ###

#' @title Create log4r Logger for package
#'
#' @param ps_logfile name of the logfile
#' @param ps_level logger level
#'
#' @return qpdt_logger
#' @export get_qpdt_logger
#'
#' @examples
#' \dontrun{
#' qpdt_logger <- get_qpdt_logger()
#' }
get_qpdt_logger <- function(ps_logfile = 'qpdt.log', ps_level = 'FATAL'){
  qpdt_logger <- log4r::create.logger(logfile = ps_logfile, level = ps_level)
  return(qpdt_logger)
}


#' @title Wrapper for log4r info
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qpdt_log_info
#'
#' @examples
#' \dontrun{
#' qpdt_logger <- get_qpdt_logger()
#' qpdt_log_level(qpdt_logger, 'INFO')
#' qpdt_log_info(qpdt_logger, 'Examples', 'test message')
#' }
qpdt_log_info <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::info(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r debug
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qpdt_log_debug
#'
#' @examples
#' \dontrun{
#' qpdt_logger <- get_qpdt_logger()
#' qpdt_log_level(qpdt_logger, 'DEBUG')
#' qpdt_log_debug(qpdt_logger, 'Examples', 'test message')
#' }
qpdt_log_debug <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::debug(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r warn
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qpdt_log_warn
#'
#' @examples
#' \dontrun{
#' qpdt_logger <- get_qpdt_logger()
#' qpdt_log_level(qpdt_logger, 'WARN')
#' qpdt_log_warn(qpdt_logger, 'Examples', 'test message')
#' }
qpdt_log_warn <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::warn(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r error
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qpdt_log_error
#'
#' @examples
#' \dontrun{
#' qpdt_logger <- get_qpdt_logger()
#' qpdt_log_level(qpdt_logger, 'ERROR')
#' qpdt_log_error(qpdt_logger, 'Examples', 'test message')
#' }
qpdt_log_error <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::error(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r fatal
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qpdt_log_fatal
#'
#' @examples
#' \dontrun{
#' qpdt_logger <- get_qpdt_logger()
#' qpdt_log_level(qpdt_logger, 'FATAL')
#' qpdt_log_fatal(qpdt_logger, 'Examples', 'test message')
#' }
qpdt_log_fatal <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::fatal(logger = plogger, message = s_msg)
}


#' @title Wrapper to set the level of a logger
#'
#' @param plogger log4r logger object
#' @param ps_level new level of plogger
#'
#' @export qpdt_log_level
#'
#' @examples
#' \dontrun{
#' qpdt_logger <- get_qpdt_logger()
#' qpdt_log_level(qpdt_logger, 'INFO')
#' }
qpdt_log_level <- function(plogger, ps_level = c('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL')){
  if (!missing(ps_level) & length(ps_level) > 1) stop(" *** ERROR in level(): only one 'level' allowed.")
  ps_level <- match.arg(ps_level)
  log4r::level(plogger) <- ps_level
}
