## ---- Check Data Types ------------------------------------------------------
#'
#' @title Checking Datatypes of Columns of a Pedigree
#'
#' @description
#' The datatypes given in pl_dtype$dtp are compared to the actual datatypes
#' in the pedigree for the columns given in pl_dtype$col. If the datatypes
#' do not match, then the records causing the problems are returned in the
#' result list as a tibble.
#'
#' @details
#' The format of the argument pl_dtype can be determined from the result of
#' the function get_pedigree_datatypes().
#'
#' @param ps_pedig_path path to the pedigree input file
#' @param ps_delim column delimiting character
#' @param ps_id_col column title for animal IDs
#' @param pl_dtype list of column names with required data-types
#' @param pcol_types column types of pedigree in ps_pedig_path used by read_prp_pedigree
#' @param ptbl_pedigree tibble containing pedigree information
#'
#' @return list with datatype problems
#'
#' @examples
#' \dontrun{
#' check_pedigree_datatypes(ps_pedig_path = system.file('extdata',
#'     'PopReport_SN_ohne_20210115.csv_adaptfin2.csv',
#'   package = 'qprppedigree'))
#' }
#'
#'
#' @export check_pedigree_datatypes
check_pedigree_datatypes <- function(ps_pedig_path,
                                     ps_delim         = '|',
                                     ps_id_col        = '#IDTier',
                                     pl_dtype         = NULL,
                                     pcol_types       = NULL,
                                     ptbl_pedigree    = NULL){
  # check whether pedigree must be read
  if (is.null(ptbl_pedigree)){
    tbl_pedigree <- read_prp_pedigree(ps_pedig_path = ps_pedig_path, ps_delim = ps_delim, pcol_types = pcol_types)
  } else {
    tbl_pedigree <- ptbl_pedigree
  }

  # define local wrappers to run do.call() on generated function name
  parse_integer <- function(x, ...) readr::parse_integer(x, ...)
  parse_character <- function(x, ...) readr::parse_character(x, ...)
  parse_date <- function(x, ...) readr::parse_date(x, ...)
  parse_datetime <- function(x, ...) readr::parse_datetime(x, ...)
  parse_double <- function(x, ...) readr::parse_double(x, ...)
  parse_factor <- function(x, ...) readr::parse_factor(x, ...)
  parse_logical <- function(x, ...) readr::parse_logical(x, ...)
  parse_number <- function(x, ...) readr::parse_number(x, ...)
  parse_time <- function(x, ...) readr::parse_time(x, ...)

  # check that all columns specified in pl_dtype$col are valid columnnames of tbl_pedigree
  vec_ped_colnames <- colnames(tbl_pedigree)
  if (! all(is.element(pl_dtype$col, vec_ped_colnames))){
    stop(" *** ERROR in check_pedigree_datatypes invalid columnnames specified: ", pl_dtype$col)
  }
  # initialise tibble that stores problem
  tbl_parse_problem <- NULL
  l_ped_dt_result <- get_pedigree_datatypes(ps_pedig_path = ps_pedig_path,
                                            ps_delim      = ps_delim,
                                            pcol_types    = pcol_types,
                                            ptbl_pedigree = tbl_pedigree)

  for (idx in seq_along(pl_dtype$col)){
    # determine datatype of current column
    s_cur_col_parser <- readr::guess_parser(tbl_pedigree[[pl_dtype$col[idx]]], guess_integer = TRUE)
    # check whether s_cur_col_parser corresponds to the required datatype
    if (s_cur_col_parser != pl_dtype$dtp[idx]){
      # find the record with problems
      fun.name <- paste('parse_', pl_dtype$dtp[idx], sep = '')
      par_result <- do.call(fun.name, args = list(tbl_pedigree[[pl_dtype$col[idx]]]))
      # extract problems
      tbl_cur_problems <- readr::problems(par_result)
      # add pedigree column name
      tbl_cur_problems <- dplyr::bind_cols(tibble::tibble(ped_col = rep(pl_dtype$col[idx], nrow(tbl_cur_problems))), tbl_cur_problems)
      # add record that cause problems to the result
      if (is.null(tbl_parse_problem)){
        tbl_parse_problem <- tbl_cur_problems
      } else {
        tbl_parse_problem <- dplyr::bind_rows(tbl_parse_problem, tbl_cur_problems)
      }
    }
  }
  # return list of results
  return(list(PedFile         = ps_pedig_path,
              ReqDType        = pl_dtype,
              CurDType        = l_ped_dt_result$l_dtype,
              DTypeProblems   = tbl_parse_problem))

}

## ---- Get Data Types --------------------------------------------------------
#'
#' @title Determine Datatypes of Columns in a Pedigree
#'
#' @description
#' The datatypes of the columns are determined using the function readr::guess_parser().
#'
#' @details
#' The result is returned as a list that can be used as input for the function
#' check_pedigree_datatypes
#'
#' @param ps_pedig_path path to pedigree input file
#' @param ps_delim column delimiter in pedigree input file
#' @param pcol_types column types in the format of cols() used by read_prp_pedigree
#' @param ptbl_pedigree a tibble containing a pedigree
#'
#' @return list with datatypes
#'
#' @examples
#' \dontrun{
#' get_pedigree_datatypes(ps_pedig_path = system.file('extdata',
#'     'PopReport_SN_ohne_20210115.csv_adaptfin2.csv',
#'   package = 'qprppedigree'))
#' }
#'
#' @export get_pedigree_datatypes
get_pedigree_datatypes <- function(ps_pedig_path,
                                   ps_delim         = '|',
                                   pcol_types       = NULL,
                                   ptbl_pedigree    = NULL){
  # check whether pedigree must be read
  if (is.null(ptbl_pedigree)){
    tbl_pedigree <- read_prp_pedigree(ps_pedig_path = ps_pedig_path,
                                      ps_delim = ps_delim,
                                      pcol_types = pcol_types)
  } else {
    tbl_pedigree <- ptbl_pedigree
  }
  # obtain the column types in tbl_pedigree
  vec_ped_col <- colnames(tbl_pedigree)
  # obtain datatypes via guess_parser
  vec_ped_dtp <- sapply(vec_ped_col,
                        function(x) readr::guess_parser(tbl_pedigree[[x]],
                                                        guess_integer = TRUE),
                        USE.NAMES = FALSE)
  # return result
  return(list(PedFile         = ps_pedig_path,
              l_dtype         = list(col = vec_ped_col,
                                     dtp = vec_ped_dtp)))

}

