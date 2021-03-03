## ---- Helper Functions --------------------------------------------------

#' @title Read Pedigree from csv-file
#'
#' @description
#' The pedigree is read from a file with a tabular structure. The columns are
#' separated by a character given by ps_delim.
#'
#' @param ps_pedig_path path to the pedigree input file
#' @param ps_delim column delimiting character
#'
#' @return tbl_pedigree tibble containing pedigree
#'
#' @examples
#' \dontrun{
#' p <- read_prp_pedigree(ps_pedig_path = system.file('extdata',
#' 'PopReport_SN_ohne_20210115.csv_adaptfin2.csv', package = 'qprppedigree'))
#' }
#' @export read_prp_pedigree
read_prp_pedigree <- function(ps_pedig_path, ps_delim = '|', pcol_types = NULL) {
  # check whether file in ps_pedig_path exists
  if (! file.exists(ps_pedig_path))
    stop(" *** ERROR: Cannot find pedigree in ", ps_pedig_path)

  # read pedigree
  tbl_pedigree <- readr::read_delim(file      = ps_pedig_path,
                                    delim     = ps_delim,
                                    col_types = pcol_types,
                                    guess_max = .Machine$integer.max-1)
  return(tbl_pedigree)
}


