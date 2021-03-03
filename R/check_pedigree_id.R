## ---- Animal IDs -----------------------------------------------------------
#'
#' @title Check Animal IDs in Pedigrees
#'
#' @description
#' The software system PopReport analyses pedigrees. The pedigrees used as input
#' must satisfy certain criteria. This function checks the required properties
#' of animal IDs in a pedigree. Animal IDs in a pedigree are the primary keys
#' that identify the individuals.
#'
#' The main property of the animal IDs consists of their uniqueness. The output
#' of this function shows potential duplicate IDs.
#'
#' @param ps_pedig_path path to the pedigree input file
#' @param ps_delim column delimiting character
#' @param ps_id_col column title where animal ID is contained
#' @param pcol_types column types of pedigree in ps_pedig_path
#' @param ptbl_pedigree tibble containing the pedigree, obtained by read_prp_pedigree()
#' @return list of pedigree characteristics
#'
#' @examples
#' \dontrun{
#' check_pedig_id(ps_pedig_path = system.file('extdata',
#'     'PopReport_SN_ohne_20210115.csv_adaptfin2.csv',
#'   package = 'qprppedigree'))
#' }
#'
#' @export check_pedig_id
check_pedig_id <- function(ps_pedig_path,
                           ps_delim      = '|',
                           ps_id_col     = '#IDTier',
                           pcol_types    = NULL,
                           ptbl_pedigree = NULL){

  # check whether pedigree must be read
  if (is.null(ptbl_pedigree)){
    tbl_pedigree <- read_prp_pedigree(ps_pedig_path = ps_pedig_path, ps_delim = ps_delim, pcol_types = pcol_types)
  }else {
    tbl_pedigree <- ptbl_pedigree
  }
  # check for duplicates
  nrec_pedig <- nrow(tbl_pedigree)
  nani_pedig <- length(unique(tbl_pedigree[[ps_id_col]]))
  # if there are duplicates, get the records
  tbl_dupl_id <- NULL
  if (nrec_pedig != nani_pedig){
    vec_dupl <- tbl_pedigree[[ps_id_col]][which(duplicated(tbl_pedigree[[ps_id_col]]))]
    for (i in vec_dupl){
      vec_cur_dupl_index <- which(tbl_pedigree[[ps_id_col]] == i)
      if (is.null(tbl_dupl_id)){
        tbl_dupl_id <- tbl_pedigree[vec_cur_dupl_index,]
      } else {
        tbl_dupl_id <- dplyr::bind_rows(tbl_dupl_id, tbl_pedigree[vec_cur_dupl_index,])
      }

    }
  }
  return(list(PedFile       = ps_pedig_path,
              NrRecord      = nrec_pedig,
              NrAnimals     = nani_pedig,
              TblDuplicates = tbl_dupl_id))

}

