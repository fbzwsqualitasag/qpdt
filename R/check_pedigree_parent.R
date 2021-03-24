## ---- Parent Properties -------------------------------------------------


#' @title Check Properties of Parents in a Pedigree
#'
#' @description
#' Some descriptive statistics about the pedigree are collected. The main
#' check consists of the comparison of the birthdates of animals to the
#' birthdates of their parents. The check of birthdates can be parametrized
#' by a minimal tolerance of the difference using the argument \code{pn_bd_tol}.
#' The last check lists all animals that have the same IDs as one of their
#' parents.
#'
#' @details
#' The comparison of the birthdates is done via a join of the parent birthdates
#' to a tibble that consists of only animals, their birthdates and their parents.
#' The comparison is done for sires and dams in two separate steps.
#'
#' @param ps_pedig_path path to the pedigree input file
#' @param ps_delim column delimiting character
#' @param ps_id_col column title for animal IDs
#' @param ps_sire_col column title for sire IDs
#' @param ps_dam_col column title for dam IDs
#' @param ps_bd_col column title for birthdates
#' @param ps_sex_col column title for sex
#' @param pvec_sire_by by argument for inner_join with sires
#' @param pvec_dam_by by argument for inner_join with dams
#' @param pvec_sire_suffix suffix for birthdates used for sire comparison
#' @param pvec_dam_suffix suffix for birthdates used for dam comparision
#' @param pcol_types column types of pedigree in ps_pedig_path
#' @param ptbl_pedigree tibble containing pedigree information
#' @param pn_bd_tol minimal tolerance for age difference between animal and parents (in days)
#'
#' @examples
#' \dontrun{
#' check_pedig_parent(ps_pedig_path = system.file('extdata',
#'     'PopReport_SN_ohne_20210115.csv_adaptfin2.csv',
#'   package = 'qprppedigree'))
#' }
#'
#' @importFrom dplyr %>%
#' @export check_pedig_parent
check_pedig_parent <- function(ps_pedig_path,
                               ps_delim         = '|',
                               ps_id_col        = '#IDTier',
                               ps_sire_col      = 'IDVater',
                               ps_dam_col       = 'IDMutter',
                               ps_bd_col        = 'Birthdate',
                               ps_sex_col       = 'Geschlecht',
                               pvec_sire_by     = c('IDVater' = '#IDTier'),
                               pvec_dam_by      = c('IDMutter' = '#IDTier'),
                               pvec_sire_suffix = c(".Tier", ".Vater"),
                               pvec_dam_suffix  = c(".Tier", ".Mutter"),
                               pcol_types       = NULL,
                               ptbl_pedigree    = NULL,
                               pn_bd_tol        = 0){
  # check whether pedigree must be read
  if (is.null(ptbl_pedigree)){
    tbl_pedigree <- read_prp_pedigree(ps_pedig_path = ps_pedig_path, ps_delim = ps_delim, pcol_types = pcol_types)
  } else {
    tbl_pedigree <- ptbl_pedigree
  }
  # animals with missing parents
  nr_missing_sire <- sum(is.na(tbl_pedigree[[ps_sire_col]]))
  nr_missing_dam <- sum(is.na(tbl_pedigree[[ps_dam_col]]))
  # number of parents that are not animals
  sire_vec <- unique(tbl_pedigree[[ps_sire_col]])
  sire_vec <- sire_vec[!is.na(sire_vec)]
  nr_sire_not_animals <- sum(!is.element(sire_vec, tbl_pedigree[[ps_id_col]]))
  dam_vec <- unique(tbl_pedigree[[ps_dam_col]])
  dam_vec <- dam_vec[!is.na(dam_vec)]
  nr_dam_not_animals <- sum(!is.element(dam_vec, tbl_pedigree[[ps_id_col]]))

  # checking birthdates of animals and parents
  # define symbols
  sym_animal_id <- dplyr::sym(ps_id_col)
  sym_sire_id <- dplyr::sym(ps_sire_col)
  sym_dam_id <- dplyr::sym(ps_dam_col)
  sym_bd_col <- dplyr::sym(ps_bd_col)
  sym_sex_col <- dplyr::sym(ps_sex_col)
  vec_sire_bd <- sapply(pvec_sire_suffix, function(x) paste(ps_bd_col, x, sep = ''), USE.NAMES = FALSE)
  sym_sire_animal_bd <- dplyr::sym(vec_sire_bd[1])
  sym_sire_parent_bd <- dplyr::sym(vec_sire_bd[2])
  vec_dam_bd <- sapply(pvec_dam_suffix, function(x) paste(ps_bd_col, x, sep = ''), USE.NAMES = FALSE)
  sym_dam_animal_bd <- dplyr::sym(vec_dam_bd[1])
  sym_dam_parent_bd <- dplyr::sym(vec_dam_bd[2])
  # create the tibble with animal, sire and birthdate
  tbl_sire_bd <- tbl_pedigree %>% dplyr::select(!!sym_animal_id, !!sym_sire_id, !!sym_bd_col)
  # join birthdate of sire and filter those with inconsistent birthdate
  tbl_sire_bd_err <- tbl_sire_bd %>%
    dplyr::inner_join(tbl_pedigree, by = pvec_sire_by, suffix = pvec_sire_suffix) %>%
    dplyr::select(!!sym_animal_id, !!sym_sire_id, !!sym_sire_animal_bd, !!sym_sire_parent_bd) %>%
    dplyr::filter(!!sym_sire_animal_bd - !!sym_sire_parent_bd < pn_bd_tol)
  # create the tibble with animal, dam and birthdate
  tbl_dam_bd <- tbl_pedigree %>% dplyr::select(!!sym_animal_id, !!sym_dam_id, !!sym_bd_col)
  # join birthdate of dam and filter those with inconsistent birthdates
  tbl_dam_bd_err <- tbl_dam_bd %>%
    dplyr::inner_join(tbl_pedigree, by = pvec_dam_by, suffix = pvec_dam_suffix) %>%
    dplyr::select(!!sym_animal_id, !!sym_dam_id, !!sym_dam_animal_bd, !!sym_dam_parent_bd) %>%
    dplyr::filter(!!sym_dam_animal_bd - !!sym_dam_parent_bd < pn_bd_tol)
  # use filter to find animals with same IDs as parents
  tbl_sire_equal_id <- tbl_pedigree %>% dplyr::filter(!!sym_animal_id == !!sym_sire_id)
  tbl_dam_equal_id <- tbl_pedigree %>% dplyr::filter(!!sym_animal_id == !!sym_dam_id)
  # check sex of parents
  tbl_sire_sex_err <- get_tbl_parent_sex_err(ptbl_pedigree = tbl_pedigree,
                                             psym_animal_id = sym_animal_id,
                                             psym_parent_id = sym_sire_id,
                                             psym_sex_col   = sym_sex_col,
                                             pvec_parent_by = pvec_sire_by,
                                             ps_wrong_sex   = 'F')

  # return results
  return(list(PedFile         = ps_pedig_path,
              NrMissingSire   = nr_missing_sire,
              NrMissingDam    = nr_missing_dam,
              NrSireNotAnimal = nr_sire_not_animals,
              NrDamNotAnimal  = nr_dam_not_animals,
              TblSireBdate    = tbl_sire_bd_err,
              TblDamBdate     = tbl_dam_bd_err,
              TblSireEqID     = tbl_sire_equal_id,
              TblDamEqID      = tbl_dam_equal_id,
              TblSireWrongSex = tbl_sire_sex_err))

}

## --- Check Sex of Parents ----------------------------------------------------
#'
#' @title Check Sex of a Parent
#'
#' @description
#' Using inner_joins to determine sex of parents. Filter out the parents with
#' the wrong sex. The result contains the ID of the animal, the parent and the
#' wrong sex.
#'
#' @param ptbl_pedigree tibble containing pedigree
#' @param psym_animal_id symbol for column header of animal ID
#' @param psym_parent_id symbol for column header of parent ID
#' @param psym_sex_col symbol for column header of sex
#' @param pvec_parent_by vector used as by argument in join
#' @param ps_wrong_sex value for the wrong sex
#'
#' @importFrom dplyr %>%
get_tbl_parent_sex_err <- function(ptbl_pedigree,
                                   psym_animal_id,
                                   psym_parent_id,
                                   psym_sex_col,
                                   pvec_parent_by,
                                   ps_wrong_sex){
  # tibble containing all parent ids
  tbl_parent_id <- ptbl_pedigree %>% distinct(!!psym_parent_id)
  # join the parent ids back to the pedigree to get the sex of the parent
  tbl_parent_sex <- tbl_parent_id %>%
    inner_join(ptbl_pedigree, by = pvec_parent_by) %>%
    select(!!psym_parent_id, !!psym_sex_col)
  # tibble with parents with the wrong sex
  tbl_parent_wrong_sex <- tbl_parent_sex %>%
    filter(!!psym_sex_col == ps_wrong_sex)
  # join back to get information
  return(tbl_parent_wrong_sex %>%
           inner_join(ptbl_pedigree, by = pvec_parent_by) %>%
           select(!!psym_animal_id, !!psym_sex_col))

}


