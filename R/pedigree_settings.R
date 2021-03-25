## --- Settings For GenMon Popreport Pedigree Types ----------------------------------
#'
#' @title Settings for PopReport Pedigrees From Genmon
#'
#' @description
#' All input parameters that change with the type of pedigree are
#' returned in a list
#'
#' @param ps_id_col column header for identifiers
#' @param ps_sire_col column header for sire
#' @param ps_dam_col column header for dam
#' @param ps_bd_col column header for birthday
#' @param ps_sex_col column header for sex
#' @param ps_plz_col column header for postal code
#' @param ps_introg_col column header for introgression
#' @param ps_inb_col column header for inbreeding
#' @param ps_cryo_col column header for cryo conservation
#' @param ps_col_delim column delimiter
#'
#' @export get_gnm_prp_settings
get_gnm_prp_settings <- function(ps_id_col     = '#animal',
                                 ps_sire_col   = 'sire',
                                 ps_dam_col    = 'dam',
                                 ps_bd_col     = 'birth_date',
                                 ps_sex_col    = 'sex',
                                 ps_plz_col    = 'plz',
                                 ps_introg_col = 'introg',
                                 ps_inb_col    = 'inb_gen',
                                 ps_cryo_col   = 'cryo',
                                 ps_col_delim  = '|'){
  return(list(id_col     = ps_id_col,
              sire_col   = ps_sire_col,
              dam_col    = ps_dam_col,
              bd_col     = ps_bd_col,
              sex_col    = ps_sex_col,
              plz_col    = ps_plz_col,
              introg_col = ps_introg_col,
              inb_col    = ps_inb_col,
              cryo_col   = ps_cryo_col,
              col_delim  = ps_col_delim))
}

## --- Settings for ARGUS Popreport Pedigrees ----------------------------------
#'
#' @title Settings for PopReport Pedigrees Exported From ARGUS
#'
#' @description
#' All input parameters that change with the type of pedigree are
#' returned in a list
#'
#' @param ps_id_col column header for identifiers
#' @param ps_sire_col column header for sire
#' @param ps_dam_col column header for dam
#' @param ps_bd_col column header for birthday
#' @param ps_sex_col column header for sex
#' @param ps_plz_col column header for postal code
#' @param ps_introg_col column header for introgression
#' @param ps_inb_col column header for inbreeding
#' @param ps_cryo_col column header for cryo conservation
#' @param ps_col_delim column delimiter
#'
#' @export get_argus_prp_settings
get_argus_prp_settings <- function(ps_id_col     = '#IDTier',
                                   ps_sire_col   = 'IDVater',
                                   ps_dam_col    = 'IDMutter',
                                   ps_bd_col     = 'Birthdate',
                                   ps_sex_col    = 'Geschlecht',
                                   ps_plz_col    = 'PLZ',
                                   ps_introg_col = 'introg',
                                   ps_inb_col    = 'inb_gen',
                                   ps_cryo_col   = 'cryo',
                                   ps_col_delim  = '|'){
  return(list(id_col     = ps_id_col,
              sire_col   = ps_sire_col,
              dam_col    = ps_dam_col,
              bd_col     = ps_bd_col,
              sex_col    = ps_sex_col,
              plz_col    = ps_plz_col,
              introg_col = ps_introg_col,
              inb_col    = ps_inb_col,
              cryo_col   = ps_cryo_col,
              col_delim  = ps_col_delim))
}

## --- Settings For Generic Pedigree Type ----------------------------------
#'
#' @title Settings for Generic Pedigree
#'
#' @description
#' All input parameters that change with the type of pedigree are
#' returned in a list
#'
#' @param ps_id_col column header for identifiers
#' @param ps_sire_col column header for sire
#' @param ps_dam_col column header for dam
#' @param ps_bd_col column header for birthday
#' @param ps_sex_col column header for sex
#' @param ps_plz_col column header for postal code
#' @param ps_introg_col column header for introgression
#' @param ps_inb_col column header for inbreeding
#' @param ps_cryo_col column header for cryo conservation
#' @param ps_col_delim column delimiter
#'
#' @export get_generic_settings
get_generic_settings <- function(ps_id_col     = 'id',
                                 ps_sire_col   = 'sire',
                                 ps_dam_col    = 'dam',
                                 ps_bd_col     = 'birthdate',
                                 ps_sex_col    = 'sex',
                                 ps_plz_col    = 'plz',
                                 ps_introg_col = 'introg',
                                 ps_inb_col    = 'inb_gen',
                                 ps_cryo_col   = 'cryo',
                                 ps_col_delim  = '|'){
  return(list(id_col     = ps_id_col,
              sire_col   = ps_sire_col,
              dam_col    = ps_dam_col,
              bd_col     = ps_bd_col,
              sex_col    = ps_sex_col,
              plz_col    = ps_plz_col,
              introg_col = ps_introg_col,
              inb_col    = ps_inb_col,
              cryo_col   = ps_cryo_col,
              col_delim  = ps_col_delim))
}


