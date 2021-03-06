## --- Pedigree Report Creation ------------------------------------------------
#'
#' @title Create Pedigree Report
#'
#' @description
#' The checks in this package are done for a given pedigree given by ps_pedigree_path.
#' The results of the checks are summarised in a report which is generated by this
#' function.
#'
#' @param ps_pedigree_path path to the pedigree file
#' @param ps_report_outdir report output directory
#' @param ps_report_rmd name of the pedigree report file
#' @param pl_params list of parameters to be included in the report
#'
#' @examples
#' \dontrun{
#' s_pedigree_path <- file.path(here::here(), 'inst', 'extdata', 'PopReport_OST_mit_20210318.csv_adaptfin5.csv')
#' create_pedigree_report(ps_pedigree_path = system.file('extdata',
#'     'PopReport_SN_ohne_20210115.csv_adaptfin2.csv',package = 'qpdt'),
#'   ps_report_outdir = file.path(here::here(), 'scratch', 'report_out'))
#' }
#'
#' @export create_pedigree_report
create_pedigree_report <- function(ps_pedigree_path,
                                   ps_report_outdir = '.',
                                   ps_report_rmd    = paste0(format(Sys.time(), '%Y%m%d%H%M%S'),
                                                             '_',
                                                             fs::path_ext_remove(basename(ps_pedigree_path)),
                                                            '_qpdt_pedigree_report.Rmd', collapse = ''),
                                   pl_params        = NULL){
  # argument checks
  if (!file.exists(ps_pedigree_path))
    stop(" *** ERROR: [qpdt::create_pedigree_report] CANNOT FIND: ", ps_pedigree_path)
  if (!dir.exists(ps_report_outdir))
    dir.create(ps_report_outdir, recursive = TRUE)
  # create report path for rmarkdown::draft
  s_draft_report_path <- file.path(ps_report_outdir, ps_report_rmd)
  # use the template to get started with the report
  s_render_report_path <- rmarkdown::draft(file = s_draft_report_path,
                                           template = 'qpdt_report',
                                           package = 'qpdt',
                                           create_dir = TRUE,
                                           edit = FALSE)
  # render the reprort with parameters to generate the output of the report

  rmarkdown::render(input = s_render_report_path,
                    output_format = "html_document",
                    params = list(pedigreePath = ps_pedigree_path,
                                  pedigreeName = pl_params$pedigreeName,
                                  pedigreeType = pl_params$pedigreeType,
                                  reportAuthor = pl_params$reportAuthor,
                                  checkLoop    = pl_params$checkLoop))


  return(invisible(TRUE))
}

