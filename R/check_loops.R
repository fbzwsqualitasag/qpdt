## ---- PedigreeCycleCheck R6 Class Definition ---------------------------------
#'
#' @title R6 Class To Find Loops in Pedigrees
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @description
#' The check for loops runs on repeated depth-first-traversals of all vertices
#' in the directed graph that represents a pedigree.
#'
#' @export PedigreeCycleCheck
#' @examples
#' \dontrun{
#' pcc <- PedigreeCycleCheck$new()
#' pcc$read_pedigree(ps_pedigree_path = system.file('extdata','data_sample2.csv', package = 'qprppedigree'))
#' pcc$has_cycle()
#' }
#'
#' @section Private Fields:
#' \describe{
#'   \item{\code{tbl_pedigree}}{node-list representation of pedigree as tibble}
#'   \item{\code{n_ani_col}}{column index for animal ids}
#'   \item{\code{n_sire_col}}{column index for sire ids}
#'   \item{\code{n_dam_col}}{column index for dam ids}
#'   \item{\code{l_set_wgb}}{list of white, grey and black set}
#'   \item{\code{b_report_cycle}}{Flag indicating whether vertices in a cycle must be reported}
#'   \item{\code{tbl_cycle}}{Tibble storing pairs of parent and offspring in the cycle}
#'   \item{\code{b_debug}}{Flag for debugging mode}
#'   \item{\code{qprp_logger}}{Logger object from log4r}
#' }
#'
#' @section Private Methods:
#' \describe{
#'   \item{\code{dfs(p_current_vertex)}}{Depth-first-search starting at vertex p_current_vertex}
#'   \item{\code{move_grey_black(p_current_vertex)}}{Move vertex p_current_vertex from grey set to black set}
#'   \item{\code{move_white_grey(p_current_vertex)}}{Move vertex p_current_vertex from white set to grey set}
#'   \item{\code{get_neighbors(p_current_vertex)}}{Find vector of neighbor vertices of current vertex p_current_vertex}
#' }
PedigreeCycleCheck <- R6::R6Class(
  classname = 'PedigreeCycleCheck',
  public = list(
    #' @description
    #' Initialise fields in object at creation.
    initialize = function(){
      private$tbl_pedigree <- NULL
      private$n_ani_col <- NULL
      private$n_sire_col <- NULL
      private$n_dam_col <- NULL
      private$l_set_wgb <- list()
      private$b_report_cycle <- FALSE
      private$tbl_cycle <- NULL
      private$b_debug <- FALSE
      private$qprp_logger <- NULL

    },

    #' @description
    #' read pedigree from file given by ps_pedigree_path
    #' @param ps_pedigree_path path to pedigree input file
    #' @param ps_delim delimiting character between columns
    #' @param pn_ani_col animal id column
    #' @param pn_sire_col sire id column
    #' @param pn_dam_col dam id column
    #' @param ... additional arguments passed to readr::read_delim
    #' @return tbl_pedigree pedigree as tibble
    read_pedigree = function(ps_pedigree_path,
                             ps_delim    = '|',
                             pn_ani_col  = 1,
                             pn_sire_col = 2,
                             pn_dam_col  = 3,
                             ...){
      # log message at the beginning
      if (private$b_debug)
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'read_pedigree',
                      ps_msg = ' * Start of method ...')
      # check wether ps_pedigree_path exists
      if (!file.exists(ps_pedigree_path))
        stop(" *** [read_pedigree] ERROR: CANNOT FIND pedigree input: ", ps_pedigree_path)
      # read pedigree and assign tbl
      private$tbl_pedigree <- readr::read_delim(file      = ps_pedigree_path,
                                                delim     = ps_delim,
                                                ...)
      # log before writing
      if (private$b_debug){
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'read_pedigree',
                      ps_msg = paste(' * Read pedigree from file: ', ps_pedigree_path, sep = ''))

        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'read_pedigree',
                      ps_msg = paste0(' * Number of rows|columns: ', nrow(private$tbl_pedigree),
                                      ' | ', ncol(private$tbl_pedigree) , collapse = ''))

      }
      # set animal, sire and dam id columns
      private$n_ani_col <- pn_ani_col
      private$n_sire_col <- pn_sire_col
      private$n_dam_col <- pn_dam_col

    },

    #' @description
    #' Run a depth-first traversal with all vertices (animals in the pedigree)
    #' until a cycle has been found.
    has_cycle = function(){
      # log start message
      if (private$b_debug)
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'has_cycle',
                      ps_msg = ' * Start of method ...')
      # check that tbl_pedigree is not null
      if (is.null(private$tbl_pedigree))
        stop(" *** [has_cycle] ERROR: Pedigree is not assigned - STOP")
      n_nr_vertex <- nrow(private$tbl_pedigree)
      # define the white set, the grey set and the black set
      private$l_set_wgb <- list(white = private$tbl_pedigree[[private$n_ani_col]],
                                grey  = NULL,
                                black = NULL)
      if (private$b_debug){
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'has_cycle',
                      ps_msg = paste(' * White set: ', paste0(private$l_set_wgb$white, collapse = ' '), sep = ''))
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'has_cycle',
                      ps_msg = paste(' * Grey set: ', paste0(private$l_set_wgb$grey, collapse = ' '), sep = ''))
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'has_cycle',
                      ps_msg = paste(' * Black set: ', paste0(private$l_set_wgb$black, collapse = ' '), sep = ''))

      }
      # save the non-visited vertices
      vec_non_visited <- private$l_set_wgb$white
      if (private$b_debug)
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'has_cycle',
                      ps_msg = paste(' * Not visited vertices: ', paste0(vec_non_visited, collapse = ' '), sep = ''))
      # loop over non-visited vertices
      while(length(private$l_set_wgb$white) > 0){
        # init tbl_cycle
        private$tbl_cycle <- NULL
        # get current vertex from white set to start new dfs
        current_vertex <- private$l_set_wgb$white[[1]]
        if (private$b_debug)
          qprp_log_info(plogger = private$qprp_logger, ps_caller = 'has_cycle',
                        ps_msg = paste0(' * Current vertex: ', current_vertex, collapse = ' '))
        if (private$dfs(p_current_vertex = current_vertex))
          return(TRUE)
      }
      return(FALSE)
    },

    #' @description
    #' Setter method for the field tbl_pedigree
    #' @param ptbl_pedigree current tibble with pedigree
    set_tbl_pedigree = function(ptbl_pedigree){
      private$tbl_pedigree <- ptbl_pedigree
    },

    #' @description
    #' Setter method for n_ani_col
    #' @param pn_ani_col current value for index of animal column
    set_n_ani_col = function(pn_ani_col){
      private$n_ani_col <- pn_ani_col
    },

    #' @description
    #' Setter method for n_sire_col
    #' @param pn_sire_col current value for index of sire column
    set_n_sire_col = function(pn_sire_col){
      private$n_sire_col <- pn_sire_col
    },

    #' @description
    #' Setter method for n_dam_col
    #' @param pn_dam_col current value for index of dam column
    set_n_dam_col = function(pn_dam_col){
      private$n_dam_col <- pn_dam_col
    },

    #' @description
    #' Setter method for b_report_cycle
    #' @param pb_report_cycle current value for b_report_cycle
    set_b_report_cycle = function(pb_report_cycle){
      private$b_report_cycle = pb_report_cycle
    },

    #' @description
    #' Setter method for b_debug
    #' @param pb_debug current value of b_debug to be set
    set_b_debug = function(pb_debug){
      private$b_debug <- pb_debug
      if (is.null(private$qprp_logger)) {
        private$qprp_logger <- get_qprp_logger(ps_logfile = paste(format(Sys.time(), "%Y%m%d%H%M%S"), '_pedigree_cycle_loop.log', sep = ''),
                                               ps_level = 'INFO')
      }
    },

    #' @description
    #' Getter method for tbl_cycle
    #' @return tbl_cycle tibble with parent offspring pairs in cycle
    get_tbl_cycle = function(){
      return(private$tbl_cycle)
    }
  ),
  private = list(
    # node-list representation of pedigree as tibble
    tbl_pedigree = NULL,
    # column index for animal ids
    n_ani_col = NULL,
    # column index for sire ids
    n_sire_col = NULL,
    # column index for dam ids
    n_dam_col = NULL,
    # list of white, grey and black set
    l_set_wgb = list(),
    # should cycles be reported
    b_report_cycle = FALSE,
    # tbl for parent-offspring pairs storing the cycle
    tbl_cycle = NULL,
    # debug status
    b_debug = FALSE,
    # log4r logger object
    qprp_logger = NULL,

    # Run depth first traversal starting at vertex p_current_vertex
    dfs = function(p_current_vertex){
      # move current vertex from white set to grey set
      private$move_white_grey(p_current_vertex = p_current_vertex)
      # obtain neighbors corresponding to offspring of current vertex
      vec_neighbors <- private$get_neighbors(p_current_vertex = p_current_vertex)
      # log message
      if (private$b_debug){
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'dfs',
                      ps_msg  = paste(' * Start dfs with current vertex', p_current_vertex, sep = ''))
        qprp_log_info(plogger = private$qprp_logger, ps_caller = 'dfs',
                      ps_msg  = paste(' * Neighbors: ', paste0(vec_neighbors, collapse = ' '), sep = ''))
      }
      # loop over neighbors
      for (nidx in seq_along(vec_neighbors)){
        current_neighbor <- vec_neighbors[nidx]
        # if current_neighbor is already in black set, continue
        if (is.element(current_neighbor, private$l_set_wgb$black))
          next
        # add the current pair of vertex and neighbor to the parent list
        if (private$b_report_cycle)
          private$add_parent_offspring(p_parent_vertex = p_current_vertex, p_offspring_vertex = current_neighbor)
        # if current neighbor is already in grey set, cycle is found
        if (is.element(current_neighbor, private$l_set_wgb$grey))
          return(TRUE)
        # continue DFT with current_neighbor as current vertex
        if (private$dfs(p_current_vertex = current_neighbor))
          return(TRUE)
      }
      # move current vertex from grey to the black set
      private$move_grey_black(p_current_vertex = p_current_vertex)
      # clean up any trailing parent list
      private$tbl_cycle <- NULL
      return(FALSE)
    },

    # Move current vertex p_current_vertex from grey set to black set
    move_grey_black = function(p_current_vertex){
      private$l_set_wgb$grey <- setdiff(private$l_set_wgb$grey, p_current_vertex)
      private$l_set_wgb$black <- union(private$l_set_wgb$black, p_current_vertex)
      return(invisible(TRUE))
    },

    # Move the current vertex p_current_vertex from white set to grey set
    move_white_grey = function(p_current_vertex){
      # check whether the current vertex is already in the grey set
      if (is.element(p_current_vertex, private$l_set_wgb$grey))
        stop(" *** [move_white_grey] - ERROR: FOUND vertex: ", p_current_vertex, " in grey set")
      # check whether current vertex can be found in the white set
      if (!is.element(p_current_vertex, private$l_set_wgb$white))
        stop(" *** [move_white_grey] - ERROR: CANNOT FIND vertex: ", p_current_vertex, " in white set")
      # remove current vertex from white set
      private$l_set_wgb$white <- setdiff(private$l_set_wgb$white, p_current_vertex)
      # add current vertex to grey set
      private$l_set_wgb$grey <- union(private$l_set_wgb$grey, p_current_vertex)
      # return
      return(invisible(TRUE))
    },

    # Obtain vector of neighbors for current vertex p_current_vertex
    get_neighbors = function(p_current_vertex){
      # get animal IDs for which p_current_vertex is either the father or the mother
      if (is.element(p_current_vertex, private$tbl_pedigree[[private$n_sire_col]])){
        vec_result_neighbor <- private$tbl_pedigree[[private$n_ani_col]][which(private$tbl_pedigree[[private$n_sire_col]] == p_current_vertex)]
      } else if (is.element(p_current_vertex, private$tbl_pedigree[[private$n_dam_col]])){
        vec_result_neighbor <- private$tbl_pedigree[[private$n_ani_col]][which(private$tbl_pedigree[[private$n_dam_col]] == p_current_vertex)]
      } else {
        vec_result_neighbor <- NULL
      }
      return(vec_result_neighbor)
    },

    # Add parent offspring pair defining the cycle
    add_parent_offspring = function(p_parent_vertex, p_offspring_vertex){
      # in case tbl_cylce is empty, start it here
      if (is.null(private$tbl_cycle)){
        private$tbl_cycle <- tibble::tibble(parent = p_parent_vertex, offspring = p_offspring_vertex)
      } else {
        private$tbl_cycle <- dplyr::bind_rows(private$tbl_cycle, tibble::tibble(parent = p_parent_vertex, offspring = p_offspring_vertex))
      }
      return(invisible(TRUE))
    }
  )
)

## ---- Wrapper For Cycle Check -----------------------------------------------
#'
#' @title Wrapper Function To Check Pedigrees For Cycles
#'
#' @description
#' The PedigreeCycleCheck R6 class is used to find cycles
#' in a pedigree. The pedigree can be specified via input file
#' or via a tibble. The result consists either of TRUE or
#' FALSE which tells whether a pedigree contains a loop or not.
#' In addition the animals that are part of the loop can be
#' listed.
#'
#' @details
#' Cycles are searched using depth-first-traversals of the pedigree.
#'
#' @param ps_pedig_path path to the pedigree input file
#' @param pb_report_cycle flag, whether animals in cycle are reported
#' @param ps_delim delimiting character of columns in input file
#' @param pn_id_col column index of animals in input file
#' @param pn_sire_col column index of sires in input file
#' @param pn_dam_col column indes of dams in input file
#' @param pcol_types data types of columns in input file, passed to readr
#' @param ptbl_pedigree pedigree given as tbl
#'
#' @examples
#' \dontrun{
#' check_cycle_pedigree(ps_pedig_path = system.file('extdata','data_sample2.csv', package = 'qprppedigree'))
#' }
#'
#' @export check_cycle_pedigree
check_cycle_pedigree <- function(ps_pedig_path    = NULL,
                                 pb_report_cycle  = FALSE,
                                 ps_delim         =  '|',
                                 pn_id_col        =    1,
                                 pn_sire_col      =    2,
                                 pn_dam_col       =    3,
                                 pcol_types       = NULL,
                                 ptbl_pedigree    = NULL){
  # define PedigreeCheck object
  pcc <- PedigreeCycleCheck$new()
  if (is.null(ptbl_pedigree)){
    if (is.null(ps_pedig_path))
      stop(" *** ERROR [check_cycle_pedigree] - Path to pedigree input file must be specified")
    pcc$read_pedigree(ps_pedigree_path = ps_pedig_path,
                      ps_delim         = ps_delim,
                      pn_ani_col       = pn_id_col,
                      pn_sire_col      = pn_sire_col,
                      pn_dam_col       = pn_dam_col,
                      col_types        = pcol_types)
  } else {
    pcc$set_tbl_pedigree(ptbl_pedigree = ptbl_pedigree)
    pcc$set_n_ani_col(pn_ani_col = pn_id_col)
    pcc$set_n_sire_col(pn_sire_col = pn_sire_col)
    pcc$set_n_dam_col(pn_dam_col = pn_dam_col)
  }
  # set whether cycles are reported
  pcc$set_b_report_cycle(pb_report_cycle = pb_report_cycle)
  # get results
  if (pb_report_cycle){
    return(list(PedFile = ps_pedig_path,
                HasCycle = pcc$has_cycle(),
                TblCycle = pcc$get_tbl_cycle()))
  } else {
    return(list(PedFile = ps_pedig_path,
                HasCycle = pcc$has_cycle()))
  }

}

