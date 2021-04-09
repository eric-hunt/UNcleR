#' Joins imported and parsed DLS and Tm/Tagg SLS summary dataframes from the same experiment
#'
#' \code{join_SLS_DLS}
#'
#' @param SLS_data a dataframe object containing SLS data
#' @param DLS_data a dataframe object containing DLS data
#' @param .by a character vector of variable names by which to join tables;
#' defaults to c("well", "sample_num", "prot_conc")
#' @return a dataframe
#' @export
join_SLS_DLS <- function(SLS_data, DLS_data, .by = c("well", "sample_num", "prot_conc")) {
  dplyr::full_join(
    SLS_data,
    DLS_data,
    by = .by
  )
}



#' Get metadata from unified Excel document
#'
#' \code{get_meta}
#'
#' @param path a character string path to an Excel document containing metadata for each experiment in separate worksheets
#' defaults to c("well")
#' @return a named list of tibbles containing metadata for experiments
#' @export
get_meta <- function(path) {
  sheets <- readxl::excel_sheets(path) %>% 
    purrr::set_names()
  sheets <- sheets[-grepl("Variables", sheets)]
  
  meta <- purrr::map(sheets, ~ readxl::read_excel(path, sheet = .x))
  
  return(meta)
}



#' Join user-defined metadata from `UNcleR::get_meta` function to imported Uncle data
#'
#' \code{add_meta}
#'
#' @param data a dataframe to assign metadata to
#' @param meta a named list of tibbles containing metadata for experiments imported from `UNcleR::get_meta` function
#' defaults to c("well")
#' @return a dataframe with metadata added as new variables, matched according to well
#' @export
add_meta <- function(data, meta) {
  wellOrder <- purrr::map2_chr(rep(c(LETTERS[1:8]), 12), purrr::flatten_chr(purrr::map(c(1:12), rep, 8)), paste0)
  dplyr::left_join(meta[[unique(stringr::str_sub(data$plate, end = -2))]], data, by = c("plate", "uni")) %>% 
    dplyr::arrange(match(well, wellOrder))
}
