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



#' Assign user-defined metadata to imported DLS or Tm/Tagg SLS summary dataframe
#'
#' \code{assign_metadata}
#'
#' @param metaCSV_path a path (relative to current working directory) to a .csv file containing metadata variables corresponding to a well (e.g. A1, E4..)
#' @param .by a character vector of variable name(s) by which to join metadata;
#' defaults to c("well")
#' @return a dataframe with metadata added as new variables, matched according to well
#' @export
assign_metadata <- function(data, metaCSV_path, .by = c("well")) {
  metadata <- readr::read_csv(metaCSV_path)
  right_join(
    x = metadata,
    y = data,
    by = c("well")
  )
}
