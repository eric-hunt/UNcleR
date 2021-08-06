#' Consolidates Uncle Experiments
#'
#' This function consolidates experiments for a protein based on directory hierarchy.
#' The hierarchy should be as follows from within the working directory where these
#' functions will be executed:
#' 
#' <wd>/
#'    <prot_dir>/
#'        "Exports"/
#'            "General Screen"
#'            "pH Screen"
#'            ...
#'
#' The \code{prot_dir} argument should match the name of the protein subdirectory within
#' the working directory. This is deliberate as it forces the user to make
#' a coherent choice about which data to process. Exported summary and spectra
#' should be contained within named subdirectories of a directory named "Exports"
#' located within the \code{prot_dir} directory.
#'
#' \code{consolidate_experiments}
#'
#' @param prot_dir a character string identical to subdirectory name of working directory
#' where the experiment "Exports" folder is located for the protein being analyzed
#' @param legacy a boolean value, TRUE if individual SLS/DLS spectra files were exported,
#' FALSE if SLS/DLS "bundle" files were exported; default is FALSE
#' @param SLSheader sets header argument for \code{uncleR::import_SLSsum}; defaults to TRUE
#' @param DLSheader sets header argument for \code{uncleR::import_DLSsum}; defaults to FALSE
#' @return a named list of dataframes containing the consolidated data for each experiment
#' @export
consolidate_experiments <- function(prot_dir, join_vars = NULL, legacy = FALSE, SLSheader = TRUE, DLSheader = FALSE) {
  if (!(dir.exists(prot_dir))) {
    stop("The provided protein directory does not exist.")
  }
  if (!(dir.exists(paste0(prot_dir, "/", "Exports")))) {
    stop("There is no 'Exports' subdirectory. Where are the export files?")
  }

  if (is.null(join_vars)) {
    join_vars <- c("date", "instrument", "protein", "plate", "uni")
  }

  dirList <- list.dirs(paste0(prot_dir, "/Exports/"), full.names = TRUE, recursive = FALSE) |>
  rlang::set_names(nm = list.dirs(paste0(prot_dir, "/Exports/"), full.names = FALSE, recursive = FALSE))

  import_experiment <- function(dir, protein) {
    if (legacy) {
      importList <- list(
        FLUORspec = uncleR::import_FLUORspec(dir),
        SLSsum = uncleR::import_SLSsum(dir, header = SLSheader),
        SLSspec266 = uncleR::import_SLSspec(dir, lambda = 266),
        SLSspec473 = uncleR::import_SLSspec(dir, lambda = 473),
        DLSsum = uncleR::import_DLSsum(dir, header = DLSheader),
        DLSspecC = uncleR::import_DLSspec(dir, pattern = "DLS Spec C", type = "C"),
        DLSspecI = uncleR::import_DLSspec(dir, pattern = "DLS Spec I", type = "I"),
        DLSspecM = uncleR::import_DLSspec(dir, pattern = "DLS Spec M", type = "M")
      )
    } else {
      importList <- list(
        SLSsum = uncleR::import_SLSsum(dir, header = SLSheader),
        DLSsum = uncleR::import_DLSsum(dir, header = DLSheader),
        specStatic = uncleR::import_staticBundle(dir),
        specDynamic = uncleR::import_dynamicBundle(dir)
      )
    }
    purrr::reduce(
      importList,
      dplyr::full_join,
      by = join_vars
    ) |>
    dplyr::select(-tidyselect::contains("sample"), -tidyselect::contains("file")) |> {
      \(df) dplyr::mutate(
        df,
        plate = stringr::str_trim(df$plate, side = "both"),
        protein = protein
      )
    }()
  }

  consolidated <- purrr::map(
    dirList,
    \(dir) import_experiment(dir, prot_dir)
  ) # |> rlang::set_names(nm = names(expList))

  return(consolidated)
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



#' Join user-defined metadata from `uncleR::get_meta` function to imported Uncle data
#'
#' \code{add_meta}
#'
#' @param data a dataframe to assign metadata to
#' @param meta a named list of tibbles containing metadata for experiments imported from `uncleR::get_meta` function
#' defaults to c("well")
#' @return a dataframe with metadata added as new variables, matched according to well
#' @export
add_meta <- function(data, meta) {
  wellOrder <- purrr::map2_chr(rep(c(LETTERS[1:8]), 12), purrr::flatten_chr(purrr::map(c(1:12), rep, 8)), paste0)
  dplyr::left_join(meta[[unique(stringr::str_sub(data$plate, end = -2))]], data, by = c("plate", "uni")) %>%
    dplyr::arrange(match(well, wellOrder))
}



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
