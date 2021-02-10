#' Import UNcle Tm/Tagg DLS summary into R
#'
#' \code{import_DLSsum}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex pattern for further selecing files in the directory,
#' defaults to "DLS Sum"
#' @param sheet character string to specify sheet if multi-sheet workbook is exported
#' @param temp_cutoff numeric value, excluding all DLS data obtained at temperatures above this value, default is 100 (°C)
#' @param header if TRUE skips first 4 rows of .xlsx file to remove UNcle header, default is FALSE
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_DLSsum <- function(directory_path, pattern = "DLS Sum", sheet = NULL, temp_cutoff = 25, header = FALSE, combine = TRUE) {
  if (!(header %in% c(TRUE, FALSE))) {
    stop("argument header must be TRUE or FALSE")
  }
  if (!(combine %in% c(TRUE, FALSE))) {
    stop("argument combine must be TRUE or FALSE")
  }
  skip <- 0
  if (header) {
    skip <- 5
  }

  file_list <- list.files(directory_path, pattern = pattern, full.names = TRUE) %>%
    purrr::set_names()

  df_list <- purrr::map(file_list, readxl::read_excel, sheet = sheet, col_types = "text", skip = skip)

  names_list <- purrr::map(
    df_list,
    function(df) {
      recode_values <- c(
        "color" = grep("color", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "capillary" = grep("well", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "sample" = grep("sample", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "temp_C" = grep("(?=.*T)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Z_D" = grep("(?=.*Z-Ave)(?=.*Dia)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Z_diffcoeff" = grep("(?=.*Z-Ave)(?=.*Diff)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Z_D_SD" = grep("(?=.*SD)(?=.*Dia)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "PdI" = grep("PDI", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "fitVar" = grep("Fit Var", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "mcr_cps" = grep("^(?=intensity)(?=.*cps)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak1_D" = grep("(?=.*Pk 1)(?=.*Dia)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak1_MW" = grep("(?=.*Pk 1)(?=.*(M.W.|MW))", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak1_poly" = grep("(?=.*Pk 1)(?=.*polydispersity)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak1_mass" = grep("(?=.*Pk 1)(?=.*mass)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak1_diffcoeff" = grep("(?=.*Pk 1)(?=.*(coeff|co-eff))", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak2_D" = grep("(?=.*Pk 2)(?=.*Dia)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak2_MW" = grep("(?=.*Pk 2)(?=.*(M.W.|MW))", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak2_poly" = grep("(?=.*Pk 2)(?=.*polydispersity)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak2_mass" = grep("(?=.*Pk 2)(?=.*mass)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak3_D" = grep("(?=.*Pk 3)(?=.*Dia)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak3_MW" = grep("(?=.*Pk 3)(?=.*(M.W.|MW))", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak3_poly" = grep("(?=.*Pk 3)(?=.*polydispersity)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "peak3_mass" = grep("(?=.*Pk 3)(?=.*mass)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "filter" = grep("filter", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "viscosity" = grep("(?=.*viscosity)(?=.*cp)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "RefI" = grep("^RI$", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "dcr_cps" = grep("(?=.*derived)(?=.*intensity)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "min_pk_area" = grep("(?=.*min)(?=.*area)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "min_Rh" = grep("(?=.*min)(?=.*Rh)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE)
      )
      return(recode_values)
    }
  )

  recode_values <- purrr::map(
    names_list,
    function(named_vector) {
      swaped_vector <- names(named_vector)
      names(swaped_vector) <- as_vector(named_vector)
      return(swaped_vector)
    }
  )

  renamed_list <- purrr::map2(
    df_list,
    recode_values,
    function(df, values) {
      names(df) <- names(df) %>% dplyr::recode(!!!values)
      return(df)
    }
  )

  vars_parse <- c(
    "temp_C",
    "Z_D",
    "Z_diffcoeff",
    "Z_D_SD", "PdI",
    "fitVar",
    "mcr_cps",
    "peak1_D",
    "peak1_MW",
    "peak1_poly",
    "peak1_mass",
    "peak1_diffcoeff",
    "peak2_D",
    "peak2_MW",
    "peak2_poly",
    "peak2_mass",
    "peak3_D",
    "peak3_MW",
    "peak3_poly",
    "peak3_mass",
    "viscosity",
    "RefI",
    "dcr_cps",
    "min_pk_area",
    "min_Rh"
  )

  parsed_list <- purrr::map2(
    renamed_list,
    names(df_list),
    function(df, name) {
      df %>%
        dplyr::select(-color) %>%
        purrr::modify_at(.at = vars_parse, readr::parse_number, na = c(">1000", "Out of Range", "-", NA, NaN, "\U221E")) %>%
        purrr::modify_if(is.double, round, digits = 2) %>%
        dplyr::filter(temp_C < temp_cutoff) %>%
        tibble::add_column(mode_Z = purrr::pmap_dbl(dplyr::select(., tidyselect::matches("peak\\d{1}_D$")), function(...) length(c(...)[!is.na(c(...))])), .after = "Z_D") %>%
        tibble::add_column(
          file_name = stringr::str_extract(name, "(?<=//).*(?=\\.xlsx)"),
          .before = "capillary"
        )
    }
  )

  if (combine == TRUE) {
    return(
      dplyr::bind_rows(parsed_list, .id = "origin") %>%
        dplyr::mutate(
          origin = dplyr::if_else(
            stringr::str_detect(.$origin, stringr::regex("\\.uni.*$")),
            stringr::str_extract(.$origin, stringr::regex("(?<=//).*(?=\\.uni)", ignore_case = TRUE)),
            stringr::str_extract(.$origin, stringr::regex("(?<=//).*(?=\\.(xls|xlsx))", ignore_case = TRUE))
          )
        ) %>%
        tidyr::separate(origin, c("date", "instrument", "protein", "plate", "file"), sep = "-") %>%
        select(-file_name)
    )
  } else {
    return(parsed_list)
  }
}




#' Import UNcle DLS spectra into R
#'
#' \code{import_DLSspec}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex pattern for further selecing files in the directory;
#' defaults to NULL to force user input that discriminates intensity from mass DLS files
#' @param type a character string, "I" or "M", to signify if the data is intensity or mass distribution DLS spectra
#' @param header if TRUE skips first 3 rows of .xlsx file to remove UNcle header; default is TRUE
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_DLSspec <- function(directory_path, pattern = NULL, type = NA, header = TRUE, combine = TRUE) {
  if (missing(pattern) | is.null(pattern)) {
    stop("you must specify a search pattern to select the appropriate DLS files, e.g. 'DLS Spec I' or 'DLS Spec M'")
  }
  if (missing(type) | !(type %in% c("I", "M"))) {
    stop("DLS spectra type is required: 'I' for intensity, 'M' for mass")
  }
  if (!(header %in% c(TRUE, FALSE))) {
    stop("argument header must be TRUE or FALSE")
  }
  if (!(combine %in% c(TRUE, FALSE))) {
    stop("argument combine must be TRUE or FALSE")
  }
  skip <- 3
  if (!(header)) {
    skip <- 0
  }

  nestedColName <- paste0("specDLS_", type)
  nestedColName <- rlang::sym(nestedColName)

  file_list <- list.files(directory_path, pattern = pattern, full.names = TRUE) %>%
    purrr::set_names()

  sheet_list <- file_list %>%
    purrr::map(readxl::excel_sheets) %>%
    purrr::map(~ .x[.x != "Sheet1"])

  # print(file_list)
  # print(sheet_list)

  spectra_list <- purrr::map2(
    file_list,
    sheet_list,
    function(files, sheets) {
      purrr::map_dfr(
        purrr::set_names(sheets), ~ suppressMessages(readxl::read_excel(files, sheet = .x, skip = skip, .name_repair = "universal")) %>%
          # purrr::modify(readr::parse_number) %>%
          # function to rename variables and reduce complexity of DLS scans at multiple temperatures
          (function(df) {
            if (any(names(df) == "Hydrodynamic.Diameter..nm.") & any(names(df) == "Amplitude")) {
              df_modified <- df %>% 
                dplyr::select("Hydrodynamic.Diameter..nm.", "Amplitude") %>% 
                dplyr::rename(hydroDia_x = Hydrodynamic.Diameter..nm., amp_y = Amplitude)
            } else {
              df_modified <- df %>% 
                dplyr::select(c(1:2)) %>% 
                dplyr::rename(hydroDia_x = 1, amp_y = 2)
              message("DLS was performed at multiple temperatures. The first temperature data will be used.")
            }
            return(df_modified)
          }) %>% 
          {suppressMessages(tidyr::nest(., !!nestedColName := c(hydroDia_x, amp_y)))} %>%
          dplyr::select(!!nestedColName),
        .id = "capillary"
      )
    }
  )

  if (combine) {
    return(
      dplyr::bind_rows(spectra_list, .id = "origin") %>%
        dplyr::mutate(
          origin = dplyr::if_else(
            stringr::str_detect(.$origin, stringr::regex("\\.uni.*$")),
            stringr::str_extract(.$origin, stringr::regex("(?<=//).*(?=\\.uni)", ignore_case = TRUE)),
            stringr::str_extract(.$origin, stringr::regex("(?<=//).*(?=\\.(xls|xlsx))", ignore_case = TRUE))
          )
        ) %>%
        tidyr::separate(origin, c("date", "instrument", "protein", "plate", "file"), sep = "-")
    )
  } else {
    return(spectra_list)
  }
}
