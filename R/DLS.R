#' Import UNcle Tm/Tagg DLS summary into R
#'
#' \code{import_DLSsum}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex pattern for further selecing files in the directory,
#' defaults to reading all .xlsx files present
#' @param sheet character string to specify sheet if multi-sheet workbook is exported
#' @param temp_cutoff numeric value, excluding all DLS data obtained at temperatures above this value, defaults to 100°C
#' @return generates a named list of tibbles,
#' each neamed element is one dataframe named with its origin file path
#' @export
import_DLSsum <- function(directory_path, pattern = ".*\\.xlsx", sheet = NULL, temp_cutoff = 100) {
  file_list <- list.files(directory_path, pattern = pattern, full.names = TRUE) %>%
    purrr::set_names()

  df_list <- purrr::map(file_list, readxl::read_excel, sheet = sheet, col_types = "text")

  names_list <- purrr::map(
    df_list,
    function(df) {
      recode_values <- c(
        "color" = grep("color", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "well" = grep("well", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
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
        purrr::modify_at(.at = vars_parse, readr::parse_number, na = c(">1000", "Out of Range")) %>%
        purrr::modify_if(is.double, round, digits = 2) %>%
        dplyr::filter(temp_C < temp_cutoff) %>%
        tibble::add_column(mode = as.numeric(NA), .after = "Z_D") %>%
        dplyr::mutate(mode = if_else(is.na(peak2_D), 1, if_else(is.na(peak3_D), 2, 3))) %>% 
        tibble::add_column(
          file_name = stringr::str_extract(name, "(?<=//).*(?=\\.xlsx)"),
          .before = "well"
        )
    }
  )

  return(parsed_list)
}
