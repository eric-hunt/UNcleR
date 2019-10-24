#' Import UNcle Tm/Tagg SLS summary into R
#'
#' \code{import_SLSsum}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex pattern for further selecing files in the directory,
#' defaults to reading all .xlsx files present
#' @param sheet character string to specify sheet if multi-sheet workbook is exported
#' @return generates a named list of tibbles,
#' each neamed element is one dataframe named with its origin file path
#' @export
import_SLSsum <- function(directory_path, pattern = ".*\\.xlsx", sheet = NULL) {
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
        "Tm1" = grep("^(?=Tm1)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm2" = grep("^(?=Tm2)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm3" = grep("^(?=Tm3)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm3_avg" = grep("(?=.*Tm3)(?=.*average)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm4_CV" = grep("(?=.*Tm3)(?=.*cv)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm4_SD" = grep("(?=.*Tm3)(?=.*sd)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tagg266" = grep("(?=.*Tagg)(?=.*266)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tagg473" = grep("(?=.*Tagg)(?=.*473)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE)
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
    "Tm1",
    "Tm2",
    "Tm3",
    "Tm3_avg",
    "Tm3_CV",
    "Tm3_SD",
    "Tagg266",
    "Tagg473"
  )
  
  parsed_list <- purrr::map2(
    renamed_list,
    names(df_list),
    function(df, name) {
      df %>%
        dplyr::select(-color) %>%
        purrr::modify_at(.at = vars_parse, readr::parse_number, na = c(">1000", "Out of Range")) %>%
        purrr::modify_if(is.double, round, digits = 2) %>%
        tibble::add_column(mode = as.numeric(NA), .after = "Tm1") %>%
        dplyr::mutate(mode = if_else(is.na(Tm2), 1, if_else(is.na(Tm3), 2, 3))) %>% 
        tibble::add_column(
          file_name = stringr::str_extract(name, "(?<=//).*(?=\\.xlsx)"),
          .before = "well"
        )
    }
  )
  
  return(parsed_list)
}
