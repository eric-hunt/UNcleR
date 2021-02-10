#' Import UNcle Tm/Tagg SLS summary into R
#'
#' \code{import_SLSsum}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex pattern for further selecing files in the directory;
#' defaults to "SLS Sum"
#' @param sheet character string to specify sheet if multi-sheet workbook is exported
#' @param header if TRUE skips first 4 rows of .xlsx file to remove UNcle header; default is FALSE
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_SLSsum <- function(directory_path, pattern = "SLS Sum", sheet = NULL, header = FALSE, combine = TRUE) {
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
        "Tonset" = grep("(?=.*Tonset)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm1" = grep("^(?=Tm1)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm2" = grep("^(?=Tm2)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm3" = grep("^(?=Tm3)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm3_avg" = grep("(?=.*Tm3)(?=.*average)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm3_CV" = grep("(?=.*Tm3)(?=.*cv)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm3_SD" = grep("(?=.*Tm3)(?=.*sd)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm4" = grep("^(?=Tm4)(?=.*\U00B0)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm4_avg" = grep("(?=.*Tm4)(?=.*average)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm4_CV" = grep("(?=.*Tm4)(?=.*cv)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
        "Tm4_SD" = grep("(?=.*Tm4)(?=.*sd)", names(df), ignore.case = TRUE, perl = TRUE, value = TRUE),
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
      names(swaped_vector) <- as.vector(named_vector)
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
    "Tonset",
    "Tm1",
    "Tm2",
    "Tm3",
    "Tm3_avg",
    "Tm3_CV",
    "Tm3_SD",
    "Tm4",
    "Tm4_avg",
    "Tm4_CV",
    "Tm4_SD",
    "Tagg266",
    "Tagg473"
  )

  parsed_list <- purrr::map2(
    renamed_list,
    names(df_list),
    function(df, name) {
      df %>%
        dplyr::select(-color) %>%
        purrr::modify_at(.at = vars_parse, readr::parse_number, na = c(">1000", "Out of Range", "-", NA, NaN, "\U221E")) %>%
        purrr::modify_if(is.double, round, digits = 2) %>%
        tibble::add_column(mode_Tm = purrr::pmap_dbl(dplyr::select(., tidyselect::matches("^Tm\\d{1}")), function(...) length(c(...)[!is.na(c(...))])), .after = "Tm1") %>%
        tibble::add_column(
          file_name = stringr::str_extract(name, "(?<=//).*(?=\\.xlsx)"),
          .before = "capillary"
        )
    }
  )

  if (combine) {
    return(
      dplyr::bind_rows(parsed_list, .id = "origin") %>%
        dplyr::mutate(
          origin = if_else(
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



#' Import UNcle Tagg spectra into R
#'
#' \code{import_SLSspec}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex pattern for further selecing files in the directory;
#' defaults to SLS Spec
#' @param lambda a number value representing wavelength for Tagg spectra, typically 266nm for small aggregates and 473nm for large aggregates;
#' default is 266nm
#' @param header if TRUE skips first 1 rows of .xlsx file to remove UNcle header; default is TRUE
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_SLSspec <- function(directory_path, pattern = "SLS Spec", lambda = 266, header = TRUE, combine = TRUE) {
  if (!(header %in% c(TRUE, FALSE))) {
    stop("argument header must be TRUE or FALSE")
  }
  if (!(combine %in% c(TRUE, FALSE))) {
    stop("argument combine must be TRUE or FALSE")
  }
  skip <- 1
  if (!(header)) {
    skip <- 0
  }

  nestedColName <- paste0("specSLS", lambda)
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
        purrr::set_names(sheets),
        ~ suppressMessages(readxl::read_excel(files, sheet = .x, skip = skip, .name_repair = "unique")) %>%
          .[-c(1:2), ] %>%
          purrr::modify(readr::parse_number) %>%
          dplyr::rename(wavelength = ...1) %>%
          dplyr::filter(abs(lambda - wavelength) == min(abs(lambda - wavelength))) %>%
          tidyr::nest(!!nestedColName := tidyselect::everything()),
        .id = "capillary"
      )
    }
  ) %>%
    purrr::map(
      function(df) {
        df %>%
          dplyr::mutate(
            !!nestedColName := purrr::modify(
              !!nestedColName, ~ tidyr::pivot_longer(
                .x, -tidyselect::one_of("wavelength"),
                names_to = "temp_x",
                names_pattern = "Temp :(.*),.*",
                names_transform = list(temp_x = as.numeric),
                # names_ptypes = list(temp_x = numeric()),
                values_to = "intensity_y",
                values_transform = list(intensity_y = as.numeric),
                # values_ptypes = list(intensity_y = numeric())
              ) %>% 
                select(temp_x, intensity_y, wavelength)
            )
          )
      }
    )

  if (combine) {
    return(
      dplyr::bind_rows(spectra_list, .id = "origin") %>%
        dplyr::mutate(
          origin = if_else(
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
