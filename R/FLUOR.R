#' Import UNcle fluorescence spectra into R
#'
#' \code{import_FLUORspec}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex pattern for further selecing files in the directory;
#' defaults to "Tm Spec"
#' @param header if TRUE skips first 3 rows of .xlsx file to remove UNcle header; default is TRUE
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_FLUORspec <- function(directory_path, pattern = "Tm Spec", header = TRUE, combine = TRUE) {
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
          dplyr::rename(temp_FLUOR = Temperature, BCM = BCM...nm) %>%
          tidyr::nest(specTm = c(temp_FLUOR, BCM)),
        .id = "capillary"
      )
    }
  )

  if (combine) {
    return(
      dplyr::bind_rows(spectra_list, .id = "origin") %>%
        dplyr::mutate(origin = stringr::str_extract(.$origin, stringr::regex("(?<=//).*\\.(xls|xlsx)", ignore_case = TRUE))) %>%
        tidyr::separate(origin, c("date", "instrument", "protein", "plate", "file"), sep = "-")
    )
  } else {
    return(spectra_list)
  }
}
