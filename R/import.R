#' Import static UNcle spectra (nanoDSF and SLS) into R
#'
#' \code{import_staticBundle}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex for narrowing selection of files in the `directory_path`;
#' defaults to "SLS Bundle"
#' @param header if TRUE skips first 3 rows of the .xlsx file to remove embedded run metadata; default is TRUE
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_staticBundle <- function(directory_path, pattern = "SLS Bundle", header = TRUE, combine = TRUE) {
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

  uniOrder <- purrr::map2_chr(
    rep(c(LETTERS[1:16]), 3),
    purrr::flatten_chr(purrr::map(c(1:3), rep, 16)),
    paste0
  )

  file_list <- list.files(directory_path, pattern = pattern, full.names = TRUE) |> {
    \(l) rlang::set_names(l,
      nm = purrr::map_chr(
        l,
        stringr::str_extract, "(?<=//).*(?=\\.(xls|xlsx))"
      )
    )
  }()

  message(paste0("Importing ", length(file_list), " files:"))
  print(file_list)

  import_file <- function(path) {
    sheets <- readxl::excel_sheets(path) |> {
      \(s) s[match(s[s != "Sheet1"], uniOrder)]
    }() |> rlang::set_names()

    bundle <- suppressMessages(purrr::map_dfr(sheets,
      readxl::read_xlsx,
      path = path,
      skip = skip,
      col_types = "numeric",
      .name_repair = "universal",
      .id = "uni"
    )) |>
    tidyr::nest(FLUORspec = c(2:3), SLSspec266 = c(4:5), SLSspec473 = c(6:7)) |>
    dplyr::mutate(dplyr::across(
      tidyselect::contains("spec"),
      \(lcol) purrr::map(lcol, \(df) dplyr::rename_with(df, .cols = 1, .fn = ~"temp_C"))
    ))

    return(bundle)
  }

  spectra_tables <- purrr::map(file_list, import_file) |> {
    \(l) rlang::set_names(l,
      nm = purrr::map_chr(
        file_list,
        stringr::str_extract, "(?<=//).*(?=\\.(xls|xlsx))"
      )
    )
  }()

  if (combine) {
    return(dplyr::bind_rows(spectra_tables, .id = "origin") |>
    tidyr::separate(origin, c("date", "instrument", "protein", "plate", "file"), sep = "-"))
  } else {
    return(spectra_tables)
  }
}
