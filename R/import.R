#' Import static Uncle spectra (nanoDSF and SLS) into R
#'
#' \code{import_staticBundle}
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex for narrowing selection of files in the `directory_path`;
#' defaults to "SLS Bundle"
#' @param skip number of rows of embedded run metadata in the .xlsx file to remove; default is 3
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_staticBundle <- function(directory_path, pattern = "SLS Bundle", skip = 3, combine = TRUE) {
  if (!(is.numeric(skip))) {
    stop("argument skip must be numeric")
  }
  if (!(combine %in% c(TRUE, FALSE))) {
    stop("argument combine must be TRUE or FALSE")
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
        stringr::str_extract, "\\d{6}(?!/).*$"
      )
    )
  }()

  message(paste0("Parsing ", length(file_list), " files:"))
  print(file_list)

  import_file <- function(path) {
    sheets <- readxl::excel_sheets(path) |> {
      \(s) s[match(s[s != "Sheet1"], uniOrder)]
    }() |> rlang::set_names()

    table <- suppressMessages(purrr::map_dfr(sheets,
      readxl::read_xlsx,
      path = path,
      skip = skip,
      col_types = "numeric",
      .name_repair = "universal",
      .id = "uni"
    )) |>
    tidyr::nest(specTm = c(2:3), specSLS266 = c(4:5), specSLS473 = c(6:7)) |>
    dplyr::mutate(dplyr::across(
      tidyselect::contains("spec"),
      \(lcol) purrr::map(lcol, \(df) dplyr::rename_with(df, .cols = 1, .fn = ~"temp_C"))
    )) |>
    # this makes spectra column names compatible with UncleDashboard modules
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("specSLS266", "specSLS473")),
      \(lcol) purrr::map(lcol, function(df) {
        dplyr::rename_with(df, .cols = c(1, 2), .fn = ~ c("temp_x", "intensity_y"))
      })
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("specTm")),
      \(lcol) purrr::map(lcol, function(df) {
        dplyr::rename_with(df, .cols = c(1, 2), .fn = ~ c("temp_x", "BCM_y"))
      })
    ))

    return(table)
  }

  spectra_tables <- purrr::map(file_list, import_file) |> {
    \(l) rlang::set_names(l,
      nm = purrr::map_chr(
        file_list,
        stringr::str_extract, "\\d{6}(?!/).*$"
      )
    )
  }()

  if (combine) {
    return(dplyr::bind_rows(spectra_tables, .id = "origin") |> {
      \(df) dplyr::mutate(df, origin = dplyr::if_else(
        stringr::str_detect(df$origin, stringr::regex("\\.uni.*$")),
        stringr::str_extract(df$origin, stringr::regex(".*(?=\\.uni)", ignore_case = TRUE)),
        stringr::str_extract(df$origin, stringr::regex(".*(?=\\.(xls|xlsx))", ignore_case = TRUE))
      ))
    }() |>
    tidyr::separate(origin, c("date", "instrument", "protein", "plate", "file"), sep = "-"))
  } else {
    return(spectra_tables)
  }
}


#' Import dynamic Uncle spectra (DLS) into R
#'
#' \code{import_dynamicBundle}
#'
#' Note: This import function will only import DLS spectra performed at
#' the beginning of the temperature ramp, i.e. the lowest temperature.
#'
#' @param directory_path a path to a directory containing the exported .xlsx files
#' @param pattern a regex for narrowing selection of files in the `directory_path`;
#' defaults to "DLS Bundle"
#' @param skip number of rows of embedded run metadata in the .xlsx file to remove; default is 2
#' @param combine if TRUE, returns all imported data merged into one unified dataframe with an "origin" column listing the original file path,
#' FALSE will return a list of dataframes; default is TRUE
#' @return a named (with filename) list of dataframes or a single merged dataframe
#' @export
import_dynamicBundle <- function(directory_path, pattern = "DLS Bundle", skip = 2, combine = TRUE) {
  if (!(is.numeric(skip))) {
    stop("argument skip must be numeric")
  }
  if (!(combine %in% c(TRUE, FALSE))) {
    stop("argument combine must be TRUE or FALSE")
  }

  file_list <- list.files(directory_path, pattern = pattern, full.names = TRUE) |> {
    \(l) rlang::set_names(l,
      nm = purrr::map_chr(
        l,
        stringr::str_extract, "\\d{6}(?!/).*$"
      )
    )
  }()

  message(paste0("Parsing ", length(file_list), " files:"))
  print(file_list)

  import_file <- function(path) {
    sheets <- readxl::excel_sheets(path) |> {
      \(s) s[s != "Sheet1"]
    }() |> rlang::set_names()

    bundle <- suppressMessages(purrr::map(
      sheets,
      \(sheet) readxl::read_xlsx(
        path = path,
        sheet = sheet,
        skip = 2,
        col_types = "numeric",
        .name_repair = "universal"
      )
    ))

    table <- reduce( # iterate through the dfs two at a time..
      list(
        # correlation
        bundle[grepl("Correlation", names(bundle))] |> {
          \(b) rlang::set_names(b,
            nm = stringr::str_extract(names(b), "(?<=\\-)[A-P]\\d+\\-\\d+$")
          )
        }() |>
        dplyr::bind_rows(.id = "uni") |>
        tidyr::nest(specDLS_C = c(2:3)),
        # intensity
        bundle[grepl("Intensity", names(bundle))] |> {
          \(b) rlang::set_names(b,
            nm = stringr::str_extract(names(b), "(?<=\\-)[A-P]\\d+\\-\\d+$")
          )
        }() |>
        dplyr::bind_rows(.id = "uni") |>
        tidyr::nest(specDLS_I = c(2:3)),
        # mass
        bundle[grepl("Mass", names(bundle))] |> {
          \(b) rlang::set_names(b,
            nm = stringr::str_extract(names(b), "(?<=\\-)[A-P]\\d+\\-\\d+$")
          )
        }() |>
        dplyr::bind_rows(.id = "uni") |>
        tidyr::nest(specDLS_M = c(2:3))
      ),
      # ..using join function
      dplyr::left_join,
      by = "uni"
    ) |>
    # select for DLS experiments performed at beginning of temperature ramp
    tidyr::separate(uni, into = c("uni", "temp_C"), sep = "-", convert = TRUE) |>
    dplyr::group_by(uni) |> # group to prevent dropping slightly higher temperatures at beginning of ramp
    dplyr::filter(temp_C == min(temp_C)) |>
    dplyr::select(-temp_C) |>
    dplyr::ungroup() |> # ungroup to avoid this causing problems downstream
    # this makes spectra column names compatible with UncleDashboard modules
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("specDLS_I", "specDLS_M")),
      \(lcol) purrr::map(lcol, function(df) {
        dplyr::rename_with(df, .cols = c(1, 2), .fn = ~ c("hydroDia_x", "amp_y"))
      })
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("specDLS_C")),
      \(lcol) purrr::map(lcol, function(df) {
        dplyr::rename_with(df, .cols = c(1, 2), .fn = ~ c("time_x", "amp_y"))
      })
    ))

    return(table)
  }

  spectra_tables <- purrr::map(file_list, import_file) |> {
    \(l) rlang::set_names(l,
      nm = purrr::map_chr(
        file_list,
        stringr::str_extract, "\\d{6}(?!/).*$"
      )
    )
  }()

  if (combine) {
    return(dplyr::bind_rows(spectra_tables, .id = "origin") |> {
      \(df) dplyr::mutate(df, origin = dplyr::if_else(
        stringr::str_detect(df$origin, stringr::regex("\\.uni.*$")),
        stringr::str_extract(df$origin, stringr::regex(".*(?=\\.uni)", ignore_case = TRUE)),
        stringr::str_extract(df$origin, stringr::regex(".*(?=\\.(xls|xlsx))", ignore_case = TRUE))
      ))
    }() |>
    tidyr::separate(origin, c("date", "instrument", "protein", "plate", "file"), sep = "-"))
  } else {
    return(spectra_tables)
  }
}
