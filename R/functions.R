`%||%` <- function(a, b) if (!is.null(a)) a else b

read_csv_safe <- function(path, decimal_mark = ".") {
  if (requireNamespace("readr", quietly = TRUE)) {
    out <- tryCatch(
      readr::read_csv(
        path,
        locale = readr::locale(decimal_mark = decimal_mark),
        guess_max = 500000,
        show_col_types = FALSE
      ),
      error = function(e) {
        cli::cli_alert_warning("readr::read_csv failed for {path}. Falling back to base::read.csv. Error: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(out)) return(out)
  }

  tibble::as_tibble(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      dec = decimal_mark
    )
  )
}

read_delim_safe <- function(path, delim = ";", decimal_mark = ".") {
  if (requireNamespace("readr", quietly = TRUE)) {
    out <- tryCatch(
      readr::read_delim(
        path,
        delim = delim,
        locale = readr::locale(decimal_mark = decimal_mark, grouping_mark = ""),
        trim_ws = TRUE,
        guess_max = 500000,
        show_col_types = FALSE
      ),
      error = function(e) {
        cli::cli_alert_warning("readr::read_delim failed for {path}. Falling back to base::read.table. Error: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(out)) return(out)
  }

  tibble::as_tibble(
    utils::read.table(
      path,
      sep = delim,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      dec = decimal_mark,
      quote = "\"",
      comment.char = ""
    )
  )
}

read_source_delimited <- function(path, delim, source, column_types, required_columns, decimal_mark = ".") {
  if (!file.exists(path)) {
    stop(source, " input file does not exist: ", path, call. = FALSE)
  }
  file_size <- file.info(path)$size
  if (is.na(file_size) || file_size == 0) {
    stop(source, " input file is empty: ", path, call. = FALSE)
  }

  out <- NULL
  if (requireNamespace("readr", quietly = TRUE)) {
    parser_types <- lapply(column_types, function(type) {
      switch(
        type,
        character = readr::col_character(),
        integer = readr::col_integer(),
        double = readr::col_double(),
        stop("Unsupported import type: ", type, call. = FALSE)
      )
    })
    names(parser_types) <- names(column_types)
    col_spec <- do.call(
      readr::cols,
      c(list(.default = readr::col_character()), parser_types)
    )

    out <- tryCatch(
      suppressWarnings(
        readr::read_delim(
          path,
          delim = delim,
          col_types = col_spec,
          locale = readr::locale(decimal_mark = decimal_mark, grouping_mark = ""),
          trim_ws = TRUE,
          show_col_types = FALSE,
          name_repair = "minimal"
        )
      ),
      error = function(error) {
        cli::cli_alert_warning(
          "{source} readr import failed for {path}; retrying with base R. {conditionMessage(error)}"
        )
        NULL
      }
    )

    if (!is.null(out)) {
      parse_problems <- readr::problems(out)
      if (nrow(parse_problems) > 0) {
        problem_columns <- unique(parse_problems$col)
        stop(
          source, " import found ", nrow(parse_problems),
          " parsing problem(s) in column(s): ",
          paste(problem_columns, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  if (is.null(out)) {
    out <- tibble::as_tibble(
      utils::read.table(
        path,
        sep = delim,
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        colClasses = "character",
        na.strings = c("", "NA"),
        dec = decimal_mark,
        quote = "\"",
        comment.char = ""
      )
    )

    for (column in names(column_types)) {
      if (!column %in% names(out)) next
      values <- out[[column]]
      type <- column_types[[column]]
      if (type == "character") next

      numeric_input <- if (decimal_mark == ".") {
        values
      } else {
        gsub(decimal_mark, ".", values, fixed = TRUE)
      }
      numeric_values <- suppressWarnings(as.numeric(numeric_input))
      invalid <- !is.na(values) & nzchar(trimws(values)) & is.na(numeric_values)
      if (type == "integer") {
        invalid <- invalid | (!is.na(numeric_values) & numeric_values != trunc(numeric_values))
        parsed_values <- as.integer(numeric_values)
      } else {
        parsed_values <- numeric_values
      }
      if (any(invalid)) {
        stop(
          source, " import found invalid ", type, " value(s) in column '",
          column, "'.",
          call. = FALSE
        )
      }
      out[[column]] <- parsed_values
    }
  }

  out <- janitor::clean_names(out)
  missing_columns <- setdiff(required_columns, names(out))
  if (length(missing_columns) > 0) {
    stop(
      source, " input is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  expected_types <- unname(c(character = "character", integer = "integer", double = "double")[column_types])
  names(expected_types) <- janitor::make_clean_names(names(column_types))
  incorrect_types <- names(expected_types)[vapply(names(expected_types), function(column) {
    typeof(out[[column]]) != expected_types[[column]]
  }, logical(1))]
  if (length(incorrect_types) > 0) {
    stop(
      source, " input has unexpected type(s) for column(s): ",
      paste(incorrect_types, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(out) == 0) {
    stop(source, " input contains no data rows: ", path, call. = FALSE)
  }

  out
}

read_fao_export <- function(path) {
  column_types <- c(
    "COUNTRY.UN_CODE" = "character",
    "SPECIES.ALPHA_3_CODE" = "character",
    "AREA.CODE" = "character",
    "ENVIRONMENT.ALPHA_2_CODE" = "character",
    MEASURE = "character",
    PERIOD = "integer",
    VALUE = "double",
    STATUS = "character"
  )
  read_source_delimited(
    path = path,
    delim = ",",
    source = "FAO",
    column_types = column_types,
    required_columns = c(
      "country_un_code", "species_alpha_3_code", "area_code",
      "environment_alpha_2_code", "measure", "period", "value", "status"
    )
  )
}

read_eumofa_export <- function(path) {
  column_types <- c(
    year = "integer",
    country = "character",
    "desc country" = "character",
    "commodity group" = "character",
    "main commercial species" = "character",
    "ers code" = "character",
    "english name" = "character",
    "volume(kg)" = "double",
    weight = "character",
    "value(eur)" = "double"
  )
  read_source_delimited(
    path = path,
    delim = ";",
    source = "EUMOFA",
    column_types = column_types,
    required_columns = c(
      "year", "country", "desc_country", "commodity_group",
      "main_commercial_species", "ers_code", "english_name",
      "volume_kg", "weight", "value_eur"
    )
  )
}

write_csv_safe <- function(df, path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    ok <- tryCatch(
      {
        readr::write_csv(df, path)
        TRUE
      },
      error = function(e) {
        cli::cli_alert_warning("readr::write_csv failed for {path}. Falling back to base::write.csv. Error: {conditionMessage(e)}")
        FALSE
      }
    )
    if (ok) return(invisible(path))
  }

  utils::write.csv(df, path, row.names = FALSE)
  invisible(path)
}

resolve_fao_country_code <- function(country_un_code, override_map = NULL) {
  country_un_code <- as.character(country_un_code)
  iso2_auto <- countrycode::countrycode(
    sourcevar = suppressWarnings(as.integer(country_un_code)),
    origin = "un",
    destination = "iso2c",
    warn = FALSE
  )

  if (!is.null(override_map)) {
    stopifnot(all(c("country_un_code", "iso2") %in% names(override_map)))
    override_idx <- match(country_un_code, as.character(override_map$country_un_code))
    iso2_override <- override_map$iso2[override_idx]
    return(dplyr::coalesce(iso2_override, iso2_auto, country_un_code))
  }

  dplyr::coalesce(iso2_auto, country_un_code)
}

load_fx_table <- function(fx_path) {
  if (!file.exists(fx_path)) {
    cli::cli_alert_warning("FX CSV not found \u2014 using fallback rates")
    return(
      tibble::tribble(
        ~year, ~eur_per_usd,
        2018, 0.85,
        2019, 0.89,
        2020, 0.88,
        2021, 0.845,
        2022, 0.95,
        2023, 0.92,
        2024, 0.93
      )
    )
  }

  cli::cli_h1("Loading FX rates from CSV")
  fx_raw <- read_csv_safe(fx_path) |> janitor::clean_names()
  year_col <- if ("year" %in% names(fx_raw)) "year" else "period"

  if ("eur_per_usd" %in% names(fx_raw)) {
    fx <- fx_raw |> dplyr::transmute(
      year = as.integer(.data[[year_col]]),
      eur_per_usd = as.numeric(eur_per_usd)
    )
  } else if ("usd_per_eur" %in% names(fx_raw)) {
    fx <- fx_raw |> dplyr::transmute(
      year = as.integer(.data[[year_col]]),
      eur_per_usd = 1 / as.numeric(usd_per_eur)
    )
  } else {
    stop("FX file missing eur_per_usd or usd_per_eur")
  }

  all_years <- tibble::tibble(year = 2010:2030)
  fx <- all_years |>
    dplyr::left_join(fx, by = "year") |>
    tidyr::fill(eur_per_usd, .direction = "downup")

  cli::cli_alert_success("FX table loaded and expanded")
  fx
}
