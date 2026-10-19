# Null-coalescing helper — defined here so functions.R is self-contained
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

# Expected pivot_wider column names — single source of truth shared across functions
.ENSURE_COLS <- c(
  "eurostat_production_tonnes",
  "fao_quantity_production_tonnes",
  "eumofa_production_tonnes",
  "eurostat_production_value_eur",
  "eumofa_production_value_eur",
  "fao_eur_production_value_eur"
)

# Resolve FAO UN country code to ISO2 using countrycode, with optional manual overrides.
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

# ============================================================
# 1) HARMONISATION
# ============================================================

harmonise_eurostat <- function(eu_df) {
  required_cols <- c("unit", "geo", "aquaenv", "freq", "species", "fishreg", "values")
  if (nrow(eu_df) == 0 || !all(required_cols %in% names(eu_df))) {
    return(tibble::tibble(
      country_code = character(),
      year = integer(),
      measure = character(),
      values = numeric(),
      unit = character(),
      source = character(),
      environment = character()
    ))
  }

  time_col <- if ("time" %in% names(eu_df)) {
    "time"
  } else if ("time_period" %in% names(eu_df)) {
    "time_period"
  } else {
    stop("Eurostat: No time column found")
  }

  eu_df %>%
    filter(unit %in% c("TLW", "EUR")) %>%
    mutate(
      year = as.integer(.data[[time_col]]),
      country_code = geo,
      environment = aquaenv
    ) %>%
    # KEEP ONLY TOTAL ROWS:
    filter(
      freq == "A",
      species == "F00",       # TOTAL
      fishreg == "0"          # TOTAL region
      # aquameth: keep all (Eurostat does not always provide TOTAL)
    ) %>%
    group_by(country_code, year, unit, environment) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      measure = case_when(
        unit == "TLW" ~ "production_tonnes",
        unit == "EUR" ~ "production_value_eur"
      ),
      source = "eurostat"
    ) %>%
    select(
      country_code, year, measure, values, unit, source, environment
    )
}

harmonise_eurostat_low_anthropic <- function(eu_df, species_labels = NULL) {
  required_cols <- c("unit", "geo", "aquaenv", "freq", "species", "fishreg", "values")
  if (nrow(eu_df) == 0 || !all(required_cols %in% names(eu_df))) {
    return(tibble::tibble(
      country_code = character(),
      year = integer(),
      production_type = character(),
      source = character(),
      measure = character(),
      values = numeric()
    ))
  }

  time_col <- if ("time" %in% names(eu_df)) {
    "time"
  } else if ("time_period" %in% names(eu_df)) {
    "time_period"
  } else {
    stop("Eurostat: No time column found")
  }

  mussel_pattern <- "mussel|oyster|clam|cockle|scallop|bivalv|mollusc|mollusk"
  algae_pattern <- "algae|alga|seaweed|kelp|wakame|spirulina|ulva|lettuce"

  eu_low <- eu_df %>%
    filter(unit %in% c("TLW", "EUR")) %>%
    mutate(
      year = as.integer(.data[[time_col]]),
      country_code = geo,
      environment = aquaenv,
      species_code = species
    ) %>%
    filter(
      freq == "A",
      species_code != "F00",
      fishreg == "0"
    )

  if (nrow(eu_low) == 0) {
    return(tibble::tibble(
      country_code = character(),
      year = integer(),
      production_type = character(),
      source = character(),
      measure = character(),
      values = numeric()
    ))
  }

  species_lookup <- tibble(species_code = unique(eu_low$species_code))
  if (!is.null(species_labels)) {
    required_label_cols <- c("species_code", "species_label")
    if (!all(required_label_cols %in% names(species_labels))) {
      stop("Eurostat species labels must contain species_code and species_label")
    }
    species_lookup <- species_lookup %>%
      left_join(
        species_labels %>%
          transmute(
            species_code = as.character(species_code),
            species_label = as.character(species_label)
          ) %>%
          distinct(species_code, .keep_all = TRUE),
        by = "species_code"
      )
  } else if (requireNamespace("eurostat", quietly = TRUE)) {
    species_lookup <- species_lookup %>%
      mutate(species_label = suppressWarnings(eurostat::label_eurostat(species_code, dic = "SPECIES")))
  } else {
    species_lookup <- species_lookup %>% mutate(species_label = NA_character_)
  }

  eu_low %>%
    left_join(species_lookup, by = "species_code") %>%
    mutate(
      species_label = tolower(as.character(species_label)),
      production_type = case_when(
        str_detect(species_label %||% "", mussel_pattern) ~ "mussels_bivalves",
        str_detect(species_label %||% "", algae_pattern) ~ "seaweed_algae",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(production_type)) %>%
    group_by(country_code, year, unit, environment, production_type) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      measure = case_when(
        unit == "TLW" ~ "production_tonnes",
        unit == "EUR" ~ "production_value_eur"
      ),
      source = "eurostat_species"
    ) %>%
    select(country_code, year, production_type, source, measure, values)
}

harmonise_fao_qty <- function(df, map = NULL) {
  n_fail <- sum(is.na(suppressWarnings(as.numeric(df$value)))) - sum(is.na(df$value))
  if (n_fail > 0) cli_alert_warning("FAO quantity: {n_fail} non-numeric value(s) coerced to NA")

  df %>%
    transmute(
      country_un_code = as.character(country_un_code),
      year = as.integer(period),
      environment = environment_alpha_2_code,
      measure = "production_tonnes",
      unit = "TONNES",
      values = suppressWarnings(as.numeric(value)),
      source = "fao_quantity"
    ) %>%
    mutate(country_code = resolve_fao_country_code(country_un_code, map)) %>%
    group_by(country_code, year, measure, unit, source, environment, country_un_code) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop")
}

harmonise_fao_val <- function(df, map = NULL) {
  n_fail <- sum(is.na(suppressWarnings(as.numeric(df$value)))) - sum(is.na(df$value))
  if (n_fail > 0) cli_alert_warning("FAO value: {n_fail} non-numeric value(s) coerced to NA")

  df %>%
    transmute(
      country_un_code = as.character(country_un_code),
      year = as.integer(period),
      environment = environment_alpha_2_code,
      measure = "production_value_usd",
      unit = "USD",
      # FAO publishes V_USD_1000 → multiply by 1000
      values_usd = suppressWarnings(as.numeric(value)) * 1000,
      source = "fao_value"
    ) %>%
    mutate(country_code = resolve_fao_country_code(country_un_code, map)) %>%
    select(
      country_code, year, measure,
      values = values_usd, unit, source, environment, country_un_code
    )
}

harmonise_eumofa <- function(df) {
  n_fail_vol <- sum(is.na(suppressWarnings(as.numeric(df$volume_kg)))) - sum(is.na(df$volume_kg))
  n_fail_val <- sum(is.na(suppressWarnings(as.numeric(df$value_eur)))) - sum(is.na(df$value_eur))
  if (n_fail_vol > 0) cli_alert_warning("EUMOFA: {n_fail_vol} volume_kg value(s) coerced to NA")
  if (n_fail_val > 0) cli_alert_warning("EUMOFA: {n_fail_val} value_eur value(s) coerced to NA")

  tonnes <- df %>%
    transmute(
      country_code = toupper(country),
      year = as.integer(year),
      measure = "production_tonnes",
      unit = "TONNES",
      values = suppressWarnings(as.numeric(volume_kg)) / 1000,
      source = "eumofa",
      commodity_group = commodity_group %||% NA_character_
    )
  value_eur <- df %>%
    transmute(
      country_code = toupper(country),
      year = as.integer(year),
      measure = "production_value_eur",
      unit = "EUR",
      values = suppressWarnings(as.numeric(value_eur)),
      source = "eumofa",
      commodity_group = commodity_group %||% NA_character_
    )
  bind_rows(tonnes, value_eur) %>% filter(!is.na(values))
}


# ============================================================
# 2) FRESHWATER FILTER FUNCTION
# ============================================================

filter_freshwater <- function(long_tbl) {
  # EUROSTAT freshwater (FRW)
  es_fw    <- long_tbl %>%
    filter(source == "eurostat", environment == "FRW")

  # FAO freshwater (IN) — quantity (tonnes)
  fao_q_fw <- long_tbl %>%
    filter(source == "fao_quantity", environment == "IN")

  # FAO freshwater (IN) — original USD values
  fao_v_fw <- long_tbl %>%
    filter(source == "fao_value", environment == "IN")

  # FAO freshwater (IN) — EUR converted values
  fao_v_fx_fw <- long_tbl %>%
    filter(source == "fao_eur", environment == "IN")

  # EUMOFA freshwater: identify using commodity_group text
  eumo_fw <- long_tbl %>%
    filter(source == "eumofa") %>%
    filter(is.na(commodity_group) |
             str_detect(tolower(commodity_group), "fresh|inland"))

  n_eumo_fw <- nrow(eumo_fw)
  cli_alert_info("EUMOFA freshwater rows matched: {n_eumo_fw} (via 'fresh|inland' in commodity_group)")

  bind_rows(es_fw, fao_q_fw, fao_v_fw, fao_v_fx_fw, eumo_fw) %>%
    arrange(country_code, year, measure, source)
}

# ============================================================
# 3) FX CONVERSION: USD → EUR (Improved)
# ============================================================

convert_fao_usd_values <- function(fao_usd, fx_rates) {
  fao_usd %>%
    left_join(fx_rates, by = "year") %>%
    mutate(
      unit = "EUR",
      measure = "production_value_eur",
      source = "fao_eur",
      values = values * eur_per_usd
    ) %>%
    select(-eur_per_usd)
}

load_fx_table <- function(fx_path) {
  if (!file.exists(fx_path)) {
    cli_alert_warning("FX CSV not found — using fallback rates")
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

  cli_h1("Loading FX rates from CSV")

  fx_raw <- read_csv_safe(fx_path) %>% clean_names()

  # Detect column names
  year_col <- if ("year" %in% names(fx_raw)) "year" else "period"

  if ("eur_per_usd" %in% names(fx_raw)) {
    fx <- fx_raw %>% transmute(
      year = as.integer(.data[[year_col]]),
      eur_per_usd = as.numeric(eur_per_usd)
    )
  } else if ("usd_per_eur" %in% names(fx_raw)) {
    fx <- fx_raw %>% transmute(
      year = as.integer(.data[[year_col]]),
      eur_per_usd = 1 / as.numeric(usd_per_eur)
    )
  } else {
    stop("FX file missing eur_per_usd or usd_per_eur")
  }

  # Fill missing years using nearest value
  all_years <- tibble(year = 2010:2030)
  fx <- all_years %>%
    left_join(fx, by = "year") %>%
    tidyr::fill(eur_per_usd, .direction = "downup")

  cli_alert_success("FX table loaded and expanded")
  return(fx)
}

# ============================================================
# 4) COMPARISON BUILDER (WITH FIX A: PRE-AGGREGATION)
# ============================================================

build_comparison <- function(long_tbl, years = NULL, countries = NULL, tolerance = 0.30) {
  cmp <- long_tbl

  if (!is.null(years))     cmp <- cmp %>% filter(year %in% years)
  if (!is.null(countries)) cmp <- cmp %>% filter(country_code %in% countries)

  # FIX A: aggregate to country-year-source-measure totals
  cmp <- cmp %>%
    mutate(values = as.numeric(values)) %>%
    group_by(country_code, year, source, measure) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop")

  cmp_wide <- cmp %>%
    pivot_wider(
      id_cols   = c(country_code, year),
      names_from  = c(source, measure),
      values_from = values
    ) %>%
    arrange(country_code, year)

  # Ensure required columns exist (create as NA_real_ if missing)
  for (col in .ENSURE_COLS) {
    if (!col %in% names(cmp_wide)) {
      cmp_wide[[col]] <- NA_real_
    }
  }

  # Warn if FAO EUR value column is all-NA (FX conversion may not have run)
  if (all(is.na(cmp_wide[["fao_eur_production_value_eur"]]))) {
    cli_alert_warning("build_comparison: 'fao_eur_production_value_eur' is all-NA — FAO USD\u2192EUR conversion may not have run")
  }

  # TONNES ratios
  cmp_wide <- cmp_wide %>%
    mutate(
      ratio_es_fao_qty  = eurostat_production_tonnes / fao_quantity_production_tonnes,
      ratio_es_eumo_qty = eurostat_production_tonnes / eumofa_production_tonnes
    )

  # VALUE ratios (EUR vs EUR; use FX-converted FAO)
  cmp_wide <- cmp_wide %>%
    mutate(
      ratio_es_fao_val =
        eurostat_production_value_eur /
        fao_eur_production_value_eur,

      ratio_es_eumo_val =
        eurostat_production_value_eur /
        eumofa_production_value_eur
    )

  # Flags with tolerance
  cmp_wide <- cmp_wide %>%
    mutate(
      flag_es_fao_qty = case_when(
        is.na(ratio_es_fao_qty) ~ NA_character_,
        abs(ratio_es_fao_qty - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      ),
      flag_es_eumo_qty = case_when(
        is.na(ratio_es_eumo_qty) ~ NA_character_,
        abs(ratio_es_eumo_qty - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      ),
      flag_es_fao_val = case_when(
        is.na(ratio_es_fao_val) ~ NA_character_,
        abs(ratio_es_fao_val - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      ),
      flag_es_eumo_val = case_when(
        is.na(ratio_es_eumo_val) ~ NA_character_,
        abs(ratio_es_eumo_val - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      )
    )

  cmp_wide
}

# ============================================================
# FUNCTION: Create consistency metrics and export CSV
# ============================================================

build_fw_consistency <- function(fw_summary) {
  fw_summary %>%
    mutate(
      ratio_es_fao = ifelse(!is.na(fao_production) & fao_production > 0,
                            eurostat_production / fao_production, NA_real_),
      ratio_es_eumo = ifelse(!is.na(eumofa_production) & eumofa_production > 0,
                             eurostat_production / eumofa_production, NA_real_),
      logdiff_es_fao = log1p(eurostat_production) - log1p(coalesce(fao_production, 0)),
      logdiff_es_eumo = log1p(eurostat_production) - log1p(coalesce(eumofa_production, 0)),
      flag_large_gap_fao = ifelse(!is.na(ratio_es_fao) & ratio_es_fao > 5, TRUE, FALSE),
      flag_large_gap_eumo = ifelse(!is.na(ratio_es_eumo) & ratio_es_eumo > 5, TRUE, FALSE),
      notes = case_when(
        is.na(eurostat_production) & !is.na(fao_production) ~ "Eurostat missing, FAO present",
        is.na(fao_production) & !is.na(eurostat_production) ~ "FAO missing, Eurostat present",
        is.na(eumofa_production) & !is.na(eurostat_production) ~ "EUMOFA missing, Eurostat present",
        ratio_es_eumo > 5 ~ "Eurostat >> EUMOFA (normal for pond countries)",
        ratio_es_eumo < 0.2 ~ "Eurostat << EUMOFA (check)",
        TRUE ~ ""
      )
    )
}

export_fw_consistency <- function(fw_summary,
                                  out_dir = "./output",
                                  filename = NULL) {

  # Create output directory if needed
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # Default filename with timestamp
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    filename <- paste0("freshwater_consistency_", timestamp, ".csv")
  }

  out_path <- file.path(out_dir, filename)

  # ---- Build consistency metrics table ----
  consistency_tbl <- build_fw_consistency(fw_summary)

  # ---- Export CSV ----
  write_csv_safe(consistency_tbl, out_path)

  message("✔ Consistency CSV written to: ", out_path)
  return(consistency_tbl)
}

# ============================================================
# FUNCTION: Build country-year summary table (shared by Europe and freshwater)
# ============================================================

build_summary_table <- function(long_fx, years, europe_codes) {
  totals_wide <- long_fx %>%
    filter(year %in% years) %>%
    mutate(values = as.numeric(values)) %>%
    group_by(country_code, year, source, measure) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      id_cols     = c(country_code, year),
      names_from  = c(source, measure),
      values_from = values
    )

  for (col in .ENSURE_COLS) {
    if (!col %in% names(totals_wide)) totals_wide[[col]] <- NA_real_
  }

  totals_wide %>%
    transmute(
      country_code,
      year,
      eurostat_production = eurostat_production_tonnes,
      fao_production      = fao_quantity_production_tonnes,
      eumofa_production   = eumofa_production_tonnes,
      eurostat_value      = eurostat_production_value_eur,
      fao_value           = fao_eur_production_value_eur,
      eumofa_value        = eumofa_production_value_eur
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
    filter(country_code %in% europe_codes) %>%
    group_by(country_code) %>%
    filter(any(!is.na(eurostat_production) |
                 !is.na(fao_production) |
                 !is.na(eumofa_production) |
                 !is.na(eurostat_value) |
                 !is.na(fao_value) |
                 !is.na(eumofa_value))) %>%
    ungroup() %>%
    arrange(country_code, year)
}

build_comparison_table <- function(observations,
                                  years,
                                  countries,
                                  memberships = NULL,
                                  membership_dimensions = character()) {
  scoped_observations <- observations %>%
    filter(year %in% years, country_code %in% countries)
  if (!is.null(memberships)) {
    scoped_observations <- scoped_observations %>%
      inner_join(memberships, by = "observation_id")
  }
  scope_columns <- c(membership_dimensions, "country_code", "year")

  eurostat_totals <- scoped_observations %>%
    filter(source == "eurostat", record_scope == "published_total") %>%
    distinct(country_code, year, measure)
  eurostat_observations <- scoped_observations %>%
    filter(source == "eurostat")
  eurostat_categories <- eurostat_observations %>%
    filter(record_scope != "published_total") %>%
    anti_join(eurostat_totals, by = c("country_code", "year", "measure"))

  comparison_values <- bind_rows(
    filter(scoped_observations, source != "eurostat"),
    filter(eurostat_observations, record_scope == "published_total"),
    eurostat_categories
  ) %>%
    group_by(across(all_of(c(scope_columns, "source", "measure")))) %>%
    summarise(
      values = if (all(is.na(comparison_value))) {
        NA_real_
      } else {
        sum(comparison_value, na.rm = TRUE)
      },
      .groups = "drop"
    ) %>%
    pivot_wider(
      id_cols = all_of(scope_columns),
      names_from = c(source, measure),
      values_from = values
    )

  expected_columns <- c(
    "eurostat_production_tonnes",
    "fao_quantity_production_tonnes",
    "eumofa_production_tonnes",
    "eurostat_production_value_eur",
    "fao_value_production_value_eur",
    "eumofa_production_value_eur"
  )
  for (column in expected_columns) {
    if (!column %in% names(comparison_values)) comparison_values[[column]] <- NA_real_
  }

  comparison_values %>%
    select(
      all_of(scope_columns),
      eurostat_production = eurostat_production_tonnes,
      fao_production = fao_quantity_production_tonnes,
      eumofa_production = eumofa_production_tonnes,
      eurostat_value = eurostat_production_value_eur,
      fao_value = fao_value_production_value_eur,
      eumofa_value = eumofa_production_value_eur
    ) %>%
    arrange(across(all_of(scope_columns)))
}
