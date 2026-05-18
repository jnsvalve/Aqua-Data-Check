# Null-coalescing helper — defined here so functions.R is self-contained
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Expected pivot_wider column names — single source of truth shared across functions
.ENSURE_COLS <- c(
  "eurostat_production_tonnes",
  "fao_quantity_production_tonnes",
  "eumofa_production_tonnes",
  "eurostat_production_value_eur",
  "eumofa_production_value_eur",
  "fao_eur_production_value_eur"
)

# ============================================================
# 1) HARMONISATION
# ============================================================

harmonise_eurostat <- function(eu_df) {
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

harmonise_fao_qty <- function(df, map) {
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
    left_join(map, by = "country_un_code") %>%
    mutate(country_code = coalesce(iso2, country_un_code)) %>%
    group_by(country_code, year, measure, unit, source, environment, country_un_code) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop")
}

harmonise_fao_val <- function(df, map) {
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
    left_join(map, by = "country_un_code") %>%
    mutate(country_code = coalesce(iso2, country_un_code)) %>%
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

  fx_raw <- read_csv(fx_path, show_col_types = FALSE) %>% clean_names()

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
  consistency_tbl <- fw_summary %>%
    mutate(
      # Ratios (Eurostat / Other sources)
      ratio_es_fao   = ifelse(!is.na(fao_production) & fao_production > 0,
                              eurostat_production / fao_production, NA_real_),
      ratio_es_eumo  = ifelse(!is.na(eumofa_production) & eumofa_production > 0,
                              eurostat_production / eumofa_production, NA_real_),

      # Log-differences (more stable indicator)
      logdiff_es_fao  = log1p(eurostat_production) - log1p(coalesce(fao_production, 0)),
      logdiff_es_eumo = log1p(eurostat_production) - log1p(coalesce(eumofa_production, 0)),

      # Basic flags
      flag_large_gap_fao  = ifelse(!is.na(ratio_es_fao)  & ratio_es_fao  > 5, TRUE, FALSE),
      flag_large_gap_eumo = ifelse(!is.na(ratio_es_eumo) & ratio_es_eumo > 5, TRUE, FALSE),

      # Human-readable notes
      notes = case_when(
        is.na(eurostat_production) & !is.na(fao_production) ~ "Eurostat missing, FAO present",
        is.na(fao_production) & !is.na(eurostat_production) ~ "FAO missing, Eurostat present",
        is.na(eumofa_production) & !is.na(eurostat_production) ~ "EUMOFA missing, Eurostat present",
        ratio_es_eumo > 5  ~ "Eurostat >> EUMOFA (normal for pond countries)",
        ratio_es_eumo < 0.2 ~ "Eurostat << EUMOFA (check)",
        TRUE ~ ""
      )
    )

  # ---- Export CSV ----
  write_csv(consistency_tbl, out_path)

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
