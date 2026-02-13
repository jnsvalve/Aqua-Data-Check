##############################################################
# UNIVERSAL AQUACULTURE DATA HARVEST SCRIPT
# Sources: Eurostat API, FAO CSVs, EUMOFA CSV
# Author: Joonas Valve
# Date: 2026/02/13
##############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(janitor)
  library(stringr)
  library(cli)
  library(eurostat)
})

# ============================================================
# 0) USER PARAMETERS
# ============================================================

years_target <- 2021:2023
fao_qty_path <- "./data/FAO/Aquaculture_Quantity.csv"
fao_val_path <- "./data/FAO/Aquaculture_Value.csv"
eumofa_path <- "./data/EUMOFA/Yearly_Aquaculture.csv"

# FAO → ISO2 mapping (expand later if needed)
fao_to_iso2 <- tibble::tribble(
  ~country_un_code, ~iso2,
  "348", "HU",
  "528", "NL",
  "642", "RO",
  "056", "BE",
  "191", "HR"
)

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

# (Optional) diagnostics: find duplicates by country-year-source-measure
inspect_duplicates <- function(long_tbl) {
  long_tbl %>%
    summarise(n = dplyr::n(), .by = c(country_code, year, source, measure)) %>%
    filter(n > 1L) %>%
    arrange(desc(n))
}

# ============================================================
# 1) EUROSTAT — RAW
# ============================================================

cli_h1("EUROSTAT AQUACULTURE — RAW DATA")

eu_raw <- get_eurostat(
  id = "fish_aq2a",
  time_format = "num",
  cache = TRUE
)

eu_clean <- eu_raw %>% clean_names()

cli_alert_success("Eurostat: loaded {nrow(eu_clean)} rows, {ncol(eu_clean)} cols")
print(head(eu_clean, 5))

# ============================================================
# 2) FAO CSVs — RAW
# ============================================================

cli_h1("FAO AQUACULTURE — RAW QUANTITY")

fao_qty_raw <- read_csv(
  fao_qty_path,
  locale = locale(decimal_mark = "."),
  guess_max = 500000,
  show_col_types = FALSE
) %>% clean_names()

cli_alert_info("FAO Quantity columns:")
print(names(fao_qty_raw))
cli_alert_success("FAO Quantity rows: {nrow(fao_qty_raw)}")

cli_h1("FAO AQUACULTURE — RAW VALUE")

fao_val_raw <- read_csv(
  fao_val_path,
  locale = locale(decimal_mark = "."),
  guess_max = 500000,
  show_col_types = FALSE
) %>% clean_names()

cli_alert_info("FAO Value columns:")
print(names(fao_val_raw))
cli_alert_success("FAO Value rows: {nrow(fao_val_raw)}")

# ============================================================
# 3) EUMOFA CSV — RAW
# ============================================================

cli_h1("EUMOFA AQUACULTURE — RAW DATA")

eumofa_raw <- read_delim(
  eumofa_path,
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ""),
  trim_ws = TRUE,
  guess_max = 500000,
  show_col_types = FALSE
) %>% clean_names()

cli_alert_info("EUMOFA columns:")
print(names(eumofa_raw))
cli_alert_success("EUMOFA rows: {nrow(eumofa_raw)}")

# ============================================================
# STORE RAW TABLES
# ============================================================

aquaculture_raw <- list(
  eurostat = eu_clean,
  fao_quantity = fao_qty_raw,
  fao_value = fao_val_raw,
  eumofa = eumofa_raw
)

cli_h2("RAW TABLES READY — NO FILTERS APPLIED")
print(lapply(aquaculture_raw, head))

##############################################################
# UNIVERSAL JOIN + COMPARISON LAYER
##############################################################

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
    group_by(country_code, year, unit, environment, aquameth, species, fishreg, freq) %>%
    summarise(values = first(values), .groups = "drop") %>%
    mutate(
      measure = case_when(
        unit == "TLW" ~ "production_tonnes",
        unit == "EUR" ~ "production_value_eur"
      ),
      source = "eurostat"
    ) %>%
    select(
      country_code, year, measure, values, unit, source,
      environment, aquameth, species, fishreg, freq
    )
}

harmonise_fao_qty <- function(df, map) {
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
    select(
      country_code, year, measure, values, unit, source,
      environment, country_un_code
    )
}

harmonise_fao_val <- function(df, map) {
  df %>%
    transmute(
      country_un_code = as.character(country_un_code),
      year = as.integer(period),
      environment = environment_alpha_2_code,
      measure = "production_value_usd",
      unit = "USD",
      values = suppressWarnings(as.numeric(value)),
      source = "fao_value"
    ) %>%
    left_join(map, by = "country_un_code") %>%
    mutate(country_code = coalesce(iso2, country_un_code)) %>%
    select(
      country_code, year, measure, values, unit, source,
      environment, country_un_code
    )
}

harmonise_eumofa <- function(df) {
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

eu_long <- harmonise_eurostat(aquaculture_raw$eurostat)
fao_q_long <- harmonise_fao_qty(aquaculture_raw$fao_quantity, fao_to_iso2)
fao_v_long <- harmonise_fao_val(aquaculture_raw$fao_value, fao_to_iso2)
eumo_long <- harmonise_eumofa(aquaculture_raw$eumofa)

all_long <- bind_rows(eu_long, fao_q_long, fao_v_long, eumo_long) %>%
  arrange(country_code, year, measure, source)

cli_h2("Unified LONG table ready")
print(head(all_long, 20))

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

  # FAO freshwater (IN) — EUR converted values (this was missing before)
  fao_v_fx_fw <- long_tbl %>%
    filter(source == "fao_value_eur_converted", environment == "IN")

  # EUMOFA freshwater: identify using commodity_group text
  eumo_fw <- long_tbl %>%
    filter(source == "eumofa") %>%
    filter(is.na(commodity_group) |
             str_detect(tolower(commodity_group), "fresh|inland"))

  bind_rows(es_fw, fao_q_fw, fao_v_fw, fao_v_fx_fw, eumo_fw) %>%
    arrange(country_code, year, measure, source)
}

# ============================================================
# 3) FX CONVERSION: USD → EUR
# ============================================================

fx_path <- "./data/FX/usd_eur_rates.csv"

if (file.exists(fx_path)) {
  cli_h1("Loading FX rates from CSV")
  fx_raw <- read_csv(fx_path, show_col_types = FALSE) %>% clean_names()
  col_year <- if ("year" %in% names(fx_raw)) {
    "year"
  } else if ("period" %in% names(fx_raw)) {
    "period"
  } else {
    stop("FX CSV must contain a year column")
  }
  if ("eur_per_usd" %in% names(fx_raw)) {
    fx_tbl <- fx_raw %>% transmute(
      year = !!sym(col_year),
      eur_per_usd = as.numeric(eur_per_usd)
    )
  } else if ("usd_per_eur" %in% names(fx_raw)) {
    fx_tbl <- fx_raw %>% transmute(
      year = !!sym(col_year),
      eur_per_usd = 1 / as.numeric(usd_per_eur)
    )
  } else {
    stop("FX CSV missing expected columns")
  }
  cli_alert_success("FX table loaded")
} else {
  cli_alert_warning("FX CSV not found — using fallback rates")
  fx_tbl <- tibble::tribble(
    ~year, ~eur_per_usd,
    2021, 0.845,
    2022, 0.950,
    2023, 0.920
  )
}

fao_v_long_fx <- fao_v_long %>%
  left_join(fx_tbl, by = "year") %>%
  mutate(
    production_value_eur_fao = values * eur_per_usd,
    measure = "production_value_eur_fao",
    unit = "EUR",
    source = "fao_value_eur_converted"
  ) %>%
  transmute(country_code, year, measure,
    values = production_value_eur_fao,
    unit, source, environment, country_un_code
  )

all_long_fx <- bind_rows(all_long, fao_v_long_fx) %>%
  arrange(country_code, year, measure, source)

cli_h2("LONG table updated with FX‑converted FAO EUR values")
print(head(all_long_fx, 20))

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
  ensure_cols <- c(
    "eurostat_production_tonnes",
    "fao_quantity_production_tonnes",
    "eumofa_production_tonnes",
    "eurostat_production_value_eur",
    "eumofa_production_value_eur",
    "fao_value_eur_converted_production_value_eur_fao"
  )
  for (col in ensure_cols) {
    if (!col %in% names(cmp_wide)) {
      cmp_wide[[col]] <- NA_real_
    }
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
        fao_value_eur_converted_production_value_eur_fao,

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
# 5) RUN EXAMPLES
# ============================================================

comparison_fx_focus <- build_comparison(
  long_tbl  = all_long_fx,
  years     = years_target,
  countries = c("HU", "NL", "RO", "BE", "HR"),
  tolerance = 0.30
)

cli_h2("Comparison ready (FX focus, country-year totals)")
print(head(comparison_fx_focus, 20))

# Freshwater-only comparison
fw_long_fx <- filter_freshwater(all_long_fx)
comparison_fw_fx <- build_comparison(
  long_tbl  = fw_long_fx,
  years     = years_target,
  countries = c("HU", "NL", "RO", "BE", "HR"),
  tolerance = 0.30
)
cli_h2("Freshwater-only comparison ready")
print(head(comparison_fw_fx, 20))
