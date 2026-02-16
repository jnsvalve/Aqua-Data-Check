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
  library(openxlsx)
})

source("R/functions.R")

# ============================================================
# 0) USER PARAMETERS
# ============================================================

years_target <- 2018:2024
fao_qty_path <- "./data/FAO/Aquaculture_Quantity.csv"
fao_val_path <- "./data/FAO/Aquaculture_Value.csv"
eumofa_path  <- "./data/EUMOFA/Yearly_Aquaculture.csv"

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
# UNIVERSAL HARMONSE JOIN + COMPARISON LAYER
##############################################################

# ============================================================
# 1) HARMONISATION
# ============================================================

eu_long <- harmonise_eurostat(aquaculture_raw$eurostat)
fao_q_long <- harmonise_fao_qty(aquaculture_raw$fao_quantity, fao_to_iso2)
fao_v_long <- harmonise_fao_val(aquaculture_raw$fao_value, fao_to_iso2)
eumo_long <- harmonise_eumofa(aquaculture_raw$eumofa)

all_long <- bind_rows(eu_long, fao_q_long, fao_v_long, eumo_long) %>%
  arrange(country_code, year, measure, source)

cli_h2("Unified LONG table ready")
print(head(all_long, 20))


# ============================================================
# 3) FX CONVERSION: USD → EUR (Improved)
# ============================================================

fx_path <- "./data/FX/usd_eur_rates.csv"

fx_tbl <- load_fx_table(fx_path)

# Convert FAO USD → EUR
fao_v_long_fx <- fao_v_long %>%
  left_join(fx_tbl, by = "year") %>%
  mutate(
    production_value_eur_fao = values * eur_per_usd,
    unit = "EUR",
    measure = "production_value_eur_fao",
    source = "fao_value_eur_converted"
  ) %>%
  transmute(
    country_code, year, measure,
    values = production_value_eur_fao,
    unit, source, environment, country_un_code
  )

all_long_fx <- bind_rows(all_long, fao_v_long_fx)

cli_h2("LONG table updated with FX‑converted FAO EUR values")
print(head(all_long_fx, 20))


# ============================================================
# UNIT SANITY CHECK
# ============================================================

cli_h2("Checking for unit inconsistencies (kg vs tonnes)")

unit_check <- all_long_fx %>%
  filter(measure == "production_tonnes") %>%
  group_by(country_code, year, source) %>%
  summarise(value_tonnes = sum(values, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    id_cols   = c(country_code, year),
    names_from = source,
    values_from = value_tonnes
  ) %>%
  # Make sure there are no NA's
  mutate(
    eurostat   = coalesce(eurostat, 0),
    fao_quantity = coalesce(fao_quantity, 0),
    eumofa     = coalesce(eumofa, 0)
  ) %>%
  mutate(
    suspicious_fao  = (fao_quantity > 1e6) & (eurostat < 1e4),
    suspicious_eumo = (eumofa       > 1e6) & (eurostat < 1e4)
  )

cli_h2("Suspicious unit mismatches (if any)")
print(unit_check %>% filter(suspicious_fao | suspicious_eumo))


# ============================================================
# 5) RUN COMPARISON EXAMPLES
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


# ============================================================
# 6) EUROPE SUMMARY TABLE
# ============================================================

# 6.1 Aggregate to country-year-source-measure totals and pivot wider
totals_wide <- all_long_fx %>%
  filter(year %in% years_target) %>%
  mutate(values = as.numeric(values)) %>%
  group_by(country_code, year, source, measure) %>%
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols   = c(country_code, year),
    names_from  = c(source, measure),
    values_from = values
  )

# 6.2 Ensure expected columns exist (fill with NA if absent)
ensure_cols <- c(
  "eurostat_production_tonnes",
  "fao_quantity_production_tonnes",
  "eumofa_production_tonnes",
  "eurostat_production_value_eur",
  "eumofa_production_value_eur",
  "fao_value_eur_converted_production_value_eur_fao"
)
for (col in ensure_cols) {
  if (!col %in% names(totals_wide)) totals_wide[[col]] <- NA_real_
}

# 6.3 Create the simplified/clarified columns and reorder for readability
summary_simple <- totals_wide %>%
  transmute(
    country_code,
    year,
    eurostat_production = eurostat_production_tonnes,
    fao_production      = fao_quantity_production_tonnes,
    eumofa_production   = eumofa_production_tonnes,
    eurostat_value      = eurostat_production_value_eur,
    fao_value           = fao_value_eur_converted_production_value_eur_fao,
    eumofa_value        = eumofa_production_value_eur
  )

# 6.4 Filter to European countries (EU + EFTA + UK/GB)
eu_iso2   <- unique(eurostat::eu_countries$code)
efta_iso2 <- unique(eurostat::efta_countries$code)
europe_codes <- unique(c(eu_iso2, efta_iso2, "UK", "GB"))

summary_europe <- summary_simple %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
  filter(country_code %in% europe_codes,
         year %in% c(2021L, 2022L, 2023L, 2024L)) %>%
  arrange(country_code, year)

cli_h2("European summary — production and value")
print(head(summary_europe, 60))

write.xlsx(summary_europe, "Europe_summary.xlsx", sheetName = "data")

# ============================================================
# 7) FRESHWATER SUMMARY TABLE (European countries only)
# ============================================================

cli_h1("Building freshwater summary for Europe")

fw_long_summary <- filter_freshwater(all_long_fx)

fw_totals <- fw_long_summary %>%
  filter(year %in% years_target) %>%
  mutate(values = as.numeric(values)) %>%
  group_by(country_code, year, source, measure) %>%
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols   = c(country_code, year),
    names_from  = c(source, measure),
    values_from = values
  )

# Ensure expected columns exist
ensure_cols <- c(
  "eurostat_production_tonnes",
  "fao_quantity_production_tonnes",
  "eumofa_production_tonnes",
  "eurostat_production_value_eur",
  "eumofa_production_value_eur",
  "fao_value_eur_converted_production_value_eur_fao"
)
for (col in ensure_cols) {
  if (!col %in% names(fw_totals)) fw_totals[[col]] <- NA_real_
}

fw_summary <- fw_totals %>%
  transmute(
    country_code,
    year,
    eurostat_production = eurostat_production_tonnes,
    fao_production      = fao_quantity_production_tonnes,
    eumofa_production   = eumofa_production_tonnes,
    eurostat_value      = eurostat_production_value_eur,
    fao_value           = fao_value_eur_converted_production_value_eur_fao,
    eumofa_value        = eumofa_production_value_eur
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
  filter(country_code %in% europe_codes) %>%
  arrange(country_code, year)

cli_h2("Freshwater summary — Europe")
print(head(fw_summary, 60))

write.xlsx(fw_summary, "Europe_freshwater_summary.xlsx", sheetName = "data")


# ============================================================
# Create consistency metrics and export CSV
# ============================================================

fw_results <- export_fw_consistency(fw_summary)

