##############################################################
# UNIVERSAL AQUACULTURE DATA HARVEST SCRIPT
# Sources: Eurostat API, FAO CSVs, EUMOFA CSV
# Author: Joonas Valve
# Date: 2026/02/13
##############################################################

suppressPackageStartupMessages({
  required_pkgs <- c(
    "dplyr", "readr", "tidyr", "janitor", "stringr", "cli", "openxlsx", "countrycode"
  )
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }

  library(dplyr)
  library(readr)
  library(tidyr)
  library(janitor)
  library(stringr)
  library(cli)
  library(openxlsx)

  has_eurostat <- requireNamespace("eurostat", quietly = TRUE)
  if (has_eurostat) {
    library(eurostat)
  } else {
    cli::cli_alert_warning("Package 'eurostat' is not available. Running in FAO/EUMOFA-only mode.")
  }
})

source("R/functions.R")

# ============================================================
# 0) USER PARAMETERS
# ============================================================

years_target <- 2018:2025
fao_qty_path <- "./data/FAO/Aquaculture_Quantity.csv"
fao_val_path <- "./data/FAO/Aquaculture_Value.csv"
eumofa_path  <- "./data/EUMOFA/Yearly_Aquaculture.csv"
fao_species_path <- "./data/FAO/CL_FI_SPECIES_GROUPS.csv"
fao_iso2_overrides_path <- "./data/FAO/fao_iso2_overrides.csv"
export_fao_diagnostics <- TRUE

# Output organization: keep category-based "latest" files easy to browse.
output_root <- "./output"
out_general <- file.path(output_root, "general")
out_freshwater <- file.path(output_root, "freshwater")
out_low_anthropic <- file.path(output_root, "low_anthropic")
out_diagnostics <- file.path(output_root, "diagnostics")

dir.create(out_general, showWarnings = FALSE, recursive = TRUE)
dir.create(out_freshwater, showWarnings = FALSE, recursive = TRUE)
dir.create(out_low_anthropic, showWarnings = FALSE, recursive = TRUE)
dir.create(out_diagnostics, showWarnings = FALSE, recursive = TRUE)

# Optional FAO UN→ISO2 overrides (only if you need manual corrections)
fao_to_iso2 <- if (file.exists(fao_iso2_overrides_path)) {
  read_csv_safe(fao_iso2_overrides_path) %>%
    clean_names() %>%
    transmute(country_un_code = as.character(country_un_code), iso2 = toupper(iso2))
} else {
  NULL
}

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

eu_raw <- if (has_eurostat) {
  tryCatch(
    {
      get_eurostat(
        id = "fish_aq2a",
        time_format = "num",
        cache = TRUE
      )
    },
    error = function(e1) {
      cli_alert_warning("Eurostat fetch failed on first attempt: {conditionMessage(e1)}")
      cli_alert_info("Retrying with cache refresh enabled...")
      tryCatch(
        {
          get_eurostat(
            id = "fish_aq2a",
            time_format = "num",
            cache = TRUE,
            update_cache = TRUE
          )
        },
        error = function(e2) {
          cli_alert_warning(
            "Eurostat download failed for dataset 'fish_aq2a'. Continuing without Eurostat data. Last error: {conditionMessage(e2)}"
          )
          tibble()
        }
      )
    }
  )
} else {
  tibble()
}

eu_clean <- eu_raw %>% clean_names()

cli_alert_success("Eurostat: loaded {nrow(eu_clean)} rows, {ncol(eu_clean)} cols")
print(head(eu_clean, 5))

# ============================================================
# 2) FAO CSVs — RAW
# ============================================================

cli_h1("FAO AQUACULTURE — RAW QUANTITY")

fao_qty_raw <- read_csv_safe(
  fao_qty_path,
  decimal_mark = "."
) %>% clean_names()

cli_alert_info("FAO Quantity columns:")
print(names(fao_qty_raw))
cli_alert_success("FAO Quantity rows: {nrow(fao_qty_raw)}")

cli_h1("FAO AQUACULTURE — RAW VALUE")

fao_val_raw <- read_csv_safe(
  fao_val_path,
  decimal_mark = "."
) %>% clean_names()

cli_alert_info("FAO Value columns:")
print(names(fao_val_raw))
cli_alert_success("FAO Value rows: {nrow(fao_val_raw)}")

if (export_fao_diagnostics && file.exists(fao_species_path)) {
  cli_h2("FAO diagnostics: environments and species groups")

  fao_species_lu <- read_csv_safe(fao_species_path) %>%
    clean_names() %>%
    transmute(
      species_alpha_3_code = x3a_code,
      species_name_en = name_en,
      major_group = major_group
    )

  fao_qty_diag <- fao_qty_raw %>%
    left_join(fao_species_lu, by = "species_alpha_3_code") %>%
    mutate(value_num = suppressWarnings(as.numeric(value)))

  fao_val_diag <- fao_val_raw %>%
    left_join(fao_species_lu, by = "species_alpha_3_code") %>%
    mutate(value_num = suppressWarnings(as.numeric(value)) * 1000)

  fao_env_qty_counts <- fao_qty_diag %>% count(environment_alpha_2_code, sort = TRUE)
  fao_env_val_counts <- fao_val_diag %>% count(environment_alpha_2_code, sort = TRUE)
  fao_major_group_qty_counts <- fao_qty_diag %>% count(major_group, sort = TRUE)
  fao_major_group_val_counts <- fao_val_diag %>% count(major_group, sort = TRUE)

  write_csv_safe(fao_env_qty_counts, file.path(out_diagnostics, "fao_env_qty_counts.csv"))
  write_csv_safe(fao_env_val_counts, file.path(out_diagnostics, "fao_env_val_counts.csv"))
  write_csv_safe(fao_major_group_qty_counts, file.path(out_diagnostics, "fao_major_group_qty_counts.csv"))
  write_csv_safe(fao_major_group_val_counts, file.path(out_diagnostics, "fao_major_group_val_counts.csv"))

  cli_alert_success("FAO diagnostics exported to output/diagnostics/")
}

# ============================================================
# 3) EUMOFA CSV — RAW
# ============================================================

cli_h1("EUMOFA AQUACULTURE — RAW DATA")

eumofa_raw <- read_delim_safe(
  eumofa_path,
  delim = ";",
  decimal_mark = "."
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
    unit    = "EUR",
    measure = "production_value_eur",
    source  = "fao_eur"
  ) %>%
  transmute(
    country_code, year, measure,
    values = values * eur_per_usd,
    unit, source, environment, country_un_code
  )

all_long_fx <- bind_rows(all_long, fao_v_long_fx)

cli_h2("LONG table updated with FX‑converted FAO EUR values")
print(head(all_long_fx, 20))


# ============================================================
# 4) LOW-ANTHROPIC CHECK (MUSSELS + SEAWEED/ALGAE)
# ============================================================

cli_h2("Building low-anthropic subset (mussels + seaweed/algae)")

# Broader low-impact mode includes all MOLLUSCA and PLANTAE AQUATICAE,
# and optionally a CRUSTACEA subset by name pattern.
include_crustacea_subset <- FALSE

if (!file.exists(fao_species_path)) {
  stop("FAO species lookup file missing: ", fao_species_path)
}

fao_species <- read_csv_safe(fao_species_path) %>%
  clean_names() %>%
  transmute(
    species_alpha_3_code = x3a_code,
    species_name_en = name_en,
    major_group = major_group
  )

mussel_pattern <- "mussel|oyster|clam|cockle|scallop|bivalv|mollusc|mollusk"
seaweed_pattern <- "seaweed|algae|kelp|wakame|spirulina|ulva|lettuce"
crustacea_pattern <- "shrimp|prawn|crab|lobster|crayfish|krill"

# Eurostat species-level (non-F00) for low-anthropic subset
eu_low_strict <- harmonise_eurostat_low_anthropic(aquaculture_raw$eurostat) %>%
  mutate(mode = "strict")

# Broad mode uses the same Eurostat subset currently available from label patterns.
eu_low_broad <- eu_low_strict %>%
  mutate(
    mode = "broad",
    production_type = case_when(
      production_type == "mussels_bivalves" ~ "mollusca_all",
      production_type == "seaweed_algae" ~ "aquatic_plants_all",
      TRUE ~ production_type
    )
  )

# FAO quantity/value filtered by explicit species metadata
fao_qty_low_base <- aquaculture_raw$fao_quantity %>%
  left_join(fao_species, by = "species_alpha_3_code") %>%
  mutate(species_name_en = tolower(species_name_en %||% ""))

fao_qty_low_strict <- fao_qty_low_base %>%
  mutate(
    production_type = case_when(
      str_detect(species_name_en, mussel_pattern) ~ "mussels_bivalves",
      major_group == "PLANTAE AQUATICAE" |
        str_detect(species_name_en, seaweed_pattern) ~ "seaweed_algae",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(production_type)) %>%
  transmute(
    country_un_code = as.character(country_un_code),
    year = as.integer(period),
    production_type,
    values = as.numeric(value),
    source = "fao_quantity",
    measure = "production_tonnes",
    mode = "strict"
  ) %>%
  mutate(country_code = resolve_fao_country_code(country_un_code, fao_to_iso2)) %>%
  select(country_code, year, production_type, source, measure, values, mode)

fao_qty_low_broad <- fao_qty_low_base %>%
  mutate(
    production_type = case_when(
      major_group == "MOLLUSCA" ~ "mollusca_all",
      major_group == "PLANTAE AQUATICAE" ~ "aquatic_plants_all",
      include_crustacea_subset & major_group == "CRUSTACEA" & str_detect(species_name_en, crustacea_pattern) ~ "crustacea_subset",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(production_type)) %>%
  transmute(
    country_un_code = as.character(country_un_code),
    year = as.integer(period),
    production_type,
    values = as.numeric(value),
    source = "fao_quantity",
    measure = "production_tonnes",
    mode = "broad"
  ) %>%
  mutate(country_code = resolve_fao_country_code(country_un_code, fao_to_iso2)) %>%
  select(country_code, year, production_type, source, measure, values, mode)

fao_val_low_base <- aquaculture_raw$fao_value %>%
  left_join(fao_species, by = "species_alpha_3_code") %>%
  mutate(species_name_en = tolower(species_name_en %||% ""))

fao_val_low_strict <- fao_val_low_base %>%
  mutate(
    production_type = case_when(
      str_detect(species_name_en, mussel_pattern) ~ "mussels_bivalves",
      major_group == "PLANTAE AQUATICAE" |
        str_detect(species_name_en, seaweed_pattern) ~ "seaweed_algae",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(production_type)) %>%
  transmute(
    country_un_code = as.character(country_un_code),
    year = as.integer(period),
    production_type,
    values_usd = as.numeric(value) * 1000
  ) %>%
  mutate(country_code = resolve_fao_country_code(country_un_code, fao_to_iso2)) %>%
  left_join(fx_tbl, by = "year") %>%
  transmute(
    country_code,
    year,
    production_type,
    source = "fao_eur",
    measure = "production_value_eur",
    values = values_usd * eur_per_usd,
    mode = "strict"
  )

fao_val_low_broad <- fao_val_low_base %>%
  mutate(
    production_type = case_when(
      major_group == "MOLLUSCA" ~ "mollusca_all",
      major_group == "PLANTAE AQUATICAE" ~ "aquatic_plants_all",
      include_crustacea_subset & major_group == "CRUSTACEA" & str_detect(species_name_en, crustacea_pattern) ~ "crustacea_subset",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(production_type)) %>%
  transmute(
    country_un_code = as.character(country_un_code),
    year = as.integer(period),
    production_type,
    values_usd = as.numeric(value) * 1000
  ) %>%
  mutate(country_code = resolve_fao_country_code(country_un_code, fao_to_iso2)) %>%
  left_join(fx_tbl, by = "year") %>%
  transmute(
    country_code,
    year,
    production_type,
    source = "fao_eur",
    measure = "production_value_eur",
    values = values_usd * eur_per_usd,
    mode = "broad"
  )

# EUMOFA filtered by commodity/species text
eumo_low_base <- aquaculture_raw$eumofa %>%
  mutate(
    species_txt = tolower(main_commercial_species %||% ""),
    commodity_txt = tolower(commodity_group %||% "")
  )

eumo_low_strict <- eumo_low_base %>%
  mutate(
    production_type = case_when(
      str_detect(species_txt, mussel_pattern) |
        str_detect(commodity_txt, "bivalv|mollusc|mollusk") ~ "mussels_bivalves",
      str_detect(species_txt, seaweed_pattern) |
        str_detect(commodity_txt, "seaweed|algae") ~ "seaweed_algae",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(production_type)) %>%
  transmute(
    country_code = toupper(country),
    year = as.integer(year),
    production_type,
    volume_tonnes = as.numeric(volume_kg) / 1000,
    value_eur = as.numeric(value_eur),
    mode = "strict"
  )

eumo_low_broad <- eumo_low_base %>%
  mutate(
    production_type = case_when(
      str_detect(species_txt, "mollusc|mollusk|bivalv|oyster|clam|cockle|scallop|mussel") |
        str_detect(commodity_txt, "mollusc|mollusk|bivalv") ~ "mollusca_all",
      str_detect(species_txt, seaweed_pattern) |
        str_detect(commodity_txt, "seaweed|algae") ~ "aquatic_plants_all",
      include_crustacea_subset & (
        str_detect(species_txt, crustacea_pattern) |
          str_detect(commodity_txt, crustacea_pattern)
      ) ~ "crustacea_subset",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(production_type)) %>%
  transmute(
    country_code = toupper(country),
    year = as.integer(year),
    production_type,
    volume_tonnes = as.numeric(volume_kg) / 1000,
    value_eur = as.numeric(value_eur),
    mode = "broad"
  )

eumo_low_tonnes_strict <- eumo_low_strict %>%
  transmute(
    country_code, year, production_type, mode,
    source = "eumofa", measure = "production_tonnes", values = volume_tonnes
  )

eumo_low_value_strict <- eumo_low_strict %>%
  transmute(
    country_code, year, production_type, mode,
    source = "eumofa", measure = "production_value_eur", values = value_eur
  )

eumo_low_tonnes_broad <- eumo_low_broad %>%
  transmute(
    country_code, year, production_type, mode,
    source = "eumofa", measure = "production_tonnes", values = volume_tonnes
  )

eumo_low_value_broad <- eumo_low_broad %>%
  transmute(
    country_code, year, production_type, mode,
    source = "eumofa", measure = "production_value_eur", values = value_eur
  )

low_anthropic_long <- bind_rows(
  eu_low_strict,
  fao_qty_low_strict,
  fao_val_low_strict,
  eumo_low_tonnes_strict,
  eumo_low_value_strict,
  eu_low_broad,
  fao_qty_low_broad,
  fao_val_low_broad,
  eumo_low_tonnes_broad,
  eumo_low_value_broad
) %>%
  group_by(mode, country_code, year, production_type, source, measure) %>%
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop")

low_anthropic_summary <- low_anthropic_long %>%
  pivot_wider(
    id_cols = c(mode, country_code, year, production_type),
    names_from = c(source, measure),
    values_from = values
  ) %>%
  arrange(mode, country_code, year, production_type)

low_anthropic_mode_compare <- low_anthropic_long %>%
  group_by(mode, country_code, year, source, measure) %>%
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(country_code, year, source, measure),
    names_from = mode,
    values_from = values
  ) %>%
  arrange(country_code, year, source, measure)

wb_low <- createWorkbook()
addWorksheet(wb_low, "by_mode")
writeData(wb_low, "by_mode", low_anthropic_summary)
addWorksheet(wb_low, "side_by_side")
writeData(wb_low, "side_by_side", low_anthropic_mode_compare)
saveWorkbook(
  wb_low,
  file.path(out_low_anthropic, "Europe_low_anthropic_summary_latest.xlsx"),
  overwrite = TRUE
)

cli_alert_success("Low-anthropic summary exported to output/low_anthropic/Europe_low_anthropic_summary_latest.xlsx (sheets: by_mode, side_by_side)")
cli_alert_info("Mode 'strict' uses species patterns; mode 'broad' includes MOLLUSCA + PLANTAE AQUATICAE and optional CRUSTACEA subset")
cli_alert_info("Eurostat broad mode currently remaps strict categories to broader labels")


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
  )

for (src_col in c("eurostat", "fao_quantity", "eumofa")) {
  if (!src_col %in% names(unit_check)) unit_check[[src_col]] <- NA_real_
}

unit_check <- unit_check %>%
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
suspicious <- unit_check %>% filter(suspicious_fao | suspicious_eumo)
if (nrow(suspicious) > 0) {
  cli_alert_danger("{nrow(suspicious)} suspicious unit mismatch(es) detected — verify raw data before using results")
}
print(suspicious)


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
# EUROPE SUMMARY (2018–2024, all countries with any data)
# ============================================================

years_consistent <- 2018:2024

if (has_eurostat) {
  eu_iso2   <- unique(eurostat::eu_countries$code)
  efta_iso2 <- unique(eurostat::efta_countries$code)
} else {
  # Fallback list keeps summary usable when eurostat package/API is unavailable.
  eu_iso2 <- c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR",
    "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
    "SE", "SI", "SK"
  )
  efta_iso2 <- c("IS", "LI", "NO", "CH")
}
europe_codes <- unique(c(eu_iso2, efta_iso2, "UK", "GB"))

summary_europe <- build_summary_table(all_long_fx, years_consistent, europe_codes)
write.xlsx(
  summary_europe,
  file.path(out_general, "Europe_summary_latest.xlsx"),
  sheetName = "data"
)


# ============================================================
# FRESHWATER SUMMARY (2018–2024, all countries with any data)
# ============================================================

fw_long_summary <- filter_freshwater(all_long_fx)
fw_summary      <- build_summary_table(fw_long_summary, years_consistent, europe_codes)
write.xlsx(
  fw_summary,
  file.path(out_freshwater, "Europe_freshwater_summary_latest.xlsx"),
  sheetName = "data"
)


# ============================================================
# Create consistency metrics and export CSV
# ============================================================

fw_results <- export_fw_consistency(
  fw_summary,
  out_dir = out_freshwater,
  filename = "freshwater_consistency_latest.csv"
)

