##############################################################
# UNIVERSAL AQUACULTURE DATA HARVEST SCRIPT
# Sources: Eurostat API, FAO CSVs, EUMOFA CSV
# Author: Joonas Valve
# Date: Feb 13, 2026
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
source("R/low_anthropic.R")
source("R/pipeline.R")

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
  read_csv_safe(fao_iso2_overrides_path) |>
    clean_names() |>
    transmute(country_un_code = as.character(country_un_code), iso2 = toupper(iso2))
} else {
  NULL
}

# (Optional) diagnostics: find duplicates by country-year-source-measure
inspect_duplicates <- function(long_tbl) {
  long_tbl |>
    summarise(
      n = dplyr::n(),
      .by = c(.data$country_code, .data$year, .data$source, .data$measure)
    ) |>
    filter(n > 1L) |>
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

eu_clean <- eu_raw |> clean_names()

cli_alert_success("Eurostat: loaded {nrow(eu_clean)} rows, {ncol(eu_clean)} cols")
print(head(eu_clean, 5))

# ============================================================
# 2) FAO CSVs — RAW
# ============================================================

cli_h1("FAO AQUACULTURE — RAW QUANTITY")

fao_qty_raw <- read_csv_safe(
  fao_qty_path,
  decimal_mark = "."
) |> clean_names()

cli_alert_info("FAO Quantity columns:")
print(names(fao_qty_raw))
cli_alert_success("FAO Quantity rows: {nrow(fao_qty_raw)}")

cli_h1("FAO AQUACULTURE — RAW VALUE")

fao_val_raw <- read_csv_safe(
  fao_val_path,
  decimal_mark = "."
) |> clean_names()

cli_alert_info("FAO Value columns:")
print(names(fao_val_raw))
cli_alert_success("FAO Value rows: {nrow(fao_val_raw)}")

if (export_fao_diagnostics && file.exists(fao_species_path)) {
  cli_h2("FAO diagnostics: environments and species groups")

  fao_species_lu <- read_csv_safe(fao_species_path) |>
    clean_names() |>
    transmute(
      species_alpha_3_code = x3a_code,
      species_name_en = name_en,
      major_group = major_group
    )

  fao_qty_diag <- fao_qty_raw |>
    left_join(fao_species_lu, by = "species_alpha_3_code") |>
    mutate(value_num = suppressWarnings(as.numeric(value)))

  fao_val_diag <- fao_val_raw |>
    left_join(fao_species_lu, by = "species_alpha_3_code") |>
    mutate(value_num = suppressWarnings(as.numeric(value)) * 1000)

  fao_env_qty_counts <- fao_qty_diag |> count(environment_alpha_2_code, sort = TRUE)
  fao_env_val_counts <- fao_val_diag |> count(environment_alpha_2_code, sort = TRUE)
  fao_major_group_qty_counts <- fao_qty_diag |> count(major_group, sort = TRUE)
  fao_major_group_val_counts <- fao_val_diag |> count(major_group, sort = TRUE)

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
) |> clean_names()

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

fx_tbl <- load_fx_table("./data/FX/usd_eur_rates.csv")

if (!file.exists(fao_species_path)) {
  stop("FAO species lookup file missing: ", fao_species_path)
}

fao_species <- read_csv_safe(fao_species_path) |>
  clean_names() |>
  transmute(
    species_alpha_3_code = x3a_code,
    species_name_en = name_en,
    major_group = major_group
  )

empty_eurostat_species_labels <- tibble::tibble(
  species_code = character(),
  species_label = character()
)
eurostat_species_labels <- if (has_eurostat && nrow(aquaculture_raw$eurostat) > 0) {
  tryCatch(
    tibble::tibble(species_code = unique(as.character(aquaculture_raw$eurostat$species))) |>
      mutate(
        species_label = suppressWarnings(
          eurostat::label_eurostat(species_code, dic = "SPECIES")
        )
      ),
    error = function(error) {
      cli_alert_warning(
        "Eurostat species dictionary unavailable; low-anthropic Eurostat rows will be skipped. {conditionMessage(error)}"
      )
      empty_eurostat_species_labels
    }
  )
} else {
  empty_eurostat_species_labels
}

years_consistent <- 2018:2024
if (has_eurostat) {
  eu_iso2 <- unique(eurostat::eu_countries$code)
  efta_iso2 <- unique(eurostat::efta_countries$code)
} else {
  eu_iso2 <- c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR",
    "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
    "SE", "SI", "SK"
  )
  efta_iso2 <- c("IS", "LI", "NO", "CH")
}
europe_codes <- unique(c(eu_iso2, efta_iso2, "UK", "GB"))

pipeline_result <- build_pipeline_outputs(
  inputs = list(
    eurostat = aquaculture_raw$eurostat,
    eurostat_species_labels = eurostat_species_labels,
    fao_quantity = aquaculture_raw$fao_quantity,
    fao_value = aquaculture_raw$fao_value,
    fao_species = fao_species,
    fao_to_iso2 = fao_to_iso2,
    fx_rates = fx_tbl,
    eumofa = aquaculture_raw$eumofa
  ),
  config = list(
    summary_years = years_consistent,
    comparison_years = years_target,
    comparison_countries = c("HU", "NL", "RO", "BE", "HR"),
    europe_codes = europe_codes,
    tolerance = 0.30,
    include_crustacea_subset = FALSE
  )
)

cli_h2("Comparison ready (FX focus, country-year totals)")
print(head(pipeline_result$diagnostics$comparison_all, 20))
cli_h2("Freshwater-only comparison ready")
print(head(pipeline_result$diagnostics$comparison_freshwater, 20))

cli_h2("Checking for unit inconsistencies (kg vs tonnes)")
unit_check <- pipeline_result$diagnostics$unit_check
suspicious <- pipeline_result$diagnostics$suspicious
if (nrow(suspicious) > 0) {
  cli_alert_danger("{nrow(suspicious)} suspicious unit mismatch(es) detected — verify raw data before using results")
}
print(suspicious)

exports <- pipeline_result$exports
wb_low <- createWorkbook()
addWorksheet(wb_low, "by_mode")
writeData(wb_low, "by_mode", exports$low_anthropic_by_mode)
addWorksheet(wb_low, "side_by_side")
writeData(wb_low, "side_by_side", exports$low_anthropic_side_by_side)
saveWorkbook(
  wb_low,
  file.path(out_low_anthropic, "Europe_low_anthropic_summary_latest.xlsx"),
  overwrite = TRUE
)
cli_alert_success("Low-anthropic summary exported to output/low_anthropic/Europe_low_anthropic_summary_latest.xlsx (sheets: by_mode, side_by_side)")

write.xlsx(
  exports$general_summary,
  file.path(out_general, "Europe_summary_latest.xlsx"),
  sheetName = "data"
)
write.xlsx(
  exports$freshwater_summary,
  file.path(out_freshwater, "Europe_freshwater_summary_latest.xlsx"),
  sheetName = "data"
)
fw_results <- exports$freshwater_consistency
write_csv_safe(fw_results, file.path(out_freshwater, "freshwater_consistency_latest.csv"))
message("✔ Consistency CSV written to: ", file.path(out_freshwater, "freshwater_consistency_latest.csv"))
