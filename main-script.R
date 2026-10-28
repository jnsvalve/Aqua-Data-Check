##############################################################
# UNIVERSAL AQUACULTURE DATA HARVEST SCRIPT
# Sources: Eurostat API, FAO CSVs, EUMOFA CSV
# Author: Joonas Valve
# Date: Feb 13, 2026
##############################################################

suppressPackageStartupMessages({
  required_pkgs <- c(
    "dplyr", "readr", "tidyr", "janitor", "stringr", "cli", "openxlsx", "countrycode", "ggplot2", "patchwork"
  )
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
    utils::install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
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
source("R/eudcf.R")
source("R/observations.R")
source("R/output.R")
source("R/segments.R")
source("R/production_views.R")
source("R/pipeline.R")

# ============================================================
# 0) USER PARAMETERS
# ============================================================

years_target <- seq.int(to = as.integer(format(Sys.Date(), "%Y")) - 1L, length.out = 20L)
low_anthropic_start_year <- 2010L
low_anthropic_profile <- "strict"
low_anthropic_top_n <- 10L
low_anthropic_continent <- NULL
fao_qty_path <- "./data/FAO/Aquaculture_Quantity.csv"
fao_val_path <- "./data/FAO/Aquaculture_Value.csv"
eumofa_path  <- "./data/EUMOFA/Yearly_Aquaculture.csv"
eudcf_path <- "./data/EUDCF/STECF_24-14_EU Aquaculture economic sector data tables.xlsx"
fao_species_path <- "./data/FAO/CL_FI_SPECIES_GROUPS.csv"
fao_iso2_overrides_path <- "./data/FAO/fao_iso2_overrides.csv"
export_fao_diagnostics <- TRUE
fao_diagnostics <- list()

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
    janitor::clean_names() |>
    dplyr::transmute(country_un_code = as.character(country_un_code), iso2 = toupper(iso2))
} else {
  NULL
}

# (Optional) diagnostics: find duplicates by country-year-source-measure
inspect_duplicates <- function(long_tbl) {
  long_tbl |>
    dplyr::summarise(
      n = dplyr::n(),
      .by = c(.data$country_code, .data$year, .data$source, .data$measure)
    ) |>
    dplyr::filter(n > 1L) |>
    dplyr::arrange(desc(n))
}

# ============================================================
# 1) EUROSTAT — RAW
# ============================================================

cli::cli_h1("EUROSTAT AQUACULTURE — RAW DATA")

eu_raw <- if (has_eurostat) {
  tryCatch(
    {
      eurostat::get_eurostat(
        id = "fish_aq2a",
        time_format = "num",
        cache = TRUE,
        keepFlags = TRUE
      )
    },
    error = function(e1) {
      cli::cli_alert_warning("Eurostat fetch failed on first attempt: {conditionMessage(e1)}")
      cli::cli_alert_info("Retrying with cache refresh enabled...")
      tryCatch(
        {
          eurostat::get_eurostat(
            id = "fish_aq2a",
            time_format = "num",
            cache = TRUE,
            update_cache = TRUE,
            keepFlags = TRUE
          )
        },
        error = function(e2) {
          cli::cli_alert_warning(
            "Eurostat download failed for dataset 'fish_aq2a'. Continuing without Eurostat data. Last error: {conditionMessage(e2)}"
          )
          tibble::tibble()
        }
      )
    }
  )
} else {
  tibble::tibble()
}

eu_clean <- eu_raw |> janitor::clean_names()
eu_retrieved_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

cli::cli_alert_success("Eurostat: loaded {nrow(eu_clean)} rows, {ncol(eu_clean)} cols")
print(utils::head(eu_clean, 5))

# ============================================================
# 2) FAO CSVs — RAW
# ============================================================

cli::cli_h1("FAO AQUACULTURE — RAW QUANTITY")

fao_qty_raw <- read_fao_export(fao_qty_path)

cli::cli_alert_info("FAO Quantity columns:")
print(names(fao_qty_raw))
cli::cli_alert_success("FAO Quantity rows: {nrow(fao_qty_raw)}")

cli::cli_h1("FAO AQUACULTURE — RAW VALUE")

fao_val_raw <- read_fao_export(fao_val_path)

cli::cli_alert_info("FAO Value columns:")
print(names(fao_val_raw))
cli::cli_alert_success("FAO Value rows: {nrow(fao_val_raw)}")

if (export_fao_diagnostics && file.exists(fao_species_path)) {
  cli::cli_h2("FAO diagnostics: environments and species groups")

  fao_species_lu <- read_csv_safe(fao_species_path) |>
    janitor::clean_names() |>
    dplyr::transmute(
      species_alpha_3_code = x3a_code,
      species_name_en = name_en,
      major_group = major_group
    )

  fao_qty_diag <- fao_qty_raw |>
    dplyr::left_join(fao_species_lu, by = "species_alpha_3_code") |>
    dplyr::mutate(value_num = suppressWarnings(as.numeric(value)))

  fao_val_diag <- fao_val_raw |>
    dplyr::left_join(fao_species_lu, by = "species_alpha_3_code") |>
    dplyr::mutate(value_num = suppressWarnings(as.numeric(value)) * 1000)

  fao_env_qty_counts <- fao_qty_diag |> dplyr::count(environment_alpha_2_code, sort = TRUE)
  fao_env_val_counts <- fao_val_diag |> dplyr::count(environment_alpha_2_code, sort = TRUE)
  fao_major_group_qty_counts <- fao_qty_diag |> dplyr::count(major_group, sort = TRUE)
  fao_major_group_val_counts <- fao_val_diag |> dplyr::count(major_group, sort = TRUE)

  fao_diagnostics <- list(
    fao_env_qty_counts = fao_env_qty_counts,
    fao_env_val_counts = fao_env_val_counts,
    fao_major_group_qty_counts = fao_major_group_qty_counts,
    fao_major_group_val_counts = fao_major_group_val_counts
  )
}

# ============================================================
# 3) EUMOFA CSV — RAW
# ============================================================

cli::cli_h1("EUMOFA AQUACULTURE — RAW DATA")

eumofa_raw <- read_eumofa_export(eumofa_path)

cli::cli_alert_info("EUMOFA columns:")
print(names(eumofa_raw))
cli::cli_alert_success("EUMOFA rows: {nrow(eumofa_raw)}")

# ============================================================
# STORE RAW TABLES
# ============================================================

aquaculture_raw <- list(
  eurostat = eu_clean,
  fao_quantity = fao_qty_raw,
  fao_value = fao_val_raw,
  eumofa = eumofa_raw
)

cli::cli_h2("RAW TABLES READY — NO FILTERS APPLIED")
print(lapply(aquaculture_raw, head))

fx_path <- "./data/FX/usd_eur_rates.csv"
fx_tbl <- load_fx_table(fx_path)
# Optional EUDCF import; kept separate from production observations.
eudcf_data <- if (file.exists(eudcf_path)) {
  read_eudcf_workbook(eudcf_path)
} else {
  cli::cli_alert_warning("EUDCF workbook not found; continuing without EUDCF data.")
  NULL
}
eudcf_table_names <- c("economic", "social", "sales", "historical_sales")
eudcf_sheet_row_counts <- if (is.null(eudcf_data)) {
  stats::setNames(integer(length(eudcf_table_names)), eudcf_table_names)
} else {
  vapply(eudcf_data[eudcf_table_names], function(table) {
    if (is.null(table)) 0L else nrow(table)
  }, integer(1))
}

if (!file.exists(fao_species_path)) {
  stop("FAO species lookup file missing: ", fao_species_path)
}

fao_species <- read_csv_safe(fao_species_path) |>
  janitor::clean_names() |>
  dplyr::transmute(
    species_alpha_3_code = x3a_code,
    species_name_en = name_en,
    major_group = major_group,
    taxonomic_code = taxonomic_code,
    isscaap_group_en = isscaap_group_en
  )

empty_eurostat_species_labels <- tibble::tibble(
  species_code = character(),
  species_label = character()
)
eurostat_species_labels <- if (has_eurostat && nrow(aquaculture_raw$eurostat) > 0) {
  tryCatch(
    tibble::tibble(species_code = unique(as.character(aquaculture_raw$eurostat$species))) |>
      dplyr::mutate(
        species_label = suppressWarnings(
          eurostat::label_eurostat(species_code, dic = "SPECIES")
        )
      ),
    error = function(error) {
      cli::cli_alert_warning(
        "Eurostat species dictionary unavailable; low-anthropic Eurostat rows will be skipped. {conditionMessage(error)}"
      )
      empty_eurostat_species_labels
    }
  )
} else {
  empty_eurostat_species_labels
}
eurostat_species_label_status <- if (nrow(eurostat_species_labels) == 0) {
  "unavailable"
} else if (all(!is.na(eurostat_species_labels$species_label))) {
  "available"
} else {
  "partial"
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
    comparison_countries = NULL,
    europe_codes = europe_codes,
    tolerance = 0.30,
    include_crustacea_subset = FALSE
  )
)

fao_low_anthropic_countries <- build_fao_low_anthropic_countries(
  pipeline_result$observations,
  pipeline_result$memberships,
  years = seq.int(from = low_anthropic_start_year, to = max(years_target)),
  profile = low_anthropic_profile,
  top_n = low_anthropic_top_n,
  continent = low_anthropic_continent,
  species_classification = fao_species
)
fao_country_plot_paths <- write_fao_low_anthropic_countries(
  fao_low_anthropic_countries, out_low_anthropic
)
cli::cli_alert_success("FAO low-anthropic country charts saved: {length(fao_country_plot_paths)}")

source_status <- c(
  eurostat = has_eurostat && nrow(aquaculture_raw$eurostat) > 0,
  fao_quantity = nrow(aquaculture_raw$fao_quantity) > 0,
  fao_value = nrow(aquaculture_raw$fao_value) > 0,
  eumofa = nrow(aquaculture_raw$eumofa) > 0
)

input_files <- list(
  fao_quantity = fao_qty_path,
  fao_value = fao_val_path,
  eumofa = eumofa_path,
  eudcf = eudcf_path,
  fao_species = fao_species_path
)
if (file.exists(fao_iso2_overrides_path)) {
  input_files$fao_iso2_overrides <- fao_iso2_overrides_path
}

manifest <- build_observation_manifest(
  source_status = source_status,
  eurostat = list(
    dataset_id = "fish_aq2a",
    retrieval_status = if (source_status[["eurostat"]]) "available" else "unavailable",
    species_label_status = eurostat_species_label_status,
    row_count = nrow(aquaculture_raw$eurostat),
    retrieved_at_utc = eu_retrieved_at_utc
  ),
  input_files = input_files,
  segment_rules_version = SEGMENT_RULES_VERSION,
  fx = list(
    source = if (file.exists(fx_path)) "csv" else "fallback",
    path = if (file.exists(fx_path)) fx_path else NULL,
    rates = fx_tbl
  ),
  optional_sources = list(
    eudcf = list(
      status = if (is.null(eudcf_data)) "unavailable" else "available",
      sheet_row_counts = eudcf_sheet_row_counts
    )
  )
)

normalized_bundle <- list(
  manifest = manifest,
  observations = pipeline_result$observations,
  memberships = pipeline_result$memberships,
  eudcf = eudcf_data
)
normalized_snapshot_path <- write_normalized_snapshot(
  normalized_bundle,
  file.path(output_root, "normalized")
)
cli::cli_alert_success("Normalized {manifest$coverage_status} snapshot saved to {normalized_snapshot_path}")

if (manifest$coverage_status == "complete") {
  cli::cli_h2("Comparison ready (FX focus, country-year totals)")
  print(utils::head(pipeline_result$diagnostics$comparison_all, 20))
  cli::cli_h2("Freshwater-only comparison ready")
  print(utils::head(pipeline_result$diagnostics$comparison_freshwater, 20))

  cli::cli_h2("Checking for unit inconsistencies (kg vs tonnes)")
  unit_check <- pipeline_result$diagnostics$unit_check
  suspicious <- pipeline_result$diagnostics$suspicious
  if (nrow(suspicious) > 0) {
    cli::cli_alert_danger("{nrow(suspicious)} suspicious unit mismatch(es) detected — verify raw data before using results")
  }
  print(suspicious)

  output_written <- write_pipeline_exports(
    exports = pipeline_result$exports,
    output_dirs = list(
      general = out_general,
      freshwater = out_freshwater,
      low_anthropic = out_low_anthropic,
      diagnostics = out_diagnostics
    ),
    coverage_status = manifest$coverage_status,
    fao_diagnostics = fao_diagnostics
  )
  if (output_written) {
    cli::cli_alert_success("Summary exports, comparison tables, plots, and diagnostics updated from complete source coverage")
  }
} else {
  cli::cli_alert_warning(
    "Source coverage is partial; latest summaries, diagnostics, and workbooks were not updated."
  )
}
