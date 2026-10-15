project_root <- normalizePath(
  file.path(testthat::test_path(), "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(cli)
})

source(file.path(project_root, "R", "functions.R"))
observations_path <- file.path(project_root, "R", "observations.R")
if (file.exists(observations_path)) source(observations_path)
output_path <- file.path(project_root, "R", "output.R")
source(output_path)
source(file.path(project_root, "R", "segments.R"))
low_anthropic_path <- file.path(project_root, "R", "low_anthropic.R")
if (file.exists(low_anthropic_path)) source(low_anthropic_path)
pipeline_path <- file.path(project_root, "R", "pipeline.R")
if (file.exists(pipeline_path)) source(pipeline_path)

test_that("FAO and EUMOFA imports enforce source schemas and preserve identifiers", {
  fao_path <- tempfile(fileext = ".csv")
  on.exit(unlink(fao_path), add = TRUE)
  writeLines(c(
    '"COUNTRY.UN_CODE","SPECIES.ALPHA_3_CODE","AREA.CODE","ENVIRONMENT.ALPHA_2_CODE","MEASURE","PERIOD","VALUE","STATUS"',
    '"004","FCY","04","IN","Q_tlw",2020,12.5,"A"'
  ), fao_path)

  fao <- read_fao_export(fao_path)
  expect_identical(fao$country_un_code, "004")
  expect_type(fao$period, "integer")
  expect_type(fao$value, "double")

  eumofa_path <- tempfile(fileext = ".csv")
  on.exit(unlink(eumofa_path), add = TRUE)
  writeLines(c(
    "year;country;desc country;commodity group;main commercial species;ers code;english name;volume(kg);weight;value(eur)",
    "2023;BG;Bulgaria;Freshwater fish;Pike-perch;FPP;Pike-perch;25660;Live weight;120970"
  ), eumofa_path)

  eumofa <- read_eumofa_export(eumofa_path)
  expect_identical(eumofa$country, "BG")
  expect_type(eumofa$year, "integer")
  expect_type(eumofa$volume_kg, "double")
  expect_type(eumofa$value_eur, "double")
})

test_that("source imports reject missing schemas and malformed numeric fields", {
  missing_column_path <- tempfile(fileext = ".csv")
  on.exit(unlink(missing_column_path), add = TRUE)
  writeLines(c(
    "year;country;value(eur)",
    "2023;BG;120970"
  ), missing_column_path)
  expect_error(read_eumofa_export(missing_column_path), "missing required column")

  malformed_path <- tempfile(fileext = ".csv")
  on.exit(unlink(malformed_path), add = TRUE)
  writeLines(c(
    '"COUNTRY.UN_CODE","SPECIES.ALPHA_3_CODE","AREA.CODE","ENVIRONMENT.ALPHA_2_CODE","MEASURE","PERIOD","VALUE","STATUS"',
    '"004","FCY","04","IN","Q_tlw","unknown",12.5,"A"'
  ), malformed_path)
  expect_error(read_fao_export(malformed_path), "parsing problem|invalid integer")
})

test_that("pipeline outputs are computed from in-memory source tables", {
  inputs <- list(
    eurostat = tibble::tibble(
      unit = c("TLW", "EUR", "TLW", "EUR", "TLW", "EUR"),
      geo = "DE",
      aquaenv = "FRW",
      freq = "A",
      aquameth = "00",
      species = c("F00", "F00", "F54", "F54", "F91", "F91"),
      fishreg = "0",
      values = c(100, 50000, 5, 2500, 2, 1000),
      time = 2020L,
      flags = c(NA_character_, NA_character_, "p", NA_character_, NA_character_, NA_character_)
    ),
    eurostat_species_labels = tibble::tibble(
      species_code = c("F54", "F91"),
      species_label = c("Mussels", "Brown seaweeds")
    ),
    fao_quantity = tibble::tibble(
      country_un_code = rep("999", 5),
      period = rep(2020L, 5),
      measure = "Q_tlw",
      area_code = rep("04", 5),
      environment_alpha_2_code = rep("IN", 5),
      value = c(10, 4, 2, 40, 0),
      status = c("A", "B", "C", "D", "E"),
      species_alpha_3_code = c("OYS", "ALG", "SHR", "SAL", "FCY")
    ),
    fao_value = tibble::tibble(
      country_un_code = rep("999", 5),
      period = rep(2020L, 5),
      measure = "V_USD_1000",
      area_code = rep("04", 5),
      environment_alpha_2_code = rep("IN", 5),
      value = c(2, 1, 0.5, 3, 0),
      status = c("A", "B", "C", "D", "E"),
      species_alpha_3_code = c("OYS", "ALG", "SHR", "SAL", "FCY")
    ),
    fao_species = tibble::tibble(
      species_alpha_3_code = c("OYS", "ALG", "SHR", "SAL", "FCY"),
      species_name_en = c("Oyster", "Seaweed", "Shrimp", "Salmon", "Cyprinids NEI"),
      major_group = c("MOLLUSCA", "PLANTAE AQUATICAE", "CRUSTACEA", "FINFISH", "PISCES"),
      taxonomic_code = c("350112601401", "724121001001", "228103103401", "175010401101", "1400141XXXXX")
    ),
    fao_to_iso2 = tibble::tibble(country_un_code = "999", iso2 = "DE"),
    fx_rates = tibble::tibble(year = 2020L, eur_per_usd = 0.5),
    eumofa = tibble::tibble(
      country = "DE",
      year = 2020L,
      volume_kg = 3000,
      value_eur = 5000,
      commodity_group = "Freshwater molluscs",
      main_commercial_species = "Oyster",
      ers_code = "OYS001",
      english_name = "Oyster, live",
      weight = "live"
    )
  )
  inputs$eumofa <- bind_rows(
    inputs$eumofa,
    tibble::tibble(
      country = "DE",
      year = 2020L,
      volume_kg = NA_real_,
      value_eur = NA_real_,
      commodity_group = "Freshwater finfish",
      main_commercial_species = "Unspecified",
      ers_code = "UNK001",
      english_name = "Unspecified freshwater fish",
      weight = NA_character_
    ),
    tibble::tibble(
      country = "DE",
      year = 2020L,
      volume_kg = NA_real_,
      value_eur = NA_real_,
      commodity_group = NA_character_,
      main_commercial_species = "Unclassified",
      ers_code = "UNK002",
      english_name = "Unclassified aquaculture product",
      weight = NA_character_
    )
  )
  config <- list(
    summary_years = 2020L,
    comparison_years = 2020L,
    comparison_countries = "DE",
    europe_codes = "DE",
    tolerance = 0.30,
    include_crustacea_subset = FALSE
  )

  result <- build_pipeline_outputs(inputs, config)

  expect_true(is.data.frame(result$observations))
  expect_equal(nrow(result$observations), 22L)
  expect_true("observation_id" %in% names(result$observations))
  expect_identical(result$observations$observation_id, seq_len(nrow(result$observations)))
  expect_length(unique(names(result$observations)), ncol(result$observations))
  expect_true(all(c(
    "source", "country_code", "native_country_code", "year", "measure",
    "native_value", "native_unit", "comparison_value", "comparison_unit",
    "conversion_factor", "fx_rate", "record_scope",
    "eurostat_species_code", "eurostat_environment_code", "eurostat_fishreg",
    "eurostat_aquameth", "eurostat_flags", "fao_species_alpha_3_code",
    "fao_species_name_en", "fao_major_group", "fao_taxonomic_code",
    "fao_area_code", "fao_environment_alpha_2_code", "fao_status",
    "eumofa_ers_code", "eumofa_english_name", "eumofa_commodity_group",
    "eumofa_main_commercial_species", "eumofa_weight"
  ) %in% names(result$observations)))

  eurostat_total_tonnes <- result$observations %>%
    filter(source == "eurostat", eurostat_species_code == "F00", measure == "production_tonnes")
  expect_equal(eurostat_total_tonnes$record_scope, "published_total")
  expect_equal(eurostat_total_tonnes$native_value, 100)
  expect_equal(eurostat_total_tonnes$comparison_value, 100)

  eurostat_detail_tonnes <- result$observations %>%
    filter(source == "eurostat", eurostat_species_code == "F54", measure == "production_tonnes")
  expect_equal(eurostat_detail_tonnes$record_scope, "source_category")
  expect_equal(eurostat_detail_tonnes$eurostat_flags, "p")

  fao_value_oyster <- result$observations %>%
    filter(source == "fao_value", fao_species_alpha_3_code == "OYS")
  expect_equal(fao_value_oyster$native_value, 2)
  expect_equal(fao_value_oyster$native_unit, "THOUSAND_USD")
  expect_equal(fao_value_oyster$comparison_value, 1000)
  expect_equal(fao_value_oyster$comparison_unit, "EUR")
  expect_equal(fao_value_oyster$fx_rate, 0.5)

  fao_nei <- result$observations %>%
    filter(source == "fao_quantity", fao_species_alpha_3_code == "FCY")
  expect_equal(fao_nei$record_scope, "source_category")
  expect_equal(fao_nei$fao_species_name_en, "Cyprinids NEI")
  expect_equal(fao_nei$fao_taxonomic_code, "1400141XXXXX")

  eumofa_oyster_volume <- result$observations %>%
    filter(source == "eumofa", eumofa_ers_code == "OYS001", measure == "production_tonnes")
  expect_equal(eumofa_oyster_volume$native_value, 3000)
  expect_equal(eumofa_oyster_volume$native_unit, "KG")
  expect_equal(eumofa_oyster_volume$comparison_value, 3)
  expect_equal(eumofa_oyster_volume$eumofa_english_name, "Oyster, live")
  eumofa_missing <- result$observations %>%
    filter(source == "eumofa", eumofa_ers_code == "UNK001")
  expect_equal(nrow(eumofa_missing), 2L)
  expect_true(all(is.na(eumofa_missing$native_value)))

  expect_named(result, c("observations", "memberships", "exports", "diagnostics"))
  expect_true(all(c(
    "observation_id", "segment", "profile", "category", "matched_by"
  ) %in% names(result$memberships)))
  expect_true(all(result$memberships$observation_id %in% result$observations$observation_id))
  observations_before_membership <- result$observations
  recomputed_memberships <- build_segment_memberships(result$observations)
  expect_identical(result$observations, observations_before_membership)
  expect_equal(recomputed_memberships, result$memberships)

  freshwater_memberships <- result$memberships %>%
    filter(segment == "freshwater")
  eurostat_freshwater_ids <- result$observations %>%
    filter(source == "eurostat", eurostat_environment_code == "FRW") %>%
    pull(observation_id)
  fao_freshwater_ids <- result$observations %>%
    filter(source %in% c("fao_quantity", "fao_value"), fao_environment_alpha_2_code == "IN") %>%
    pull(observation_id)
  eumofa_freshwater_ids <- result$observations %>%
    filter(source == "eumofa", eumofa_commodity_group == "Freshwater finfish") %>%
    pull(observation_id)
  expect_true(all(eurostat_freshwater_ids %in% freshwater_memberships$observation_id))
  expect_true(all(fao_freshwater_ids %in% freshwater_memberships$observation_id))
  expect_true(all(eumofa_freshwater_ids %in% freshwater_memberships$observation_id))
  expect_true(all(freshwater_memberships$matched_by[match(
    c(eurostat_freshwater_ids, fao_freshwater_ids, eumofa_freshwater_ids),
    freshwater_memberships$observation_id
  )] %in% c(
    "eurostat_environment_code",
    "fao_environment_alpha_2_code",
    "eumofa_commodity_group"
  )))

  eumo_oyster_id <- eumofa_oyster_volume$observation_id
  oyster_memberships <- result$memberships %>%
    filter(observation_id == eumo_oyster_id)
  expect_true(any(oyster_memberships$segment == "freshwater"))
  expect_true(any(
    oyster_memberships$segment == "low_anthropic" &
      oyster_memberships$profile == "strict" &
      oyster_memberships$category == "mussels_bivalves"
  ))
  expect_true(any(
    oyster_memberships$segment == "low_anthropic" &
      oyster_memberships$profile == "broad" &
      oyster_memberships$category == "mollusca_all"
  ))

  shrimp_id <- result$observations %>%
    filter(source == "fao_quantity", fao_species_alpha_3_code == "SHR") %>%
    pull(observation_id)
  default_crustacea_membership <- result$memberships %>%
    filter(
      observation_id == shrimp_id,
      segment == "low_anthropic",
      category == "crustacea_subset"
    )
  expect_equal(nrow(default_crustacea_membership), 0L)
  crustacea_memberships <- build_segment_memberships(
    result$observations,
    include_crustacea_subset = TRUE
  )
  expect_true(any(
    crustacea_memberships$observation_id == shrimp_id &
      crustacea_memberships$segment == "low_anthropic" &
      crustacea_memberships$profile == "broad" &
      crustacea_memberships$category == "crustacea_subset"
  ))

  unclassified_id <- result$observations %>%
    filter(source == "eumofa", eumofa_ers_code == "UNK002", measure == "production_tonnes") %>%
    pull(observation_id)
  expect_true(all(is.na(result$observations$native_value[match(
    unclassified_id,
    result$observations$observation_id
  )])))
  expect_false(any(result$memberships$observation_id %in% unclassified_id))

  expect_named(
    result$exports,
    c(
      "general_summary",
      "freshwater_summary",
      "low_anthropic_by_mode",
      "low_anthropic_side_by_side",
      "freshwater_consistency"
    )
  )
  expect_equal(result$exports$general_summary$eurostat_production, 100)
  expect_equal(result$exports$general_summary$fao_production, 56)
  expect_equal(result$exports$general_summary$eumofa_production, 3)
  expect_equal(result$exports$general_summary$fao_value, 3250)
  expect_equal(
    result$diagnostics$comparison_all$fao_value_production_value_usd,
    6500
  )

  low_anthropic_types <- result$exports$low_anthropic_by_mode$production_type
  expect_setequal(
    low_anthropic_types,
    c("mussels_bivalves", "seaweed_algae", "mollusca_all", "aquatic_plants_all")
  )
  low_anthropic <- result$exports$low_anthropic_by_mode
  strict_mussels <- low_anthropic %>%
    filter(mode == "strict", production_type == "mussels_bivalves")
  broad_mollusca <- low_anthropic %>%
    filter(mode == "broad", production_type == "mollusca_all")
  strict_seaweed <- low_anthropic %>%
    filter(mode == "strict", production_type == "seaweed_algae")
  broad_plants <- low_anthropic %>%
    filter(mode == "broad", production_type == "aquatic_plants_all")
  expect_equal(strict_mussels$eurostat_species_production_tonnes, 5)
  expect_equal(strict_mussels$fao_quantity_production_tonnes, 10)
  expect_equal(strict_mussels$fao_eur_production_value_eur, 1000)
  expect_equal(strict_mussels$eumofa_production_tonnes, 3)
  expect_equal(broad_mollusca$eumofa_production_tonnes, 3)
  expect_equal(strict_seaweed$eurostat_species_production_tonnes, 2)
  expect_equal(strict_seaweed$fao_quantity_production_tonnes, 4)
  expect_equal(broad_mollusca$fao_quantity_production_tonnes, 10)
  expect_equal(broad_plants$fao_quantity_production_tonnes, 4)

  crustacea_inputs <- inputs
  crustacea_inputs$eumofa <- bind_rows(
    inputs$eumofa,
    tibble::tibble(
      country = "DE",
      year = 2020L,
      volume_kg = 5000,
      value_eur = 7000,
      commodity_group = "Freshwater crustaceans",
      main_commercial_species = "Shrimp"
    )
  )
  crustacea_config <- config
  crustacea_config$include_crustacea_subset <- TRUE
  crustacea_result <- build_pipeline_outputs(crustacea_inputs, crustacea_config)
  crustacea_rows <- crustacea_result$exports$low_anthropic_by_mode %>%
    filter(mode == "broad", production_type == "crustacea_subset")
  expect_equal(crustacea_rows$fao_quantity_production_tonnes, 2)
  expect_equal(crustacea_rows$eumofa_production_tonnes, 5)

  labels_unavailable_inputs <- inputs
  labels_unavailable_inputs$eurostat_species_labels <- tibble::tibble(
    species_code = character(),
    species_label = character()
  )
  labels_unavailable_result <- build_pipeline_outputs(
    labels_unavailable_inputs,
    config
  )
  expect_equal(labels_unavailable_result$exports$general_summary$eurostat_production, 100)
  expect_equal(labels_unavailable_result$exports$freshwater_summary$eurostat_production, 100)
  expect_true("unit_check" %in% names(result$diagnostics))
  expect_true("comparison_freshwater" %in% names(result$diagnostics))
})

test_that("pipeline outputs remain available when Eurostat is empty", {
  inputs <- list(
    eurostat = tibble::tibble(),
    eurostat_species_labels = tibble::tibble(
      species_code = character(),
      species_label = character()
    ),
    fao_quantity = tibble::tibble(
      country_un_code = "999",
      period = 2020L,
      environment_alpha_2_code = "IN",
      value = 10,
      species_alpha_3_code = "SAL"
    ),
    fao_value = tibble::tibble(
      country_un_code = "999",
      period = 2020L,
      environment_alpha_2_code = "IN",
      value = 2,
      species_alpha_3_code = "SAL"
    ),
    fao_species = tibble::tibble(
      species_alpha_3_code = "SAL",
      species_name_en = "Salmon",
      major_group = "FINFISH"
    ),
    fao_to_iso2 = tibble::tibble(country_un_code = "999", iso2 = "DE"),
    fx_rates = tibble::tibble(year = 2020L, eur_per_usd = 0.5),
    eumofa = tibble::tibble(
      country = "DE",
      year = 2020L,
      volume_kg = 3000,
      value_eur = 5000,
      commodity_group = "Freshwater finfish",
      main_commercial_species = "Salmon"
    )
  )
  config <- list(
    summary_years = 2020L,
    comparison_years = 2020L,
    comparison_countries = "DE",
    europe_codes = "DE",
    tolerance = 0.30,
    include_crustacea_subset = FALSE
  )

  result <- build_pipeline_outputs(inputs, config)

  expect_equal(result$exports$general_summary$fao_production, 10)
  expect_equal(result$exports$general_summary$eumofa_production, 3)
  expect_true(is.na(result$exports$general_summary$eurostat_production))
  expect_equal(result$exports$freshwater_summary$fao_production, 10)
})

test_that("partial snapshots do not replace the latest complete snapshot", {
  output_dir <- tempfile("normalized-observations-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  complete_bundle <- list(
    manifest = list(
      schema_version = "1.0.0",
      coverage_status = "complete",
      generated_at_utc = "2026-10-14T00:00:00Z"
    ),
    observations = tibble::tibble(
      source = "eurostat",
      country_code = "DE",
      year = 2020L,
      measure = "production_tonnes",
      native_value = 100,
      native_unit = "TLW"
    ),
    memberships = tibble::tibble(
      observation_id = 1L,
      segment = "freshwater",
      profile = NA_character_,
      category = NA_character_,
      matched_by = "eurostat_environment_code"
    )
  )
  complete_path <- write_normalized_snapshot(complete_bundle, output_dir)

  expect_equal(basename(complete_path), "aquaculture_observations_latest.rds")
  expect_equal(readRDS(complete_path), complete_bundle)
  complete_checksum <- unname(tools::md5sum(complete_path))

  partial_bundle <- complete_bundle
  partial_bundle$manifest$coverage_status <- "partial"
  partial_bundle$observations$source <- "fao_quantity"
  partial_path <- write_normalized_snapshot(partial_bundle, output_dir)

  expect_equal(basename(partial_path), "aquaculture_observations_partial_latest.rds")
  expect_equal(readRDS(partial_path), partial_bundle)
  expect_identical(unname(tools::md5sum(complete_path)), complete_checksum)
})

test_that("observation manifest records provenance and partial source coverage", {
  source_dir <- tempfile("observation-provenance-")
  dir.create(source_dir)
  on.exit(unlink(source_dir, recursive = TRUE), add = TRUE)
  source_file <- file.path(source_dir, "fao-quantity.csv")
  fx_file <- file.path(source_dir, "fx.csv")
  writeLines("source data", source_file)
  writeLines("fx data", fx_file)

  manifest <- build_observation_manifest(
    source_status = c(
      eurostat = TRUE,
      fao_quantity = TRUE,
      fao_value = TRUE,
      eumofa = TRUE
    ),
    eurostat = list(
      dataset_id = "fish_aq2a",
      retrieval_status = "available",
      row_count = 6L,
      retrieved_at_utc = "2026-10-14T00:00:00Z"
    ),
    input_files = list(fao_quantity = source_file),
    segment_rules_version = SEGMENT_RULES_VERSION,
    fx = list(
      source = "csv",
      path = fx_file,
      rates = tibble::tibble(year = 2020L, eur_per_usd = 0.5)
    ),
    generated_at_utc = "2026-10-14T00:00:00Z"
  )

  expect_equal(manifest$schema_version, "1.0.0")
  expect_equal(manifest$segment_rules_version, SEGMENT_RULES_VERSION)
  expect_equal(manifest$coverage_status, "complete")
  expect_equal(manifest$eurostat$dataset_id, "fish_aq2a")
  expect_equal(manifest$input_files$fao_quantity$md5, unname(tools::md5sum(source_file)))
  expect_equal(manifest$fx$source, "csv")
  expect_equal(manifest$fx$rates$eur_per_usd, 0.5)

  partial_manifest <- build_observation_manifest(
    source_status = c(
      eurostat = FALSE,
      fao_quantity = TRUE,
      fao_value = TRUE,
      eumofa = TRUE
    ),
    eurostat = list(
      dataset_id = "fish_aq2a",
      retrieval_status = "unavailable",
      row_count = 0L,
      retrieved_at_utc = NA_character_
    ),
    input_files = list(fao_quantity = source_file),
    segment_rules_version = SEGMENT_RULES_VERSION,
    fx = list(source = "fallback", path = NULL, rates = tibble::tibble()),
    generated_at_utc = "2026-10-14T00:01:00Z"
  )
  expect_equal(partial_manifest$coverage_status, "partial")
  expect_equal(partial_manifest$fx$source, "fallback")
})

test_that("partial runs do not overwrite latest exports or diagnostics", {
  output_root <- tempfile("partial-output-guard-")
  output_dirs <- list(
    root = output_root,
    general = file.path(output_root, "general"),
    freshwater = file.path(output_root, "freshwater"),
    low_anthropic = file.path(output_root, "low_anthropic"),
    diagnostics = file.path(output_root, "diagnostics")
  )
  lapply(output_dirs, dir.create, recursive = TRUE)

  existing_paths <- file.path(
    c(output_dirs$general, output_dirs$freshwater, output_dirs$low_anthropic,
      output_dirs$freshwater, output_dirs$diagnostics, output_dirs$diagnostics,
      output_dirs$diagnostics, output_dirs$diagnostics),
    c(
      "Europe_summary_latest.xlsx",
      "Europe_freshwater_summary_latest.xlsx",
      "Europe_low_anthropic_summary_latest.xlsx",
      "freshwater_consistency_latest.csv",
      "fao_env_qty_counts.csv",
      "fao_env_val_counts.csv",
      "fao_major_group_qty_counts.csv",
      "fao_major_group_val_counts.csv"
    )
  )
  for (path in existing_paths) writeLines("previous complete output", path)
  before_hashes <- unname(tools::md5sum(existing_paths))

  published <- write_pipeline_exports(
    exports = list(),
    output_dirs = output_dirs,
    coverage_status = "partial"
  )

  expect_false(published)
  expect_identical(unname(tools::md5sum(existing_paths)), before_hashes)
})

test_that("complete runs write summaries and diagnostics", {
  output_root <- tempfile("complete-output-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  output_dirs <- list(
    root = output_root,
    general = file.path(output_root, "general"),
    freshwater = file.path(output_root, "freshwater"),
    low_anthropic = file.path(output_root, "low_anthropic"),
    diagnostics = file.path(output_root, "diagnostics")
  )
  exports <- list(
    general_summary = tibble::tibble(country_code = "DE", year = 2020L),
    freshwater_summary = tibble::tibble(country_code = "DE", year = 2020L),
    low_anthropic_by_mode = tibble::tibble(mode = "strict", country_code = "DE"),
    low_anthropic_side_by_side = tibble::tibble(country_code = "DE"),
    freshwater_consistency = tibble::tibble(country_code = "DE", year = 2020L)
  )
  fao_diagnostics <- list(
    fao_env_qty_counts = tibble::tibble(environment_alpha_2_code = "IN", n = 1L),
    fao_env_val_counts = tibble::tibble(environment_alpha_2_code = "IN", n = 1L),
    fao_major_group_qty_counts = tibble::tibble(major_group = "MOLLUSCA", n = 1L),
    fao_major_group_val_counts = tibble::tibble(major_group = "MOLLUSCA", n = 1L)
  )

  published <- write_pipeline_exports(
    exports,
    output_dirs,
    coverage_status = "complete",
    fao_diagnostics = fao_diagnostics
  )

  expect_true(published)
  expect_length(list.files(output_root, recursive = TRUE), 8L)
  expect_setequal(
    openxlsx::getSheetNames(file.path(output_dirs$low_anthropic, "Europe_low_anthropic_summary_latest.xlsx")),
    c("by_mode", "side_by_side")
  )
  expect_equal(
    nrow(openxlsx::read.xlsx(file.path(output_dirs$general, "Europe_summary_latest.xlsx"), sheet = "data")),
    1L
  )
  expect_equal(
    nrow(read.csv(file.path(output_dirs$diagnostics, "fao_env_qty_counts.csv"))),
    1L
  )
})