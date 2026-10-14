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
low_anthropic_path <- file.path(project_root, "R", "low_anthropic.R")
if (file.exists(low_anthropic_path)) source(low_anthropic_path)
pipeline_path <- file.path(project_root, "R", "pipeline.R")
if (file.exists(pipeline_path)) source(pipeline_path)

test_that("pipeline outputs are computed from in-memory source tables", {
  inputs <- list(
    eurostat = tibble::tibble(
      unit = c("TLW", "EUR", "TLW", "EUR", "TLW", "EUR"),
      geo = "DE",
      aquaenv = "FRW",
      freq = "A",
      species = c("F00", "F00", "F54", "F54", "F91", "F91"),
      fishreg = "0",
      values = c(100, 50000, 5, 2500, 2, 1000),
      time = 2020L
    ),
    eurostat_species_labels = tibble::tibble(
      species_code = c("F54", "F91"),
      species_label = c("Mussels", "Brown seaweeds")
    ),
    fao_quantity = tibble::tibble(
      country_un_code = rep("999", 4),
      period = rep(2020L, 4),
      environment_alpha_2_code = rep("IN", 4),
      value = c(10, 4, 2, 40),
      species_alpha_3_code = c("OYS", "ALG", "SHR", "SAL")
    ),
    fao_value = tibble::tibble(
      country_un_code = rep("999", 4),
      period = rep(2020L, 4),
      environment_alpha_2_code = rep("IN", 4),
      value = c(2, 1, 0.5, 3),
      species_alpha_3_code = c("OYS", "ALG", "SHR", "SAL")
    ),
    fao_species = tibble::tibble(
      species_alpha_3_code = c("OYS", "ALG", "SHR", "SAL"),
      species_name_en = c("Oyster", "Seaweed", "Shrimp", "Salmon"),
      major_group = c("MOLLUSCA", "PLANTAE AQUATICAE", "CRUSTACEA", "FINFISH")
    ),
    fao_to_iso2 = tibble::tibble(country_un_code = "999", iso2 = "DE"),
    fx_rates = tibble::tibble(year = 2020L, eur_per_usd = 0.5),
    eumofa = tibble::tibble(
      country = "DE",
      year = 2020L,
      volume_kg = 3000,
      value_eur = 5000,
      commodity_group = "Freshwater molluscs",
      main_commercial_species = "Oyster"
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

  expect_named(result, c("exports", "diagnostics"))
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