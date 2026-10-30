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
source(file.path(project_root, "R", "production_views.R"))
source(file.path(project_root, "R", "eudcf.R"))
source(file.path(project_root, "R", "eudcf_dashboard.R"))
pipeline_path <- file.path(project_root, "R", "pipeline.R")
if (file.exists(pipeline_path)) source(pipeline_path)

testthat::test_that("DCF economics filters low-anthropic species and chooses a reported segment", {
  economic <- tibble::tibble(
    country_code = c("DEU", "DEU", "DEU", "DEU", "FRA"),
    year = c(2021L, 2022L, 2021L, 2020L, 2022L),
    species_group = c("Mussel", "Mussel", "All", "Mussel", "Oyster"),
    segment_code = c("seg.10_10", "seg.10_10", "tot", "seg.old", "seg.11_11"),
    segment_name = c("Mussel Longline", "Mussel Longline", "National Total", "Old segment", "Oyster On-bottom"),
    variable_code = "GST", variable_name = "Gross sales", value = c(100, 120, 500, 80, 60),
    unit = "EURO"
  )
  groups <- eudcf_dashboard_low_anthropic_groups(economic)
  testthat::expect_setequal(groups, c("Mussel", "Oyster"))
  segment <- eudcf_dashboard_default_economic_segment(economic, "Mussel", 2021:2022)
  testthat::expect_identical(segment$segment_code, "seg.10_10")
  testthat::expect_identical(
    eudcf_dashboard_economic_segments(economic, "Mussel", "DEU", c(2021L, 2022L))$segment_code,
    "seg.10_10"
  )
  testthat::expect_equal(
    nrow(eudcf_dashboard_economic_segments(economic, "Mussel", "DEU", c(2023L, 2024L))),
    0L
  )
  selected <- eudcf_dashboard_economic_data(
    economic, "DEU", "seg.10_10", "GST", species_group = "Mussel", year_range = 2021:2022
  )
  testthat::expect_equal(nrow(selected), 2L)
  testthat::expect_true(all(selected$species_group == "Mussel"))
  testthat::expect_false(any(selected$segment_code == "tot"))
})

testthat::test_that("DCF plot series preserve annual gaps and reported zeroes", {
  reported <- tibble::tibble(
    country_code = c("DEU", "DEU", "FRA"),
    country_name = c("Germany", "Germany", "France"),
    year = c(2020L, 2022L, 2021L), value = c(0, 2000, 1000), unit = "EURO"
  ) |>
    eudcf_dashboard_plot_values()
  series <- eudcf_dashboard_plot_series(reported, c(2020L, 2022L))
  germany <- dplyr::filter(series, country_code == "DEU")
  testthat::expect_equal(nrow(series), 6L)
  testthat::expect_equal(germany$plot_value, c(0, NA, 2))
  testthat::expect_true(all(germany$country_name == "Germany"))
  testthat::expect_true(all(germany$plot_unit == "thousand EUR"))
  testthat::expect_equal(nrow(reported), 3L)
  testthat::expect_equal(eudcf_dashboard_plot_series(reported[0, ], 2020:2022), reported[0, ])
})

testthat::test_that("Dashboard chart defaults use sparse integer years and a shared theme", {
  testthat::expect_equal(aquaculture_year_breaks(c(2021, 2022)), 2021:2022)
  testthat::expect_equal(aquaculture_year_breaks(c(2022, 2022)), 2022)
  testthat::expect_lte(length(aquaculture_year_breaks(c(2000, 2025))), 8L)
  testthat::expect_equal(aquaculture_plot_theme()$legend.position, "bottom")
  rows <- tibble::tibble(year = 2020:2022, plot_value = c(0, NA, 2))
  plot <- ggplot2::ggplot(rows, ggplot2::aes(year, plot_value)) +
    ggplot2::geom_line() + aquaculture_plot_theme()
  testthat::expect_true(is.na(ggplot2::ggplot_build(plot)$data[[1]]$y[2]))
})

testthat::test_that("DCF graph values scale euros only and segments follow the country", {
  plot_values <- eudcf_dashboard_plot_values(tibble::tibble(
    value = c(1250000, 275), unit = c("EURO", "KG")
  ))
  testthat::expect_equal(plot_values$plot_value, c(1250, 275))
  testthat::expect_identical(plot_values$plot_unit, c("thousand EUR", "KG"))

  economic <- tibble::tibble(
    country_code = c("DEU", "DEU", "FRA"),
    species_group = "Mussel", year = 2022L,
    segment_code = c("seg.10_10", "seg.10_11", "seg.10_9"),
    segment_name = c("Longline", "On-bottom", "Rafts"),
    variable_code = "GST", variable_name = "Gross sales", value = 10,
    unit = "EURO"
  )
  testthat::expect_identical(
    eudcf_dashboard_economic_segments(economic, "Mussel", "DEU", c(2022L, 2022L))$segment_code,
    c("seg.10_10", "seg.10_11")
  )

  sales <- tibble::tibble(
    country_code = c("DEU", "DEU", "DEU", "FRA"),
    species_code = "MUS", variable_code = "GST", year = c(2022L, 2022L, 2020L, 2022L),
    segment_code = c("seg.10_10", "seg.10_11", "seg.old", "seg.10_9"),
    segment_name = c("Longline", "On-bottom", "Old segment", "Rafts"),
    value = c("100", "200", "80", "300"), unit = "EURO"
  )
  testthat::expect_identical(
    eudcf_dashboard_sales_segments(sales, "DEU", "GST", "MUS", c(2022L, 2022L))$segment_code,
    c("seg.10_10", "seg.10_11")
  )
  testthat::expect_equal(
    nrow(eudcf_dashboard_sales_segments(sales, "DEU", "GST", "MUS", c(2023L, 2024L))),
    0L
  )
})

testthat::test_that("DCF dependent selections resolve available choices together", {
  economic <- tibble::tibble(
    country_code = c("DEU", "FRA"),
    species_group = "Mussel",
    year = 2022L,
    segment_code = c("seg.10_10", "seg.11_10"),
    segment_name = c("Longline", "On-bottom")
  )
  german_economic <- eudcf_dashboard_resolve_economic_selection(
    economic, "Mussel", "DEU", c(2022L, 2022L)
  )
  french_economic <- eudcf_dashboard_resolve_economic_selection(
    economic, "Mussel", "FRA", c(2022L, 2022L)
  )
  testthat::expect_identical(german_economic$selected_segment, "seg.10_10")
  testthat::expect_identical(french_economic$selected_segment, "seg.11_10")
  testthat::expect_identical(french_economic$segment_choices$segment_code, "seg.11_10")

  sales <- tibble::tibble(
    country_code = c("DEU", "DEU", "DEU", "FRA"),
    variable_code = "GST",
    species_code = c("MUS", "MUS", "OYS", "OYS"),
    species_name = c("Mussel", "Mussel", "Oyster", "Oyster"),
    year = 2022L,
    segment_code = c("seg.10_10", "seg.10_11", "seg.12_10", "seg.20_10"),
    segment_name = c("Longline", "Rafts", "Oyster on-bottom", "Oyster on-bottom"),
    value = 100
  )
  initial_sales <- eudcf_dashboard_resolve_sales_selection(
    sales, "DEU", "GST", c(2022L, 2022L),
    selected_species = "MUS", selected_segment = "seg.10_11"
  )
  changed_sales <- eudcf_dashboard_resolve_sales_selection(
    sales, "FRA", "GST", c(2022L, 2022L),
    selected_species = "MUS", selected_segment = "seg.10_11"
  )
  empty_sales <- eudcf_dashboard_resolve_sales_selection(
    sales, "FRA", "GST", c(2023L, 2023L),
    selected_species = "OYS", selected_segment = "seg.20_10"
  )

  testthat::expect_identical(initial_sales$selected_species, "MUS")
  testthat::expect_identical(initial_sales$selected_segment, "seg.10_11")
  testthat::expect_identical(initial_sales$segment_choices$segment_code, c("seg.10_10", "seg.10_11"))
  testthat::expect_identical(changed_sales$selected_species, "OYS")
  testthat::expect_identical(changed_sales$selected_segment, "seg.20_10")
  testthat::expect_identical(changed_sales$segment_choices$segment_code, "seg.20_10")
  testthat::expect_length(empty_sales$species_choices$species_code, 0L)
  testthat::expect_identical(empty_sales$selected_species, character())
  testthat::expect_length(empty_sales$segment_choices$segment_code, 0L)
  testthat::expect_identical(empty_sales$selected_segment, character())
})

testthat::test_that("DCF sales output follows a country change", {
  previous_directory <- setwd(project_root)
  on.exit(setwd(previous_directory), add = TRUE)
  app_environment <- new.env(parent = globalenv())
  sys.source(file.path(project_root, "app.R"), envir = app_environment)

  shiny::testServer(app_environment$server, {
    session$setInputs(
      sales_country = "FIN", sales_variable = "TGS", sales_species = "SAL",
      sales_segment = "seg.02_6", sales_years = c(2021L, 2022L)
    )
    session$flushReact()
    testthat::expect_match(output$sales_status, "1 reported rows")
    finland_rows <- paste(unlist(output$sales_table, use.names = FALSE), collapse = " ")
    testthat::expect_match(finland_rows, "Trout Cages")

    session$setInputs(sales_country = "HRV")
    session$flushReact()
    testthat::expect_identical(output$sales_status, "No rows match these filters.")

    session$setInputs(sales_species = "CGM", sales_segment = "seg.04_1")
    session$flushReact()
    testthat::expect_match(output$sales_status, "1 reported rows")
    croatia_rows <- paste(unlist(output$sales_table, use.names = FALSE), collapse = " ")
    testthat::expect_match(croatia_rows, "Carp Ponds")
    testthat::expect_match(croatia_rows, "Africa-bighead catfish, hybrid")
    testthat::expect_false(grepl("Trout Cages", croatia_rows, fixed = TRUE))
  })
})

testthat::test_that("DCF sales display rounds values and achieved sample without changing source rows", {
  sales <- tibble::tibble(
    value = c("1234.56", "NULL"),
    achieved_sample = c("75.236", "NULL")
  )
  display <- eudcf_dashboard_sales_display(sales)
  testthat::expect_identical(display$value, c("1,235", "NULL"))
  testthat::expect_identical(display$achieved_sample, c("75.24", "NULL"))
  testthat::expect_identical(sales$value, c("1234.56", "NULL"))
  testthat::expect_identical(sales$achieved_sample, c("75.236", "NULL"))
})

testthat::test_that("DCF economics display rounds values without changing source rows", {
  economics <- tibble::tibble(value = c("1234.56", "NULL"))
  display <- eudcf_dashboard_economic_display(economics)
  testthat::expect_identical(display$value, c("1,235", "NULL"))
  testthat::expect_identical(economics$value, c("1234.56", "NULL"))
})

testthat::test_that("FAO country ranking balances quantity and value using three complete years", {
  observations <- tidyr::expand_grid(
    country_code = c("DE", "FR", "IT", "ES"), year = 2019:2023,
    measure = c("production_tonnes", "production_value_eur"),
    category = c("mussels_bivalves", "seaweed_algae")
  ) |>
    dplyr::mutate(
      observation_id = dplyr::row_number(),
      source = ifelse(measure == "production_tonnes", "fao_quantity", "fao_value"),
      fao_species_alpha_3_code = ifelse(category == "mussels_bivalves", "MUS", "SWE"),
      comparison_value = dplyr::case_when(
        country_code == "DE" ~ ifelse(measure == "production_tonnes", 100, 1000),
        country_code == "FR" ~ ifelse(measure == "production_tonnes", 1, 100000),
        country_code == "IT" ~ ifelse(measure == "production_tonnes", 20, 20000),
        TRUE ~ NA_real_
      ),
      comparison_value = ifelse(country_code == "DE" & year < 2021, 1e9, comparison_value)
    )
  memberships <- observations |>
    dplyr::transmute(observation_id, segment = "low_anthropic", profile = "strict", category)
  species_classification <- tibble::tibble(
    species_alpha_3_code = c("MUS", "SWE"),
    isscaap_group_en = c("Mussels", "Brown seaweeds")
  )
  observations <- observations |> dplyr::select(-category)
  result <- build_fao_low_anthropic_countries(
    observations, memberships, 2019:2024, top_n = 2L,
    species_classification = species_classification
  )
  testthat::expect_identical(result$ranking$country_code[1], "IT")
  testthat::expect_equal(result$ranking$average_tonnes[result$ranking$country_code == "DE"], 200)
  testthat::expect_equal(result$ranking$average_thousand_eur[result$ranking$country_code == "IT"], 40)
  testthat::expect_equal(sum(result$ranking$selected), 2)
  testthat::expect_true(all(result$ranking$ranking_start == 2021))
  testthat::expect_true(all(result$ranking$ranking_end == 2023))
  testthat::expect_false("ES" %in% result$ranking$country_code)
  testthat::expect_setequal(unique(result$annual_data$category), c("Mussels", "Brown seaweeds"))
  incomplete <- observations |> dplyr::filter(!(country_code == "IT" & year == 2022))
  incomplete_result <- build_fao_low_anthropic_countries(
    incomplete, memberships, 2019:2024, species_classification = species_classification
  )
  testthat::expect_false("IT" %in% incomplete_result$ranking$country_code)
  broad <- dplyr::mutate(memberships, profile = "broad", category = "mollusca_all")
  broad_result <- build_fao_low_anthropic_countries(
    observations, broad, 2019:2024, profile = "broad",
    species_classification = species_classification
  )
  testthat::expect_setequal(unique(broad_result$annual_data$category), c("Mussels", "Brown seaweeds"))
  plot <- build_fao_low_anthropic_country_plot(result, "IT")
  testthat::expect_s3_class(plot, "patchwork")
  testthat::expect_equal(length(plot), 2L)
  testthat::expect_equal(max(plot[[2]]$data$value, na.rm = TRUE), 20)
  country_specific <- result
  country_specific$annual_data <- result$annual_data |>
    dplyr::filter(
      (country_code == "DE" & category == "Mussels") |
        (country_code == "FR" & category == "Brown seaweeds")
    )
  country_specific$annual_data$category <- factor(
    country_specific$annual_data$category,
    levels = c("Mussels", "Brown seaweeds", "Green seaweeds")
  )
  country_plot <- build_fao_low_anthropic_country_plot(country_specific, "DE")
  testthat::expect_identical(unique(country_plot[[1]]$data$category), "Mussels")
  testthat::expect_identical(unique(country_plot[[2]]$data$category), "Mussels")
  for (panel_index in seq_len(2L)) {
    y_breaks <- plot[[panel_index]]$scales$get_scales("y")$breaks
    testthat::expect_false(0 %in% y_breaks(c(0, 20)))
  }
  output_dir <- tempfile("fao-country-charts-")
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  paths <- write_fao_low_anthropic_countries(result, output_dir)
  testthat::expect_length(paths, 2L)
  testthat::expect_true(all(file.info(paths)$size > 0))
  testthat::expect_true(file.exists(file.path(dirname(paths[1]), "country_ranking.csv")))
  testthat::expect_true(file.exists(file.path(dirname(paths[1]), "annual_categories.csv")))
  history_gap <- result
  history_gap$annual_data <- result$annual_data |> dplyr::filter(year != 2020)
  gap_plot <- build_fao_low_anthropic_country_plot(history_gap, "IT")
  testthat::expect_true(all(is.na(gap_plot[[1]]$data$value[gap_plot[[1]]$data$year == 2020])))
  testthat::expect_gt(dplyr::n_distinct(gap_plot[[1]]$data$series), 2L)
  empty <- build_fao_low_anthropic_countries(
    observations[0, ], memberships, 2019:2024,
    species_classification = species_classification
  )
  testthat::expect_equal(nrow(empty$ranking), 0L)
  testthat::expect_error(build_fao_low_anthropic_countries(observations, memberships, 2019:2024, profile = "invalid"))
})

testthat::test_that("FAO continent selection filters countries before ranking and choosing years", {
  observations <- tidyr::expand_grid(
    country_code = c("DE", "FR", "CN"), year = 2020:2023,
    measure = c("production_tonnes", "production_value_eur")
  ) |>
    dplyr::filter(country_code == "CN" | year < 2023) |>
    dplyr::mutate(
      observation_id = dplyr::row_number(),
      source = ifelse(measure == "production_tonnes", "fao_quantity", "fao_value"),
      comparison_value = dplyr::case_when(
        country_code == "CN" ~ 1e6,
        country_code == "DE" ~ 100,
        TRUE ~ 50
      )
    )
  memberships <- observations |>
    dplyr::transmute(observation_id, segment = "low_anthropic", profile = "strict", category = "mussels_bivalves")
  worldwide <- build_fao_low_anthropic_countries(observations, memberships, 2020:2023, top_n = 1L)
  europe <- build_fao_low_anthropic_countries(observations, memberships, 2020:2023, top_n = 1L, continent = "Europe")
  testthat::expect_identical(worldwide$ranking$country_code[1], "CN")
  testthat::expect_identical(europe$ranking$country_code[1], "DE")
  testthat::expect_setequal(europe$ranking$country_code, c("DE", "FR"))
  testthat::expect_identical(unique(europe$annual_data$country_code), "DE")
  testthat::expect_true(all(europe$ranking$ranking_end == 2022))
  testthat::expect_equal(sum(europe$ranking$quantity_share), 1)
  testthat::expect_identical(europe$continent, "Europe")
  testthat::expect_identical(unique(europe$ranking$continent), "Europe")
  testthat::expect_null(worldwide$continent)
  europe_plot <- build_fao_low_anthropic_country_plot(europe, "DE")
  testthat::expect_match(europe_plot$patches$annotation$subtitle, "Europe")
  output_dir <- tempfile("fao-continent-output-")
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  world_paths <- write_fao_low_anthropic_countries(worldwide, output_dir)
  world_hash <- unname(tools::md5sum(world_paths))
  europe_paths <- write_fao_low_anthropic_countries(europe, output_dir)
  testthat::expect_equal(
    normalizePath(dirname(europe_paths), winslash = "/"),
    normalizePath(file.path(output_dir, "fao_top_countries", "strict", "Europe"), winslash = "/")
  )
  testthat::expect_true(all(file.info(europe_paths)$size > 0))
  testthat::expect_identical(unname(tools::md5sum(world_paths)), world_hash)
  empty <- build_fao_low_anthropic_countries(observations, memberships, 2020:2023, continent = "Oceania")
  testthat::expect_equal(nrow(empty$ranking), 0L)
  testthat::expect_error(
    build_fao_low_anthropic_countries(observations, memberships, 2020:2023, continent = "invalid"),
    "arg"
  )
})

testthat::test_that("FAO and EUMOFA imports enforce source schemas and preserve identifiers", {
  fao_path <- tempfile(fileext = ".csv")
  on.exit(unlink(fao_path), add = TRUE)
  writeLines(c(
    '"COUNTRY.UN_CODE","SPECIES.ALPHA_3_CODE","AREA.CODE","ENVIRONMENT.ALPHA_2_CODE","MEASURE","PERIOD","VALUE","STATUS"',
    '"004","FCY","04","IN","Q_tlw",2020,12.5,"A"'
  ), fao_path)

  fao <- read_fao_export(fao_path)
  testthat::expect_identical(fao$country_un_code, "004")
  testthat::expect_type(fao$period, "integer")
  testthat::expect_type(fao$value, "double")

  eumofa_path <- tempfile(fileext = ".csv")
  on.exit(unlink(eumofa_path), add = TRUE)
  writeLines(c(
    "year;country;desc country;commodity group;main commercial species;ers code;english name;volume(kg);weight;value(eur)",
    "2023;BG;Bulgaria;Freshwater fish;Pike-perch;FPP;Pike-perch;25660;Live weight;120970"
  ), eumofa_path)

  eumofa <- read_eumofa_export(eumofa_path)
  testthat::expect_identical(eumofa$country, "BG")
  testthat::expect_type(eumofa$year, "integer")
  testthat::expect_type(eumofa$volume_kg, "double")
  testthat::expect_type(eumofa$value_eur, "double")
})

testthat::test_that("source imports reject missing schemas and malformed numeric fields", {
  missing_column_path <- tempfile(fileext = ".csv")
  on.exit(unlink(missing_column_path), add = TRUE)
  writeLines(c(
    "year;country;value(eur)",
    "2023;BG;120970"
  ), missing_column_path)
  testthat::expect_error(read_eumofa_export(missing_column_path), "missing required column")

  malformed_path <- tempfile(fileext = ".csv")
  on.exit(unlink(malformed_path), add = TRUE)
  writeLines(c(
    '"COUNTRY.UN_CODE","SPECIES.ALPHA_3_CODE","AREA.CODE","ENVIRONMENT.ALPHA_2_CODE","MEASURE","PERIOD","VALUE","STATUS"',
    '"004","FCY","04","IN","Q_tlw","unknown",12.5,"A"'
  ), malformed_path)
  testthat::expect_error(read_fao_export(malformed_path), "parsing problem|invalid integer")
})

testthat::test_that("EUDCF workbook keeps economic, social, and sales tables distinct", {
  make_row <- function(columns, values) {
    row <- stats::setNames(as.list(rep(NA, length(columns))), columns)
    row[names(values)] <- values
    as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  }

  economic_columns <- c(
    "country_code", "country_name", "environment", "environment_hn", "technique",
    "species_group", "segment_name", "segment_code", "year", "variable_group",
    "variable_name", "variable_code", "value", "unit", "framework"
  )
  social_columns <- c(
    "country_code", "country_name", "year", "variable_group", "variable_code", "value",
    "unit", "environment_uploaded", "species", "technique_uploaded", "geo_indicator",
    "segment_name", "segment_uploaded", "farmer", "gender_uploaded", "age_code",
    "education_uploaded", "nationality_uploaded", "employment_status_uploaded",
    "population_farm", "responses_farm", "population_farmer", "responses_farmer",
    "sampling_strategy", "comments", "upload_date"
  )
  sales_columns <- c(
    "country_code", "country_name", "scientific_name", "common_name", "species_name",
    "species_code", "variable_group", "variable_name", "variable_code", "value", "unit",
    "year", "environment", "environment_hn", "technique", "species_group", "segment_name",
    "segment_code", "sample", "population", "achieved_sample", "precision_level",
    "sampling_strategy", "comments", "log_upload_id", "upload_date", "framework", "unit_legend"
  )
  historical_sales_columns <- c(
    "country_code", "country_name", "species", "common_name", "scientific_name",
    "variable_group", "variable_code", "variable_name", "environment", "environment_hn",
    "segment_code", "segment_name", "value", "unit", "year", "sample", "population",
    "sample_rate", "sampling_strategy", "precision_level", "comments", "ordering", "date", "template"
  )

  workbook_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(workbook_path), add = TRUE)
  openxlsx::write.xlsx(
    list(
      "Economic data" = make_row(economic_columns, list(
        country_code = "DEU", segment_code = "tot", year = 2023, variable_code = "GST",
        value = 120.5, unit = "EURO", framework = "eum"
      )),
      "Social data" = make_row(social_columns, list(
        country_code = "DEU", year = 2023, variable_code = "socfte", value = "NULL", unit = "NUMBER"
      )),
      "Sales data" = make_row(sales_columns, list(
        country_code = "DEU", species_code = "SAL", segment_code = "tot", year = 2023,
        variable_code = "TWS", value = 900, unit = "KG", framework = "eum"
      )),
      "old DCF Sales data" = make_row(historical_sales_columns, list(
        country_code = "DEU", species = "SAL", segment_code = "seg.01_1", year = 2008,
        variable_code = "TPD", value = 750, unit = "EURO"
      ))
    ),
    workbook_path,
    overwrite = TRUE
  )

  eudcf <- read_eudcf_workbook(workbook_path)

  testthat::expect_named(
    eudcf,
    c("economic", "social", "sales", "historical_sales", "metadata")
  )
  testthat::expect_identical(eudcf$economic$segment_code, "tot")
  testthat::expect_identical(eudcf$economic$variable_code, "GST")
  testthat::expect_equal(eudcf$economic$value, 120.5)
  testthat::expect_identical(eudcf$social$value, "NULL")
  testthat::expect_identical(eudcf$sales$variable_code, "TWS")
  testthat::expect_equal(eudcf$sales$value, 900)
  testthat::expect_identical(eudcf$historical_sales$variable_code, "TPD")
  testthat::expect_equal(eudcf$historical_sales$value, 750)
  testthat::expect_error(read_eudcf_workbook(paste0(workbook_path, ".missing")), "does not exist")
})

testthat::test_that("pipeline outputs are computed from in-memory source tables", {
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
  inputs$eumofa <- dplyr::bind_rows(
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

  testthat::expect_true(is.data.frame(result$observations))
  testthat::expect_equal(nrow(result$observations), 22L)
  testthat::expect_true("observation_id" %in% names(result$observations))
  testthat::expect_identical(result$observations$observation_id, seq_len(nrow(result$observations)))
  testthat::expect_length(unique(names(result$observations)), ncol(result$observations))
  testthat::expect_true(all(c(
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

  eurostat_total_tonnes <- result$observations |>
    dplyr::filter(source == "eurostat", eurostat_species_code == "F00", measure == "production_tonnes")
  testthat::expect_equal(eurostat_total_tonnes$record_scope, "published_total")
  testthat::expect_equal(eurostat_total_tonnes$native_value, 100)
  testthat::expect_equal(eurostat_total_tonnes$comparison_value, 100)

  eurostat_detail_tonnes <- result$observations |>
    dplyr::filter(source == "eurostat", eurostat_species_code == "F54", measure == "production_tonnes")
  testthat::expect_equal(eurostat_detail_tonnes$record_scope, "source_category")
  testthat::expect_equal(eurostat_detail_tonnes$eurostat_flags, "p")

  fao_value_oyster <- result$observations |>
    dplyr::filter(source == "fao_value", fao_species_alpha_3_code == "OYS")
  testthat::expect_equal(fao_value_oyster$native_value, 2)
  testthat::expect_equal(fao_value_oyster$native_unit, "THOUSAND_USD")
  testthat::expect_equal(fao_value_oyster$comparison_value, 1000)
  testthat::expect_equal(fao_value_oyster$comparison_unit, "EUR")
  testthat::expect_equal(fao_value_oyster$fx_rate, 0.5)

  fao_nei <- result$observations |>
    dplyr::filter(source == "fao_quantity", fao_species_alpha_3_code == "FCY")
  testthat::expect_equal(fao_nei$record_scope, "source_category")
  testthat::expect_equal(fao_nei$fao_species_name_en, "Cyprinids NEI")
  testthat::expect_equal(fao_nei$fao_taxonomic_code, "1400141XXXXX")

  eumofa_oyster_volume <- result$observations |>
    dplyr::filter(source == "eumofa", eumofa_ers_code == "OYS001", measure == "production_tonnes")
  testthat::expect_equal(eumofa_oyster_volume$native_value, 3000)
  testthat::expect_equal(eumofa_oyster_volume$native_unit, "KG")
  testthat::expect_equal(eumofa_oyster_volume$comparison_value, 3)
  testthat::expect_equal(eumofa_oyster_volume$eumofa_english_name, "Oyster, live")
  eumofa_missing <- result$observations |>
    dplyr::filter(source == "eumofa", eumofa_ers_code == "UNK001")
  testthat::expect_equal(nrow(eumofa_missing), 2L)
  testthat::expect_true(all(is.na(eumofa_missing$native_value)))

  testthat::expect_named(result, c("observations", "memberships", "exports", "diagnostics"))
  testthat::expect_true(all(c(
    "observation_id", "segment", "profile", "category", "matched_by"
  ) %in% names(result$memberships)))
  testthat::expect_true(all(result$memberships$observation_id %in% result$observations$observation_id))
  observations_before_membership <- result$observations
  recomputed_memberships <- build_segment_memberships(result$observations)
  testthat::expect_identical(result$observations, observations_before_membership)
  testthat::expect_equal(recomputed_memberships, result$memberships)

  freshwater_memberships <- result$memberships |>
    dplyr::filter(segment == "freshwater")
  eurostat_freshwater_ids <- result$observations |>
    dplyr::filter(source == "eurostat", eurostat_environment_code == "FRW") |>
    dplyr::pull(observation_id)
  fao_freshwater_ids <- result$observations |>
    dplyr::filter(source %in% c("fao_quantity", "fao_value"), fao_environment_alpha_2_code == "IN") |>
    dplyr::pull(observation_id)
  eumofa_freshwater_ids <- result$observations |>
    dplyr::filter(source == "eumofa", eumofa_commodity_group == "Freshwater finfish") |>
    dplyr::pull(observation_id)
  testthat::expect_true(all(eurostat_freshwater_ids %in% freshwater_memberships$observation_id))
  testthat::expect_true(all(fao_freshwater_ids %in% freshwater_memberships$observation_id))
  testthat::expect_true(all(eumofa_freshwater_ids %in% freshwater_memberships$observation_id))
  testthat::expect_true(all(freshwater_memberships$matched_by[match(
    c(eurostat_freshwater_ids, fao_freshwater_ids, eumofa_freshwater_ids),
    freshwater_memberships$observation_id
  )] %in% c(
    "eurostat_environment_code",
    "fao_environment_alpha_2_code",
    "eumofa_commodity_group"
  )))

  eumo_oyster_id <- eumofa_oyster_volume$observation_id
  oyster_memberships <- result$memberships |>
    dplyr::filter(observation_id == eumo_oyster_id)
  testthat::expect_true(any(oyster_memberships$segment == "freshwater"))
  testthat::expect_true(any(
    oyster_memberships$segment == "low_anthropic" &
      oyster_memberships$profile == "strict" &
      oyster_memberships$category == "mussels_bivalves"
  ))
  testthat::expect_true(any(
    oyster_memberships$segment == "low_anthropic" &
      oyster_memberships$profile == "broad" &
      oyster_memberships$category == "mollusca_all"
  ))

  shrimp_id <- result$observations |>
    dplyr::filter(source == "fao_quantity", fao_species_alpha_3_code == "SHR") |>
    dplyr::pull(observation_id)
  default_crustacea_membership <- result$memberships |>
    dplyr::filter(
      observation_id == shrimp_id,
      segment == "low_anthropic",
      category == "crustacea_subset"
    )
  testthat::expect_equal(nrow(default_crustacea_membership), 0L)
  crustacea_memberships <- build_segment_memberships(
    result$observations,
    include_crustacea_subset = TRUE
  )
  testthat::expect_true(any(
    crustacea_memberships$observation_id == shrimp_id &
      crustacea_memberships$segment == "low_anthropic" &
      crustacea_memberships$profile == "broad" &
      crustacea_memberships$category == "crustacea_subset"
  ))
  scalloped_lobster <- result$observations |>
    dplyr::filter(source == "fao_quantity", fao_species_alpha_3_code == "SHR") |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
      observation_id = max(result$observations$observation_id) + 1L,
      fao_species_alpha_3_code = "LOK",
      fao_species_name_en = "Scalloped spiny lobster"
    )
  lobster_memberships <- build_segment_memberships(scalloped_lobster)
  testthat::expect_false(any(
    lobster_memberships$segment == "low_anthropic" &
      lobster_memberships$profile == "strict"
  ))

  unclassified_id <- result$observations |>
    dplyr::filter(source == "eumofa", eumofa_ers_code == "UNK002", measure == "production_tonnes") |>
    dplyr::pull(observation_id)
  testthat::expect_true(all(is.na(result$observations$native_value[match(
    unclassified_id,
    result$observations$observation_id
  )])))
  testthat::expect_false(any(result$memberships$observation_id %in% unclassified_id))

  testthat::expect_named(
    result$exports,
    c(
      "general_summary",
      "freshwater_summary",
      "low_anthropic_by_mode",
      "low_anthropic_side_by_side",
      "freshwater_consistency",
      "comparison_tables",
      "comparison_plots"
    )
  )
  testthat::expect_equal(result$exports$general_summary$eurostat_production, 100)
  testthat::expect_equal(result$exports$general_summary$fao_production, 56)
  testthat::expect_equal(result$exports$general_summary$eumofa_production, 3)
  testthat::expect_equal(result$exports$general_summary$fao_value, 3250)
  testthat::expect_equal(
    result$diagnostics$comparison_all$fao_value_production_value_usd,
    6500
  )
  testthat::expect_equal(
    result$exports$comparison_tables$all_production |>
      dplyr::select(
        country_code, year, eurostat_production, fao_production,
        eumofa_production, eurostat_value, fao_value, eumofa_value
      ),
    tibble::tibble(
      country_code = "DE",
      year = 2020L,
      eurostat_production = 100,
      fao_production = 56,
      eumofa_production = 3,
      eurostat_value = 50000,
      fao_value = 3250,
      eumofa_value = 5000
    )
  )
  testthat::expect_equal(
    result$exports$comparison_tables$freshwater |>
      dplyr::select(
        country_code, year, eurostat_production, fao_production,
        eumofa_production, eurostat_value, fao_value, eumofa_value
      ),
    tibble::tibble(
      country_code = "DE",
      year = 2020L,
      eurostat_production = 100,
      fao_production = 56,
      eumofa_production = 3,
      eurostat_value = 50000,
      fao_value = 3250,
      eumofa_value = 5000
    )
  )
  testthat::expect_equal(
    result$exports$comparison_tables$low_anthropic |>
      dplyr::filter(profile == "strict", category == "mussels_bivalves") |>
      dplyr::select(
        profile, category, country_code, year, eurostat_production,
        fao_production, eumofa_production, eurostat_value, fao_value, eumofa_value
      ),
    tibble::tibble(
      profile = "strict",
      category = "mussels_bivalves",
      country_code = "DE",
      year = 2020L,
      eurostat_production = 5,
      fao_production = 10,
      eumofa_production = 3,
      eurostat_value = 2500,
      fao_value = 1000,
      eumofa_value = 5000
    )
  )
  testthat::expect_setequal(
    names(result$exports$comparison_plots),
    c(
      "all_production_quantity", "all_production_value",
      "freshwater_quantity", "freshwater_value",
      "low_anthropic_quantity", "low_anthropic_value"
    )
  )
  testthat::expect_equal(
    result$exports$comparison_plots$all_production_quantity$data |>
      dplyr::filter(year == 2020L, source == "FAO") |>
      dplyr::pull(value),
    56
  )

  scoped_inputs <- inputs
  scoped_inputs$fao_to_iso2 <- dplyr::bind_rows(
    inputs$fao_to_iso2,
    tibble::tibble(country_un_code = c("250", "356"), iso2 = c("FR", "IN"))
  )
  scoped_inputs$fao_quantity <- dplyr::bind_rows(
    inputs$fao_quantity,
    tibble::tibble(
      country_un_code = c("999", "250", "356"),
      period = c(2021L, 2022L, 2021L),
      measure = "Q_tlw",
      area_code = "04",
      environment_alpha_2_code = "IN",
      value = c(0, 99, 88),
      status = "A",
      species_alpha_3_code = "SAL"
    )
  )
  scoped_inputs$eumofa <- dplyr::bind_rows(
    inputs$eumofa,
    tibble::tibble(
      country = c("DE", "FR"),
      year = c(2021L, 2021L),
      volume_kg = NA_real_,
      value_eur = NA_real_,
      commodity_group = "Freshwater finfish",
      main_commercial_species = "Unreported",
      ers_code = c("MISS-DE", "MISS-FR"),
      english_name = "Unreported freshwater fish",
      weight = NA_character_
    )
  )
  scoped_config <- config
  scoped_config$comparison_years <- 2020:2021
  scoped_config$comparison_countries <- c("DE", "FR")
  scoped_result <- build_pipeline_outputs(scoped_inputs, scoped_config)
  all_production <- scoped_result$exports$comparison_tables$all_production
  testthat::expect_equal(
    all_production |> dplyr::select(country_code, year),
    tibble::tibble(
      country_code = c("DE", "DE", "FR"),
      year = c(2020L, 2021L, 2021L)
    )
  )
  testthat::expect_equal(
    scoped_result$exports$comparison_plots$all_production_quantity$data |>
      dplyr::distinct(year),
    tibble::tibble(year = c(2020L, 2021L))
  )
  all_country_config <- scoped_config
  all_country_config["comparison_countries"] <- list(NULL)
  all_country_result <- build_pipeline_outputs(scoped_inputs, all_country_config)
  testthat::expect_setequal(
    all_country_result$exports$comparison_tables$all_production$country_code,
    c("DE", "FR", "IN")
  )
  testthat::expect_equal(
    all_country_result$exports$comparison_plots$all_production_quantity$data |>
      dplyr::filter(year == 2021L, source == "FAO") |>
      dplyr::pull(value),
    88
  )
  testthat::expect_equal(
    all_production |>
      dplyr::filter(country_code == "DE", year == 2021L) |>
      dplyr::select(fao_production, eumofa_production),
    tibble::tibble(fao_production = 0, eumofa_production = NA_real_)
  )
  testthat::expect_true(is.na(
    all_production |>
      dplyr::filter(country_code == "FR", year == 2021L) |>
      dplyr::pull(eumofa_production)
  ))

  low_anthropic_types <- result$exports$low_anthropic_by_mode$production_type
  testthat::expect_setequal(
    low_anthropic_types,
    c("mussels_bivalves", "seaweed_algae", "mollusca_all", "aquatic_plants_all")
  )
  low_anthropic <- result$exports$low_anthropic_by_mode
  strict_mussels <- low_anthropic |>
    dplyr::filter(mode == "strict", production_type == "mussels_bivalves")
  broad_mollusca <- low_anthropic |>
    dplyr::filter(mode == "broad", production_type == "mollusca_all")
  strict_seaweed <- low_anthropic |>
    dplyr::filter(mode == "strict", production_type == "seaweed_algae")
  broad_plants <- low_anthropic |>
    dplyr::filter(mode == "broad", production_type == "aquatic_plants_all")
  testthat::expect_equal(strict_mussels$eurostat_species_production_tonnes, 5)
  testthat::expect_equal(strict_mussels$fao_quantity_production_tonnes, 10)
  testthat::expect_equal(strict_mussels$fao_eur_production_value_eur, 1000)
  testthat::expect_equal(strict_mussels$eumofa_production_tonnes, 3)
  testthat::expect_equal(broad_mollusca$eumofa_production_tonnes, 3)
  testthat::expect_equal(strict_seaweed$eurostat_species_production_tonnes, 2)
  testthat::expect_equal(strict_seaweed$fao_quantity_production_tonnes, 4)
  testthat::expect_equal(broad_mollusca$fao_quantity_production_tonnes, 10)
  testthat::expect_equal(broad_plants$fao_quantity_production_tonnes, 4)

  crustacea_inputs <- inputs
  crustacea_inputs$eumofa <- dplyr::bind_rows(
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
  crustacea_rows <- crustacea_result$exports$low_anthropic_by_mode |>
    dplyr::filter(mode == "broad", production_type == "crustacea_subset")
  testthat::expect_equal(crustacea_rows$fao_quantity_production_tonnes, 2)
  testthat::expect_equal(crustacea_rows$eumofa_production_tonnes, 5)

  labels_unavailable_inputs <- inputs
  labels_unavailable_inputs$eurostat_species_labels <- tibble::tibble(
    species_code = character(),
    species_label = character()
  )
  labels_unavailable_result <- build_pipeline_outputs(
    labels_unavailable_inputs,
    config
  )
  testthat::expect_equal(labels_unavailable_result$exports$general_summary$eurostat_production, 100)
  testthat::expect_equal(labels_unavailable_result$exports$freshwater_summary$eurostat_production, 100)
  testthat::expect_true("unit_check" %in% names(result$diagnostics))
  testthat::expect_true("comparison_freshwater" %in% names(result$diagnostics))

  no_total_inputs <- inputs
  no_total_inputs$eurostat <- dplyr::filter(inputs$eurostat, species != "F00")
  no_total_result <- build_pipeline_outputs(no_total_inputs, config)
  testthat::expect_true(is.na(no_total_result$exports$general_summary$eurostat_production))
  testthat::expect_true(is.na(no_total_result$exports$freshwater_summary$eurostat_production))
  testthat::expect_equal(
    no_total_result$exports$comparison_tables$all_production$eurostat_production,
    7
  )
  testthat::expect_equal(
    no_total_result$exports$low_anthropic_by_mode |>
      dplyr::filter(mode == "strict", production_type == "mussels_bivalves") |>
      dplyr::pull(eurostat_species_production_tonnes),
    5
  )
})

testthat::test_that("pipeline outputs remain available when Eurostat is empty", {
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

  testthat::expect_equal(result$exports$general_summary$fao_production, 10)
  testthat::expect_equal(result$exports$general_summary$eumofa_production, 3)
  testthat::expect_true(is.na(result$exports$general_summary$eurostat_production))
  testthat::expect_equal(result$exports$freshwater_summary$fao_production, 10)
})

testthat::test_that("partial snapshots do not replace the latest complete snapshot", {
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

  testthat::expect_equal(basename(complete_path), "aquaculture_observations.rds")
  testthat::expect_equal(readRDS(complete_path), complete_bundle)
  complete_checksum <- unname(tools::md5sum(complete_path))

  partial_bundle <- complete_bundle
  partial_bundle$manifest$coverage_status <- "partial"
  partial_bundle$observations$source <- "fao_quantity"
  partial_path <- write_normalized_snapshot(partial_bundle, output_dir)

  testthat::expect_equal(basename(partial_path), "aquaculture_observations_partial.rds")
  testthat::expect_equal(readRDS(partial_path), partial_bundle)
  testthat::expect_identical(unname(tools::md5sum(complete_path)), complete_checksum)
})

testthat::test_that("observation manifest records provenance and partial source coverage", {
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
    generated_at_utc = "2026-10-14T00:00:00Z",
    optional_sources = list(
      eudcf = list(status = "unavailable", sheet_row_counts = integer())
    )
  )

  testthat::expect_equal(manifest$schema_version, "1.1.0")
  testthat::expect_equal(manifest$segment_rules_version, SEGMENT_RULES_VERSION)
  testthat::expect_equal(manifest$coverage_status, "complete")
  testthat::expect_equal(manifest$optional_sources$eudcf$status, "unavailable")
  testthat::expect_equal(manifest$eurostat$dataset_id, "fish_aq2a")
  testthat::expect_equal(manifest$input_files$fao_quantity$md5, unname(tools::md5sum(source_file)))
  testthat::expect_equal(manifest$fx$source, "csv")
  testthat::expect_equal(manifest$fx$rates$eur_per_usd, 0.5)

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
  testthat::expect_equal(partial_manifest$coverage_status, "partial")
  testthat::expect_equal(partial_manifest$fx$source, "fallback")
})

testthat::test_that("partial runs do not overwrite complete exports or diagnostics", {
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
    c(output_dirs$general, output_dirs$general, output_dirs$freshwater, output_dirs$low_anthropic,
      output_dirs$freshwater, output_dirs$diagnostics, output_dirs$diagnostics,
      output_dirs$diagnostics, output_dirs$diagnostics),
    c(
      "Europe_summary.xlsx",
      "production_comparisons.xlsx",
      "Europe_freshwater_summary.xlsx",
      "Europe_low_anthropic_summary.xlsx",
      "freshwater_consistency.csv",
      "fao_env_qty_counts.csv",
      "fao_env_val_counts.csv",
      "fao_major_group_qty_counts.csv",
      "fao_major_group_val_counts.csv"
    )
  )
  plot_names <- c(
    "all_production_quantity", "all_production_value",
    "freshwater_quantity", "freshwater_value",
    "low_anthropic_quantity", "low_anthropic_value"
  )
  comparison_plot_dir <- file.path(output_dirs$general, "comparison_plots")
  dir.create(comparison_plot_dir, recursive = TRUE)
  existing_paths <- c(
    existing_paths,
    file.path(comparison_plot_dir, paste0(plot_names, ".png"))
  )
  for (path in existing_paths) writeLines("previous complete output", path)
  before_hashes <- unname(tools::md5sum(existing_paths))
  partial_plot <- build_comparison_plot(
    tibble::tibble(
      country_code = "DE",
      year = 2020L,
      eurostat_production = 100,
      fao_production = 80,
      eumofa_production = 70
    ),
    measure = "production_tonnes",
    segment = "all_production"
  )

  published <- write_pipeline_exports(
    exports = list(comparison_plots = list(all_production_quantity = partial_plot)),
    output_dirs = output_dirs,
    coverage_status = "partial"
  )

  testthat::expect_false(published)
  testthat::expect_identical(unname(tools::md5sum(existing_paths)), before_hashes)
  partial_plot_path <- file.path(
    comparison_plot_dir,
    "all_production_quantity_partial.png"
  )
  testthat::expect_true(file.exists(partial_plot_path))
  testthat::expect_gt(file.info(partial_plot_path)$size, 0)
})

testthat::test_that("complete runs write summaries and diagnostics", {
  output_root <- tempfile("complete-output-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  output_dirs <- list(
    root = output_root,
    general = file.path(output_root, "general"),
    freshwater = file.path(output_root, "freshwater"),
    low_anthropic = file.path(output_root, "low_anthropic"),
    diagnostics = file.path(output_root, "diagnostics")
  )
  plot_names <- c(
    "all_production_quantity", "all_production_value",
    "freshwater_quantity", "freshwater_value",
    "low_anthropic_quantity", "low_anthropic_value"
  )
  comparison_plot <- build_comparison_plot(
    tibble::tibble(
      country_code = "DE",
      year = 2020L,
      eurostat_production = 100,
      fao_production = 80,
      eumofa_production = 70
    ),
    measure = "production_tonnes",
    segment = "all_production"
  )
  comparison_plots <- stats::setNames(
    rep(list(comparison_plot), length(plot_names)),
    plot_names
  )
  exports <- list(
    general_summary = tibble::tibble(country_code = "DE", year = 2020L),
    freshwater_summary = tibble::tibble(country_code = "DE", year = 2020L),
    low_anthropic_by_mode = tibble::tibble(mode = "strict", country_code = "DE"),
    low_anthropic_side_by_side = tibble::tibble(country_code = "DE"),
    freshwater_consistency = tibble::tibble(country_code = "DE", year = 2020L),
    comparison_tables = list(
      all_production = tibble::tibble(country_code = "DE", year = 2020L, eurostat_production = 100),
      freshwater = tibble::tibble(country_code = "DE", year = 2020L, eurostat_production = 80),
      low_anthropic = tibble::tibble(
        profile = "strict", category = "mussels_bivalves",
        country_code = "DE", year = 2020L, eurostat_production = 5
      )
    ),
    comparison_plots = comparison_plots
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

  testthat::expect_true(published)
  testthat::expect_length(list.files(output_root, recursive = TRUE), 15L)
  comparison_plot_dir <- file.path(output_dirs$general, "comparison_plots")
  testthat::expect_setequal(
    list.files(comparison_plot_dir),
    paste0(plot_names, ".png")
  )
  testthat::expect_true(all(
    file.info(file.path(comparison_plot_dir, paste0(plot_names, ".png")))$size > 0
  ))
  comparison_path <- file.path(output_dirs$general, "production_comparisons.xlsx")
  testthat::expect_setequal(
    openxlsx::getSheetNames(comparison_path),
    c("all_production", "freshwater", "low_anthropic")
  )
  testthat::expect_equal(
    openxlsx::read.xlsx(comparison_path, sheet = "all_production")$eurostat_production,
    100
  )
  testthat::expect_equal(
    openxlsx::read.xlsx(comparison_path, sheet = "freshwater")$eurostat_production,
    80
  )
  testthat::expect_equal(
    openxlsx::read.xlsx(comparison_path, sheet = "low_anthropic")$category,
    "mussels_bivalves"
  )
  testthat::expect_setequal(
    openxlsx::getSheetNames(file.path(output_dirs$low_anthropic, "Europe_low_anthropic_summary.xlsx")),
    c("by_mode", "side_by_side")
  )
  testthat::expect_equal(
    nrow(openxlsx::read.xlsx(file.path(output_dirs$general, "Europe_summary.xlsx"), sheet = "data")),
    1L
  )
  testthat::expect_equal(
    nrow(utils::read.csv(file.path(output_dirs$diagnostics, "fao_env_qty_counts.csv"))),
    1L
  )
})

testthat::test_that("comparison plots preserve values and label quantity units", {
  comparison_table <- tibble::tibble(
    country_code = c("DE", "DE"),
    year = c(2020L, 2021L),
    eurostat_production = c(100, NA_real_),
    fao_production = c(80, 0),
    eumofa_production = c(NA_real_, NA_real_)
  )

  plot <- build_comparison_plot(
    comparison_table,
    measure = "production_tonnes",
    segment = "all_production",
    facet_countries = TRUE
  )

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_identical(plot$labels$title, "Production quantity (million tonnes)")
  testthat::expect_identical(plot$labels$subtitle, "All production")
  testthat::expect_equal(
    plot$data |> dplyr::select(country_code, year, source, measure, unit, value),
    tibble::tibble(
      country_code = rep("DE", 6),
      year = rep(c(2020L, 2021L), each = 3),
      source = rep(c("Eurostat", "FAO", "EUMOFA"), 2),
      measure = rep("production_tonnes", 6),
      unit = rep("tonnes", 6),
      value = c(100, 80, NA_real_, NA_real_, 0, NA_real_)
    )
  )
  testthat::expect_null(plot$labels$x)
  testthat::expect_null(plot$labels$y)
  testthat::expect_identical(plot$labels$colour, "Source")
  testthat::expect_identical(
    plot$scales$get_scales("y")$get_labels(c(0, 1000, 1000000)),
    c("0.000", "0.001", "1.000")
  )
})

testthat::test_that("comparison value plots use compatible EUR values", {
  comparison_table <- tibble::tibble(
    country_code = c("DE", "DE"),
    year = c(2020L, 2021L),
    eurostat_value = c(3000, NA_real_),
    fao_value = c(1500, 0),
    eumofa_value = c(NA_real_, 1000)
  )

  plot <- build_comparison_plot(
    comparison_table,
    measure = "production_value_eur",
    segment = "all_production",
    facet_countries = TRUE
  )

  testthat::expect_equal(
    plot$data |> dplyr::select(country_code, year, source, measure, unit, value),
    tibble::tibble(
      country_code = rep("DE", 6),
      year = rep(c(2020L, 2021L), each = 3),
      source = rep(c("Eurostat", "FAO", "EUMOFA"), 2),
      measure = rep("production_value_eur", 6),
      unit = rep("EUR", 6),
      value = c(3000, 1500, NA_real_, NA_real_, 0, 1000)
    )
  )
  testthat::expect_identical(plot$labels$title, "Production value (million EUR)")
  testthat::expect_identical(plot$labels$subtitle, "All production")
  testthat::expect_null(plot$labels$y)
  testthat::expect_identical(plot$labels$colour, "Source")
  testthat::expect_identical(
    plot$scales$get_scales("y")$get_labels(c(0, 1000, 1000000000)),
    c("0.000", "0.001", "1,000.000")
  )
})

testthat::test_that("comparison year labels adapt to the span without fractional years", {
  for (year_range in list(2020:2023, 2006:2024, 2008:2023, 2010:2024, 1990:2025, 1950:2025, 2020L, integer())) {
    comparison_table <- tibble::tibble(
      country_code = rep("DE", length(year_range)),
      year = year_range,
      eurostat_production = rep(100, length(year_range)),
      fao_production = rep(80, length(year_range)),
      eumofa_production = rep(NA_real_, length(year_range))
    )
    plot <- build_comparison_plot(comparison_table, "production_tonnes", "all_production")
    built <- ggplot2::ggplot_build(plot)
    axis <- built$layout$panel_params[[1]]$x
    testthat::expect_length(axis$minor_breaks, 0L)
    breaks <- axis$get_breaks()
    breaks <- breaks[is.finite(breaks)]
    testthat::expect_equal(breaks, round(breaks))
    testthat::expect_lte(length(breaks), 10L)
    if (length(year_range) >= 14) {
      testthat::expect_gte(length(breaks), 7L)
    }
    testthat::expect_equal(
      axis$scale$get_labels(breaks),
      format(breaks, scientific = FALSE, trim = TRUE)
    )
    if (length(year_range) > 0 && length(year_range) <= 4) {
      testthat::expect_true(all(year_range %in% breaks))
    }
  }
})

testthat::test_that("low-anthropic plots retain profile and category values", {
  comparison_table <- tibble::tibble(
    profile = c("strict", "broad"),
    category = c("mussels_bivalves", "mollusca_all"),
    country_code = c("DE", "DE"),
    year = c(2020L, 2020L),
    eurostat_production = c(5, 7),
    fao_production = c(10, 12),
    eumofa_production = c(3, 4)
  )

  plot <- build_comparison_plot(
    comparison_table,
    measure = "production_tonnes",
    segment = "low_anthropic",
    facet_countries = TRUE
  )

  testthat::expect_identical(plot$labels$title, "Production quantity (million tonnes)")
  testthat::expect_identical(plot$labels$subtitle, "Low-anthropic")
  testthat::expect_null(plot$labels$y)
  testthat::expect_named(plot$facet$params$facets, c("category", "country_code"))
  testthat::expect_equal(length(unique(plot$data$series)), 6L)
  testthat::expect_equal(nrow(ggplot2::ggplot_build(plot)$layout$layout), 2L)
  testthat::expect_equal(
    plot$data |> dplyr::select(profile, category, country_code, year, source, value),
    tibble::tibble(
      profile = rep(c("strict", "broad"), each = 3),
      category = rep(c("mussels_bivalves", "mollusca_all"), each = 3),
      country_code = rep("DE", 6),
      year = rep(2020L, 6),
      source = rep(c("Eurostat", "FAO", "EUMOFA"), 2),
      value = c(5, 10, 3, 7, 12, 4)
    )
  )
})

testthat::test_that("overview plots sum countries separately by source, year and category", {
  comparison_table <- tibble::tibble(
    profile = "broad",
    category = c("mollusca_all", "mollusca_all", "aquatic_plants_all",
                 "aquatic_plants_all", "mollusca_all", "mollusca_all"),
    country_code = rep(c("DE", "FR"), 3),
    year = c(2020L, 2020L, 2020L, 2020L, 2021L, 2021L),
    eurostat_production = c(10, 20, 100, 200, NA_real_, NA_real_),
    fao_production = c(1, NA_real_, 3, 4, 0, NA_real_),
    eumofa_production = NA_real_
  ) |>
    dplyr::mutate(
      eurostat_value = eurostat_production * 1000,
      fao_value = fao_production * 1000,
      eumofa_value = eumofa_production
    )

  for (measure in c("production_tonnes", "production_value_eur")) {
    plot <- build_comparison_plot(comparison_table, measure, "low_anthropic")
    multiplier <- if (measure == "production_tonnes") 1 else 1000
    molluscs <- plot$data |>
      dplyr::filter(category == "mollusca_all") |>
      dplyr::arrange(year)

    testthat::expect_false("country_code" %in% names(plot$data))
    testthat::expect_named(plot$facet$params$facets, "category")
    testthat::expect_equal(molluscs$value[molluscs$source == "Eurostat"], c(30, NA_real_) * multiplier)
    testthat::expect_equal(molluscs$value[molluscs$source == "FAO"], c(1, 0) * multiplier)
    testthat::expect_true(all(is.na(plot$data$value[plot$data$source == "EUMOFA"])))
    testthat::expect_equal(
      plot$data$value[plot$data$category == "aquatic_plants_all" & plot$data$source == "Eurostat"],
      300 * multiplier
    )
    testthat::expect_equal(nrow(ggplot2::ggplot_build(plot)$layout$layout), 2L)
  }

  country_plot <- build_comparison_plot(
    comparison_table, "production_tonnes", "low_anthropic", facet_countries = TRUE
  )
  testthat::expect_equal(nrow(country_plot$data), 18L)
  testthat::expect_equal(nrow(ggplot2::ggplot_build(country_plot)$layout$layout), 4L)

  total_plot <- build_comparison_plot(
    dplyr::select(comparison_table, -profile, -category), "production_tonnes", "all_production"
  )
  testthat::expect_s3_class(total_plot$facet, "FacetNull")
  testthat::expect_equal(
    total_plot$data$value[total_plot$data$source == "Eurostat"], c(330, NA_real_)
  )
})

testthat::test_that("empty segment plots can be built without facet values", {
  comparison_table <- tibble::tibble(
    profile = character(),
    category = character(),
    country_code = character(),
    year = integer(),
    eurostat_production = double(),
    fao_production = double(),
    eumofa_production = double()
  )

  plot <- build_comparison_plot(
    comparison_table,
    measure = "production_tonnes",
    segment = "low_anthropic"
  )

  testthat::expect_no_error(ggplot2::ggplot_build(plot))
})

testthat::test_that("default comparison window covers the latest 20 completed years", {
  script <- parse(file.path(project_root, "main-script.R"))
  year_assignment <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("years_target"))
  }, as.list(script))
  config <- new.env()
  eval(year_assignment[[1]], envir = config)
  latest_year <- as.integer(format(Sys.Date(), "%Y")) - 1L

  testthat::expect_equal(config$years_target, seq.int(latest_year - 19L, latest_year))
})