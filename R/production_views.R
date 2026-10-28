build_fao_low_anthropic_countries <- function(observations, memberships, years,
                                              profile = "strict", top_n = 10L,
                                              continent = NULL,
                                              species_classification = NULL) {
  profile <- match.arg(profile, c("strict", "broad"))
  if (!is.null(continent)) {
    continent <- match.arg(continent, c("Africa", "Americas", "Asia", "Europe", "Oceania", "Antarctica"))
  }
  if (length(top_n) != 1L || is.na(top_n) || top_n < 1 || top_n != as.integer(top_n)) {
    stop("top_n must be a positive integer", call. = FALSE)
  }
  years <- sort(unique(as.integer(years)))
  if (length(years) < 3L || anyNA(years)) {
    stop("Provide at least three years", call. = FALSE)
  }
  selected_memberships <- memberships |>
    dplyr::filter(segment == "low_anthropic", .data$profile == .env$profile) |>
    dplyr::distinct(observation_id, category)
  if (anyDuplicated(selected_memberships$observation_id)) {
    stop("Low-anthropic categories must not overlap within a profile", call. = FALSE)
  }
  if (!is.null(species_classification)) {
    required_classification <- c("species_alpha_3_code", "isscaap_group_en")
    if (!all(required_classification %in% names(species_classification))) {
      stop("species_classification must include species_alpha_3_code and isscaap_group_en", call. = FALSE)
    }
    species_classification <- species_classification |>
      dplyr::transmute(
        fao_species_alpha_3_code = as.character(species_alpha_3_code),
        fao_isscaap_group_en = as.character(isscaap_group_en)
      ) |>
      dplyr::filter(!is.na(fao_species_alpha_3_code), !is.na(fao_isscaap_group_en)) |>
      dplyr::distinct()
    conflicting_groups <- species_classification |>
      dplyr::count(fao_species_alpha_3_code) |>
      dplyr::filter(n > 1L)
    if (nrow(conflicting_groups) > 0) {
      stop("FAO species codes must map to only one ISSCAAP group", call. = FALSE)
    }
    observations <- observations |>
      dplyr::select(-dplyr::any_of("fao_isscaap_group_en")) |>
      dplyr::left_join(species_classification, by = "fao_species_alpha_3_code")
  } else if (!"fao_isscaap_group_en" %in% names(observations)) {
    observations$fao_isscaap_group_en <- NA_character_
  }
  annual <- observations |>
    dplyr::filter(
      source %in% c("fao_quantity", "fao_value"), year %in% years,
      !is.na(country_code), grepl("^[A-Z]{2}$", country_code)
    ) |>
    dplyr::inner_join(selected_memberships, by = "observation_id") |>
    dplyr::mutate(
      category = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(fao_isscaap_group_en), ""),
        category
      )
    ) |>
    dplyr::group_by(country_code, year, measure, category) |>
    dplyr::summarise(
      value = if (any(!is.finite(comparison_value))) NA_real_ else sum(comparison_value),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      value = ifelse(measure == "production_value_eur", value / 1000, value),
      continent = countrycode::countrycode(country_code, "iso2c", "continent", warn = FALSE)
    )
  if (!is.null(continent)) {
    annual <- annual |> dplyr::filter(.data$continent == .env$continent)
  }
  totals <- annual |>
    dplyr::group_by(country_code, year, measure) |>
    dplyr::summarise(
      value = if (any(!is.finite(value))) NA_real_ else sum(value),
      .groups = "drop"
    )
  common_years <- intersect(
    totals$year[totals$measure == "production_tonnes" & is.finite(totals$value)],
    totals$year[totals$measure == "production_value_eur" & is.finite(totals$value)]
  )
  ranking_years <- if (length(common_years)) seq.int(max(common_years) - 2L, max(common_years)) else integer()
  averages <- totals |>
    dplyr::filter(year %in% ranking_years, is.finite(value)) |>
    dplyr::group_by(country_code, measure) |>
    dplyr::filter(dplyr::n() == 3L) |>
    dplyr::summarise(average = mean(value), .groups = "drop")
  ranking <- averages |>
    dplyr::filter(measure == "production_tonnes") |>
    dplyr::transmute(country_code, average_tonnes = average) |>
    dplyr::inner_join(
      averages |>
        dplyr::filter(measure == "production_value_eur") |>
        dplyr::transmute(country_code, average_thousand_eur = average),
      by = "country_code"
    ) |>
    dplyr::filter(average_tonnes > 0, average_thousand_eur > 0) |>
    dplyr::mutate(
      quantity_share = average_tonnes / sum(average_tonnes),
      value_share = average_thousand_eur / sum(average_thousand_eur),
      joint_score = sqrt(quantity_share * value_share)
    ) |>
    dplyr::arrange(dplyr::desc(joint_score), country_code) |>
    dplyr::mutate(
      rank = dplyr::row_number(), selected = rank <= top_n,
      ranking_start = if (length(ranking_years)) min(ranking_years) else NA_integer_,
      ranking_end = if (length(ranking_years)) max(ranking_years) else NA_integer_,
      profile = .env$profile
    ) |>
    dplyr::left_join(dplyr::distinct(annual, country_code, continent), by = "country_code")
  list(
    ranking = ranking,
    annual_data = annual |> dplyr::filter(country_code %in% ranking$country_code[ranking$selected]),
    years = years, profile = profile, continent = continent
  )
}

production_view_columns <- c(
  "eurostat_production_tonnes",
  "fao_quantity_production_tonnes",
  "eumofa_production_tonnes",
  "eurostat_production_value_eur",
  "eumofa_production_value_eur",
  "fao_eur_production_value_eur"
)

production_source_rows <- function(observations) {
  eurostat <- observations |>
    dplyr::filter(
      source == "eurostat",
      record_scope == "published_total",
      !is.na(measure)
    ) |>
    dplyr::transmute(
      country_code,
      year,
      measure,
      values = comparison_value,
      unit = comparison_unit,
      source = "eurostat",
      environment = eurostat_environment_code,
      commodity_group = NA_character_
    )

  fao_quantity <- observations |>
    dplyr::filter(source == "fao_quantity") |>
    dplyr::transmute(
      country_code,
      year,
      measure,
      values = comparison_value,
      unit = comparison_unit,
      source = "fao_quantity",
      environment = fao_environment_alpha_2_code,
      commodity_group = NA_character_
    )

  fao_value_usd <- observations |>
    dplyr::filter(source == "fao_value") |>
    dplyr::transmute(
      country_code,
      year,
      measure = "production_value_usd",
      values = native_value * 1000,
      unit = "USD",
      source = "fao_value",
      environment = fao_environment_alpha_2_code,
      commodity_group = NA_character_
    )

  fao_value_eur <- observations |>
    dplyr::filter(source == "fao_value") |>
    dplyr::transmute(
      country_code,
      year,
      measure,
      values = comparison_value,
      unit = comparison_unit,
      source = "fao_eur",
      environment = fao_environment_alpha_2_code,
      commodity_group = NA_character_
    )

  eumofa <- observations |>
    dplyr::filter(source == "eumofa", !is.na(comparison_value)) |>
    dplyr::transmute(
      country_code,
      year,
      measure,
      values = comparison_value,
      unit = comparison_unit,
      source = "eumofa",
      environment = NA_character_,
      commodity_group = eumofa_commodity_group
    )

  dplyr::bind_rows(eurostat, fao_quantity, fao_value_usd, fao_value_eur, eumofa) |>
    dplyr::arrange(country_code, year, measure, source)
}

filter_freshwater_production <- function(long_table) {
  eurostat <- long_table |>
    dplyr::filter(source == "eurostat", environment == "FRW")
  fao <- long_table |>
    dplyr::filter(source %in% c("fao_quantity", "fao_value", "fao_eur"), environment == "IN")
  eumofa <- long_table |>
    dplyr::filter(source == "eumofa") |>
    dplyr::filter(
      is.na(commodity_group) |
        stringr::str_detect(tolower(commodity_group), "fresh|inland")
    )

  dplyr::bind_rows(eurostat, fao, eumofa) |>
    dplyr::arrange(country_code, year, measure, source)
}

build_production_summary <- function(long_table, years, country_codes) {
  totals_wide <- long_table |>
    dplyr::filter(year %in% years) |>
    dplyr::mutate(values = as.numeric(values)) |>
    dplyr::group_by(country_code, year, source, measure) |>
    dplyr::summarise(values = sum(values, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      id_cols = c(country_code, year),
      names_from = c(source, measure),
      values_from = values
    )

  for (column in production_view_columns) {
    if (!column %in% names(totals_wide)) totals_wide[[column]] <- NA_real_
  }

  totals_wide |>
    dplyr::transmute(
      country_code,
      year,
      eurostat_production = eurostat_production_tonnes,
      fao_production = fao_quantity_production_tonnes,
      eumofa_production = eumofa_production_tonnes,
      eurostat_value = eurostat_production_value_eur,
      fao_value = fao_eur_production_value_eur,
      eumofa_value = eumofa_production_value_eur
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 0))) |>
    dplyr::filter(country_code %in% country_codes) |>
    dplyr::group_by(country_code) |>
    dplyr::filter(any(
      !is.na(eurostat_production) |
        !is.na(fao_production) |
        !is.na(eumofa_production) |
        !is.na(eurostat_value) |
        !is.na(fao_value) |
        !is.na(eumofa_value)
    )) |>
    dplyr::ungroup() |>
    dplyr::arrange(country_code, year)
}

build_production_comparison <- function(long_table,
                                        years = NULL,
                                        countries = NULL,
                                        tolerance = 0.30) {
  comparison <- long_table
  if (!is.null(years)) comparison <- comparison |> dplyr::filter(year %in% years)
  if (!is.null(countries)) comparison <- comparison |> dplyr::filter(country_code %in% countries)

  comparison <- comparison |>
    dplyr::mutate(values = as.numeric(values)) |>
    dplyr::group_by(country_code, year, source, measure) |>
    dplyr::summarise(values = sum(values, na.rm = TRUE), .groups = "drop")

  comparison_wide <- comparison |>
    tidyr::pivot_wider(
      id_cols = c(country_code, year),
      names_from = c(source, measure),
      values_from = values
    ) |>
    dplyr::arrange(country_code, year)

  for (column in production_view_columns) {
    if (!column %in% names(comparison_wide)) comparison_wide[[column]] <- NA_real_
  }

  if (all(is.na(comparison_wide[["fao_eur_production_value_eur"]]))) {
    cli::cli_alert_warning("build_comparison: 'fao_eur_production_value_eur' is all-NA \u2014 FAO USD\u2192EUR conversion may not have run")
  }

  comparison_wide |>
    dplyr::mutate(
      ratio_es_fao_qty = eurostat_production_tonnes / fao_quantity_production_tonnes,
      ratio_es_eumo_qty = eurostat_production_tonnes / eumofa_production_tonnes,
      ratio_es_fao_val = eurostat_production_value_eur / fao_eur_production_value_eur,
      ratio_es_eumo_val = eurostat_production_value_eur / eumofa_production_value_eur
    ) |>
    dplyr::mutate(
      flag_es_fao_qty = dplyr::case_when(
        is.na(ratio_es_fao_qty) ~ NA_character_,
        abs(ratio_es_fao_qty - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      ),
      flag_es_eumo_qty = dplyr::case_when(
        is.na(ratio_es_eumo_qty) ~ NA_character_,
        abs(ratio_es_eumo_qty - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      ),
      flag_es_fao_val = dplyr::case_when(
        is.na(ratio_es_fao_val) ~ NA_character_,
        abs(ratio_es_fao_val - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      ),
      flag_es_eumo_val = dplyr::case_when(
        is.na(ratio_es_eumo_val) ~ NA_character_,
        abs(ratio_es_eumo_val - 1) > tolerance ~ "CHECK",
        TRUE ~ ""
      )
    )
}

build_production_comparison_table <- function(observations,
                                              years,
                                              countries,
                                              memberships = NULL,
                                              membership_dimensions = character()) {
  scoped_observations <- observations |>
    dplyr::filter(year %in% years, country_code %in% countries)
  if (!is.null(memberships)) {
    scoped_observations <- scoped_observations |>
      dplyr::inner_join(memberships, by = "observation_id")
  }
  scope_columns <- c(membership_dimensions, "country_code", "year")

  eurostat_totals <- scoped_observations |>
    dplyr::filter(source == "eurostat", record_scope == "published_total") |>
    dplyr::distinct(country_code, year, measure)
  eurostat_observations <- scoped_observations |>
    dplyr::filter(source == "eurostat")
  eurostat_categories <- eurostat_observations |>
    dplyr::filter(record_scope != "published_total") |>
    dplyr::anti_join(eurostat_totals, by = c("country_code", "year", "measure"))

  comparison_values <- dplyr::bind_rows(
    dplyr::filter(scoped_observations, source != "eurostat"),
    dplyr::filter(eurostat_observations, record_scope == "published_total"),
    eurostat_categories
  ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(scope_columns, "source", "measure")))) |>
    dplyr::summarise(
      values = if (all(is.na(comparison_value))) NA_real_ else sum(comparison_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(scope_columns),
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

  comparison_values |>
    dplyr::select(
      dplyr::all_of(scope_columns),
      eurostat_production = eurostat_production_tonnes,
      fao_production = fao_quantity_production_tonnes,
      eumofa_production = eumofa_production_tonnes,
      eurostat_value = eurostat_production_value_eur,
      fao_value = fao_value_production_value_eur,
      eumofa_value = eumofa_production_value_eur
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(scope_columns)))
}

build_low_anthropic_views <- function(observations, memberships) {
  low_anthropic_long <- observations |>
    dplyr::inner_join(
      memberships |>
        dplyr::filter(segment == "low_anthropic") |>
        dplyr::select(observation_id, profile, category),
      by = "observation_id"
    ) |>
    dplyr::filter(!is.na(measure)) |>
    dplyr::transmute(
      mode = profile,
      country_code,
      year,
      production_type = category,
      source = dplyr::case_when(
        source == "eurostat" ~ "eurostat_species",
        source == "fao_value" ~ "fao_eur",
        TRUE ~ source
      ),
      measure,
      values = comparison_value
    ) |>
    dplyr::group_by(mode, country_code, year, production_type, source, measure) |>
    dplyr::summarise(values = sum(values, na.rm = TRUE), .groups = "drop")

  by_mode <- low_anthropic_long |>
    tidyr::pivot_wider(
      id_cols = c(mode, country_code, year, production_type),
      names_from = c(source, measure),
      values_from = values
    ) |>
    dplyr::arrange(mode, country_code, year, production_type)

  side_by_side <- low_anthropic_long |>
    dplyr::group_by(mode, country_code, year, source, measure) |>
    dplyr::summarise(values = sum(values, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      id_cols = c(country_code, year, source, measure),
      names_from = mode,
      values_from = values
    ) |>
    dplyr::arrange(country_code, year, source, measure)

  list(by_mode = by_mode, side_by_side = side_by_side)
}

build_production_comparison_views <- function(observations, memberships, years, countries) {
  tables <- list(
    all_production = build_production_comparison_table(
      observations, years = years, countries = countries
    ),
    freshwater = build_production_comparison_table(
      observations,
      years = years,
      countries = countries,
      memberships = memberships |> dplyr::filter(segment == "freshwater") |> dplyr::select(observation_id)
    ),
    low_anthropic = build_production_comparison_table(
      observations,
      years = years,
      countries = countries,
      memberships = memberships |>
        dplyr::filter(segment == "low_anthropic") |>
        dplyr::select(observation_id, profile, category),
      membership_dimensions = c("profile", "category")
    )
  )
  plots <- list()
  plot_measures <- c(production_tonnes = "quantity", production_value_eur = "value")
  for (segment in names(tables)) {
    for (measure in names(plot_measures)) {
      plot_name <- paste(segment, plot_measures[[measure]], sep = "_")
      plots[[plot_name]] <- build_comparison_plot(tables[[segment]], measure, segment)
    }
  }

  list(tables = tables, plots = plots)
}

build_freshwater_consistency <- function(freshwater_summary) {
  freshwater_summary |>
    dplyr::mutate(
      ratio_es_fao = ifelse(!is.na(fao_production) & fao_production > 0,
                            eurostat_production / fao_production, NA_real_),
      ratio_es_eumo = ifelse(!is.na(eumofa_production) & eumofa_production > 0,
                             eurostat_production / eumofa_production, NA_real_),
      logdiff_es_fao = log1p(eurostat_production) - log1p(dplyr::coalesce(fao_production, 0)),
      logdiff_es_eumo = log1p(eurostat_production) - log1p(dplyr::coalesce(eumofa_production, 0)),
      flag_large_gap_fao = ifelse(!is.na(ratio_es_fao) & ratio_es_fao > 5, TRUE, FALSE),
      flag_large_gap_eumo = ifelse(!is.na(ratio_es_eumo) & ratio_es_eumo > 5, TRUE, FALSE),
      notes = dplyr::case_when(
        is.na(eurostat_production) & !is.na(fao_production) ~ "Eurostat missing, FAO present",
        is.na(fao_production) & !is.na(eurostat_production) ~ "FAO missing, Eurostat present",
        is.na(eumofa_production) & !is.na(eurostat_production) ~ "EUMOFA missing, Eurostat present",
        ratio_es_eumo > 5 ~ "Eurostat >> EUMOFA (normal for pond countries)",
        ratio_es_eumo < 0.2 ~ "Eurostat << EUMOFA (check)",
        TRUE ~ ""
      )
    )
}

build_production_views <- function(observations, memberships, config) {
  long_table <- production_source_rows(observations)
  freshwater_long <- filter_freshwater_production(long_table)
  general_summary <- build_production_summary(
    long_table, config$summary_years, config$europe_codes
  )
  freshwater_summary <- build_production_summary(
    freshwater_long, config$summary_years, config$europe_codes
  )
  low_anthropic <- build_low_anthropic_views(observations, memberships)
  comparisons <- build_production_comparison_views(
    observations,
    memberships,
    years = config$comparison_years,
    countries = config$comparison_countries
  )

  unit_check <- long_table |>
    dplyr::filter(measure == "production_tonnes") |>
    dplyr::group_by(country_code, year, source) |>
    dplyr::summarise(value_tonnes = sum(values, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      id_cols = c(country_code, year),
      names_from = source,
      values_from = value_tonnes
    )
  for (source in c("eurostat", "fao_quantity", "eumofa")) {
    if (!source %in% names(unit_check)) unit_check[[source]] <- NA_real_
  }
  unit_check <- unit_check |>
    dplyr::mutate(
      eurostat = dplyr::coalesce(eurostat, 0),
      fao_quantity = dplyr::coalesce(fao_quantity, 0),
      eumofa = dplyr::coalesce(eumofa, 0)
    ) |>
    dplyr::mutate(
      suspicious_fao = (fao_quantity > 1e6) & (eurostat < 1e4),
      suspicious_eumo = (eumofa > 1e6) & (eurostat < 1e4)
    )

  list(
    exports = list(
      general_summary = general_summary,
      freshwater_summary = freshwater_summary,
      low_anthropic_by_mode = low_anthropic$by_mode,
      low_anthropic_side_by_side = low_anthropic$side_by_side,
      freshwater_consistency = build_freshwater_consistency(freshwater_summary),
      comparison_tables = comparisons$tables,
      comparison_plots = comparisons$plots
    ),
    diagnostics = list(
      comparison_all = build_production_comparison(
        long_table,
        years = config$comparison_years,
        countries = config$comparison_countries,
        tolerance = config$tolerance
      ),
      comparison_freshwater = build_production_comparison(
        freshwater_long,
        years = config$comparison_years,
        countries = config$comparison_countries,
        tolerance = config$tolerance
      ),
      unit_check = unit_check,
      suspicious = dplyr::filter(unit_check, suspicious_fao | suspicious_eumo)
    )
  )
}
