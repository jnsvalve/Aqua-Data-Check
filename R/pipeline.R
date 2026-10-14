build_pipeline_outputs <- function(inputs, config) {
  required_inputs <- c(
    "eurostat", "eurostat_species_labels", "fao_quantity", "fao_value",
    "fao_species", "fx_rates", "eumofa"
  )
  missing_inputs <- setdiff(required_inputs, names(inputs))
  if (length(missing_inputs) > 0) {
    stop("Missing pipeline input(s): ", paste(missing_inputs, collapse = ", "))
  }

  required_config <- c(
    "summary_years", "comparison_years", "comparison_countries",
    "europe_codes", "tolerance"
  )
  missing_config <- setdiff(required_config, names(config))
  if (length(missing_config) > 0) {
    stop("Missing pipeline configuration: ", paste(missing_config, collapse = ", "))
  }

  eu_long <- harmonise_eurostat(inputs$eurostat)
  fao_q_long <- harmonise_fao_qty(inputs$fao_quantity, inputs$fao_to_iso2)
  fao_v_long <- harmonise_fao_val(inputs$fao_value, inputs$fao_to_iso2)
  eumo_long <- harmonise_eumofa(inputs$eumofa)

  all_long <- bind_rows(eu_long, fao_q_long, fao_v_long, eumo_long) %>%
    arrange(country_code, year, measure, source)

  fao_v_long_fx <- convert_fao_usd_values(fao_v_long, inputs$fx_rates)

  all_long_fx <- bind_rows(all_long, fao_v_long_fx)

  low_anthropic <- build_low_anthropic_outputs(
    inputs,
    include_crustacea_subset = config$include_crustacea_subset %||% FALSE
  )

  unit_check <- all_long_fx %>%
    filter(measure == "production_tonnes") %>%
    group_by(country_code, year, source) %>%
    summarise(value_tonnes = sum(values, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      id_cols = c(country_code, year),
      names_from = source,
      values_from = value_tonnes
    )

  for (source_col in c("eurostat", "fao_quantity", "eumofa")) {
    if (!source_col %in% names(unit_check)) unit_check[[source_col]] <- NA_real_
  }

  unit_check <- unit_check %>%
    mutate(
      eurostat = coalesce(eurostat, 0),
      fao_quantity = coalesce(fao_quantity, 0),
      eumofa = coalesce(eumofa, 0)
    ) %>%
    mutate(
      suspicious_fao = (fao_quantity > 1e6) & (eurostat < 1e4),
      suspicious_eumo = (eumofa > 1e6) & (eurostat < 1e4)
    )

  summary_europe <- build_summary_table(
    all_long_fx,
    config$summary_years,
    config$europe_codes
  )
  freshwater_long <- filter_freshwater(all_long_fx)
  freshwater_summary <- build_summary_table(
    freshwater_long,
    config$summary_years,
    config$europe_codes
  )

  list(
    exports = list(
      general_summary = summary_europe,
      freshwater_summary = freshwater_summary,
      low_anthropic_by_mode = low_anthropic$by_mode,
      low_anthropic_side_by_side = low_anthropic$side_by_side,
      freshwater_consistency = build_fw_consistency(freshwater_summary)
    ),
    diagnostics = list(
      comparison_all = build_comparison(
        all_long_fx,
        years = config$comparison_years,
        countries = config$comparison_countries,
        tolerance = config$tolerance
      ),
      comparison_freshwater = build_comparison(
        freshwater_long,
        years = config$comparison_years,
        countries = config$comparison_countries,
        tolerance = config$tolerance
      ),
      unit_check = unit_check,
      suspicious = filter(unit_check, suspicious_fao | suspicious_eumo)
    )
  )
}