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

  observations <- build_normalized_observations(inputs) |>
    dplyr::mutate(observation_id = seq_len(dplyr::n()), .before = 1)
  if (is.null(config$comparison_countries)) {
    config$comparison_countries <- unique(observations$country_code)
  }
  memberships <- build_segment_memberships(
    observations,
    include_crustacea_subset = config$include_crustacea_subset %||% FALSE
  )
  views <- build_production_views(observations, memberships, config)

  list(
    observations = observations,
    memberships = memberships,
    exports = views$exports,
    diagnostics = views$diagnostics
  )
}