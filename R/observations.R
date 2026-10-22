observation_prototype <- function() {
  tibble::tibble(
    source = character(),
    native_country_code = character(),
    country_code = character(),
    year = integer(),
    measure = character(),
    native_measure = character(),
    native_value = numeric(),
    native_unit = character(),
    comparison_value = numeric(),
    comparison_unit = character(),
    conversion_factor = numeric(),
    fx_rate = numeric(),
    record_scope = character(),
    eurostat_frequency = character(),
    eurostat_unit_code = character(),
    eurostat_species_code = character(),
    eurostat_species_label = character(),
    eurostat_environment_code = character(),
    eurostat_fishreg = character(),
    eurostat_aquameth = character(),
    eurostat_flags = character(),
    fao_species_alpha_3_code = character(),
    fao_species_name_en = character(),
    fao_major_group = character(),
    fao_taxonomic_code = character(),
    fao_area_code = character(),
    fao_environment_alpha_2_code = character(),
    fao_status = character(),
    fao_measure = character(),
    eumofa_country = character(),
    eumofa_desc_country = character(),
    eumofa_commodity_group = character(),
    eumofa_main_commercial_species = character(),
    eumofa_ers_code = character(),
    eumofa_english_name = character(),
    eumofa_weight = character()
  )
}

optional_character <- function(data, column) {
  if (column %in% names(data)) as.character(data[[column]]) else rep(NA_character_, nrow(data))
}

optional_numeric <- function(data, column) {
  if (column %in% names(data)) suppressWarnings(as.numeric(data[[column]])) else rep(NA_real_, nrow(data))
}

build_normalized_observations <- function(inputs) {
  bind_rows(
    observation_prototype(),
    normalize_eurostat_observations(
      inputs$eurostat,
      inputs$eurostat_species_labels
    ),
    normalize_fao_quantity_observations(
      inputs$fao_quantity,
      inputs$fao_species,
      inputs$fao_to_iso2
    ),
    normalize_fao_value_observations(
      inputs$fao_value,
      inputs$fao_species,
      inputs$fao_to_iso2,
      inputs$fx_rates
    ),
    normalize_eumofa_observations(inputs$eumofa)
  )
}

normalize_eurostat_observations <- function(eurostat, species_labels) {
  if (nrow(eurostat) == 0) return(observation_prototype())

  time_column <- if ("time" %in% names(eurostat)) {
    "time"
  } else if ("time_period" %in% names(eurostat)) {
    "time_period"
  } else {
    stop("Eurostat: No time column found")
  }

  labels <- species_labels |>
    transmute(
      eurostat_species_code = as.character(species_code),
      eurostat_species_label = as.character(species_label)
    ) |>
    distinct(eurostat_species_code, .keep_all = TRUE)

  eurostat |>
    mutate(
      eurostat_species_code = as.character(species),
      eurostat_frequency = optional_character(eurostat, "freq"),
      eurostat_unit_code = as.character(unit),
      eurostat_environment_code = optional_character(eurostat, "aquaenv"),
      eurostat_fishreg = optional_character(eurostat, "fishreg"),
      eurostat_aquameth = optional_character(eurostat, "aquameth"),
      eurostat_flags = optional_character(eurostat, "flags")
    ) |>
    left_join(labels, by = "eurostat_species_code") |>
    transmute(
      source = "eurostat",
      native_country_code = as.character(geo),
      country_code = toupper(as.character(geo)),
      year = as.integer(.data[[time_column]]),
      measure = case_when(
        unit == "TLW" ~ "production_tonnes",
        unit == "EUR" ~ "production_value_eur",
        TRUE ~ NA_character_
      ),
      native_measure = as.character(unit),
      native_value = optional_numeric(eurostat, "values"),
      native_unit = as.character(unit),
      comparison_value = case_when(
        unit %in% c("TLW", "EUR") ~ optional_numeric(eurostat, "values"),
        TRUE ~ NA_real_
      ),
      comparison_unit = case_when(
        unit == "TLW" ~ "TONNES",
        unit == "EUR" ~ "EUR",
        TRUE ~ NA_character_
      ),
      conversion_factor = if_else(unit %in% c("TLW", "EUR"), 1, NA_real_),
      fx_rate = NA_real_,
      record_scope = if_else(
        species == "F00" & fishreg == "0" & freq == "A",
        "published_total",
        "source_category"
      ),
      eurostat_frequency,
      eurostat_unit_code,
      eurostat_species_code,
      eurostat_species_label,
      eurostat_environment_code,
      eurostat_fishreg,
      eurostat_aquameth,
      eurostat_flags
    )
}

prepare_fao_species <- function(fao_species) {
  if (!"taxonomic_code" %in% names(fao_species)) {
    fao_species$taxonomic_code <- NA_character_
  }
  fao_species |>
    transmute(
      fao_species_alpha_3_code = as.character(species_alpha_3_code),
      fao_species_name_en = as.character(species_name_en),
      fao_major_group = as.character(major_group),
      fao_taxonomic_code = as.character(taxonomic_code)
    ) |>
    distinct(fao_species_alpha_3_code, .keep_all = TRUE)
}

normalize_fao_quantity_observations <- function(fao_quantity, fao_species, country_map) {
  if (nrow(fao_quantity) == 0) return(observation_prototype())

  fao_quantity |>
    transmute(
      native_country_code = as.character(country_un_code),
      country_code = resolve_fao_country_code(country_un_code, country_map),
      year = as.integer(period),
      fao_species_alpha_3_code = as.character(species_alpha_3_code),
      fao_area_code = optional_character(fao_quantity, "area_code"),
      fao_environment_alpha_2_code = optional_character(fao_quantity, "environment_alpha_2_code"),
      fao_status = optional_character(fao_quantity, "status"),
      fao_measure = optional_character(fao_quantity, "measure"),
      native_value = optional_numeric(fao_quantity, "value")
    ) |>
    left_join(prepare_fao_species(fao_species), by = "fao_species_alpha_3_code") |>
    transmute(
      source = "fao_quantity",
      native_country_code,
      country_code,
      year,
      measure = "production_tonnes",
      native_measure = fao_measure,
      native_value,
      native_unit = "TONNES",
      comparison_value = native_value,
      comparison_unit = "TONNES",
      conversion_factor = 1,
      fx_rate = NA_real_,
      record_scope = "source_category",
      fao_species_alpha_3_code,
      fao_species_name_en,
      fao_major_group,
      fao_taxonomic_code,
      fao_area_code,
      fao_environment_alpha_2_code,
      fao_status,
      fao_measure
    )
}

normalize_fao_value_observations <- function(fao_value, fao_species, country_map, fx_rates) {
  if (nrow(fao_value) == 0) return(observation_prototype())

  rate_table <- fx_rates |>
    transmute(year = as.integer(year), fx_rate = as.numeric(eur_per_usd)) |>
    distinct(year, .keep_all = TRUE)

  fao_value |>
    transmute(
      native_country_code = as.character(country_un_code),
      country_code = resolve_fao_country_code(country_un_code, country_map),
      year = as.integer(period),
      fao_species_alpha_3_code = as.character(species_alpha_3_code),
      fao_area_code = optional_character(fao_value, "area_code"),
      fao_environment_alpha_2_code = optional_character(fao_value, "environment_alpha_2_code"),
      fao_status = optional_character(fao_value, "status"),
      fao_measure = optional_character(fao_value, "measure"),
      native_value = optional_numeric(fao_value, "value")
    ) |>
    left_join(prepare_fao_species(fao_species), by = "fao_species_alpha_3_code") |>
    left_join(rate_table, by = "year") |>
    transmute(
      source = "fao_value",
      native_country_code,
      country_code,
      year,
      measure = "production_value_eur",
      native_measure = fao_measure,
      native_value,
      native_unit = "THOUSAND_USD",
      comparison_value = native_value * 1000 * fx_rate,
      comparison_unit = "EUR",
      conversion_factor = 1000 * fx_rate,
      fx_rate,
      record_scope = "source_category",
      fao_species_alpha_3_code,
      fao_species_name_en,
      fao_major_group,
      fao_taxonomic_code,
      fao_area_code,
      fao_environment_alpha_2_code,
      fao_status,
      fao_measure
    )
}

normalize_eumofa_observations <- function(eumofa) {
  if (nrow(eumofa) == 0) return(observation_prototype())

  common <- eumofa |>
    transmute(
      native_country_code = as.character(country),
      country_code = toupper(as.character(country)),
      year = as.integer(year),
      eumofa_country = as.character(country),
      eumofa_desc_country = optional_character(eumofa, "desc_country"),
      eumofa_commodity_group = optional_character(eumofa, "commodity_group"),
      eumofa_main_commercial_species = optional_character(eumofa, "main_commercial_species"),
      eumofa_ers_code = optional_character(eumofa, "ers_code"),
      eumofa_english_name = optional_character(eumofa, "english_name"),
      eumofa_weight = optional_character(eumofa, "weight"),
      volume_kg = optional_numeric(eumofa, "volume_kg"),
      value_eur = optional_numeric(eumofa, "value_eur")
    )

  volume_observations <- common |>
    transmute(
      source = "eumofa",
      native_country_code,
      country_code,
      year,
      measure = "production_tonnes",
      native_measure = "volume_kg",
      native_value = volume_kg,
      native_unit = "KG",
      comparison_value = volume_kg / 1000,
      comparison_unit = "TONNES",
      conversion_factor = 0.001,
      fx_rate = NA_real_,
      record_scope = "source_category",
      eumofa_country,
      eumofa_desc_country,
      eumofa_commodity_group,
      eumofa_main_commercial_species,
      eumofa_ers_code,
      eumofa_english_name,
      eumofa_weight
    )

  value_observations <- common |>
    transmute(
      source = "eumofa",
      native_country_code,
      country_code,
      year,
      measure = "production_value_eur",
      native_measure = "value_eur",
      native_value = value_eur,
      native_unit = "EUR",
      comparison_value = value_eur,
      comparison_unit = "EUR",
      conversion_factor = 1,
      fx_rate = NA_real_,
      record_scope = "source_category",
      eumofa_country,
      eumofa_desc_country,
      eumofa_commodity_group,
      eumofa_main_commercial_species,
      eumofa_ers_code,
      eumofa_english_name,
      eumofa_weight
    )

  bind_rows(volume_observations, value_observations)
}

write_normalized_snapshot <- function(bundle, output_dir) {
  if (!is.list(bundle) || !is.data.frame(bundle$observations) || is.null(bundle$manifest)) {
    stop("A normalized snapshot must contain manifest and observations")
  }

  coverage_status <- bundle$manifest$coverage_status
  if (!coverage_status %in% c("complete", "partial")) {
    stop("Snapshot manifest coverage_status must be complete or partial")
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  filename <- if (coverage_status == "complete") {
    "aquaculture_observations_latest.rds"
  } else {
    "aquaculture_observations_partial_latest.rds"
  }
  target_path <- file.path(output_dir, filename)
  staged_path <- tempfile(".normalized-snapshot-", tmpdir = output_dir, fileext = ".rds")
  on.exit(unlink(staged_path), add = TRUE)
  saveRDS(bundle, staged_path, compress = "gzip")

  previous_path <- NULL
  if (file.exists(target_path)) {
    previous_path <- tempfile(".previous-snapshot-", tmpdir = output_dir)
    if (!file.rename(target_path, previous_path)) {
      stop("Could not stage the previous normalized snapshot")
    }
  }

  if (!file.rename(staged_path, target_path)) {
    if (!is.null(previous_path)) file.rename(previous_path, target_path)
    stop("Could not publish the normalized snapshot")
  }
  if (!is.null(previous_path)) unlink(previous_path)

  invisible(target_path)
}

observation_file_provenance <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(list(
      path = if (length(path)) as.character(path) else NA_character_,
      available = FALSE,
      md5 = NA_character_,
      bytes = NA_real_
    ))
  }

  list(
    path = as.character(path),
    available = TRUE,
    md5 = unname(tools::md5sum(path)[[1]]),
    bytes = as.numeric(file.info(path)$size)
  )
}

build_observation_manifest <- function(source_status,
                                       eurostat,
                                       input_files,
                                       fx,
                                       segment_rules_version = NA_character_,
                                       generated_at_utc = NULL) {
  source_status <- unlist(source_status, use.names = TRUE)
  if (is.null(names(source_status)) || any(!nzchar(names(source_status)))) {
    stop("Source status must be a named logical vector")
  }
  if (!is.logical(source_status) || anyNA(source_status)) {
    stop("Source status values must be non-missing logicals")
  }

  if (is.null(generated_at_utc)) {
    generated_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  list(
    schema_version = "1.0.0",
    segment_rules_version = segment_rules_version,
    generated_at_utc = generated_at_utc,
    coverage_status = if (all(source_status)) "complete" else "partial",
    source_status = as.list(source_status),
    eurostat = eurostat,
    input_files = lapply(input_files, observation_file_provenance),
    fx = list(
      source = fx$source,
      file = observation_file_provenance(fx$path),
      rates = fx$rates
    )
  )
}