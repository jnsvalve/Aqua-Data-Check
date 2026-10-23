build_low_anthropic_outputs <- function(inputs, include_crustacea_subset = FALSE) {
  mussel_pattern <- "mussel|oyster|clam|cockle|scallop|bivalv|mollusc|mollusk"
  seaweed_pattern <- "seaweed|algae|kelp|wakame|spirulina|ulva|lettuce"
  crustacea_pattern <- "shrimp|prawn|crab|lobster|crayfish|krill"

  eu_strict <- harmonise_eurostat_low_anthropic(
    inputs$eurostat,
    species_labels = inputs$eurostat_species_labels
  ) |>
    dplyr::mutate(mode = "strict")

  eu_broad <- eu_strict |>
    dplyr::mutate(
      mode = "broad",
      production_type = dplyr::case_when(
        production_type == "mussels_bivalves" ~ "mollusca_all",
        production_type == "seaweed_algae" ~ "aquatic_plants_all",
        TRUE ~ production_type
      )
    )

  fao_quantity <- inputs$fao_quantity |>
    dplyr::transmute(
      country_un_code = as.character(country_un_code),
      year = as.integer(period),
      species_alpha_3_code,
      source = "fao_quantity",
      measure = "production_tonnes",
      values = as.numeric(value)
    )

  fao_value_usd <- inputs$fao_value |>
    dplyr::transmute(
      country_un_code = as.character(country_un_code),
      year = as.integer(period),
      species_alpha_3_code,
      values = as.numeric(value) * 1000,
      unit = "USD",
      measure = "production_value_usd",
      source = "fao_value"
    )
  fao_value <- convert_fao_usd_values(fao_value_usd, inputs$fx_rates)

  fao_records <- dplyr::bind_rows(fao_quantity, fao_value) |>
    dplyr::left_join(inputs$fao_species, by = "species_alpha_3_code") |>
    dplyr::mutate(
      species_name_en = tolower(species_name_en %||% ""),
      country_code = resolve_fao_country_code(country_un_code, inputs$fao_to_iso2)
    )

  fao_strict <- fao_records |>
    dplyr::mutate(
      production_type = dplyr::case_when(
        stringr::str_detect(species_name_en, mussel_pattern) ~ "mussels_bivalves",
        major_group == "PLANTAE AQUATICAE" |
          stringr::str_detect(species_name_en, seaweed_pattern) ~ "seaweed_algae",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(production_type)) |>
    dplyr::transmute(
      country_code, year, production_type, source, measure, values,
      mode = "strict"
    )

  fao_broad <- fao_records |>
    dplyr::mutate(
      production_type = dplyr::case_when(
        major_group == "MOLLUSCA" ~ "mollusca_all",
        major_group == "PLANTAE AQUATICAE" ~ "aquatic_plants_all",
        include_crustacea_subset & major_group == "CRUSTACEA" &
          stringr::str_detect(species_name_en, crustacea_pattern) ~ "crustacea_subset",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(production_type)) |>
    dplyr::transmute(
      country_code, year, production_type, source, measure, values,
      mode = "broad"
    )

  eumo_base <- inputs$eumofa |>
    dplyr::transmute(
      country_code = toupper(country),
      year = as.integer(year),
      species_txt = tolower(main_commercial_species %||% ""),
      commodity_txt = tolower(commodity_group %||% ""),
      volume_tonnes = as.numeric(volume_kg) / 1000,
      value_eur = as.numeric(value_eur)
    ) |>
    dplyr::mutate(
      strict_type = dplyr::case_when(
        stringr::str_detect(species_txt, mussel_pattern) |
          stringr::str_detect(commodity_txt, "bivalv|mollusc|mollusk") ~ "mussels_bivalves",
        stringr::str_detect(species_txt, seaweed_pattern) |
          stringr::str_detect(commodity_txt, "seaweed|algae") ~ "seaweed_algae",
        TRUE ~ NA_character_
      ),
      broad_type = dplyr::case_when(
        stringr::str_detect(species_txt, "mollusc|mollusk|bivalv|oyster|clam|cockle|scallop|mussel") |
          stringr::str_detect(commodity_txt, "mollusc|mollusk|bivalv") ~ "mollusca_all",
        stringr::str_detect(species_txt, seaweed_pattern) |
          stringr::str_detect(commodity_txt, "seaweed|algae") ~ "aquatic_plants_all",
        include_crustacea_subset & (
          stringr::str_detect(species_txt, crustacea_pattern) |
            stringr::str_detect(commodity_txt, crustacea_pattern)
        ) ~ "crustacea_subset",
        TRUE ~ NA_character_
      )
    )

  eumo_records <- eumo_base |>
    tidyr::pivot_longer(
      cols = c(volume_tonnes, value_eur),
      names_to = "measure",
      values_to = "values"
    ) |>
    dplyr::mutate(
      measure = dplyr::case_when(
        measure == "volume_tonnes" ~ "production_tonnes",
        measure == "value_eur" ~ "production_value_eur"
      ),
      source = "eumofa"
    ) |>
    tidyr::pivot_longer(
      cols = c(strict_type, broad_type),
      names_to = "mode",
      values_to = "production_type"
    ) |>
    dplyr::filter(!is.na(production_type)) |>
    dplyr::mutate(mode = dplyr::if_else(mode == "strict_type", "strict", "broad")) |>
    dplyr::select(country_code, year, production_type, source, measure, values, mode)

  low_anthropic_long <- dplyr::bind_rows(
    eu_strict,
    eu_broad,
    fao_strict,
    fao_broad,
    eumo_records
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