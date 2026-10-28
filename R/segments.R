SEGMENT_RULES_VERSION <- "1.0.0"

segment_membership_prototype <- function() {
  tibble::tibble(
    observation_id = integer(),
    segment = character(),
    profile = character(),
    category = character(),
    matched_by = character()
  )
}

segment_membership_rows <- function(observations, segment, profile, category, matched_by) {
  keep <- !is.na(matched_by)
  if (!any(keep)) return(segment_membership_prototype())

  tibble::tibble(
    observation_id = observations$observation_id[keep],
    segment = rep(segment, sum(keep)),
    profile = profile[keep],
    category = category[keep],
    matched_by = matched_by[keep]
  )
}

build_segment_memberships <- function(observations,
                                      include_crustacea_subset = FALSE) {
  if (!is.data.frame(observations)) {
    stop("Observations must be a data frame")
  }
  if (!"observation_id" %in% names(observations)) {
    stop("Observations must include observation_id")
  }
  if (anyNA(observations$observation_id) || anyDuplicated(observations$observation_id)) {
    stop("Observation IDs must be unique and non-missing within the snapshot")
  }
  if (!is.logical(include_crustacea_subset) || length(include_crustacea_subset) != 1L ||
      is.na(include_crustacea_subset)) {
    stop("include_crustacea_subset must be one non-missing logical value")
  }

  observations <- observations |>
    dplyr::mutate(
      source = as.character(source),
      eurostat_species_label_text = tolower(dplyr::coalesce(eurostat_species_label, "")),
      fao_species_name_text = tolower(dplyr::coalesce(fao_species_name_en, "")),
      eumofa_species_text = tolower(dplyr::coalesce(eumofa_main_commercial_species, "")),
      eumofa_commodity_text = tolower(dplyr::coalesce(eumofa_commodity_group, ""))
    )
  observation_source <- observations$source

  freshwater_reason <- dplyr::case_when(
    observation_source == "eurostat" & observations$eurostat_environment_code == "FRW" ~ "eurostat_environment_code",
    observation_source %in% c("fao_quantity", "fao_value") & observations$fao_environment_alpha_2_code == "IN" ~ "fao_environment_alpha_2_code",
    observation_source == "eumofa" & stringr::str_detect(observations$eumofa_commodity_text, "fresh|inland") ~ "eumofa_commodity_group",
    TRUE ~ NA_character_
  )
  freshwater_memberships <- segment_membership_rows(
    observations,
    segment = "freshwater",
    profile = rep(NA_character_, nrow(observations)),
    category = rep(NA_character_, nrow(observations)),
    matched_by = freshwater_reason
  )

  mussel_pattern <- "mussel|oyster|clam|cockle|scallops?\\b|bivalv|mollusc|mollusk"
  seaweed_pattern <- "seaweed|algae|kelp|wakame|spirulina|ulva|lettuce"
  crustacea_pattern <- "shrimp|prawn|crab|lobster|crayfish|krill"
  fao_source <- observation_source %in% c("fao_quantity", "fao_value")

  eurostat_detail <- observation_source == "eurostat" & observations$record_scope == "source_category" &
    observations$eurostat_frequency == "A" & observations$eurostat_fishreg == "0"
  strict_category <- dplyr::case_when(
    eurostat_detail & stringr::str_detect(observations$eurostat_species_label_text, mussel_pattern) ~ "mussels_bivalves",
    eurostat_detail & stringr::str_detect(observations$eurostat_species_label_text, seaweed_pattern) ~ "seaweed_algae",
    fao_source & stringr::str_detect(observations$fao_species_name_text, mussel_pattern) ~ "mussels_bivalves",
    fao_source & (observations$fao_major_group == "PLANTAE AQUATICAE" | stringr::str_detect(observations$fao_species_name_text, seaweed_pattern)) ~ "seaweed_algae",
    observation_source == "eumofa" & (stringr::str_detect(observations$eumofa_species_text, mussel_pattern) | stringr::str_detect(observations$eumofa_commodity_text, "bivalv|mollusc|mollusk")) ~ "mussels_bivalves",
    observation_source == "eumofa" & (stringr::str_detect(observations$eumofa_species_text, seaweed_pattern) | stringr::str_detect(observations$eumofa_commodity_text, "seaweed|algae")) ~ "seaweed_algae",
    TRUE ~ NA_character_
  )
  strict_match <- dplyr::case_when(
    !is.na(strict_category) & observation_source == "eurostat" ~ "eurostat_species_label",
    !is.na(strict_category) & fao_source ~ "fao_species_name_or_major_group",
    !is.na(strict_category) & observation_source == "eumofa" ~ "eumofa_species_or_commodity",
    TRUE ~ NA_character_
  )
  strict_memberships <- segment_membership_rows(
    observations,
    segment = "low_anthropic",
    profile = rep("strict", nrow(observations)),
    category = strict_category,
    matched_by = strict_match
  )

  broad_category <- dplyr::case_when(
    observation_source == "eurostat" & strict_category == "mussels_bivalves" ~ "mollusca_all",
    observation_source == "eurostat" & strict_category == "seaweed_algae" ~ "aquatic_plants_all",
    fao_source & observations$fao_major_group == "MOLLUSCA" ~ "mollusca_all",
    fao_source & observations$fao_major_group == "PLANTAE AQUATICAE" ~ "aquatic_plants_all",
    fao_source & include_crustacea_subset & observations$fao_major_group == "CRUSTACEA" & stringr::str_detect(observations$fao_species_name_text, crustacea_pattern) ~ "crustacea_subset",
    observation_source == "eumofa" & (stringr::str_detect(observations$eumofa_species_text, "mollusc|mollusk|bivalv|oyster|clam|cockle|scallop|mussel") | stringr::str_detect(observations$eumofa_commodity_text, "mollusc|mollusk|bivalv")) ~ "mollusca_all",
    observation_source == "eumofa" & (stringr::str_detect(observations$eumofa_species_text, seaweed_pattern) | stringr::str_detect(observations$eumofa_commodity_text, "seaweed|algae")) ~ "aquatic_plants_all",
    observation_source == "eumofa" & include_crustacea_subset & (stringr::str_detect(observations$eumofa_species_text, crustacea_pattern) | stringr::str_detect(observations$eumofa_commodity_text, crustacea_pattern)) ~ "crustacea_subset",
    TRUE ~ NA_character_
  )
  broad_match <- dplyr::case_when(
    !is.na(broad_category) & observation_source == "eurostat" ~ "eurostat_species_label",
    !is.na(broad_category) & fao_source & observations$fao_major_group %in% c("MOLLUSCA", "PLANTAE AQUATICAE") ~ "fao_major_group",
    !is.na(broad_category) & fao_source ~ "fao_major_group_and_species_name",
    !is.na(broad_category) & observation_source == "eumofa" ~ "eumofa_species_or_commodity",
    TRUE ~ NA_character_
  )
  broad_memberships <- segment_membership_rows(
    observations,
    segment = "low_anthropic",
    profile = rep("broad", nrow(observations)),
    category = broad_category,
    matched_by = broad_match
  )

  dplyr::bind_rows(
    freshwater_memberships,
    strict_memberships,
    broad_memberships
  ) |>
    dplyr::arrange(observation_id, segment, profile, category)
}