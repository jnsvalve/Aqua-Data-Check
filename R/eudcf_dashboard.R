eudcf_dashboard_filter_years <- function(data, year_range) {
  if (length(year_range) != 2L || anyNA(year_range)) {
    stop("year_range must contain a start and end year", call. = FALSE)
  }
  year_range <- sort(as.integer(year_range))
  data |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::filter(year >= year_range[1], year <= year_range[2])
}

eudcf_dashboard_economic_data <- function(economic, country_codes = NULL,
                                          segment_code = "tot",
                                          variable_code = "GST",
                                          species_group = NULL,
                                          year_range = NULL) {
  data <- economic |>
    dplyr::mutate(value = suppressWarnings(as.numeric(as.character(value)))) |>
    dplyr::filter(segment_code == .env$segment_code, variable_code == .env$variable_code)
  if (!is.null(country_codes) && length(country_codes) > 0) {
    data <- data |> dplyr::filter(country_code %in% .env$country_codes)
  }
  if (!is.null(species_group) && nzchar(species_group)) {
    data <- data |> dplyr::filter(species_group == .env$species_group)
  }
  if (!is.null(year_range)) data <- eudcf_dashboard_filter_years(data, year_range)
  data
}

eudcf_dashboard_low_anthropic_groups <- function(economic) {
  candidates <- c(
    "Clam", "Mussel", "Oyster", "Other molluscs", "Other shellfish",
    "Seaweeds", "Macroalgae", "Microalgae"
  )
  intersect(candidates, unique(as.character(economic$species_group)))
}

eudcf_dashboard_default_economic_segment <- function(economic, species_group,
                                                      year_range = NULL) {
  data <- economic |> dplyr::filter(species_group == .env$species_group)
  if (!is.null(year_range)) {
    in_range <- eudcf_dashboard_filter_years(data, year_range)
    if (nrow(in_range) > 0) data <- in_range
  }
  data |>
    dplyr::count(segment_code, segment_name, sort = TRUE) |>
    dplyr::slice_head(n = 1L)
}

eudcf_dashboard_sales_data <- function(sales, country_code, segment_code,
                                       variable_code, species_code = NULL,
                                       year_range = NULL) {
  data <- sales |>
    dplyr::mutate(value = suppressWarnings(as.numeric(as.character(value)))) |>
    dplyr::filter(
      country_code == .env$country_code,
      segment_code == .env$segment_code,
      variable_code == .env$variable_code
    )
  if (!is.null(species_code) && nzchar(species_code)) {
    data <- data |> dplyr::filter(species_code == .env$species_code)
  }
  if (!is.null(year_range)) data <- eudcf_dashboard_filter_years(data, year_range)
  data
}

eudcf_dashboard_sales_catalog <- function(sales) {
  sales |>
    dplyr::mutate(
      value = suppressWarnings(as.numeric(as.character(value))),
      year = as.integer(year)
    ) |>
    dplyr::filter(
      is.finite(value),
      !is.na(country_code), nzchar(country_code), country_code != "NULL",
      !is.na(segment_code), nzchar(segment_code), segment_code != "NULL",
      !is.na(variable_code), nzchar(variable_code), variable_code != "NULL",
      !is.na(species_code), nzchar(species_code), species_code != "NULL"
    )
}

eudcf_dashboard_default_sales_row <- function(sales, year_range = NULL) {
  available <- eudcf_dashboard_sales_catalog(sales)
  if (!is.null(year_range)) {
    in_range <- eudcf_dashboard_filter_years(available, year_range)
    if (nrow(in_range) > 0) available <- in_range
  }
  available |>
    dplyr::arrange(
      dplyr::desc(year), country_code, segment_code, variable_code, species_code
    ) |>
    dplyr::slice_head(n = 1L)
}

eudcf_dashboard_national_coverage <- function(economic) {
  economic |>
    dplyr::filter(segment_code == "tot") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      countries = dplyr::n_distinct(country_code),
      variable_rows = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(year)
}

eudcf_dashboard_workbook_notes <- function(metadata) {
  note_sheets <- intersect(c("introduction", "coverage"), names(metadata))
  notes <- unlist(metadata[note_sheets], use.names = FALSE)
  notes <- trimws(as.character(notes))
  unique(notes[!is.na(notes) & nzchar(notes)])
}