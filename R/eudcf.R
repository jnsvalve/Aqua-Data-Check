eudcf_required_columns <- list(
  economic = c(
    "country_code", "country_name", "environment", "environment_hn", "technique",
    "species_group", "segment_name", "segment_code", "year", "variable_group",
    "variable_name", "variable_code", "value", "unit", "framework"
  ),
  social = c(
    "country_code", "country_name", "year", "variable_group", "variable_code", "value",
    "unit", "environment_uploaded", "species", "technique_uploaded", "geo_indicator",
    "segment_name", "segment_uploaded", "farmer", "gender_uploaded", "age_code",
    "education_uploaded", "nationality_uploaded", "employment_status_uploaded",
    "population_farm", "responses_farm", "population_farmer", "responses_farmer",
    "sampling_strategy", "comments", "upload_date"
  ),
  sales = c(
    "country_code", "country_name", "scientific_name", "common_name", "species_name",
    "species_code", "variable_group", "variable_name", "variable_code", "value", "unit",
    "year", "environment", "environment_hn", "technique", "species_group", "segment_name",
    "segment_code", "sample", "population", "achieved_sample", "precision_level",
    "sampling_strategy", "comments", "log_upload_id", "upload_date", "framework", "unit_legend"
  ),
  historical_sales = c(
    "country_code", "country_name", "species", "common_name", "scientific_name",
    "variable_group", "variable_code", "variable_name", "environment", "environment_hn",
    "segment_code", "segment_name", "value", "unit", "year", "sample", "population",
    "sample_rate", "sampling_strategy", "precision_level", "comments", "ordering", "date", "template"
  )
)

eudcf_read_sheet <- function(path, sheet, required_columns) {
  data <- openxlsx::read.xlsx(path, sheet = sheet, check.names = FALSE)
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "EUDCF sheet '", sheet, "' is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  tibble::as_tibble(data, .name_repair = "minimal")
}

read_eudcf_workbook <- function(path) {
  if (!file.exists(path)) {
    stop("EUDCF workbook does not exist: ", path, call. = FALSE)
  }
  file_size <- file.info(path)$size
  if (is.na(file_size) || file_size == 0) {
    stop("EUDCF workbook is empty: ", path, call. = FALSE)
  }

  workbook_sheets <- openxlsx::getSheetNames(path)
  required_sheets <- c("Economic data", "Social data", "Sales data")
  missing_sheets <- setdiff(required_sheets, workbook_sheets)
  if (length(missing_sheets) > 0) {
    stop(
      "EUDCF workbook is missing required sheet(s): ",
      paste(missing_sheets, collapse = ", "),
      call. = FALSE
    )
  }

  tables <- list(
    economic = eudcf_read_sheet(path, "Economic data", eudcf_required_columns$economic),
    social = eudcf_read_sheet(path, "Social data", eudcf_required_columns$social),
    sales = eudcf_read_sheet(path, "Sales data", eudcf_required_columns$sales),
    historical_sales = if ("old DCF Sales data" %in% workbook_sheets) {
      eudcf_read_sheet(path, "old DCF Sales data", eudcf_required_columns$historical_sales)
    } else {
      NULL
    }
  )

  metadata_sheets <- c(
    introduction = "Introduction",
    coverage = "Coverage",
    variables_and_segments = "Variables & segments",
    species_codes = "FAO 3-alpha species codes"
  )
  available_metadata <- metadata_sheets[metadata_sheets %in% workbook_sheets]
  metadata <- lapply(available_metadata, function(sheet) {
    tibble::as_tibble(
      openxlsx::read.xlsx(path, sheet = sheet, check.names = FALSE),
      .name_repair = "minimal"
    )
  })
  names(metadata) <- names(available_metadata)

  c(tables, list(metadata = metadata))
}
