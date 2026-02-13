##########################################################
# Freshwater aquaculture (2021–2023): Data harvest
# Sources: Eurostat, FAO, EUMOFA
##########################################################

# ---- 0) Packages ----
# install.packages(c("eurostat","dplyr","tidyr","readr","stringr","janitor"))
# install.packages("fishstat")   # FAO quantity
# Optional later: install.packages(c("httr2","jsonlite"))

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(janitor)
library(eurostat)
library(fishstat)

# ---- 1) User parameters ----
years_target <- 2021:2023
eu_countries_focus <- c("HU","NL","RO","BE","HR")  # Hungary, Netherlands, Romania, Belgium, Croatia

# FAO -> ISO-2 crosswalk for just your five focus countries
faostat_to_iso2 <- tibble::tribble(
  ~country_name,  ~iso2,
  "Hungary",      "HU",
  "Netherlands",  "NL",
  "Romania",      "RO",
  "Belgium",      "BE",
  "Croatia",      "HR"
)

# ---- 2) EUROSTAT: fish_aq2a via API ----
# Dimensions (Eurostat): freshwater(FRW), all methods(TOTAL), total aquatic animals(F01), units TLW (tonnes) and EUR (value).
# Ref. dataset/product pages (structure & codes): see README notes.  [DBnomics + Eurostat]
eu_raw <- get_eurostat(
  id = "fish_aq2a",
  time_format = "num",
  filters = list(
    aquaenv = "FRW",
    aquameth = "TOTAL",
    species = "F01",
    unit = c("TLW","EUR"),
    geo  = eu_countries_focus
  ),
  cache = TRUE
)

# Clean duplicates (Eurostat SDMX backend sometimes returns multiple rows per cell)
eu_2021_2023 <- eu_raw %>%
  filter(time %in% years_target) %>%
  select(geo, time, unit, values) %>%
  group_by(geo, time, unit) %>%
  summarise(values = first(values), .groups = "drop") %>%    # safe collapse
  mutate(
    indicator = case_when(
      unit == "TLW" ~ "Production_Tonnes_Eurostat",
      unit == "EUR" ~ "Production_EUR_Eurostat",
      TRUE ~ unit
    )
  ) %>%
  select(-unit) %>%
  tidyr::pivot_wider(names_from = indicator, values_from = values) %>%
  rename(country_code = geo, year = time) %>%
  arrange(country_code, year)

message("EUROSTAT pull OK:")
print(eu_2021_2023, n = 50)

# ---- 3) FAO – quantities via fishstat (correct freshwater code = 'IN') ----
data("aquaculture", package = "fishstat")  # production quantities by species, area, country, environment, year
data("country", package = "fishstat")      # lookup for country names/codes
data("environment", package = "fishstat")  # lookup for environment codes ('IN'=Freshwater, 'BW', 'MA', 'AL')

# Aggregate freshwater quantity to country-year totals
faostat_qty <- aquaculture %>%
  filter(environment == "IN", year %in% years_target) %>%
  inner_join(country, by = "country") %>%                  # adds country_name
  group_by(country_name, year) %>%
  summarise(Production_Tonnes_FAO = sum(value, na.rm = TRUE), .groups = "drop") %>%
  # Keep only your five and add ISO-2 for joining
  inner_join(faostat_to_iso2, by = "country_name") %>%
  select(country_code = iso2, year, Production_Tonnes_FAO) %>%
  arrange(country_code, year)

message("FAO quantity pull OK:")
print(faostat_qty, n = 50)

# ---- 3b) FAO VALUE (USD) via CSV (optional; fill when you have it) ----
# Download from FAO Statistical Query Panel:
#   https://www.fao.org/fishery/statistics-query/en/aquaculture
#   -> Global aquaculture production (Value)
#   -> Filter Environment = Freshwater / Inland; choose 2021-2023; select your countries; export CSV.
# Set the path and adapt column names to your export once inspected with names().
#
# fao_value_csv <- "PATH/TO/FAO_Global_aquaculture_value_freshwater_2021_2023.csv"
# fao_val_raw <- readr::read_csv(fao_value_csv) %>% clean_names()
# names(fao_val_raw)  # inspect and adjust the rename below:
# faostat_val <- fao_val_raw %>%
#   rename(country_name = country,      # <-- adjust to actual column name
#          year         = year,         # <-- might already be 'year'
#          value_usd    = value) %>%    # <-- adjust to actual value column
#   group_by(country_name, year) %>%
#   summarise(Production_USD_FAO = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
#   inner_join(faostat_to_iso2, by = "country_name") %>%
#   select(country_code = iso2, year, Production_USD_FAO)

# ---- 4) EUMOFA – Bulk CSV ingestion ----
# Get the most recent CSV that includes aquaculture annual data from:
#   https://eumofa.eu/bulk-download
# Note: the site was revamped in 2024; column names may differ. Inspect and update the mapping below. [EUMOFA Bulk]
#
# Option 1: local file
# eumofa_csv <- "PATH/TO/EUMOFA_bulk.csv"
#
# Option 2: direct URL (if available in your environment)
# eumofa_csv <- "https://eumofa.eu/....csv"
#
# if (file.exists(eumofa_csv)) {
#   eu_aqua_raw <- readr::read_csv(eumofa_csv, guess_max = 500000) %>% clean_names()
#   print(names(eu_aqua_raw))
#
#   # ---- Adapt this block to your column names ----
#   # Typical logical filters:
#   #  - Stage == "Aquaculture"
#   #  - Environment (or water) contains "Fresh" or "Inland"
#   #  - Frequency/Aggregation == "Annual"
#   #  - Country code in your 2-letter list
#   #  - Separate volume and value columns (units may be 'tonnes' and 'EUR')
#
#   eu_aqua_fw <- eu_aqua_raw %>%
#     filter(str_detect(tolower(stage), "aquaculture")) %>%
#     filter(str_detect(tolower(environment), "fresh|inland")) %>%
#     filter(year %in% years_target) %>%
#     filter(country_code %in% eu_countries_focus) %>%
#     group_by(country_code, year) %>%
#     summarise(
#       Production_Tonnes_EUMOFA = sum(volume_tonnes, na.rm = TRUE),  # adjust column names!
#       Production_EUR_EUMOFA    = sum(value_eur, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     arrange(country_code, year)
#
#   message("EUMOFA bulk CSV filtered (freshwater aquaculture):")
#   print(eu_aqua_fw, n = 50)
# } else {
#   message("EUMOFA CSV not found yet. Download from https://eumofa.eu/bulk-download and set 'eumofa_csv'.")
# }

# ---- 5) Build a combined “harvest” table (no comparisons yet) ----
# Start with Eurostat (ISO-2 already), then add FAO qty and (later) FAO value + EUMOFA
harvest <- eu_2021_2023 %>%
  left_join(faostat_qty, by = c("country_code","year"))
# %>% left_join(faostat_val,  by = c("country_code","year"))   # uncomment when FAO value CSV ingested
# %>% left_join(eu_aqua_fw,   by = c("country_code","year"))   # uncomment when EUMOFA CSV ingested

message("Combined harvest (Eurostat + FAO qty; placeholders for FAO value & EUMOFA):")
print(harvest, n = 50)

# ---- END ----
