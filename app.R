suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
})

app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(app_root, "R", "eudcf.R"), local = TRUE)
source(file.path(app_root, "R", "eudcf_dashboard.R"), local = TRUE)
source(file.path(app_root, "R", "functions.R"), local = TRUE)
source(file.path(app_root, "R", "production_views.R"), local = TRUE)
source(file.path(app_root, "R", "output.R"), local = TRUE)

export_filename <- function(prefix, values, extension) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  slug <- gsub("[^A-Za-z0-9]+", "_", paste(values, collapse = "_"))
  slug <- gsub("(^_+|_+$)", "", slug)
  paste0(prefix, "_", slug, ".", extension)
}

save_plot_png <- function(plot, file, width, height) {
  ggplot2::ggsave(
    filename = file, plot = plot, width = width, height = height,
    units = "in", dpi = 300, bg = "white", device = "png"
  )
}

workbook_path <- file.path(
  app_root, "data", "EUDCF", "STECF_24-14_EU Aquaculture economic sector data tables.xlsx"
)
workbook <- tryCatch(read_eudcf_workbook(workbook_path), error = identity)
fao_snapshot_path <- file.path(app_root, "output", "normalized", "aquaculture_observations.rds")
fao_data <- tryCatch(local({
  snapshot <- readRDS(fao_snapshot_path)
  species_classification <- read_csv_safe(file.path(app_root, "data", "FAO", "CL_FI_SPECIES_GROUPS.csv")) |>
    janitor::clean_names() |>
    dplyr::transmute(species_alpha_3_code = x3a_code, isscaap_group_en)
  fao_observations <- snapshot$observations |>
    dplyr::filter(source %in% c("fao_quantity", "fao_value"))
  fao_memberships <- snapshot$memberships |>
    dplyr::filter(segment == "low_anthropic") |>
    dplyr::inner_join(
      fao_observations |>
        dplyr::transmute(observation_id, source, fao_species_alpha_3_code),
      by = "observation_id"
    ) |>
    dplyr::filter(!(source %in% c("fao_quantity", "fao_value") &
                      fao_species_alpha_3_code == "LOK" & profile == "strict")) |>
    dplyr::select(observation_id, segment, profile, category, matched_by)
  list(
    observations = fao_observations,
    memberships = fao_memberships,
    species_classification = species_classification
  )
}), error = identity)

if (inherits(workbook, "error")) {
  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    title = "Aquaculture Explorer",
    h1("Aquaculture Explorer"),
    div(class = "alert alert-danger", paste("Could not load the DCF workbook:", conditionMessage(workbook)))
  )
  server <- function(input, output, session) {}
} else {
  economic <- workbook$economic
  sales <- workbook$sales
  usable_sales <- eudcf_dashboard_sales_catalog(sales)
  historical_sales <- workbook$historical_sales
  notes <- eudcf_dashboard_workbook_notes(workbook$metadata)
  key_notes <- notes[grepl(
    "may not necessarily correspond|quality assurance|full coverage|threshold|improbable|inconsistent|not included in the analysis|Data requested refers",
    notes, ignore.case = TRUE
  )]
  economic_countries <- economic |>
    dplyr::distinct(country_code, country_name) |>
    dplyr::filter(!is.na(country_code), nzchar(country_code)) |>
    dplyr::mutate(country_name = dplyr::if_else(is.na(country_name) | !nzchar(country_name), country_code, country_name)) |>
    dplyr::arrange(country_name)
  economic_variables <- economic |>
    dplyr::distinct(variable_code, variable_name, unit) |>
    dplyr::filter(!is.na(variable_code), nzchar(variable_code)) |>
    dplyr::mutate(
      variable_name = dplyr::if_else(is.na(variable_name) | !nzchar(variable_name), variable_code, variable_name),
      unit = dplyr::if_else(is.na(unit) | !nzchar(unit), "unit not reported", unit)
    ) |>
    dplyr::arrange(variable_name)
  economic_choices <- stats::setNames(
    economic_variables$variable_code,
    paste0(economic_variables$variable_name, " (", economic_variables$variable_code, ", ", economic_variables$unit, ")")
  )
  country_colours <- stats::setNames(
    grDevices::hcl.colors(nrow(economic_countries), palette = "Dark 3"),
    economic_countries$country_code
  )
  low_anthropic_groups <- eudcf_dashboard_low_anthropic_groups(economic)
  sales_countries <- usable_sales |>
    dplyr::distinct(country_code, country_name) |>
    dplyr::filter(!is.na(country_code), nzchar(country_code)) |>
    dplyr::mutate(country_name = dplyr::if_else(is.na(country_name) | !nzchar(country_name), country_code, country_name)) |>
    dplyr::arrange(country_name)
  sales_variables <- usable_sales |>
    dplyr::distinct(variable_code, variable_name, unit) |>
    dplyr::filter(!is.na(variable_code), nzchar(variable_code)) |>
    dplyr::mutate(
      variable_name = dplyr::if_else(is.na(variable_name) | !nzchar(variable_name), variable_code, variable_name),
      unit = dplyr::if_else(is.na(unit) | !nzchar(unit), "unit not reported", unit)
    ) |>
    dplyr::arrange(variable_name)
  sales_choices <- stats::setNames(
    sales_variables$variable_code,
    paste0(sales_variables$variable_name, " (", sales_variables$variable_code, ", ", sales_variables$unit, ")")
  )
  sales_species <- usable_sales |>
    dplyr::distinct(species_code, species_name) |>
    dplyr::mutate(species_name = dplyr::if_else(is.na(species_name) | !nzchar(species_name), species_code, species_name)) |>
    dplyr::arrange(species_name)
  min_year <- min(c(economic$year, sales$year), na.rm = TRUE)
  max_year <- max(c(economic$year, sales$year), na.rm = TRUE)
  recent_year_range <- c(max(min_year, max_year - 5L), max_year)
  if (max_year >= 2022L) recent_year_range <- c(max(2021L, min_year), 2022L)
  default_economic_group <- if ("Mussel" %in% low_anthropic_groups) "Mussel" else low_anthropic_groups[1]
  economic_segments <- economic |>
    eudcf_dashboard_economic_segments(
      species_group = default_economic_group, year_range = recent_year_range
    )
  if (!nrow(economic_segments)) {
    economic_segments <- eudcf_dashboard_economic_segments(economic, default_economic_group)
  }
  default_economic_segment <- eudcf_dashboard_default_economic_segment(
    economic, default_economic_group, recent_year_range
  )$segment_code[1]
  default_sales_row <- eudcf_dashboard_default_sales_row(sales, recent_year_range)
  if (!nrow(default_sales_row)) default_sales_row <- eudcf_dashboard_default_sales_row(sales)
  default_economic_countries <- intersect(c("DEU", "ESP", "FRA"), economic_countries$country_code)
  if (!length(default_economic_countries)) default_economic_countries <- economic_countries$country_code[1]
  default_sales_country <- default_sales_row$country_code[1]
  sales_segments <- eudcf_dashboard_sales_segments(
    sales, default_sales_country, default_sales_row$variable_code[1],
    default_sales_row$species_code[1], recent_year_range
  )
  default_sales_segment <- if (default_sales_row$segment_code[1] %in% sales_segments$segment_code) {
    default_sales_row$segment_code[1]
  } else {
    sales_segments$segment_code[1]
  }
  default_economic_variable <- if ("GST" %in% economic_variables$variable_code) "GST" else economic_variables$variable_code[1]
  default_sales_variable <- default_sales_row$variable_code[1]
  default_sales_species <- default_sales_row$species_code[1]
  fao_years_available <- sort(unique(fao_data$observations$year))
  fao_start_min <- min(fao_years_available, na.rm = TRUE)
  fao_end_year <- min(max(fao_years_available, na.rm = TRUE), as.integer(format(Sys.Date(), "%Y")) - 1L)
  fao_start_max <- max(fao_start_min, fao_end_year - 2L)
  fao_start_default <- min(max(fao_start_min, 2010L), fao_start_max)

  ui <- bslib::page_navbar(
    title = "Aquaculture Explorer",
    id = "main_nav",
    selected = "Economics",
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f4f7f5",
      fg = "#1c302c",
      primary = "#176b5b",
      secondary = "#62766f",
      base_font = bslib::font_google("Public Sans")
    ),
    header = tagList(
      tags$head(tags$style(HTML("\
        body { background: #f4f7f5; }
        .navbar { border-bottom: 1px solid #dbe5e1; }
        .source-note { background: #fff; border: 1px solid #dbe5e1; border-left: 4px solid #c18a2d; padding: 14px 18px; margin: 18px 0; }
        .source-note p { margin: 0 0 6px; }
        .source-note details { margin-top: 8px; }
        .view-title { margin: 8px 0 4px; }
        .main { min-width: 0; }
        .view-title h2 { margin: 0 0 4px; font-size: 1.4rem; font-weight: 600; }
        .view-description { color: #52665f; margin-bottom: 18px; font-size: .96rem; font-weight: 400; }
        .shiny-input-container { width: 100%; }
        .table { background: white; }
        .caveat-list li { margin-bottom: 8px; }
        .status-line { color: #52665f; font-size: .92rem; margin-top: 8px; }
        .table-scroll { max-width: 100%; overflow-x: auto; }
        .plot-actions { display: flex; flex-wrap: wrap; gap: 8px; margin: 8px 0 16px; }
      "))),
      div(
        class = "container-fluid source-note",
        p(strong("Primary source: "), basename(workbook_path), " | Workbook-reported DCF values"),
        p("Currency is nominal EUR. Economic values are country/segment aggregates; the sales sheet includes sample and population metadata."),
        tags$details(
          tags$summary("Workbook scope, validation, and coverage notes"),
          tags$ul(class = "caveat-list", lapply(notes, tags$li))
        )
      )
    ),
    bslib::nav_panel(
      "FAO production",
      if (inherits(fao_data, "error")) {
        div(
          class = "container-fluid",
          div(class = "view-title", tags$h2("FAO low-anthropic production")),
          div(class = "alert alert-warning", paste("Could not load the normalized FAO snapshot:", conditionMessage(fao_data)))
        )
      } else {
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            selectInput("fao_profile", "Low-anthropic profile", choices = c("Strict" = "strict", "Broad" = "broad"), selected = "strict"),
            selectInput("fao_continent", "Country scope", choices = c("Worldwide" = "", "Africa", "Americas", "Asia", "Europe", "Oceania", "Antarctica"), selected = ""),
            numericInput("fao_top_n", "Countries to rank", value = 10L, min = 1L, max = 50L, step = 1L),
            sliderInput("fao_start_year", "Plot start year", min = fao_start_min, max = fao_start_max, value = fao_start_default, step = 1L, sep = ""),
            selectInput("fao_country", "Ranked country", choices = character()),
            width = 300
          ),
          div(
            class = "view-title", tags$h2("FAO low-anthropic production"),
            p(class = "view-description", "Select a country from the joint quantity/value ranking. Quantity and nominal value are shown side by side, stacked by FAO ISSCAAP group."),
            div(class = "status-line", textOutput("fao_status")),
            plotOutput("fao_country_plot", height = "auto"),
            div(class = "plot-actions",
              downloadButton("download_fao_plot", "Download PNG"),
              downloadButton("download_fao_data", "Chart data CSV")),
            h4("Country ranking"),
            div(class = "plot-actions", downloadButton("download_fao_ranking", "Ranking CSV")),
            div(class = "table-scroll", tableOutput("fao_ranking_table"))
          )
        )
      }
    ),
    bslib::nav_panel(
      "Sales by species",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          selectInput(
            "sales_country", "Country",
            choices = stats::setNames(sales_countries$country_code, sales_countries$country_name),
            selected = default_sales_country
          ),
          selectInput(
            "sales_segment", "Sector segment",
            choices = stats::setNames(sales_segments$segment_code, sales_segments$segment_name),
            selected = default_sales_segment
          ),
          selectInput("sales_variable", "Reported sales variable", choices = sales_choices, selected = default_sales_variable),
          selectizeInput(
            "sales_species", "Species", choices = stats::setNames(sales_species$species_code, sales_species$species_name),
            selected = default_sales_species, multiple = FALSE,
            options = list(placeholder = "Select a species")
          ),
          sliderInput(
            "sales_years", "Years", min = min_year, max = max_year,
            value = recent_year_range, step = 1, sep = ""
          ),
          width = 300
        ),
        div(
          class = "view-title", tags$h2("Sales records by species"),
          p(class = "view-description", "Sales are shown as reported for the selected country, species, variable, and segment. Sample and population details are displayed below where present."),
          plotOutput("sales_plot", height = 390),
            div(class = "plot-actions",
              downloadButton("download_sales_plot", "Download PNG"),
              downloadButton("download_sales_plot_data", "Chart data CSV")),
          div(class = "status-line", textOutput("sales_status")),
          h4("Sample and precision metadata"),
          tableOutput("sales_metadata_table"),
          h4("Selected workbook rows"),
          div(class = "table-scroll", tableOutput("sales_table"))
        )
      )
    ),
    bslib::nav_panel(
      "Economics",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          selectizeInput(
            "economic_countries", "Countries (empty = all)", choices = stats::setNames(
              economic_countries$country_code, economic_countries$country_name
            ), selected = NULL, multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "All countries with data")
          ),
          selectInput(
            "economic_species_group", "Low-anthropic species group",
            choices = low_anthropic_groups, selected = default_economic_group
          ),
          selectInput(
            "economic_segment", "Sector segment",
            choices = stats::setNames(economic_segments$segment_code, economic_segments$segment_name),
            selected = default_economic_segment
          ),
          selectInput(
            "economic_variable", "Reported variable", choices = economic_choices,
            selected = default_economic_variable
          ),
          sliderInput(
            "economic_years", "Years", min = min_year, max = max_year,
            value = recent_year_range, step = 1, sep = ""
          ),
          downloadButton("download_economic", "Download filtered data"),
          width = 300
        ),
        div(
          class = "view-title", tags$h2("Economic measures"),
          p(class = "view-description", "Explore workbook-reported income, operating costs, labor, and capital for low-anthropic species groups. Values remain in nominal EUR; this view does not derive a profit margin."),
          plotOutput("economic_plot", height = 440),
            div(class = "plot-actions",
              downloadButton("download_economic_plot", "Download PNG"),
              downloadButton("download_economic_plot_data", "Chart data CSV")),
          div(class = "status-line", textOutput("economic_status")),
          div(class = "table-scroll", tableOutput("economic_table"))
        )
      )
    ),
    bslib::nav_panel(
      "Coverage & quality",
      div(
        class = "container-fluid",
        div(class = "view-title", tags$h2("National-total coverage")),
        p(class = "view-description", "Country counts below use the workbook's `tot` segment. Missing submissions and country-specific caveats remain visible rather than being filled or treated as zero."),
        plotOutput("coverage_plot", height = 360),
        div(class = "plot-actions",
          downloadButton("download_coverage_plot", "Download PNG"),
          downloadButton("download_coverage_data", "Chart data CSV")),
        h4("Country-year coverage"),
        tableOutput("coverage_table"),
        h4("Key workbook caveats"),
        tags$ul(class = "caveat-list", lapply(key_notes, tags$li)),
        p("Counts show workbook row presence, not full production coverage. The detailed notes are available in the source banner above.")
      )
    ),
    footer = tags$div(class = "container-fluid status-line", "STECF workbook viewer | No source records are changed or reconciled in the app.")
  )

  server <- function(input, output, session) {
    if (!inherits(fao_data, "error")) {
      fao_analysis <- reactive({
        req(input$fao_profile, input$fao_top_n, input$fao_start_year)
        selected_continent <- if (nzchar(input$fao_continent)) input$fao_continent else NULL
        build_fao_low_anthropic_countries(
          fao_data$observations,
          fao_data$memberships,
          years = seq.int(as.integer(input$fao_start_year), fao_end_year),
          profile = input$fao_profile,
          top_n = as.integer(input$fao_top_n),
          continent = selected_continent,
          species_classification = fao_data$species_classification
        )
      })

      fao_ranked_countries <- reactive({
        ranking <- fao_analysis()$ranking |>
          dplyr::filter(selected)
        if (!nrow(ranking)) return(ranking)
        country_names <- suppressWarnings(
          countrycode::countrycode(ranking$country_code, "iso2c", "country.name", warn = FALSE)
        )
        ranking |>
          dplyr::mutate(country_name = dplyr::coalesce(country_names, country_code))
      })

      observeEvent(fao_ranked_countries(), {
        ranking <- fao_ranked_countries()
        if (!nrow(ranking)) {
          updateSelectInput(session, "fao_country", choices = character(), selected = character())
        } else {
          choices <- stats::setNames(ranking$country_code, ranking$country_name)
          selected_country <- if (input$fao_country %in% ranking$country_code) {
            input$fao_country
          } else {
            ranking$country_code[1]
          }
          updateSelectInput(session, "fao_country", choices = choices, selected = selected_country)
        }
      }, ignoreInit = FALSE)

      output$fao_status <- renderText({
        analysis <- fao_analysis()
        ranking <- fao_ranked_countries()
        if (!nrow(ranking)) {
          return("No countries have complete quantity and value observations for the ranking period.")
        }
        paste0(
          nrow(ranking), " countries | ranking averages: ",
          ranking$ranking_start[1], "-", ranking$ranking_end[1],
          " | ", if (is.null(analysis$continent)) "worldwide" else analysis$continent,
          " | FAO estimate flags are retained, not filtered"
        )
      })

      fao_country_plot <- reactive({
        req(input$fao_country)
        analysis <- fao_analysis()
        validate(need(input$fao_country %in% analysis$ranking$country_code, "No country chart for this ranking."))
        build_fao_low_anthropic_country_plot(
          analysis, input$fao_country,
          compact = isTRUE(session$clientData$output_fao_country_plot_width < 650)
        )
      })

      output$fao_country_plot <- renderPlot(fao_country_plot(), height = function() {
        if (isTRUE(session$clientData$output_fao_country_plot_width < 650)) 1000 else 620
      })

      output$download_fao_plot <- downloadHandler(
        filename = function() export_filename(
          "fao_low_anthropic",
          c(input$fao_profile, input$fao_continent, input$fao_country,
            input$fao_start_year, fao_end_year),
          "png"
        ),
        content = function(file) save_plot_png(
          build_fao_low_anthropic_country_plot(fao_analysis(), input$fao_country),
          file, 14, 7
        )
      )

      output$download_fao_data <- downloadHandler(
        filename = function() export_filename(
          "fao_low_anthropic_data",
          c(input$fao_profile, input$fao_continent, input$fao_country,
            input$fao_start_year, fao_end_year),
          "csv"
        ),
        content = function(file) {
          analysis <- fao_analysis()
          data <- analysis$annual_data |>
            dplyr::filter(country_code == input$fao_country) |>
            dplyr::left_join(
              analysis$ranking |>
                dplyr::select(country_code, rank, ranking_start, ranking_end),
              by = "country_code"
            ) |>
            dplyr::mutate(value_unit = dplyr::if_else(
              measure == "production_tonnes", "tonnes", "thousand EUR"
            ))
          req(nrow(data) > 0)
          utils::write.csv(data, file, row.names = FALSE, na = "")
        }
      )

      output$download_fao_ranking <- downloadHandler(
        filename = function() export_filename(
          "fao_low_anthropic_ranking",
          c(input$fao_profile, input$fao_continent, input$fao_start_year, fao_end_year),
          "csv"
        ),
        content = function(file) {
          utils::write.csv(fao_ranked_countries(), file, row.names = FALSE, na = "")
        }
      )

      output$fao_ranking_table <- renderTable({
        fao_ranked_countries() |>
          dplyr::transmute(
            rank, country_name,
            `3-year average tonnes` = scales::label_comma()(average_tonnes),
            `3-year average thousand EUR` = scales::label_comma()(average_thousand_eur),
            `Quantity share` = scales::percent(quantity_share, accuracy = 0.1),
            `Value share` = scales::percent(value_share, accuracy = 0.1),
            `Joint score` = formatC(joint_score, format = "f", digits = 4)
          ) |>
          dplyr::as_tibble()
      }, striped = TRUE, bordered = FALSE, spacing = "s")
    }

    observeEvent(list(input$economic_species_group, input$economic_countries, input$economic_years), {
      req(input$economic_species_group, input$economic_years)
      selection <- eudcf_dashboard_resolve_economic_selection(
        economic,
        input$economic_species_group,
        input$economic_countries,
        input$economic_years
      )
      updateSelectInput(
        session, "economic_segment",
        choices = stats::setNames(
          selection$segment_choices$segment_code,
          selection$segment_choices$segment_name
        ),
        selected = selection$selected_segment
      )
    }, ignoreInit = TRUE)

    observeEvent(list(input$sales_country, input$sales_variable, input$sales_years), {
      req(input$sales_country, input$sales_variable, input$sales_years)
      selection <- eudcf_dashboard_resolve_sales_selection(
        sales,
        input$sales_country,
        input$sales_variable,
        input$sales_years,
        selected_species = input$sales_species,
        selected_segment = input$sales_segment
      )
      updateSelectizeInput(
        session, "sales_species",
        choices = stats::setNames(
          selection$species_choices$species_code,
          selection$species_choices$species_name
        ),
        selected = selection$selected_species, server = TRUE
      )
      updateSelectInput(
        session, "sales_segment",
        choices = stats::setNames(
          selection$segment_choices$segment_code,
          selection$segment_choices$segment_name
        ),
        selected = selection$selected_segment
      )
    }, ignoreInit = FALSE)

    observeEvent(input$sales_species, {
      req(input$sales_country, input$sales_variable, input$sales_years)
      selection <- eudcf_dashboard_resolve_sales_selection(
        sales,
        input$sales_country,
        input$sales_variable,
        input$sales_years,
        selected_species = input$sales_species,
        selected_segment = input$sales_segment
      )
      updateSelectInput(
        session, "sales_segment",
        choices = stats::setNames(
          selection$segment_choices$segment_code,
          selection$segment_choices$segment_name
        ),
        selected = selection$selected_segment
      )
    }, ignoreInit = TRUE)

    selected_economic <- reactive({
      req(input$economic_species_group, input$economic_variable, input$economic_years)
      if (is.null(input$economic_segment) || !nzchar(input$economic_segment)) return(economic[0, ])
      eudcf_dashboard_economic_data(
        economic,
        country_codes = input$economic_countries,
        segment_code = input$economic_segment,
        variable_code = input$economic_variable,
        species_group = input$economic_species_group,
        year_range = input$economic_years
      )
    })

    economic_plot_data <- reactive(eudcf_dashboard_plot_values(selected_economic()))

    economic_plot <- reactive({
      data <- economic_plot_data()
      validate(need(nrow(data) > 0, "No economic rows for this selection."))
      variable <- economic_variables |> dplyr::filter(variable_code == input$economic_variable) |> dplyr::slice(1)
      display_unit <- paste(unique(data$plot_unit), collapse = ", ")
      segment_name <- unique(data$segment_name)[1]
      monetary <- any(toupper(data$unit) %in% c("EURO", "EUR"))
      endpoints <- data |>
        dplyr::filter(is.finite(plot_value)) |>
        dplyr::group_by(country_code) |>
        dplyr::slice_max(year, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
      direct_labels <- nrow(endpoints) > 0 && nrow(endpoints) <= 6
      data <- eudcf_dashboard_plot_series(data, input$economic_years)
      plot <- ggplot(data, aes(x = year, y = plot_value, colour = country_code, group = country_code)) +
        geom_line(linewidth = 0.8, na.rm = TRUE) +
        geom_point(size = 2, na.rm = TRUE) +
        scale_colour_manual(values = country_colours, breaks = economic_countries$country_code,
                            labels = economic_countries$country_name) +
        scale_x_continuous(breaks = aquaculture_year_breaks,
                           expand = expansion(mult = c(0.03, if (direct_labels) 0.25 else 0.03))) +
        coord_cartesian(xlim = range(input$economic_years), clip = "off") +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(
          title = variable$variable_name,
          subtitle = paste(input$economic_species_group, "|", segment_name),
          x = NULL, y = display_unit, colour = NULL,
          caption = paste0("Source: STECF / EUDCF | ", paste(range(input$economic_years), collapse = "-"),
                           if (monetary) " | Nominal EUR" else "",
                           "\nReported values; missing years remain gaps.")
        ) +
        aquaculture_plot_theme()
      if (direct_labels) {
        plot <- plot +
          ggrepel::geom_text_repel(
            data = endpoints, aes(label = country_name), direction = "y",
            nudge_x = max(0.15, diff(range(input$economic_years)) * 0.04),
            hjust = 0, size = 3.5, seed = 42, max.overlaps = Inf,
            min.segment.length = 0, show.legend = FALSE
          ) +
          theme(legend.position = "none")
      }
      plot
    })

    output$economic_plot <- renderPlot(
      aquaculture_wrap_plot_labels(economic_plot(), session$clientData$output_economic_plot_width)
    )

    output$download_economic_plot <- downloadHandler(
      filename = function() export_filename(
        "eudcf_economics",
        c(input$economic_variable, input$economic_species_group, input$economic_segment,
          min(input$economic_years), max(input$economic_years)),
        "png"
      ),
      content = function(file) save_plot_png(economic_plot(), file, 11, 6)
    )

    output$download_economic_plot_data <- downloadHandler(
      filename = function() export_filename(
        "eudcf_economics_chart_data",
        c(input$economic_variable, input$economic_species_group, input$economic_segment,
          min(input$economic_years), max(input$economic_years)),
        "csv"
      ),
      content = function(file) {
        data <- economic_plot_data()
        req(nrow(data) > 0)
        utils::write.csv(data, file, row.names = FALSE, na = "")
      }
    )

    output$economic_status <- renderText({
      data <- selected_economic()
      paste(nrow(data), "reported rows |", dplyr::n_distinct(data$country_code), "countries | missing values remain missing")
    })

    output$economic_table <- renderTable({
      selected_economic() |>
        dplyr::select(year, country_name, segment_name, value, unit) |>
        eudcf_dashboard_economic_display() |>
        dplyr::arrange(year, country_name)
    }, striped = TRUE, bordered = FALSE, spacing = "s")

    output$download_economic <- downloadHandler(
      filename = function() paste0("eudcf_economic_", input$economic_variable, ".csv"),
      content = function(file) utils::write.csv(selected_economic(), file, row.names = FALSE, na = "")
    )

    selected_sales <- reactive({
      req(input$sales_country, input$sales_variable, input$sales_years)
      if (is.null(input$sales_species) || !nzchar(input$sales_species) ||
          is.null(input$sales_segment) || !nzchar(input$sales_segment)) return(sales[0, ])
      eudcf_dashboard_sales_data(
        sales,
        country_code = input$sales_country,
        segment_code = input$sales_segment,
        variable_code = input$sales_variable,
        species_code = input$sales_species,
        year_range = input$sales_years
      )
    })

    sales_plot_data <- reactive(eudcf_dashboard_plot_values(selected_sales()))

    sales_plot <- reactive({
      data <- sales_plot_data()
      validate(need(nrow(data) > 0, "No sales rows for this selection."))
      display_unit <- paste(unique(data$plot_unit), collapse = ", ")
      variable_name <- unique(data$variable_name)[1]
      species_name <- unique(data$species_name)[1]
      country_name <- unique(data$country_name)[1]
      segment_name <- unique(data$segment_name)[1]
      monetary <- any(toupper(data$unit) %in% c("EURO", "EUR"))
      data <- eudcf_dashboard_plot_series(data, input$sales_years)
      ggplot(data, aes(x = year, y = plot_value)) +
        geom_line(colour = "#176b5b", linewidth = 0.9, na.rm = TRUE) +
        geom_point(colour = "#176b5b", size = 2, na.rm = TRUE) +
        scale_x_continuous(breaks = aquaculture_year_breaks, limits = range(input$sales_years)) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(
          title = paste(country_name, "-", variable_name),
          subtitle = paste(species_name, "|", segment_name),
          x = NULL, y = display_unit,
          caption = paste0("Source: STECF / EUDCF | ", paste(range(input$sales_years), collapse = "-"),
                           if (monetary) " | Nominal EUR" else "",
                           "\nAs reported; not expanded from samples. Missing years remain gaps.")
        ) +
        aquaculture_plot_theme()
    })

    output$sales_plot <- renderPlot(
      aquaculture_wrap_plot_labels(sales_plot(), session$clientData$output_sales_plot_width)
    )

    output$download_sales_plot <- downloadHandler(
      filename = function() export_filename(
        "eudcf_sales",
        c(input$sales_country, input$sales_species, input$sales_variable, input$sales_segment,
          min(input$sales_years), max(input$sales_years)),
        "png"
      ),
      content = function(file) save_plot_png(sales_plot(), file, 11, 5.5)
    )

    output$download_sales_plot_data <- downloadHandler(
      filename = function() export_filename(
        "eudcf_sales_chart_data",
        c(input$sales_country, input$sales_species, input$sales_variable, input$sales_segment,
          min(input$sales_years), max(input$sales_years)),
        "csv"
      ),
      content = function(file) {
        data <- sales_plot_data()
        req(nrow(data) > 0)
        utils::write.csv(data, file, row.names = FALSE, na = "")
      }
    )

    output$sales_status <- renderText({
      data <- selected_sales()
      if (!nrow(data)) return("No rows match these filters.")
      paste(nrow(data), "reported rows | values are not expanded from sample data by this app")
    })

    output$sales_metadata_table <- renderTable({
      data <- selected_sales()
      if (!nrow(data)) return(NULL)
      data |>
        dplyr::distinct(sample, population, achieved_sample, sampling_strategy, precision_level) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ na_if(as.character(.x), "NULL")))
    }, striped = TRUE, bordered = FALSE, spacing = "s")

    output$sales_table <- renderTable({
      selected_sales() |>
        dplyr::select(year, species_name, segment_name, value, unit, sample,
                      population, achieved_sample) |>
        eudcf_dashboard_sales_display() |>
        dplyr::arrange(year)
    }, digits = 2, striped = TRUE, bordered = FALSE, spacing = "s")

    coverage_data <- reactive(eudcf_dashboard_national_coverage(economic))

    coverage_plot <- reactive({
      coverage <- coverage_data()
      ggplot(coverage, aes(x = year, y = countries)) +
        geom_col(fill = "#176b5b", width = 0.75) +
        scale_x_continuous(breaks = aquaculture_year_breaks) +
        scale_y_continuous(breaks = function(limits) unique(round(scales::breaks_pretty()(limits))),
                           expand = expansion(mult = c(0, 0.08))) +
        labs(title = "National-total reporting coverage", x = NULL, y = "Countries with national-total data",
             caption = "Source: STECF / EUDCF\nCounts reflect presence of workbook rows, not completeness or reliability.") +
        aquaculture_plot_theme()
    })

    output$coverage_plot <- renderPlot(
      aquaculture_wrap_plot_labels(coverage_plot(), session$clientData$output_coverage_plot_width)
    )

    output$download_coverage_plot <- downloadHandler(
      filename = function() "eudcf_national_coverage.png",
      content = function(file) save_plot_png(coverage_plot(), file, 10, 4.5)
    )

    output$download_coverage_data <- downloadHandler(
      filename = function() "eudcf_national_coverage.csv",
      content = function(file) utils::write.csv(coverage_data(), file, row.names = FALSE, na = "")
    )

    output$coverage_table <- renderTable({
      coverage_data()
    }, striped = TRUE, bordered = FALSE, spacing = "s")
  }
}

shinyApp(ui, server)
