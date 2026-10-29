suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
})

app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(app_root, "R", "eudcf.R"), local = TRUE)
source(file.path(app_root, "R", "eudcf_dashboard.R"), local = TRUE)

workbook_path <- file.path(
  app_root, "data", "EUDCF", "STECF_24-14_EU Aquaculture economic sector data tables.xlsx"
)
workbook <- tryCatch(read_eudcf_workbook(workbook_path), error = identity)

if (inherits(workbook, "error")) {
  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    title = "Aquaculture DCF Explorer",
    h1("Aquaculture DCF Explorer"),
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
  sales_segments <- usable_sales |>
    dplyr::distinct(segment_code, segment_name) |>
    dplyr::filter(!is.na(segment_code), nzchar(segment_code)) |>
    dplyr::mutate(segment_name = dplyr::if_else(is.na(segment_name) | !nzchar(segment_name), segment_code, segment_name)) |>
    dplyr::arrange(segment_code)
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
    dplyr::filter(species_group == default_economic_group, year >= recent_year_range[1], year <= recent_year_range[2]) |>
    dplyr::distinct(segment_code, segment_name) |>
    dplyr::filter(!is.na(segment_code), nzchar(segment_code)) |>
    dplyr::mutate(segment_name = dplyr::if_else(is.na(segment_name) | !nzchar(segment_name), segment_code, segment_name)) |>
    dplyr::arrange(segment_code)
  if (!nrow(economic_segments)) {
    economic_segments <- economic |>
      dplyr::filter(species_group == default_economic_group) |>
      dplyr::distinct(segment_code, segment_name) |>
      dplyr::filter(!is.na(segment_code), nzchar(segment_code)) |>
      dplyr::mutate(segment_name = dplyr::if_else(is.na(segment_name) | !nzchar(segment_name), segment_code, segment_name)) |>
      dplyr::arrange(segment_code)
  }
  default_economic_segment <- eudcf_dashboard_default_economic_segment(
    economic, default_economic_group, recent_year_range
  )$segment_code[1]
  default_sales_row <- eudcf_dashboard_default_sales_row(sales, recent_year_range)
  if (!nrow(default_sales_row)) default_sales_row <- eudcf_dashboard_default_sales_row(sales)
  default_economic_countries <- intersect(c("DEU", "ESP", "FRA"), economic_countries$country_code)
  if (!length(default_economic_countries)) default_economic_countries <- economic_countries$country_code[1]
  default_sales_country <- default_sales_row$country_code[1]
  default_sales_segment <- default_sales_row$segment_code[1]
  default_economic_variable <- if ("GST" %in% economic_variables$variable_code) "GST" else economic_variables$variable_code[1]
  default_sales_variable <- default_sales_row$variable_code[1]
  default_sales_species <- default_sales_row$species_code[1]

  ui <- bslib::page_navbar(
    title = "Aquaculture DCF Explorer",
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
          div(class = "status-line", textOutput("economic_status")),
          div(class = "table-scroll", tableOutput("economic_table"))
        )
      )
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
          div(class = "status-line", textOutput("sales_status")),
          h4("Sample and precision metadata"),
          tableOutput("sales_metadata_table"),
          h4("Selected workbook rows"),
          div(class = "table-scroll", tableOutput("sales_table"))
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
    observeEvent(list(input$economic_species_group, input$economic_years), {
      req(input$economic_species_group, input$economic_years)
      selected_group <- input$economic_species_group
      selected_years <- input$economic_years
      segment_data <- economic |>
        dplyr::filter(
          species_group == .env$selected_group,
          year >= min(.env$selected_years), year <= max(.env$selected_years)
        )
      if (!nrow(segment_data)) {
        segment_data <- economic |> dplyr::filter(species_group == .env$selected_group)
      }
      segment_choices <- segment_data |>
        dplyr::distinct(segment_code, segment_name) |>
        dplyr::filter(!is.na(segment_code), nzchar(segment_code)) |>
        dplyr::mutate(segment_name = dplyr::if_else(is.na(segment_name) | !nzchar(segment_name), segment_code, segment_name)) |>
        dplyr::arrange(segment_code)
      selected_segment <- eudcf_dashboard_default_economic_segment(
        economic, selected_group, selected_years
      )$segment_code[1]
      if (!length(selected_segment) || is.na(selected_segment)) {
        selected_segment <- segment_choices$segment_code[1]
      }
      updateSelectInput(
        session, "economic_segment",
        choices = stats::setNames(segment_choices$segment_code, segment_choices$segment_name),
        selected = selected_segment
      )
    }, ignoreInit = TRUE)

    selected_economic <- reactive({
      req(input$economic_species_group, input$economic_variable, input$economic_segment, input$economic_years)
      eudcf_dashboard_economic_data(
        economic,
        country_codes = input$economic_countries,
        segment_code = input$economic_segment,
        variable_code = input$economic_variable,
        species_group = input$economic_species_group,
        year_range = input$economic_years
      )
    })

    output$economic_plot <- renderPlot({
      data <- selected_economic()
      validate(need(nrow(data) > 0, "No economic rows for this selection."))
      variable <- economic_variables |> dplyr::filter(variable_code == input$economic_variable) |> dplyr::slice(1)
      ggplot(data, aes(x = year, y = value, colour = country_name, group = country_code)) +
        geom_line(linewidth = 0.8, na.rm = TRUE) +
        geom_point(size = 2, na.rm = TRUE) +
        scale_x_continuous(breaks = scales::breaks_width(1), limits = range(input$economic_years)) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(
          title = variable$variable_name,
          subtitle = paste(input$economic_species_group, "|", input$economic_segment, "|", variable$unit, "| nominal values"),
          x = NULL, y = variable$unit, colour = "Country"
        ) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
    })

    output$economic_status <- renderText({
      data <- selected_economic()
      paste(nrow(data), "reported rows |", dplyr::n_distinct(data$country_code), "countries | missing values remain missing")
    })

    output$economic_table <- renderTable({
      selected_economic() |>
        dplyr::select(year, country_name, segment_name, value, unit) |>
        dplyr::arrange(year, country_name)
    }, digits = 2, striped = TRUE, bordered = FALSE, spacing = "s")

    output$download_economic <- downloadHandler(
      filename = function() paste0("eudcf_economic_", input$economic_variable, ".csv"),
      content = function(file) utils::write.csv(selected_economic(), file, row.names = FALSE, na = "")
    )

    selected_sales <- reactive({
      req(input$sales_country, input$sales_segment, input$sales_variable, input$sales_species, input$sales_years)
      eudcf_dashboard_sales_data(
        sales,
        country_code = input$sales_country,
        segment_code = input$sales_segment,
        variable_code = input$sales_variable,
        species_code = input$sales_species,
        year_range = input$sales_years
      )
    })

    output$sales_plot <- renderPlot({
      data <- selected_sales()
      validate(need(nrow(data) > 0, "No sales rows for this selection."))
      ggplot(data, aes(x = year, y = value)) +
        geom_line(colour = "#176b5b", linewidth = 0.9, na.rm = TRUE) +
        geom_point(colour = "#176b5b", size = 2, na.rm = TRUE) +
        scale_x_continuous(breaks = scales::breaks_width(1), limits = range(input$sales_years)) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(
          title = unique(data$variable_name)[1],
          subtitle = paste(unique(data$species_name)[1], "|", unique(data$unit)[1]),
          x = NULL, y = unique(data$unit)[1]
        ) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    })

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
        dplyr::arrange(year)
    }, digits = 2, striped = TRUE, bordered = FALSE, spacing = "s")

    output$coverage_plot <- renderPlot({
      coverage <- eudcf_dashboard_national_coverage(economic)
      ggplot(coverage, aes(x = year, y = countries)) +
        geom_col(fill = "#176b5b", width = 0.75) +
        scale_x_continuous(breaks = scales::breaks_width(1)) +
        scale_y_continuous(breaks = scales::breaks_pretty(), expand = expansion(mult = c(0, 0.08))) +
        labs(x = NULL, y = "Countries with national-total data", caption = "Counts reflect presence of workbook rows, not completeness or reliability.") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    })

    output$coverage_table <- renderTable({
      eudcf_dashboard_national_coverage(economic)
    }, striped = TRUE, bordered = FALSE, spacing = "s")
  }
}

shinyApp(ui, server)
