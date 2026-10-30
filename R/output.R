aquaculture_plot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#e0e6e3", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold", colour = "#1c302c"),
      plot.subtitle = ggplot2::element_text(colour = "#52665f", margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "#52665f", margin = ggplot2::margin(t = 12)),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      legend.key.width = grid::unit(1.3, "lines"),
      plot.margin = ggplot2::margin(12, 16, 12, 12)
    )
}

aquaculture_year_breaks <- function(limits) {
  years <- seq.int(ceiling(min(limits)), floor(max(limits)))
  step <- max(1L, ceiling(length(years) / 8L))
  years[seq.int(1L, length(years), by = step)]
}

aquaculture_wrap_plot_labels <- function(plot, width) {
  if (is.null(width) || width >= 600) return(plot)
  plot + ggplot2::labs(
    title = stringr::str_wrap(plot$labels$title, width = 30),
    subtitle = stringr::str_wrap(plot$labels$subtitle, width = 35),
    caption = stringr::str_wrap(plot$labels$caption, width = 42)
  )
}

build_fao_low_anthropic_country_plot <- function(analysis, country_code, compact = FALSE) {
  country_data <- analysis$annual_data |>
    dplyr::filter(.data$country_code == .env$country_code) |>
    dplyr::mutate(category = as.character(category))
  if (!nrow(country_data)) stop("No chart data for country: ", country_code, call. = FALSE)
  category_colours <- c(
    "Abalones, winkles, conchs" = "#D49B37",
    "Clams, cockles, arkshells" = "#C96851",
    "Freshwater molluscs" = "#B279A2",
    "Miscellaneous marine molluscs" = "#4C78A8",
    "Mussels" = "#287D8E",
    "Oysters" = "#6E9940",
    "Pearls, mother-of-pearl, shells" = "#FF9DA6",
    "Scallops, pectens" = "#7C62A3",
    "Squids, cuttlefishes, octopuses" = "#72B7B2",
    "Brown seaweeds" = "#59A14F",
    "Green seaweeds" = "#8CD17D",
    "Red seaweeds" = "#E15759",
    "Miscellaneous aquatic plants" = "#F28E2B"
  )
  categories <- country_data |>
    dplyr::filter(measure == "production_tonnes", is.finite(value), value > 0) |>
    dplyr::distinct(category) |>
    dplyr::pull(category) |>
    sort()
  unknown_categories <- setdiff(categories, names(category_colours))
  if (length(unknown_categories) > 0) {
    category_colours <- c(
      category_colours,
      stats::setNames(scales::hue_pal()(length(unknown_categories)), unknown_categories)
    )
  }
  panels <- lapply(c("production_tonnes", "production_value_eur"), function(selected_measure) {
    panel_data <- tidyr::expand_grid(year = analysis$years, category = categories) |>
      dplyr::left_join(
        country_data |>
          dplyr::filter(measure == selected_measure) |>
          dplyr::select(year, category, value),
        by = c("year", "category")
      ) |>
      dplyr::group_by(year) |>
      dplyr::mutate(
        value = if (any(is.finite(value))) tidyr::replace_na(value, 0) else NA_real_
      ) |>
      dplyr::ungroup()
    unknown_years <- country_data |>
      dplyr::filter(measure == selected_measure, !is.finite(value)) |>
      dplyr::pull(year)
    panel_data <- panel_data |>
      dplyr::mutate(value = ifelse(year %in% unknown_years, NA_real_, value)) |>
      dplyr::group_by(category) |>
      dplyr::arrange(year, .by_group = TRUE) |>
      dplyr::mutate(run = cumsum(!is.finite(value)), series = paste(category, run)) |>
      dplyr::ungroup()
    ggplot2::ggplot(panel_data, ggplot2::aes(year, value, fill = category, group = series)) +
      ggplot2::geom_area(stat = "identity", na.rm = TRUE) +
      ggplot2::scale_fill_manual(
        values = category_colours[categories], breaks = categories,
        labels = categories, drop = FALSE
      ) +
      ggplot2::scale_x_continuous(
        limits = range(analysis$years), breaks = aquaculture_year_breaks,
        labels = scales::label_number(accuracy = 1, big.mark = ""), expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_pretty(),
        labels = scales::label_comma(),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        title = if (selected_measure == "production_tonnes") "Quantity (tonnes)" else "Value (thousand EUR, nominal)",
        x = NULL, y = NULL, fill = NULL
      ) +
      aquaculture_plot_theme() +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = if (compact) 1 else 3))
  })
  country_name <- suppressWarnings(countrycode::countrycode(country_code, "iso2c", "country.name"))
  if (is.na(country_name)) country_name <- country_code
  country_rank <- analysis$ranking |> dplyr::filter(.data$country_code == .env$country_code)
  ranking_scope <- if (is.null(analysis$continent)) "Worldwide" else analysis$continent
  patchwork::wrap_plots(panels, ncol = if (compact) 1 else 2, guides = "collect") +
    patchwork::plot_annotation(
      title = stringr::str_wrap(paste(country_name, "- FAO low-anthropic aquaculture"),
                                width = if (compact) 30 else 80),
      subtitle = stringr::str_wrap(paste0(
        analysis$profile, " profile | ", ranking_scope, " | Joint quantity/value rank ", country_rank$rank,
        " | Three-year averages: ", country_rank$ranking_start, "-", country_rank$ranking_end
      ), width = if (compact) 36 else 110),
      caption = stringr::str_wrap(
        "Source: FAO; values converted using annual USD/EUR rates. Missing years remain gaps.",
        width = if (compact) 40 else 110
      ),
      theme = aquaculture_plot_theme()
    ) & ggplot2::theme(legend.position = "bottom")
}

write_fao_low_anthropic_countries <- function(analysis, output_dir) {
  if (!nrow(analysis$ranking) || !any(analysis$ranking$selected)) {
    cli::cli_alert_warning("No FAO countries have three complete ranking years for both quantity and value.")
    return(invisible(character()))
  }
  output_dir <- file.path(output_dir, "fao_top_countries", analysis$profile)
  if (!is.null(analysis$continent)) output_dir <- file.path(output_dir, analysis$continent)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_safe(analysis$ranking, file.path(output_dir, "country_ranking.csv"))
  write_csv_safe(analysis$annual_data, file.path(output_dir, "annual_categories.csv"))
  countries <- analysis$ranking$country_code[analysis$ranking$selected]
  paths <- file.path(output_dir, paste0(countries, ".png"))
  for (country_index in seq_along(countries)) {
    ggplot2::ggsave(
      filename = paths[country_index],
      plot = build_fao_low_anthropic_country_plot(analysis, countries[country_index]),
      width = 14, height = 6, units = "in", dpi = 150, bg = "white"
    )
  }
  invisible(paths)
}

build_comparison_plot <- function(comparison_table, measure, segment, facet_countries = FALSE) {
  plot_spec <- switch(
    measure,
    production_tonnes = list(
      source_columns = c(
        eurostat_production = "Eurostat",
        fao_production = "FAO",
        eumofa_production = "EUMOFA"
      ),
      unit = "tonnes",
      label_scale = 1e-6,
      axis_label = "Production quantity (million tonnes)"
    ),
    production_value_eur = list(
      source_columns = c(
        eurostat_value = "Eurostat",
        fao_value = "FAO",
        eumofa_value = "EUMOFA"
      ),
      unit = "EUR",
      label_scale = 1e-6,
      axis_label = "Production value (million EUR)"
    ),
    stop("Unsupported comparison measure: ", measure, call. = FALSE)
  )
  source_columns <- plot_spec$source_columns
  dimensions <- intersect(c("profile", "category"), names(comparison_table))
  plot_data <- comparison_table |>
    dplyr::select(dplyr::all_of(c(dimensions, "country_code", "year", names(source_columns)))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(names(source_columns)),
      names_to = "source_column",
      values_to = "value"
    ) |>
    dplyr::mutate(
      source = unname(source_columns[source_column]),
      measure = measure,
      unit = plot_spec$unit
    ) |>
    dplyr::select(dplyr::all_of(dimensions), country_code, year, source, measure, unit, value)
  country_dimensions <- if (facet_countries) "country_code" else character()
  if (!facet_countries) {
    plot_data <- plot_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(dimensions, "year", "source", "measure", "unit")))) |>
      dplyr::summarise(
        value = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
        .groups = "drop"
      )
  }
  group_columns <- c(country_dimensions, dimensions, "source")
  plot_data$series <- do.call(
    interaction,
    c(unname(as.list(plot_data[group_columns])), list(drop = TRUE))
  )
  facet_columns <- c(setdiff(dimensions, "profile"), country_dimensions)
  segment_title <- switch(
    segment,
    all_production = "All production",
    freshwater = "Freshwater",
    low_anthropic = "Low-anthropic",
    segment
  )
  years <- plot_data$year[is.finite(plot_data$year)]
  year_span <- if (length(years) > 0) diff(range(years)) else 0
  year_step <- max(1, round(year_span / 8))

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = year,
      y = value,
      colour = source,
      group = series
    )
  ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(year_step),
      minor_breaks = NULL,
      labels = scales::label_number(accuracy = 1, big.mark = "")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(scale = plot_spec$label_scale)
    ) +
    ggplot2::labs(
      title = plot_spec$axis_label,
      subtitle = segment_title,
      x = NULL,
      y = NULL,
      colour = "Source"
    )

  if (nrow(plot_data) > 0 && length(facet_columns) > 0) {
    plot <- plot + ggplot2::facet_wrap(stats::reformulate(facet_columns))
  }
  plot
}

write_pipeline_exports <- function(exports,
                                   output_dirs,
                                   coverage_status,
                                   fao_diagnostics = list()) {
  if (!identical(coverage_status, "complete")) {
    if (identical(coverage_status, "partial") && length(exports$comparison_plots) > 0) {
      comparison_plot_dir <- file.path(output_dirs$general, "comparison_plots")
      dir.create(comparison_plot_dir, recursive = TRUE, showWarnings = FALSE)
      for (plot_name in names(exports$comparison_plots)) {
        ggplot2::ggsave(
          filename = file.path(
            comparison_plot_dir,
            paste0(plot_name, "_partial.png")
          ),
          plot = exports$comparison_plots[[plot_name]],
          width = 10,
          height = 6,
          units = "in",
          dpi = 150
        )
      }
    }
    return(invisible(FALSE))
  }

  dir.create(output_dirs$general, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dirs$freshwater, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dirs$low_anthropic, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dirs$diagnostics, recursive = TRUE, showWarnings = FALSE)

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "by_mode")
  openxlsx::writeData(workbook, "by_mode", exports$low_anthropic_by_mode)
  openxlsx::addWorksheet(workbook, "side_by_side")
  openxlsx::writeData(workbook, "side_by_side", exports$low_anthropic_side_by_side)
  openxlsx::saveWorkbook(
    workbook,
    file.path(output_dirs$low_anthropic, "Europe_low_anthropic_summary.xlsx"),
    overwrite = TRUE
  )

  openxlsx::write.xlsx(
    exports$general_summary,
    file.path(output_dirs$general, "Europe_summary.xlsx"),
    sheetName = "data"
  )
  comparison_workbook <- openxlsx::createWorkbook()
  for (table_name in names(exports$comparison_tables)) {
    openxlsx::addWorksheet(comparison_workbook, table_name)
    openxlsx::writeData(
      comparison_workbook,
      table_name,
      exports$comparison_tables[[table_name]]
    )
  }
  openxlsx::saveWorkbook(
    comparison_workbook,
    file.path(output_dirs$general, "production_comparisons.xlsx"),
    overwrite = TRUE
  )
  comparison_plot_dir <- file.path(output_dirs$general, "comparison_plots")
  dir.create(comparison_plot_dir, recursive = TRUE, showWarnings = FALSE)
  for (plot_name in names(exports$comparison_plots)) {
    ggplot2::ggsave(
      filename = file.path(comparison_plot_dir, paste0(plot_name, ".png")),
      plot = exports$comparison_plots[[plot_name]],
      width = 10,
      height = 6,
      units = "in",
      dpi = 150
    )
  }
  openxlsx::write.xlsx(
    exports$freshwater_summary,
    file.path(output_dirs$freshwater, "Europe_freshwater_summary.xlsx"),
    sheetName = "data"
  )
  write_csv_safe(
    exports$freshwater_consistency,
    file.path(output_dirs$freshwater, "freshwater_consistency.csv")
  )

  diagnostic_files <- c(
    fao_env_qty_counts = "fao_env_qty_counts.csv",
    fao_env_val_counts = "fao_env_val_counts.csv",
    fao_major_group_qty_counts = "fao_major_group_qty_counts.csv",
    fao_major_group_val_counts = "fao_major_group_val_counts.csv"
  )
  for (diagnostic_name in intersect(names(fao_diagnostics), names(diagnostic_files))) {
    write_csv_safe(
      fao_diagnostics[[diagnostic_name]],
      file.path(output_dirs$diagnostics, diagnostic_files[[diagnostic_name]])
    )
  }

  invisible(TRUE)
}