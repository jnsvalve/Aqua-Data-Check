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
      label_scale = 1,
      axis_label = "Production quantity (tonnes)"
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
    select(all_of(c(dimensions, "country_code", "year", names(source_columns)))) |>
    pivot_longer(
      cols = all_of(names(source_columns)),
      names_to = "source_column",
      values_to = "value"
    ) |>
    mutate(
      source = unname(source_columns[source_column]),
      measure = measure,
      unit = plot_spec$unit
    ) |>
    select(all_of(dimensions), country_code, year, source, measure, unit, value)
  country_dimensions <- if (facet_countries) "country_code" else character()
  if (!facet_countries) {
    plot_data <- plot_data |>
      group_by(across(all_of(c(dimensions, "year", "source", "measure", "unit")))) |>
      summarise(
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
  year_steps <- as.vector(outer(c(1, 2, 5), 10^(0:3)))
  year_step <- min(year_steps[year_steps >= max(1, year_span / 7)])

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
      labels = scales::label_number(accuracy = 1, big.mark = "")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(scale = plot_spec$label_scale)
    ) +
    ggplot2::labs(
      title = plot_spec$axis_label,
      subtitle = segment_title,
      x = "Year",
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
            paste0(plot_name, "_partial_latest.png")
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
    file.path(output_dirs$low_anthropic, "Europe_low_anthropic_summary_latest.xlsx"),
    overwrite = TRUE
  )

  openxlsx::write.xlsx(
    exports$general_summary,
    file.path(output_dirs$general, "Europe_summary_latest.xlsx"),
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
    file.path(output_dirs$general, "production_comparisons_latest.xlsx"),
    overwrite = TRUE
  )
  comparison_plot_dir <- file.path(output_dirs$general, "comparison_plots")
  dir.create(comparison_plot_dir, recursive = TRUE, showWarnings = FALSE)
  for (plot_name in names(exports$comparison_plots)) {
    ggplot2::ggsave(
      filename = file.path(comparison_plot_dir, paste0(plot_name, "_latest.png")),
      plot = exports$comparison_plots[[plot_name]],
      width = 10,
      height = 6,
      units = "in",
      dpi = 150
    )
  }
  openxlsx::write.xlsx(
    exports$freshwater_summary,
    file.path(output_dirs$freshwater, "Europe_freshwater_summary_latest.xlsx"),
    sheetName = "data"
  )
  write_csv_safe(
    exports$freshwater_consistency,
    file.path(output_dirs$freshwater, "freshwater_consistency_latest.csv")
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