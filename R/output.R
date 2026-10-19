write_pipeline_exports <- function(exports,
                                   output_dirs,
                                   coverage_status,
                                   fao_diagnostics = list()) {
  if (!identical(coverage_status, "complete")) return(invisible(FALSE))

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