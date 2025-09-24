### NC extras ###

quality_chart = function(data, dataset_code) {
  return (
    data_quality_bar(data, dataset_code)
  )
}

as_quality_table = function(NC_data_quality, include_descriptions = TRUE) {
  return(rearrange_data_quality(NC_data_quality, include_descriptions = include_descriptions))
}

reactive_dataTable_quality = function(NC_data_quality, input_filters, download = FALSE) {
  reactive({
    as_quality_table(
      filter_data_quality(NC_data_quality, input_filters)
      [order(+YEAR, +FLEET_CODE, +FISHERY_CODE, +GEAR_CODE, +SPECIES_CATEGORY_CODE, +SPECIES_CODE)],
      include_descriptions = !download
    )
  })
}

reactive_plotQualityChart = function(NC_data_quality, input_filters, dataset_code = "NC") {
  reactive({
    quality_chart(
      data = filter_data_quality(NC_data_quality, input_filters),
      dataset_code = dataset_code
    )
  })
}

prepare_NC_handlers = function(current_data, data_quality, input, output, prefix, source_dataset, last_update, configuration) {
  
  print(head(current_data))
  
  DEBUG("Prepare server for NC")
  
  data_prefix = paste0("NC_", prefix)
  NC_prefix = paste0(data_prefix, "_NC")
  CE_prefix = paste0(data_prefix, "_CE")
  SF_prefix = paste0(data_prefix, "_SF")
  
  #current_data is passed twice, as it's both used to draw the charts and the data tables
  common_prepare_handlers(current_data, current_data, input, output, data_prefix, source_dataset, last_update, configuration)
  
  # Reactive data table functions
  
  dataTable_quality          = reactive_dataTable_quality(data_quality, input, download = FALSE)
  dataTable_quality_download = reactive_dataTable_quality(data_quality, input, download = TRUE)
  
  # Output tables

  output$qualityTable = { renderDataTable(options = list(pageLength = 10), dataTable_quality()) }
  
  # Output plots
   
  output$ncQualityChart   = { renderPlot(plotNCQualityChart()) }
  output$ceQualityChart   = { renderPlot(plotCEQualityChart()) }
  output$sfQualityChart   = { renderPlot(plotSFQualityChart()) }

  # Reactive plot functions
  
  plotNCQualityChart   = reactive_plotQualityChart(data_quality, input, "NC")
  plotCEQualityChart   = reactive_plotQualityChart(data_quality, input, "CE")
  plotSFQualityChart   = reactive_plotQualityChart(data_quality, input, "SF")
  
  # Download handlers for data tables

  output$downloadDataQualityTable = prepare_csv_file(input, paste0(data_prefix, "_QUALITY"), NO_UNITS, dataTable_quality_download)
  
  # Download handlers for plots
  
  output$downloadNCQualityChart   = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(NC_prefix,"_QUALITY"), "Yearly quality of nominal catches",
                                                     NO_UNITS,
                                                     NO_CATEGORIES,
                                                     plotNCQualityChart)
  
  output$downloadCEQualityChart   = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(CE_prefix,"_QUALITY"), "Yearly quality of raw georeferenced catches",
                                                     NO_UNITS,
                                                     NO_CATEGORIES,
                                                     plotCEQualityChart)
  
  output$downloadSFQualityChart   = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(SF_prefix,"_QUALITY"), "Yearly quality of raw georeferenced size-frequencies",
                                                     NO_UNITS,
                                                     NO_CATEGORIES,
                                                     plotSFQualityChart)
}