### SF extras ###

print("LOADING SF_extras.R")

ridgeplot_chart = function(show_samples_proportion, show_median, show_mean,
                           num_cols, max_categories, category, use_category_colors, measureType, measureUnit, data) {
  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  label = paste0("Samples")

  return (
    size_ridges(
      data,
      category,
      custom_colors,
      show_samples_proportion = show_samples_proportion,
      show_median = show_median,
      show_mean = show_mean,
      measure_type = measureType,
      measure_unit = measureUnit,
      num_cols = num_cols
    )
  )
}

samples_by_size_bar_chart = function(max_categories, category, use_category_colors, measureType, size_bin, data) {
  if(category == C_YEAR) data$YEAR = factor(data$YEAR)

  size_bin = as.numeric(size_bin)
  break_every = size_bin

  if(size_bin == 2) break_every = 4
  else if(size_bin == 1) break_every = 5

  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  label = "Samples"

  data$CLASS_LOW = floor(data$CLASS_LOW / size_bin) * size_bin

  mc = min(as.numeric(data$CLASS_LOW))
  Mc = max(as.numeric(data$CLASS_LOW))

  return (
    bar.value(
      data,
      value = C_FISH_COUNT,
      time = C_CLASS_LOW,
      category,
      max_categories,
      custom_colors,
      num_legend_rows =  2, #Number of legend rows
      scale = 1,
      y_axis_label = label,
      #x_axis_label = "Fork length (cm)",
      x_axis_label = measure_type_description_by_code(measureType),
      x_breaks_every = break_every #,
      #discrete = FALSE
    ) +
    geom_col(width = size_bin) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

samples_by_size_bar_chart_relative = function(max_categories, category, use_category_colors, measureType, size_bin, data) {
  if(category == C_YEAR) data$YEAR = factor(data$YEAR)

  size_bin = as.numeric(size_bin)
  break_every = size_bin

  if(size_bin == 2) break_every = 4
  else if(size_bin == 1) break_every = 5

  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  data$CLASS_LOW = floor(data$CLASS_LOW / size_bin) * size_bin

  mc = min(as.numeric(data$CLASS_LOW))
  Mc = max(as.numeric(data$CLASS_LOW))

  return (
    bar.value.rel(
      data,
      value = C_FISH_COUNT,
      time = C_CLASS_LOW,
      category,
      max_categories,
      custom_colors,
      num_legend_rows =  2, #Number of legend rows
      y_axis_label = "Samples proportion (%)",
      x_axis_label = measure_type_description_by_code(measureType),
      x_breaks_every = break_every #,
      #discrete = FALSE
    ) +
    geom_col(width = size_bin) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

samples_by_size_line_chart = function(max_categories, category, use_category_colors, measureType, size_bin, data) {
  if(category == C_YEAR) data$YEAR = factor(data$YEAR)

  size_bin = as.numeric(size_bin)
  break_every = size_bin

  if(size_bin == 2) break_every = 4
  else if(size_bin == 1) break_every = 5

  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  label = "Samples"

  data$CLASS_LOW = floor(data$CLASS_LOW / size_bin) * size_bin

  return (
    line.value(data,
               value = C_FISH_COUNT,
               time = C_CLASS_LOW,
               category,
               max_categories,
               custom_colors,
               num_legend_rows = 2,
               scale = 1,
               plot_points = TRUE,
               y_axis_label = label,
               x_axis_label = measure_type_description_by_code(measureType),
               x_breaks_every = break_every) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

samples_by_size_line_chart_rel = function(max_categories, category, use_category_colors, measureType, size_bin, data) {
  if(category == C_YEAR) data$YEAR = factor(data$YEAR)

  size_bin = as.numeric(size_bin)
  break_every = size_bin

  if(size_bin == 2) break_every = 4
  else if(size_bin == 1) break_every = 5

  data$CLASS_LOW = floor(data$CLASS_LOW / size_bin) * size_bin

  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  label = "Samples proportion (%)"

  colnames(data)[which(colnames(data) == category)] = "FILL_BY"

  is_year_or_quarter = category %in% c(C_YEAR, C_QUARTER)

  if(!is_year_or_quarter) {
    colnames(data)[which(colnames(data) == paste0(category, "_CODE"))] = "FILL_BY_CODE"
    data = data[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = .(FILL_BY, FILL_BY_CODE, CLASS_LOW)]

    cat_data = data_should_not_be_empty(data)[, .( FISH_COUNT = sum(FISH_COUNT) ), keyby = .( FILL_BY, FILL_BY_CODE )][order(-FISH_COUNT)]
  } else {
    data = data[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = .(FILL_BY, CLASS_LOW)]

    cat_data = data_should_not_be_empty(data)[, .( FISH_COUNT = sum(FISH_COUNT) ), keyby = .( FILL_BY )][order(-FISH_COUNT)]
  }


  tot_categories = length(cat_data$FILL_BY)

  if(max_categories > tot_categories) max_categories = tot_categories

  top_categories = head(cat_data, max_categories)$FILL_BY

  data[!FILL_BY %in% top_categories, `:=`(FILL_BY = "All others", FILL_BY_CODE = "All others")]

  if(!is_year_or_quarter) {
    data = data[, .( FISH_COUNT = sum(FISH_COUNT) ), keyby = .(CLASS_LOW, FILL_BY, FILL_BY_CODE)]
    data = data[, `:=`(FISH_TOT = sum(FISH_COUNT)), by = .(FILL_BY, FILL_BY_CODE)]
  } else {
    data = data[, .( FISH_COUNT = sum(FISH_COUNT) ), keyby = .(CLASS_LOW, FILL_BY)]
    data = data[, `:=`(FISH_TOT = sum(FISH_COUNT)), by = .(FILL_BY)]
  }

  data = data[, FISH_COUNT := FISH_COUNT * 100 / FISH_TOT]

  colnames(data)[which(colnames(data) == "FILL_BY")] = category
  if(!is_year_or_quarter) colnames(data)[which(colnames(data) == "FILL_BY_CODE")] = paste0(category, "_CODE")

  data$FISH_TOT = NULL

  return (
    line.value(data,
               value = C_FISH_COUNT,
               time = C_CLASS_LOW,
               category,
               max_categories,
               custom_colors,
               num_legend_rows = 2,
               scale = 1,
               plot_points = TRUE,
               y_axis_label = label,
               x_axis_label = measure_type_description_by_code(measureType),
               x_breaks_every = break_every) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

reactive_plotSizeDistribution = function(SF_data, input_filters, configuration) {
  reactive({
    unit = measure_type_by_code(input_filters$unit)

    ridgeplot_chart(
      input_filters$showSampleProportion, input_filters$showMedian, input_filters$showMean, input_filters$numColumns,
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      unit$CODE, unit$NAME_EN,
      filter_data(SF_data, input_filters)
    )
  })
}

reactive_plotSamplesBySizeBar = function(data_SF, input_filter, configuration) {
  reactive({
    samples_by_size_bar_chart(
      input_filter$maxCats, input_filter$category, input_filter$categoryColors,
      input_filter$unit,
      input_filter$sizeBin,
      filter_data(data_SF, input_filter)
    )
  })
}

reactive_plotSamplesBySizeBarRel = function(data_SF, input_filter, configuration) {
  reactive({
    samples_by_size_bar_chart_relative(
      input_filter$maxCats, input_filter$category, input_filter$categoryColors,
      input_filter$unit,
      input_filter$sizeBin,
      filter_data(data_SF, input_filter)
    )
  })
}

reactive_plotSamplesBySizeLine = function(data_SF, input_filter, configuration) {
  reactive({
    samples_by_size_line_chart(
      input_filter$maxCats, input_filter$category, input_filter$categoryColors,
      input_filter$unit,
      input_filter$sizeBin,
      filter_data(data_SF, input_filter)
    )
  })
}

reactive_plotSamplesBySizeLineRel  = function(data_SF, input_filter, configuration) {
  reactive({
    samples_by_size_line_chart_rel(
      input_filter$maxCats, input_filter$category, input_filter$categoryColors,
      input_filter$unit,
      input_filter$sizeBin,
      filter_data(data_SF, input_filter)
    )
  })
}

SF_prepare_handlers = function(data, tabular_data, input, output, prefix, source_dataset, last_update, configuration) {
  # Helpers

  calculate_width_for_size_distribution = function() {
    filtered = filter_data(tabular_data, input)

    column = which(colnames(filtered) == input$category)

    num_categories = nrow(unique(filtered[, ..column]))

    #return(min(num_categories, input$numColumns) * 300)

    if(input$numColumns == 1 | num_categories == 1)
      return(500)

    return(1200)
  }

  calculate_height_for_size_distribution = function() {
    filtered = filter_data(tabular_data, input)

    column = which(colnames(filtered) == input$category)

    num_categories = nrow(unique(filtered[, ..column]))
    num_years = max(filtered$YEAR) - min(filtered$YEAR) + 1

    height = 150 + ceiling(num_categories / input$numColumns) * SD_ROW_YEAR_HEIGHT * num_years

    return(height)
  }

  # Data tables

  dataTable          = reactive_dataTable(tabular_data, input, download = FALSE)
  dataTable_download = reactive_dataTable(tabular_data, input, download = TRUE)

  # Download handlers for data tables

  output$downloadDataTable = prepare_csv_file(input, prefix, configuration$UNITS, dataTable_download)

  # Output plots

  plotSizeDistribution     = reactive_plotSizeDistribution    (tabular_data, input, configuration)

  plotSamplesBySizeBar     = reactive_plotSamplesBySizeBar    (tabular_data, input, configuration)
  plotSamplesBySizeBarRel  = reactive_plotSamplesBySizeBarRel (tabular_data, input, configuration)
  plotSamplesBySizeLine    = reactive_plotSamplesBySizeLine   (tabular_data, input, configuration)
  plotSamplesBySizeLineRel = reactive_plotSamplesBySizeLineRel(tabular_data, input, configuration)

  output$sizeDistribution = { renderPlot(width  = calculate_width_for_size_distribution,
                                         height = calculate_height_for_size_distribution,
                                         plotSizeDistribution()) }

  output$samplesBySizeBar     = { renderPlot(plotSamplesBySizeBar()) }
  output$samplesBySizeBarRel  = { renderPlot(plotSamplesBySizeBarRel()) }
  output$samplesBySizeLine    = { renderPlot(plotSamplesBySizeLine()) }
  output$samplesBySizeLineRel = { renderPlot(plotSamplesBySizeLineRel()) }

  # Download handlers for plots

  output$downloadSizeDistribution     = prepare_png_file_with_fixed_unit(input, source_dataset, last_update,
                                                                         paste0(prefix, "_DISTRIBUTION"), "Size distribution",
                                                                         configuration$UNITS, input$unit,
                                                                         configuration$CATEGORIES,
                                                                         plotSizeDistribution,
                                                                         12, 16)

  output$downloadSamplesBySizeBar     = prepare_png_file_with_fixed_unit(input, SOURCE_DATASET, last_update,
                                                                         paste0(prefix, "_SAMPLES_BAR"), "Samples by size",
                                                                         configuration$UNITS, input$unit,
                                                                         configuration$CATEGORIES,
                                                                         plotSamplesBySizeBar)

  output$downloadSamplesBySizeBarRel  = prepare_png_file_with_fixed_unit(input, SOURCE_DATASET, last_update,
                                                                         paste0(prefix, "_SAMPLES_BAR_REL"), "Relative samples by size",
                                                                         configuration$UNITS, input$unit,
                                                                         configuration$CATEGORIES,
                                                                         plotSamplesBySizeBarRel)

  output$downloadSamplesBySizeLine    = prepare_png_file_with_fixed_unit(input, SOURCE_DATASET, last_update,
                                                                         paste0(prefix, "_SAMPLES_LINE"), "Samples by size",
                                                                         configuration$UNITS, input$unit,
                                                                         configuration$CATEGORIES,
                                                                         plotSamplesBySizeLine)

  output$downloadSamplesBySizeLineRel = prepare_png_file_with_fixed_unit(input, SOURCE_DATASET, last_update,
                                                                         paste0(prefix, "_SAMPLES_LINE_REL"), "Relative samples by size",
                                                                         configuration$UNITS, input$unit,
                                                                         configuration$CATEGORIES,
                                                                         plotSamplesBySizeLineRel)

}

print("LOADED SF_extras.R")
