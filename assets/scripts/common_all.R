print("LOADING common.R")

#db_debug_connections(TRUE)

bar_chart = function(value, time, max_categories, category, use_category_colors, current_data, scale, y_axis_label) {
  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  return (
    bar.value(
      data = current_data,
      value = value,
      time = time,
      fill_by = category,
      max_categories = max_categories,
      colors = custom_colors,
      num_legend_rows =  2,
      scale = scale,
      y_axis_label = y_axis_label)
  )
}

bar_chart_expanded = function(value, time, max_categories, category, use_category_colors, current_data, y_axis_label) {
  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  return (
    bar.value.rel(
      data = current_data,
      value = value,
      time = time,
      fill_by = category,
      max_categories = max_categories,
      colors = custom_colors,
      num_legend_rows = 2,
      y_axis_label = y_axis_label)
  )
}

line_chart = function(value, time, max_categories, category, use_category_colors, current_data, scale, y_axis_label) {
  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  return (
    line.value(
      data = current_data,
      value = value,
      time = time,
      color_by = category,
      max_categories = max_categories,
      colors = custom_colors,
      num_legend_rows = 2,
      scale = scale,
      plot_points = TRUE,
      y_axis_label = y_axis_label
    )
  )
}

treemap_chart = function(value, max_categories, category, use_category_colors, current_data) {
  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  return (
    treemap.value(
      data = current_data,
      value = value,
      fill_by = category,
      max_categories = max_categories,
      colors = custom_colors
    )
  )
}

pareto_chart = function(value, categorize_by, max_categories, category, use_category_colors, current_data, scale, y_axis_label) {
  custom_colors = NA

  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS

  return (
    pareto.value(
      data = current_data,
      value = value,
      categorize_by = categorize_by,
      fill_by = category,
      max_categories = 10,                  #To be shown on the X-axis
      max_fill_categories = max_categories, #To be used to colorize the bars
      colors = custom_colors,
      scale = scale,
      y_axis_label = y_axis_label
    )
  )
}

as_table = function(current_data, include_descriptions = TRUE) {
  return(rearrange_data(current_data, include_descriptions))
}

reactive_plotBarChart = function(current_data, input_filters, configuration) {
  reactive({
    label = BAR_CHART_Y_LABEL

    if(is_available(configuration$UNITS)) {
      LABEL = tolower(label_for(configuration$UNITS, input_filters$unit, TRUE))

      if(length(LABEL) > 0) label = paste0(label, " (", LABEL, ")")
    }

    bar_chart(
      VALUE_COLUMN,
      TIME_COLUMN,
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      filter_data(current_data, input_filters),
      SCALE,
      label
    )
  })
}

reactive_plotBarChartExpanded = function(current_data, input_filters, configuration) {
  reactive({
    label = BAR_CHART_EXPANDED_Y_LABEL

    if(is_available(configuration$UNITS)) {
      LABEL = tolower(label_for(configuration$UNITS, input_filters$unit, TRUE))

      if(length(LABEL) > 0) label = paste0(label, " (", LABEL, ")")
    }

    bar_chart_expanded(
      VALUE_COLUMN,
      TIME_COLUMN,
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      filter_data(current_data, input_filters),
      BAR_CHART_EXPANDED_Y_LABEL
    )
  })
}

reactive_plotLineChart = function(current_data, input_filters, configuration) {
  reactive({
    label = LINE_CHART_Y_LABEL

    if(is_available(configuration$UNITS)) {
      LABEL = tolower(label_for(configuration$UNITS, input_filters$unit, TRUE))

      if(length(LABEL) > 0) label = paste0(label, " (", LABEL, ")")
    }

    line_chart(
      VALUE_COLUMN,
      TIME_COLUMN,
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      filter_data(current_data, input_filters),
      SCALE,
      label
    )
  })
}

reactive_plotTreemapChart = function(current_data, input_filters, configuration) {
  reactive({
    treemap_chart(
      VALUE_COLUMN,
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      filter_data(current_data, input_filters)
    )
  })
}

reactive_plotParetoChart = function(current_data, input_filters, configuration) {
  reactive({
    label = PARETO_CHART_Y_LABEL

    if(is_available(configuration$UNITS)) {
      LABEL = tolower(label_for(configuration$UNITS, input_filters$unit, TRUE))

      if(length(LABEL) > 0) label = paste0(label, " (", LABEL, ")")
    }

    pareto_chart(
      VALUE_COLUMN,
      PARETO_CATEGORY_COLUMN,
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      filter_data(current_data, input_filters),
      SCALE,
      label
    )
  })
}

reactive_dataTable = function(current_data, input_filters, download = FALSE) {
  reactive({
    as_table(
      order_data(
        filter_data(current_data, input_filters)
      ),
      include_descriptions = !download
    )
  })
}

common_prepare_handlers = function(current_data, current_data_table, input, output, prefix, source_dataset, last_update, configuration) {
  # Reactive data table functions

  dataTable          = reactive_dataTable(current_data_table, input, download = FALSE)
  dataTable_download = reactive_dataTable(current_data_table, input, download = TRUE)

  # Output tables

  output$table        = { 
    DT::renderDT(
      dataTable(),
      server = TRUE
    ) 
  }

  # Reactive plot functions

  plotBarChart         = reactive_plotBarChart        (current_data, input, configuration)
  plotBarChartExpanded = reactive_plotBarChartExpanded(current_data, input, configuration)
  plotLineChart        = reactive_plotLineChart       (current_data, input, configuration)
  plotTreemapChart     = reactive_plotTreemapChart    (current_data, input, configuration)
  plotParetoChart      = reactive_plotParetoChart     (current_data, input, configuration)

  # Output plots

  output$barChart         = { renderPlot(plotBarChart()) }
  output$barChartExpanded = { renderPlot(plotBarChartExpanded()) }
  output$lineChart        = { renderPlot(plotLineChart()) }
  output$treemapChart     = { renderPlot(plotTreemapChart()) }
  output$paretoChart      = { renderPlot(plotParetoChart()) }

  # Download handlers for data tables

  output$downloadDataTable        = prepare_csv_file(input, prefix, configuration$UNITS, dataTable_download)

  # Download handlers for plots

  output$downloadBarChart         = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(prefix, "_BAR"), configuration$BAR,
                                                     configuration$UNITS,
                                                     configuration$CATEGORIES,
                                                     plotBarChart)

  output$downloadBarChartExpanded = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(prefix, "_BAR_REL"), configuration$BAR_EXPANDED,
                                                     configuration$UNITS,
                                                     configuration$CATEGORIES,
                                                     plotBarChartExpanded)

  output$downloadLineChart        = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(prefix, "_LINE"), configuration$LINE,
                                                     configuration$UNITS,
                                                     configuration$CATEGORIES,
                                                     plotLineChart)

  output$downloadTreemapChart     = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(prefix, "_TREEMAP"), configuration$TREEMAP,
                                                     configuration$UNITS,
                                                     configuration$CATEGORIES,
                                                     plotTreemapChart,
                                                     width = 12, height = 12)

  output$downloadParetoChart      = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(prefix, "_PARETO"), configuration$PARETO,
                                                     configuration$UNITS,
                                                     configuration$CATEGORIES,
                                                     plotParetoChart)
}

print("LOADED common.R")
