print("LOADING common_georeferenced.R")

spatially_disaggregate = function(source_data, target_grid_type_code) {
  print(paste0("Begin disaggregating by ", target_grid_type_code, " (", nrow(source_data), " records)"))
  
  grid_codes = unique(source_data$FISHING_GROUND_CODE)
  
  data_target = grid_intersections_by_target_grid_type(grid_codes, target_grid_type_code)
  
  data_dis = merge(x = source_data, y = data_target, all.x = TRUE, by.x = "FISHING_GROUND_CODE", by.y = "SOURCE_FISHING_GROUND_CODE", allow.cartesian = TRUE)
  data_dis = data_dis[!is.na(TARGET_FISHING_GROUND_CODE)]
  
  data_dis$FISHING_GROUND_CODE = data_dis$TARGET_FISHING_GROUND_CODE
  data_dis$TARGET_FISHING_GROUND_CODE = NULL
  
  data_dis    = data_dis[, .(FISH_COUNT = sum(FISH_COUNT * PROPORTION)),
                         keyby = setdiff(names(data_dis),  c(FISH_COUNT, "PROPORTION"))]
  
  print(paste0("End disaggregating by ", target_grid_type_code, " (", nrow(data_dis), " records)"))
  
  return (data_dis)
}

pie_geo = function(value, max_categories, category, use_category_colors, resolution, quadrant, pie_size, opaque_colors, use_exact_center, show_IO, show_HS, current_data, unit_label, legend_title) {
  if(!category %in% colnames(current_data)) stop(paste0("Cannot categorize this dataset by '", category, "'"))
  
  custom_colors = NA
  
  if(use_category_colors == FALSE) custom_colors = CUSTOM_COLORS
  
  limits = limits_for_quadrant(quadrant)
  
  return (
    geo.grid.pie(
      data = current_data,
      value = value,
      fill_by = category,
      colors = custom_colors,
      standard_grid = resolution,
      fixed_radius = pie_size,
      use_centroid = !use_exact_center,
      max_categories = max_categories,
      unit = unit_label,
      show_IO = show_IO,
      show_high_seas = show_HS,
      xlim = limits$xlim,
      ylim = limits$ylim,
      opacity = ifelse(opaque_colors, 1, ifelse(is.na(pie_size), .7, NA)),
      legend_title = legend_title
    )
  )
}

heat_geo = function(value, resolution, quadrant, show_IO, show_HS, current_data, unit_label, legend_title) {
  limits = limits_for_quadrant(quadrant)
  
  return (
    geo.grid.heatmap(
      data = current_data,
      value = value,
      standard_grid = resolution,
      num_intervals = 10,
      unit = unit_label,
      show_IO = show_IO,
      show_high_seas = show_HS,
      xlim = limits$xlim,
      ylim = limits$ylim,
      legend_title = legend_title
    )
  )
}

reactive_plotGeoPiemap = function(current_data, input_filters, configuration) {
  reactive({
    pie_geo(
      VALUE_COLUMN,
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      ifelse(input_filters$resolution == "NA", NA, input_filters$resolution),
      input_filters$quadrant,
      ifelse(input_filters$fixSize, pie_size_by_resolution(input_filters$resolution), NA),
      input_filters$opacity, input_filters$useCenter,
      input_filters$showIO, input_filters$showHS,
      filter_data(current_data, input_filters),
      ifelse(
        is_available(configuration$GEO_PIE_UNIT),
        configuration$GEO_PIE_UNIT,
        str_to_lower(label_for(configuration$UNITS, input_filters$unit, TRUE))
      ),
      label_for(configuration$CATEGORIES, input_filters$category, TRUE)
    )
  })
}

reactive_plotGeoHeatmap = function(current_data, input_filters, configuration) {
  reactive({
    heat_geo(
      VALUE_COLUMN,
      ifelse(input_filters$resolution == "NA", NA, input_filters$resolution),
      input_filters$quadrant,
      input_filters$showIO,
      input_filters$showHS,
      filter_data(current_data, input_filters),
      ifelse(
        is_available(configuration$GEO_HEAT_UNIT),
        configuration$GEO_HEAT_UNIT,
        str_to_lower(label_for(configuration$UNITS, input_filters$unit, TRUE))
      ),
      ifelse(
        is_available(configuration$GEO_HEAT_UNIT),
        "",
        paste("Avg.", str_to_lower(VALUE_COLUMN))
      )
    )
  })
}

georeferenced_prepare_handlers = function(current_data, input, output, prefix, source_dataset, last_update, configuration) {
  # Output plots

  output$geoPiemap  = { renderPlot(plotGeoPiemap()) }
  output$geoHeatmap = { renderPlot(plotGeoHeatmap()) }
  
  # Reactive plot functions

  plotGeoPiemap       = reactive_plotGeoPiemap (current_data, input, configuration)
  plotGeoHeatmap      = reactive_plotGeoHeatmap(current_data, input, configuration)
   
  # Download handlers for geo plots
  output$downloadGeoPiemap        = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(prefix, "_GEO_PIE"), configuration$GEO_PIE,
                                                     configuration$UNIT,
                                                     configuration$CATEGORIES,
                                                     plotGeoPiemap)
  
  output$downloadGeoHeatmap       = prepare_png_file(input, source_dataset, last_update,
                                                     paste0(prefix, "_GEO_HEATMAP"), configuration$GEO_HEAT,
                                                     configuration$UNIT,
                                                     NA, #onfiguration$CATEGORIES,
                                                     plotGeoHeatmap)
}

print("LOADED common_georeferenced.R")