avg_weight_table = function(max_categories, category, data) {
  data_avg = data_should_not_be_empty(data)
  
  colnames(data_avg)[which(colnames(data_avg) == category)] = "FILL_BY"
  if(category != C_QUARTER) colnames(data_avg)[which(colnames(data_avg) == paste0(category, "_CODE"))] = "FILL_BY_CODE"
  
  cat_data = data_avg[, .( MT = sum(CATCH) ), keyby = .(FILL_BY)][order(-MT)]
  
  tot_categories = length(cat_data$FILL_BY)
  
  if(max_categories > tot_categories) max_categories = tot_categories
  
  top_categories = head(cat_data, max_categories)$FILL_BY
  
  data_avg[!FILL_BY %in% top_categories, `:=`(FILL_BY = "All others", FILL_BY_CODE = "All others")]
  
  data_avg = data_avg[, .( MT = round(sum(CATCH), 2), NO = round(sum(CATCH_IN_NUMBERS), 0) ), keyby = .(YEAR, FILL_BY, FILL_BY_CODE)]
  
  data_avg = data_avg[, AVERAGE_WEIGHT_KG_FISH := round(MT * 1000 / NO, 2)]
  
  colnames(data_avg)[which(colnames(data_avg) == "FILL_BY")] = category
  if(category != C_QUARTER) {
    colnames(data_avg)[which(colnames(data_avg) == "FILL_BY_CODE")] = paste0(category, "_CODE")
  } else {
    data_avg$FILL_BY_CODE = NULL
  }
  
  return (data_avg)
}

avg_weight_table_geo = function(data) {
  data_avg = data_should_not_be_empty(data)[, .(MT = round(sum(CATCH), 2), NO = round(sum(CATCH_IN_NUMBERS), 0)), keyby = .(FISHING_GROUND_CODE)]
  data_avg[, AVERAGE_WEIGHT_KG_FISH := round(MT * 1000 / NO, 2)]
  
  return (data_avg)
}

avg_weight_line_chart = function(max_categories, category, use_category_colors, data) {
  data = data_should_not_be_empty(data)
  
  colors = CUSTOM_COLORS
  
  if(use_category_colors) {
    colors = factorize_colors(data, category)
    
    colnames(colors)[which(colnames(colors) == category)] = "FILL_BY"
    colnames(colors)[which(colnames(colors) == paste0(category, "_CODE"))] = "FILL_BY_CODE"
  }
  
  colnames(data)[which(colnames(data) == category)] = "FILL_BY"
  colnames(data)[which(colnames(data) == paste0(category, "_CODE"))] = "FILL_BY_CODE"
  
  has_fill_by_code = length(which(colnames(data) == "FILL_BY_CODE")) > 0
  
  num_categories = length(unique(data$FILL_BY))

  if(!is.na(max_categories) && num_categories > max_categories) {
    cat_data = data[, .( MT = sum(CATCH) ), keyby = .(FILL_BY)][order(-MT)]
    
    top_categories = as.character(head(cat_data, max_categories)$FILL_BY)
    
    data[!FILL_BY %in% top_categories, `:=`(FILL_BY = "All others", FILL_BY_CODE = "All others")]
    
    if(!use_category_colors) {
      colors = head(colors, max_categories)
      colors$FILL_BY = top_categories
    }    
    
    #Sometimes 'colors' is seen as a data.frame and therefore the [FILL_BY ...] doesn't work
    #Therefore, we force 'colors' to be a data table
    colors = as.data.table(colors)[FILL_BY %in% top_categories]

    if(has_fill_by_code)
      all_others = data.table(FILL_BY_CODE = "All others", FILL = "#333333", OUTLINE = "#000000", FILL_BY = "All others")
    else
      all_others = data.table(FILL = "#333333", OUTLINE = "#000000", FILL_BY = "All others")
    
    colors = rbind(colors, all_others)
    
    colnames(colors)[which(colnames(colors) == "FILL_BY")] = category
    colnames(colors)[which(colnames(colors) == "FILL_BY_CODE")] = paste0(category, "_CODE")
  }
  
  if(has_fill_by_code)
    data = data[, .( MT = sum(CATCH), NO = sum(CATCH_IN_NUMBERS) ), keyby = .(YEAR, FILL_BY, FILL_BY_CODE)]
  else
    data = data[, .( MT = sum(CATCH), NO = sum(CATCH_IN_NUMBERS) ), keyby = .(YEAR, FILL_BY)]
  
  data = data[, AVERAGE_WEIGHT_KG_FISH := round(MT * 1000 / NO, 2)]
  
  colnames(data)[which(colnames(data) == "FILL_BY")] = category
  colnames(data)[which(colnames(data) == "FILL_BY_CODE")] = paste0(category, "_CODE")
  
  return (
    line.value(data,
               value = "AVERAGE_WEIGHT_KG_FISH",
               time = C_YEAR,
               category,
               NA,
               colors,
               num_legend_rows = 2,
               scale = 1,
               plot_points = TRUE,
               y_axis_label = "Average weight (kg / fish)"
    )
  )
}

avg_weight_heat_geo = function(value = "AVERAGE_WEIGHT_KG_FISH", quadrant, show_IO, show_HS, data) {
  limits = limits_for_quadrant(quadrant)
  
  data_avg = data_should_not_be_empty(data)[, .(MT = sum(CATCH), NO = sum(CATCH_IN_NUMBERS)), keyby = .(FISHING_GROUND_CODE)]
  data_avg[, AVERAGE_WEIGHT_KG_FISH := MT * 1000 / NO]
  
  return (
    geo.grid.heatmap(data_avg,
                     value,
                     standard_grid = grid_5x5,
                     num_intervals = 10,
                     unit = "kg / fish",
                     show_IO = show_IO,
                     show_high_seas = show_HS,
                     xlim = limits$xlim,
                     ylim = limits$ylim,
                     yearly_average = FALSE,
                     legend_title = "Average weight")
  )
}

reactive_avgWeightTable = function(original_data, input_filters) {
  reactive({
    avg_weight_table(
      input_filters$maxCats, input_filters$category,
      filter_data_core(
        original_data,
        input_filters$period[1], input_filters$period[2],
        NA, #FISHING GROUNDS
        input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
        input_filters$fleets,
        input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
        input_filters$IUCNStatus
      )
    )
  })
}

reactive_avgWeightGeoTable = function(original_data, input_filters) {
  reactive({
    avg_weight_table_geo(
      filter_data_core(
        original_data,
        input_filters$period[1], input_filters$period[2],
        NA, #FISHING GROUNDS
        input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
        input_filters$fleets,
        input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
        input_filters$IUCNStatus
      )
    )
  })
}

reactive_plotAvgWeightLineChart = function(original_data, input_filters, configuration) {
  reactive({
    avg_weight_line_chart(
      input_filters$maxCats, input_filters$category, input_filters$categoryColors,
      filter_data_core(
        original_data,
        input_filters$period[1], input_filters$period[2],
        NA,
        input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
        input_filters$fleets,
        input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
        input_filters$IUCNStatus
      )
    )
  })
}

reactive_plotAvgWeightGeoHeatmap = function(original_data, input_filters, configuration) {
  reactive({
    avg_weight_heat_geo(
      "AVERAGE_WEIGHT_KG_FISH",
      input_filters$avgWeightQuadrant,
      input_filters$avgWeightShowIO,
      input_filters$avgWeightShowHS,
      filter_data_core(
        original_data,
        input_filters$period[1], input_filters$period[2],
        NA,
        input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
        input_filters$fleets,
        input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
        input_filters$IUCNStatus
      )
    )
  })
}
