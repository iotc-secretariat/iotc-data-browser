
VALUE_COLUMN           = C_EFFORT
TIME_COLUMN            = C_YEAR
PARETO_CATEGORY_COLUMN = C_FLEET

SCALE = 1

BAR_CHART_Y_LABEL          = "Effort"
BAR_CHART_EXPANDED_Y_LABEL = "Proportion (%)"
LINE_CHART_Y_LABEL         = BAR_CHART_Y_LABEL
PARETO_CHART_Y_LABEL       = "Average yearly effort"

PLOT_CONFIGURATIONS = list(
  BAR          = "Yearly efforts",
  BAR_EXPANDED = "Proportion of yearly efforts",
  LINE         = "Yearly efforts",
  TREEMAP      = "Distribution of total efforts",
  PARETO       = "Yearly average efforts by fleet",
  GEO_PIE      = "Average yearly efforts",
  GEO_HEAT     = "Average yearly efforts"
)

current_configuration = function(ref) {
  configuration = PLOT_CONFIGURATIONS
  
  configuration$UNITS = ref$EFFORT_UNITS
  configuration$CATEGORIES = EFFORT_CATEGORIES

  return(configuration)  
}

filter_data = function(EF_data, input_filters) {
  return (
    filter_data_core(
      EF_data,
      input_filters$period[1], input_filters$period[2],
      NA, #FISHING GROUNDS
      input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
      input_filters$fleets,
      NA, NA, NA, NA,
      NA
    )
    [EFFORT_UNIT_CODE == input_filters$unit]
    [, EFFORT := round(EFFORT, 2)]
  )
}

rearrange_data = function(EF_data, include_descriptions = TRUE) {
  if(include_descriptions) {
    return(
      EF_data[, .(YEAR,
                  QUARTER,
                  #MONTH_START, MONTH_END,
                  FISHING_GROUND_CODE,
                  FLEET_CODE, FLEET,
                  FISHERY_TYPE_CODE, FISHERY_TYPE,
                  FISHERY_GROUP_CODE, FISHERY_GROUP,
                  FISHERY_CODE, FISHERY,
                  GEAR_CODE, GEAR,
                  EFFORT_SCHOOL_TYPE_CODE = SCHOOL_TYPE_CODE,
                  EFFORT,
                  EFFORT_UNIT_CODE, EFFORT_UNIT)
      ]
    )
  } else {
    return(
      EF_data[, .(YEAR,
                  QUARTER,
                  #MONTH_START, MONTH_END,
                  FISHING_GROUND_CODE,
                  FLEET_CODE,
                  FISHERY_TYPE_CODE,
                  FISHERY_GROUP_CODE,
                  FISHERY_CODE,
                  GEAR_CODE,
                  EFFORT_SCHOOL_TYPE_CODE = SCHOOL_TYPE_CODE,
                  EFFORT,
                  EFFORT_UNIT_CODE)
      ]
    )
  }
}

order_data = function(EF_data) {
  return(
    EF_data[order(+YEAR, +QUARTER, +FISHING_GROUND_CODE, +FLEET_CODE, +FISHERY_CODE, +GEAR_CODE, +EFFORT_UNIT_CODE)]
  )
}

print("LOADED EF_configuration.R")
