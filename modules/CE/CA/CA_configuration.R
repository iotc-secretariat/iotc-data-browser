### CA configuration ###

print("LOADING CA_configuration.R")

VALUE_COLUMN           = C_CATCH
TIME_COLUMN            = C_YEAR
PARETO_CATEGORY_COLUMN = C_FLEET

SCALE = 1

BAR_CHART_Y_LABEL          = "Catch"
BAR_CHART_EXPANDED_Y_LABEL = "Proportion (%)"
LINE_CHART_Y_LABEL         = BAR_CHART_Y_LABEL
PARETO_CHART_Y_LABEL       = "Average yearly catch"

PLOT_CONFIGURATIONS = list(
  BAR          = "Yearly catches",
  BAR_EXPANDED = "Proportion of yearly catches",
  LINE         = "Yearly catches",
  TREEMAP      = "Distribution of total catches",
  PARETO       = "Yearly average catches by fleet",
  GEO_PIE      = "Average yearly catches",
  GEO_HEAT     = "Average yearly catches"
)

current_configuration = function(ref) {
  configuration = PLOT_CONFIGURATIONS
  
  configuration$UNITS = ref$CATCH_UNITS
  configuration$CATEGORIES = CATCH_CATEGORIES

  return(configuration)  
}

filter_data = function(CA_data, input_filters) {
  return (
    filter_data_core(
      CA_data,
      input_filters$period[1], input_filters$period[2],
      NA, #FISHING GROUNDS
      input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
      input_filters$fleets,
      input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
      input_filters$IUCNStatus
    )
    [CATCH_UNIT_CODE == input_filters$unit]
    [, CATCH := round(CATCH, 2)]
  )
}

rearrange_data = function(CA_data, include_descriptions = TRUE) {
  if(include_descriptions) {
    return(
      CA_data[, .(YEAR,
                  QUARTER,
                  #MONTH_START, MONTH_END,
                  FISHING_GROUND_CODE,
                  FLEET_CODE, FLEET,
                  FISHERY_TYPE_CODE, FISHERY_TYPE,
                  FISHERY_GROUP_CODE, FISHERY_GROUP,
                  FISHERY_CODE, FISHERY,
                  GEAR_CODE, GEAR,
                  CATCH_SCHOOL_TYPE_CODE,
                  IUCN_STATUS_CODE, IUCN_STATUS,
                  SPECIES_WP_CODE, SPECIES_WP,
                  SPECIES_GROUP_CODE, SPECIES_GROUP,
                  SPECIES_CATEGORY_CODE, SPECIES_CATEGORY,
                  SPECIES_CODE, SPECIES,
                  SPECIES_SCIENTIFIC,
                  SPECIES_FAMILY,
                  SPECIES_ORDER,
                  IS_IOTC_SPECIES,
                  IS_SPECIES_AGGREGATE,
                  IS_SSI,
                  #FATE_TYPE_CODE, FATE_TYPE,
                  #FATE_CODE, FATE,
                  CATCH, CATCH_UNIT_CODE)
      ]
    )
  } else {
    return(
      CA_data[, .(YEAR,
                  QUARTER,
                  #MONTH_START, MONTH_END,
                  FISHING_GROUND_CODE,
                  FLEET_CODE,
                  FISHERY_TYPE_CODE,
                  FISHERY_GROUP_CODE,
                  FISHERY_CODE,
                  GEAR_CODE,
                  CATCH_SCHOOL_TYPE_CODE,
                  IUCN_STATUS_CODE,
                  SPECIES_WP_CODE,
                  SPECIES_GROUP_CODE,
                  SPECIES_CATEGORY_CODE,
                  SPECIES_CODE,
                  #SPECIES_SCIENTIFIC,
                  #SPECIES_FAMILY,
                  #SPECIES_ORDER,
                  IS_IOTC_SPECIES,
                  IS_SPECIES_AGGREGATE,
                  IS_SSI,
                  #FATE_TYPE_CODE,
                  #FATE_CODE,
                  CATCH, CATCH_UNIT_CODE)
      ]
    )
  }
}

order_data = function(CA_data) {
  return(
    CA_data[order(+YEAR, +QUARTER, +FISHING_GROUND_CODE, +FLEET_CODE, +FISHERY_CODE, +GEAR_CODE, +SPECIES_CATEGORY_CODE, +SPECIES_CODE, +CATCH_UNIT_CODE)]
  )
}

print("LOADED CA_configuration.R")
