### SF configuration ###

print("LOADING SF_configuration.R")

VALUE_COLUMN           = C_FISH_COUNT
TIME_COLUMN            = C_YEAR
PARETO_CATEGORY_COLUMN = C_FLEET

SCALE = 1

BAR_CHART_Y_LABEL          = "Samples"
BAR_CHART_EXPANDED_Y_LABEL = "Proportion (%)"
LINE_CHART_Y_LABEL         = BAR_CHART_Y_LABEL
PARETO_CHART_Y_LABEL       = "Average yearly samples"

DEFAULT_MEASURE_TYPE = "FL"

PLOT_CONFIGURATIONS = list(
  BAR          = "Yearly number of samples",
  BAR_EXPANDED = "Proportion of yearly number of samples",
  LINE         = "Yearly number of samples",
  TREEMAP      = "Distribution of total number of samples",
  PARETO       = "Yearly average number of samples by fleet",
  GEO_PIE      = "Average yearly number of samples",
  GEO_PIE_UNIT = "Samples",
  GEO_HEAT     = "Average yearly number of samples",
  GEO_HEAT_UNIT= "Samples"
)

measure_type_by_code = function(measure_type_code) {
  unit = ifelse(input_is_available(measure_type_code), measure_type_code, DEFAULT_MEASURE_TYPE)

  return(iotc.data.reference.codelists::LEGACY_MEASURE_TYPES_IOTDB[CODE == unit])
}

measure_type_description_by_code = function(measure_type_code) {
  measure_type = measure_type_by_code(measure_type_code)

  return(paste0(measure_type$NAME_EN, " (", str_to_lower(measure_type$MEASURE_UNIT), ")"))
}

filter_data = function(data_SF, input_filters) {
  has_unit    = input_is_available(input_filters$unit)
  has_raising = input_is_available(input_filters$raising)

  result = (
    filter_data_core(
      data_SF,
      input_filters$period[1], input_filters$period[2],
      NA, #FISHING GROUNDS
      input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
      input_filters$fleets,
      input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
      input_filters$IUCNStatus
    )
  )

  if(has_unit) result = result[MEASURE_TYPE_CODE == input_filters$unit]
  if(has_raising) result = result[RAISE_CODE %in% input_filters$raising]

  result = result[, FISH_COUNT := round(FISH_COUNT, 2)]

  return(result)
}

do_rearrange_data = function(data_SF, include_descriptions = TRUE) {
  if(include_descriptions) {
    return(
      data_SF[, .(YEAR,
                  QUARTER,
                  #MONTH_START, MONTH_END,
                  FISHING_GROUND_CODE,
                  FLEET_CODE, FLEET,
                  FISHERY_TYPE_CODE, FISHERY_TYPE,
                  FISHERY_GROUP_CODE, FISHERY_GROUP,
                  FISHERY_CODE, FISHERY,
                  GEAR_CODE, GEAR,
                  SCHOOL_TYPE_CODE,
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
                  #FATE_CODE, FATE
                  SEX_CODE,
                  MEASURE_TYPE_CODE,
                  CLASS_LOW, CLASS_HIGH,
                  MEASURE_UNIT_CODE,
                  RAISE_CODE, RAISING,
                  FISH_COUNT)
      ]
    )
  } else {
    return(
      data_SF[, .(YEAR,
                  QUARTER,
                  #MONTH_START, MONTH_END,
                  FISHING_GROUND_CODE,
                  FLEET_CODE,
                  FISHERY_TYPE_CODE,
                  FISHERY_GROUP_CODE,
                  FISHERY_CODE,
                  GEAR_CODE,
                  SCHOOL_TYPE_CODE,
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
                  #FATE_TYPE_CODE, FATE_TYPE,
                  #FATE_CODE, FATE
                  SEX_CODE,
                  MEASURE_TYPE_CODE,
                  CLASS_LOW, CLASS_HIGH,
                  MEASURE_UNIT_CODE,
                  RAISE_CODE,
                  FISH_COUNT)
      ]
    )
  }
}

rearrange_SF_RAW_data = function(data_SF, include_descriptions = TRUE) {
  return(do_rearrange_data(data_SF, include_descriptions))
}

rearrange_SF_STD_data = function(data_SF, include_descriptions = TRUE) {
  data_SF = do_rearrange_data(data_SF, include_descriptions)
  data_SF$MEASURE_UNIT_CODE = "CM"
  data_SF$RAISING = NULL

  return(data_SF)
}

order_data = function(data_SF) {
  return(
    data_SF[order(+YEAR, +QUARTER, +FISHING_GROUND_CODE, +FLEET_CODE, +FISHERY_CODE, +GEAR_CODE, +SPECIES_CATEGORY_CODE, +SPECIES_CODE, +MEASURE_UNIT_CODE, +CLASS_LOW)]
  )
}

print("LOADED SF_configuration.R")
