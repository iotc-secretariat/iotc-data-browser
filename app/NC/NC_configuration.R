### NC common configuration ###

print("LOADING NC_configuration.R")

NC_COMMON_PREFIX = "NC"

SOURCE_DATASET = "< to be specified >"

VALUE_COLUMN           = C_CATCH
TIME_COLUMN            = C_YEAR
PARETO_CATEGORY_COLUMN = C_FLEET

SCALE = 1000

BAR_CHART_Y_LABEL          = "Catches (x 1,000 t)"
BAR_CHART_EXPANDED_Y_LABEL = "Proportion (%)"
LINE_CHART_Y_LABEL         = BAR_CHART_Y_LABEL
PARETO_CHART_Y_LABEL       = "Average yearly catches (x 1,000 t)"

PLOT_CONFIGURATIONS = list(
  BAR = "Yearly catches",
  BAR_EXPANDED = "Proportion of yearly catches",
  LINE = "Yearly catches",
  TREEMAP = "Distribution of total catches",
  PARETO = "Yearly average catches by fleet"
)

current_configuration = function(ref) {
  configuration = PLOT_CONFIGURATIONS

  configuration$UNIT = NO_UNITS
  configuration$CATEGORIES = CATCH_CATEGORIES

  return(configuration)
}

### NC data

filter_data = function(NC_data, input_filters) {
  return (
    filter_data_core(
      NC_data,
      input_filters$period[1], input_filters$period[2],
      input_filters$fishingGrounds,
      input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
      input_filters$fleets,
      input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
      input_filters$IUCNStatus
    )
    [, CATCH := round(CATCH, 2)]
  )
}

rearrange_data = function(NC_data, include_descriptions = TRUE) {
  if(include_descriptions) {
    return(
      NC_data[, .(YEAR,
                  FISHING_GROUND_CODE, FISHING_GROUND,
                  FLEET_CODE, FLEET,
                  FISHERY_TYPE_CODE, FISHERY_TYPE,
                  FISHERY_GROUP_CODE, FISHERY_GROUP,
                  FISHERY_CODE, FISHERY,
                  GEAR_CODE, GEAR,
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
      NC_data[, .(YEAR,
                  FISHING_GROUND_CODE,
                  FLEET_CODE,
                  FISHERY_TYPE_CODE,
                  FISHERY_GROUP_CODE,
                  FISHERY_CODE,
                  GEAR_CODE,
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

order_data = function(NC_data) {
  return(
    NC_data[order(+YEAR, +FISHING_GROUND_CODE, +FLEET_CODE, +FISHERY_CODE, +GEAR_CODE, +SPECIES_CATEGORY_CODE, +SPECIES_CODE)]
  )
}

### NC data quality

filter_data_quality = function(NC_data_quality, input_filters) {
  return (
    filter_data_core(
      NC_data_quality,
      input_filters$period[1], input_filters$period[2],
      NA,
      input_filters$fisheryTypes, input_filters$fisheryGroups, input_filters$fisheries, input_filters$gears,
      input_filters$fleets,
      input_filters$speciesWps, input_filters$speciesGroups, input_filters$speciesCategories, input_filters$species,
      NA
    )
  )
}

rearrange_data_quality = function(NC_data_quality, include_descriptions = TRUE) {
  if(include_descriptions) {
    return(
      NC_data_quality[, .(YEAR,
                          FLEET_CODE, FLEET,
                          FISHERY_TYPE_CODE, FISHERY_TYPE,
                          FISHERY_GROUP_CODE, FISHERY_GROUP,
                          FISHERY_CODE, FISHERY,
                          GEAR_CODE, GEAR,
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
                          CATCH, CATCH_UNIT_CODE = "MT",
                          Q_NC = NC,
                          Q_CE = CE,
                          Q_SF = SF)
      ]
    )
  } else {
    return(
      NC_data_quality[, .(YEAR,
                          FLEET_CODE,
                          FISHERY_TYPE_CODE,
                          FISHERY_GROUP_CODE,
                          FISHERY_CODE,
                          GEAR_CODE,
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
                          CATCH, CATCH_UNIT_CODE = "MT",
                          Q_NC = NC,
                          Q_CE = CE,
                          Q_SF = SF)
      ]
    )
  }
}

order_data_quality = function(NC_data_quality) {
  return(
    NC_data_quality[order(+YEAR, +FLEET_CODE, +FISHERY_CODE, +GEAR_CODE, +SPECIES_CATEGORY_CODE, +SPECIES_CODE)]
  )
}

print("LOADED NC_configuration.R")
