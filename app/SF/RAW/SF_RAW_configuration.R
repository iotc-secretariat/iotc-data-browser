### SF RAW configuration ###

print("LOADING SF_RAW_configuration.R")

SF_RAW_PREFIX = paste0(SF_COMMON_PREFIX, "_RAW")

SOURCE_DATASET = "raw georeferenced size-frequencies"

rearrange_data = rearrange_SF_RAW_data

current_configuration = function(ref) {
  configuration = PLOT_CONFIGURATIONS
  
  configuration$UNITS = ref$MEASURE_TYPES
  configuration$CATEGORIES = SIZE_CATEGORIES
  
  return(configuration)  
}

print("LOADED SF_RAW_configuration.R")
