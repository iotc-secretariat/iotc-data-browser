### SF RAW configuration ###

print("LOADING SF_RAW_configuration.R")

rearrange_data = rearrange_SF_RAW_data

current_configuration = function(ref) {
  configuration = PLOT_CONFIGURATIONS
  
  configuration$UNITS = ref$MEASURE_TYPES
  configuration$CATEGORIES = SIZE_CATEGORIES
  
  return(configuration)  
}

print("LOADED SF_RAW_configuration.R")
