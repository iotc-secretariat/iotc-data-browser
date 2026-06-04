### SF STD configuration ###

print("LOADING SF_STD_configuration.R")

rearrange_data = rearrange_SF_STD_data

current_configuration = function(ref) {
  configuration = PLOT_CONFIGURATIONS

  configuration$UNITS = ref$MEASURE_TYPES
  configuration$CATEGORIES = SIZE_CATEGORIES

  return(configuration)
}

print("LOADED SF_STD_configuration.R")
