### CA initialization ###

print("LOADING NC_initialization.R")

initialize_NC_reference_data = function(data) {
  ref = initialize_reference_data(data)
  
  fishing_ground_codes = unique(data$FISHING_GROUND_CODE)
  fishing_grounds      = unique(paste(data$FISHING_GROUND_CODE, "-", data$FISHING_GROUND))
  FISHING_GROUNDS      = sort(setNames(as.character(fishing_ground_codes), fishing_grounds))
  
  ref = append(ref, list(FISHING_GROUNDS = FISHING_GROUNDS))  
  
  return(ref)
}

print("LOADED NC_initialization.R")