### CA initialization ###

print("LOADING CAR_initialization.R")

initialize_CA_reference_data = function(data_CA) {
  ref = initialize_reference_data(data_CA)
  
  CATCH_UNITS = c("Tons" = "MT", "Numbers" = "NO")
  
  ref = append(ref, list(CATCH_UNITS = CATCH_UNITS))
  
  return(ref)
}

update_data = function(ref, data_CA) {
  DATA_MT = copy(data_CA)
  DATA_MT = DATA_MT[, CATCH_IN_NUMBERS := NULL]
  DATA_NO = copy(data_CA)
  DATA_NO[, `:=`(CATCH = CATCH_IN_NUMBERS, CATCH_UNIT_CODE = "NO")][, CATCH_IN_NUMBERS := NULL]
  
  return(
    rbind(DATA_MT, DATA_NO)
  )
}

print("LOADED CAR_initialization.R")