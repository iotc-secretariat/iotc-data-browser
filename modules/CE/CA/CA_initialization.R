### CA initialization ###

print("LOADING CA_initialization.R")

initialize_CA_reference_data = function(data) {
  ref = initialize_reference_data(data)
  
  CATCH_UNITS = c("Tons" = "MT", "Numbers" = "NO")
  
  ref = append(ref, list(CATCH_UNITS = CATCH_UNITS))
  
  return(ref)
}

update_data = function(ref, data) {
  return(data)
}

print("LOADED CA_initialization.R")