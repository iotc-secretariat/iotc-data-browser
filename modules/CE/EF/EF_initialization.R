### EF initialization ###

print("LOADING EF_initialization.R")

initialize_EF_reference_data = function(data) {
  ref = initialize_reference_data(data)
  
  all_effort_units  = iotc.data.reference.codelists::LEGACY_EFFORT_UNITS_IOTDB[CODE %in% unique(data$EFFORT_UNIT_CODE)][, .(EFFORT_UNIT_CODE = CODE, EFFORT_UNIT = NAME_EN)]
  effort_unit_codes = unique(all_effort_units$EFFORT_UNIT_CODE)
  effort_units      = unique(paste(all_effort_units$EFFORT_UNIT_CODE, "-", all_effort_units$EFFORT_UNIT))
  
  EFFORT_UNITS = sort(setNames(as.character(effort_unit_codes), effort_units))
  
  ref = append(ref, list(EFFORT_UNITS = EFFORT_UNITS))
  
  return(ref)
}

update_data = function(ref, data) {
  all_effort_units  = iotc.data.reference.codelists::LEGACY_EFFORT_UNITS_IOTDB[CODE %in% unique(data$EFFORT_UNIT_CODE)][, .(EFFORT_UNIT_CODE = CODE, EFFORT_UNIT = NAME_EN)]
  data = merge(data, all_effort_units, by = "EFFORT_UNIT_CODE")
}

print("LOADED EF_initialization.R")