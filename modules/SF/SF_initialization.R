### SF initialization ###

print("LOADING SF_initialization.R")

initialize_SF_reference_data = function(data) {
  ref = initialize_reference_data(data)
  
  all_measure_types  = iotc.data.reference.codelists::LEGACY_MEASURE_TYPES_IOTDB[CODE %in% unique(data$MEASURE_TYPE_CODE)]
  all_measure_types  = all_measure_types[, .(SORT, MEASURE_TYPE_CODE = CODE, MEASURE_TYPE = NAME_EN, MEASURE_UNIT)]
  all_measure_types  = all_measure_types[order(MEASURE_UNIT, SORT)]
  
  measure_type_codes = unique(all_measure_types$MEASURE_TYPE_CODE)
  measure_types      = unique(paste(all_measure_types$MEASURE_TYPE_CODE, "-", all_measure_types$MEASURE_TYPE, paste0("(", all_measure_types$MEASURE_UNIT, ")")))
  
  MEASURE_TYPES = setNames(as.character(measure_type_codes), measure_types)
  
  ref = append(ref, list(MEASURE_TYPES = MEASURE_TYPES))
  
  if(length(which(colnames(data) == "RAISE_CODE")) > 0) {
    raise_codes = unique(data$RAISE_CODE)
    raising     = unique(paste(data$RAISE_CODE, "-", data$RAISING))
    RAISINGS    = sort(setNames(as.character(raise_codes), raising))
    
    ref = append(ref, list(RAISINGS = RAISINGS))
  }
  
  return(ref)
}

print("LOADED SF_initialization.R")