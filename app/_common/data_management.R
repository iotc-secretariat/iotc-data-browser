data_should_not_be_empty = function(data) {
  if(!is_available(data) | nrow(data) == 0) {
    stop("No data identified by current filtering criteria!")
  }

  return(data)
}

input_is_available = function(input_field) {
  return(is_available(input_field) && !is.null(input_field) && input_field != "")
}

initialize_reference_data = function(data) {
  fleets = all_fleets()[order(SORT)]
  FLEETS  = setNames(as.character(fleets$CODE), paste(fleets$CODE, "-", fleets$NAME_EN))

  fishery_type_codes = unique(data$FISHERY_TYPE_CODE)
  fishery_types      = unique(paste(data$FISHERY_TYPE_CODE, "-", data$FISHERY_TYPE))
  FISHERY_TYPES      = sort(setNames(as.character(fishery_type_codes), fishery_types))

  fishery_group_codes = unique(data$FISHERY_GROUP_CODE)
  fishery_groups      = unique(paste(data$FISHERY_GROUP_CODE, "-", data$FISHERY_GROUP))
  FISHERY_GROUPS      = sort(setNames(as.character(fishery_group_codes), fishery_groups))

  fishery_codes = unique(data$FISHERY_CODE)
  fisheries     = unique(paste(data$FISHERY_CODE, "-", data$FISHERY))
  FISHERIES     = sort(setNames(as.character(fishery_codes), fisheries))

  gear_codes = unique(data$GEAR_CODE)
  gears      = unique(paste(data$GEAR_CODE, "-", data$GEAR))
  GEARS      = sort(setNames(as.character(gear_codes), gears))

  SPECIES_WPS        = NA
  SPECIES_GROUPS     = NA
  SPECIES_CATEGORIES = NA
  SPECIES            = NA
  IUCN_STATUS        = NA

  if(is_available(which(colnames(data) == C_IUCN_STATUS_CODE))) {
    IUCN_status_codes = unique(data$IUCN_STATUS_CODE)
    IUCN_status       = unique(paste(data$IUCN_STATUS_CODE, "-", data$IUCN_STATUS))
    IUCN_STATUS       = sort(setNames(as.character(IUCN_status_codes), IUCN_status))
  }

  if(is_available(which(colnames(data) == C_SPECIES_WP_CODE))) {
    species_WP_codes = unique(data$SPECIES_WP_CODE)
    species_WPs      = unique(paste(data$SPECIES_WP_CODE, "-", data$SPECIES_WP))
    SPECIES_WPS      = sort(setNames(as.character(species_WP_codes), species_WPs))
  }

  if(is_available(which(colnames(data) == C_SPECIES_GROUP_CODE))) {
    species_group_codes = unique(data$SPECIES_GROUP_CODE)
    species_groups      = unique(paste(data$SPECIES_GROUP_CODE, "-", data$SPECIES_GROUP))
    SPECIES_GROUPS      = sort(setNames(as.character(species_group_codes), species_groups))
  }

  if(is_available(which(colnames(data) == C_SPECIES_CATEGORY_CODE))) {
    species_category_codes = unique(data$SPECIES_CATEGORY_CODE)
    species_categories     = unique(paste(data$SPECIES_CATEGORY_CODE, "-", data$SPECIES_CATEGORY))
    SPECIES_CATEGORIES     = sort(setNames(as.character(species_category_codes), species_categories))
  }

  if(is_available(which(colnames(data) == C_SPECIES_CODE))) {
    species_codes = unique(data$SPECIES_CODE)
    species       = unique(paste(data$SPECIES_CODE, "-", data$SPECIES))
    SPECIES       = sort(setNames(as.character(species_codes), species))
  }

  ym = min(data$YEAR)
  yM = max(data$YEAR)

  return(list(YEARS = list(MIN = ym, MAX = yM),
              FISHERY_TYPES = FISHERY_TYPES, FISHERY_GROUPS = FISHERY_GROUPS, FISHERIES = FISHERIES, GEARS = GEARS,
              FLEETS = FLEETS,
              SPECIES_WPS = SPECIES_WPS, SPECIES_GROUPS = SPECIES_GROUPS, SPECIES_CATEGORIES = SPECIES_CATEGORIES, SPECIES = SPECIES,
              IUCN_STATUS = IUCN_STATUS))
}

filter_data_core = function(all_data,
                            year_from, year_to,
                            fishing_grounds,
                            fishery_types, fishery_groups, fisheries, gears,
                            fleets,
                            species_wps, species_groups, species_categories, species,
                            IUCN_status) {
  data_filtered = all_data

  if(is_available(year_from))      data_filtered = data_filtered[ YEAR >= year_from, ]
  if(is_available(year_to))        data_filtered = data_filtered[ YEAR <= year_to, ]

  if(is_available(fishing_grounds))data_filtered = data_filtered[ FISHING_GROUND_CODE %in% fishing_grounds ]

  if(is_available(fishery_types))  data_filtered = data_filtered[ FISHERY_TYPE_CODE %in% fishery_types, ]
  if(is_available(fishery_groups)) data_filtered = data_filtered[ FISHERY_GROUP_CODE %in% fishery_groups, ]
  if(is_available(fisheries))      data_filtered = data_filtered[ FISHERY_CODE %in% fisheries, ]
  if(is_available(gears))          data_filtered = data_filtered[ GEAR_CODE %in% gears, ]
  if(is_available(fleets))         data_filtered = data_filtered[ FLEET_CODE %in% fleets, ]

  if(is_available(species_wps))        data_filtered = data_filtered[ SPECIES_WP_CODE %in% species_wps, ]
  if(is_available(species_groups))     data_filtered = data_filtered[ SPECIES_GROUP_CODE %in% species_groups, ]
  if(is_available(species_categories)) data_filtered = data_filtered[ SPECIES_CATEGORY_CODE %in% species_categories, ]
  if(is_available(species))            data_filtered = data_filtered[ SPECIES_CODE %in% species, ]
  if(is_available(IUCN_status))        data_filtered = data_filtered[ IUCN_STATUS_CODE %in% IUCN_status, ]

  if(nrow(data_filtered) == 0) stop("No data identified by current criteria!")

  return (data_filtered)
}
