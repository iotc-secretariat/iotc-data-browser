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
  fleets = iotc.data.reference.codelists::LEGACY_FLEETS_IOTDB[order(SORT)]
  FLEETS  = setNames(as.character(fleets$CODE), paste(fleets$CODE, "-", fleets$NAME_EN))

  fishery_type_codes = unique(data$FISHERY_TYPE_CODE)
  fishery_types = iotc.data.reference.codelists::LEGACY_FISHERY_TYPES_IOTDB[order(SORT)]
  fishery_types = fishery_types[fishery_types$CODE %in% fishery_type_codes,]
  FISHERY_TYPES      = sort(setNames(as.character(fishery_types$CODE), nm = paste(fishery_types$CODE, "-", fishery_types$NAME_EN)))
  
  fishery_group_codes = unique(data$FISHERY_GROUP_CODE)
  fishery_groups = iotc.data.reference.codelists::LEGACY_FISHERY_GROUPS_IOTDB[order(SORT)]
  fishery_groups = fishery_groups[fishery_groups$CODE %in% fishery_group_codes,]
  FISHERY_GROUPS      = sort(setNames(as.character(fishery_groups$CODE), nm = paste(fishery_groups$CODE, "-", fishery_groups$NAME_EN)))
  
  fishery_codes = unique(data$FISHERY_CODE)
  fisheries = iotc.data.reference.codelists::LEGACY_FISHERIES_IOTDB[order(SORT)]
  fisheries = fisheries[fisheries$CODE %in% fishery_codes,]
  FISHERIES     = sort(setNames(as.character(fisheries$CODE), nm = paste(fisheries$CODE,"-",fisheries$NAME_EN)))
  
  gear_codes = unique(data$GEAR_CODE)
  gears = iotc.data.reference.codelists::LEGACY_GEARS_IOTDB[order(SORT)]
  gears = gears[gears$CODE %in% gear_codes,]
  GEARS      = sort(setNames(as.character(gears$CODE), nm = paste(gears$CODE,"-",gears$NAME_EN)))
  
  SPECIES_WPS        = NA
  SPECIES_GROUPS     = NA
  SPECIES_CATEGORIES = NA
  SPECIES            = NA
  IUCN_STATUS        = NA

  if(is_available(which(colnames(data) == C_IUCN_STATUS_CODE))) {
    IUCN_status_codes = unique(data$IUCN_STATUS_CODE)
    IUCN_status = iotc.data.reference.codelists::LEGACY_IUCN_STATUS_IOTDB[order(SORT)]
    IUCN_status = IUCN_status[IUCN_status$CODE %in% IUCN_status_codes,]
    IUCN_STATUS       = sort(setNames(as.character(IUCN_status$CODE), nm = paste(IUCN_status$CODE, "-", IUCN_status$NAME_EN)))
  }

  if(is_available(which(colnames(data) == C_SPECIES_WP_CODE))) {
    species_WP_codes = unique(data$SPECIES_WP_CODE)
    species_WPs = iotc.data.reference.codelists::LEGACY_WORKING_PARTIES_IOTDB[order(SORT)]
    species_WPs = species_WPs[species_WPs$CODE %in% species_WP_codes,]
    SPECIES_WPS = sort(setNames(as.character(species_WPs$CODE), nm = paste(species_WPs$CODE, "-", species_WPs$NAME_EN)))
  }

  if(is_available(which(colnames(data) == C_SPECIES_GROUP_CODE))) {
    species_group_codes = unique(data$SPECIES_GROUP_CODE)
    species_groups = iotc.data.reference.codelists::LEGACY_SPECIES_GROUPS_IOTDB[order(SORT)]
    species_groups = species_groups[species_groups$CODE %in% species_group_codes,]
    SPECIES_GROUPS = sort(setNames(as.character(species_groups$CODE), nm = paste(species_groups$CODE, "-", species_groups$NAME_EN)))
  }

  if(is_available(which(colnames(data) == C_SPECIES_CATEGORY_CODE))) {
    species_category_codes = unique(data$SPECIES_CATEGORY_CODE)
    species_categories = iotc.data.reference.codelists::LEGACY_SPECIES_CATEGORIES_IOTDB[order(SORT)]
    species_categories = species_categories[species_categories$CODE %in% species_category_codes,]
    SPECIES_CATEGORIES = sort(setNames(as.character(species_categories$CODE), nm = paste(species_categories$CODE,"-",species_categories$NAME_EN)))
  }

  if(is_available(which(colnames(data) == C_SPECIES_CODE))) {
    species_codes = unique(data$SPECIES_CODE)
    species = iotc.data.reference.codelists::LEGACY_SPECIES_IOTDB[order(SORT)]
    species[, CODE := trimws(CODE)] #species cl to clean
    species[, NAME_EN := trimws(NAME_EN)] #species cl to clean
    species = unique(species[,.(CODE, NAME_EN)])
    species = species[species$CODE %in% species_codes,]
    SPECIES = sort(setNames(as.character(species$CODE), nm = paste(species$CODE, "-", species$NAME_EN)))
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

get_codelist_for_term = function(base_name){
  codelist_name <- switch(base_name,
         "FISHERY" = "LEGACY_FISHERIES_IOTDB",
         "IUCN_STATUS" = "LEGACY_IUCN_STATUS_IOTDB",
         "SPECIES_WP" = "LEGACY_WORKING_PARTIES_IOTDB",
         "SPECIES_CATEGORY" = "LEGACY_SPECIES_CATEGORIES_IOTDB",
         "SPECIES" = "LEGACY_SPECIES_IOTDB",
         "CATCH_UNIT" = "LEGACY_CATCH_UNITS",
         "FATE" = "LEGACY_FATES",
         "SCHOOL_TYPE" = "LEGACY_SCHOOL_TYPES",
         "MEASURE_UNIT" = "LEGACY_MEASUREMENT_TYPES",
         "RAISE" = "LEGACY_RAISINGS",
         "SEX" = "SEX",
         paste0("LEGACY_", base_name, "S_IOTDB")
  )
  codelist <- try(get(codelist_name, envir = asNamespace("iotc.data.reference.codelists")), silent = TRUE)
  if(is(codelist, "try-error")){
    warning(sprintf("No codelist '%s'", codelist_name))
  }
  return(codelist)
}

enrich_data_table_with_descriptions <- function(dt, code_selection = NULL){
  
  code_cols <- grep("_CODE$", names(dt), value = TRUE)
  if(!is.null(code_selection)) code_cols = code_selection
  
  print(code_cols)
  
  for (code_col in code_cols) {
    # Extract the base name
    base_name <- sub("_CODE$", "", code_col)
    
    # Construct codelist name
    codelist <- get_codelist_for_term(base_name)
    
    if(is(codelist, "try-error")){
      next
    }
    
    label_key = "NAME_EN"
    if(!code_col %in% names(codelist)) codelist = codelist |> dplyr::rename(!!rlang::sym(code_col) := "CODE")
    has_sort <- "SORT" %in% names(codelist)
    select_cols <- c(code_col, "NAME_EN")
    if(has_sort) select_cols <- c(select_cols, "SORT")
    codelist = codelist |> dplyr::select(dplyr::all_of(select_cols)) |> unique()
    
    dt = dt |> 
      dplyr::left_join(y = codelist, by = code_col)
    
    if(has_sort) {
      ordered_levels <- codelist |>
        dplyr::filter(!is.na(NAME_EN)) |>
        dplyr::arrange(SORT) |>
        dplyr::pull(NAME_EN) |>
        unique()
      
      dt <- dt |> 
        dplyr::mutate(!!rlang::sym(base_name) := factor(NAME_EN, levels = ordered_levels)) |>
        dplyr::select(-NAME_EN, -SORT)
    } else {
      dt <- dt |> dplyr::rename(!!rlang::sym(base_name) := "NAME_EN")
    }
    
    dt = dt |>
      dplyr::relocate(dplyr::all_of(base_name), .after = dplyr::all_of(code_col))
  }
  return(dt)
}
