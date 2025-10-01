
server = function(input, output, session) {
  
  model <- reactiveValues(page = "home")
  
  homeUI <- function(){
    tagList(
      tags$head(includeHTML(("www/google-analytics.html"))),
      includeCSS("www/css/home.css"),
      titlePanel(
        windowTitle = "IOTC data browser - explore the content of IOTC datasets",
        title = ""
      ),
      fluidRow(
        column = 12,
        tags$div(class="container",
                 #tags$h1(
                 #  tags$img(src="iotc-logo.png"),
                 #  tags$span(
                 #    "Data browser - Explore the current content of IOTC datasets"
                 #  )
                 #),
                 tags$div(class="panel panel-primary",
                          tags$div(class="panel-heading",
                                   tags$h2(
                                     faw_("balance-scale"),
                                     faw_("calendar-alt"),
                                     faw_("globe-asia"),
                                     "Retained catches by year, IOTC area, fleet, fishery, and species"
                                   )
                          ),
                          tags$div(class="panel-body",
                                   tags$h3(
                                     actionLink(inputId = "module_nc_raw", class="btn btn-primary text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "NC-RAW"
                                     ),
                                     tags$span("Retained catches for all IOTC and non-IOTC species")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.NC::METADATA$RAW$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.NC::METADATA$RAW$LAST_UPDATE),
                                     RAW(),
                                     RAISED(),
                                     YEARLY(),
                                     CATCH_LIVE_WEIGHT(),
                                     IO_AREAS(),
                                     ALL_SPECIES()
                                   ),
                                   tags$h3(
                                     actionLink(inputId = "module_nc_sci", class="btn btn-primary text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "NC-SCI"
                                     ),
                                     tags$span("Best scientific estimates of retained catches of IOTC species")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.NC::METADATA$SCI$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.NC::METADATA$SCI$LAST_UPDATE),
                                     DISAGGREGATED(),
                                     RAISED(),
                                     YEARLY(),
                                     CATCH_LIVE_WEIGHT(),
                                     IO_AREAS(),
                                     IOTC_SPECIES()
                                   )
                          )
                 ),
                 tags$div(class="panel panel-info",
                          tags$div(class="panel-heading",
                                   tags$h2(
                                     faw_("balance-scale"),
                                     faw_("calendar-day"),
                                     faw_("map-marker-alt"),
                                     "Retained catches and efforts by year, quarter, grid, fleet, fishery, and species")
                          ),
                          tags$div(class="panel-body",
                                   tags$h3(
                                     actionLink(inputId = "module_ce_ef",class="btn btn-info text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "CE-EF"
                                     ),
                                     tags$span("Reported geo-referenced efforts")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.CE::METADATA$RAW.EF$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE),
                                     RAW(),
                                     UNRAISED("Not raised to total efforts for the stratum"),
                                     QUARTERLY(),
                                     EFFORT(),
                                     REGULAR_GRIDS()
                                   ),
                                   tags$h3(
                                     actionLink(inputId = "module_ce_ca", class="btn btn-info text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "CE-CA"
                                     ),
                                     tags$span("Reported geo-referenced catches (weight and/or numbers)")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.CE::METADATA$CA$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.CE::METADATA$CA$LAST_UPDATE),
                                     RAW(),
                                     UNRAISED("Not raised to total retained catches for the stratum"),
                                     QUARTERLY(),
                                     MULTIPLE(
                                       CATCH_LIVE_WEIGHT(),
                                       CATCH_NUMBERS()
                                     ),
                                     REGULAR_GRIDS(),
                                     ALL_SPECIES()
                                   )
                          )
                 ),
                 # tags$div(class="panel panel-warning",
                 #          tags$div(class="panel-heading",
                 #                   tags$h2(
                 #                     faw_("balance-scale-left"),
                 #                     faw_("calendar-day"),
                 #                     faw_("map-marker-alt"),
                 #                     "Raised catches by year, quarter, 5x5 grid, fleet, fishery and species"
                 #                   ),
                 #          ),
                 #          tags$div(class="panel-body",
                 #                   tags$h3(
                 #                     tags$code(
                 #                       tags$span(class="glyphicon glyphicon-ban-circle", " "),
                 #                       tags$span("RESTRICTED ACCESS")
                 #                     )
                 #                   ),
                 #                   tags$h3(
                 #                     actionLink(inputId = "module_ca_raised", class="btn btn-warning text-monospace button-link", title="Click to explore this dataset",
                 #                                icon = faw_("search"),
                 #                                label = "CA-RAISED"
                 #                     ),
                 #                     tags$span("Estimated, raised georeferenced quarterly catches (weight and numbers) for main species")
                 #                   ),
                 #                   tags$div(
                 #                     RECORDS(iotc.data.reference.datasets.CE::METADATA$RSD.CA$DATA),
                 #                     LAST_UPDATED(iotc.data.reference.datasets.CE::METADATA$RSD.CA$LAST_UPDATE),
                 #                     DISAGGREGATED(),
                 #                     RAISED("Raised to represent total catch for the stratum"),
                 #                     QUARTERLY(),
                 #                     MULTIPLE(
                 #                       CATCH_LIVE_WEIGHT(),
                 #                       CATCH_NUMBERS()
                 #                     ),
                 #                     REGULAR_5x5_GRIDS(),
                 #                     MAIN_IOTC_SPECIES()
                 #                   )
                 #          )
                 # ),
                 tags$div(class="panel panel-success",
                          tags$div(class="panel-heading",
                                   tags$h2(
                                     faw_("sliders-h"),
                                     faw_("calendar-day"),
                                     faw_("map-marker-alt"),
                                     "Size-frequencies of catches by year, quarter, grid, fleet, fishery, and species"
                                   )
                          ),
                          tags$div(class="panel-body",
                                   tags$h3(
                                     actionLink(inputId = "module_sf_raw", class="btn btn-success text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "SF-RAW"
                                     ),
                                     tags$span("Reported geo-referenced size-frequencies for all IOTC and non-IOTC species")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE),
                                     RAW(),
                                     UNRAISED(),
                                     QUARTERLY(),
                                     MULTIPLE(
                                       LENGTHS(),
                                       WEIGHTS()
                                     ),
                                     REGULAR_GRIDS(),
                                     ALL_SPECIES()
                                   ),
                                   tags$h3(
                                     actionLink(inputId = "module_sf_std", class="btn btn-success text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "SF-STD"
                                     ),
                                     tags$span("Standardised geo-referenced size-frequencies for IOTC species and common pelagic sharks")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.SF.std::METADATA$STD.SF$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.SF.std::METADATA$STD.SF$LAST_UPDATE),
                                     CONVERTED("Converted using species-specific equations"),
                                     UNRAISED(),
                                     QUARTERLY(),
                                     MULTIPLE(
                                       FL_LENGTHS(),
                                       ESTIMATED_WEIGHTS()
                                     ),
                                     REGULAR_5x5_GRIDS(),
                                     IOTC_AND_SHARK_SPECIES()
                                   )
                          )
                 )
        )
      )
    )
  }
  
  output$main_ui <- renderUI({
    
    switch(model$page,
      "home" = {
        homeUI()
      },
      "nc-raw" = {
        INFO("Load NC-RAW module")
        source("./modules/NC/NC_configuration.R")
        source("./modules/NC/NC_initialization.R")
        source("./modules/NC/NC_extras.R")
        SOURCE_DATASET = "raw nominal catches"
        REF = initialize_NC_reference_data(iotc.data.reference.datasets.NC::RAW)
        nc_raw_server("nc-raw", SOURCE_DATASET, REF, input, output, session)
        nc_raw_ui("nc-raw", SOURCE_DATASET, REF)
      },
      "nc-sci" = {
        INFO("Load NC-SCI module")
        source("./modules/NC/NC_configuration.R")
        source("./modules/NC/NC_initialization.R")
        source("./modules/NC/NC_extras.R")
        SOURCE_DATASET = "best scientific estimates of nominal catches"
        REF = initialize_NC_reference_data(iotc.data.reference.datasets.NC::SCI)
        nc_sci_server("nc-sci", SOURCE_DATASET, REF, input, output, session)
        nc_sci_ui("nc-sci", SOURCE_DATASET, REF)
      },
      "ce-ef" = {
        INFO("Load CE-EF module")
        source("./modules/CE/EF/EF_configuration.R")
        source("./modules/CE/EF/EF_initialization.R")
        SOURCE_DATASET = "raw georeferenced efforts"
        REF = initialize_EF_reference_data(iotc.data.reference.datasets.CE::RAW.EF)
        DATA = update_data(REF, iotc.data.reference.datasets.CE::RAW.EF)
        ce_ef_server("ce-ef", SOURCE_DATASET, DATA, REF, input, output, session)
        ce_ef_ui("ce-ef", SOURCE_DATASET, REF)
      },
      "ce-ca" = {
        INFO("Load CE-CA module")
        source("./modules/CE/CA/CA_configuration.R")
        source("./modules/CE/CA/CA_initialization.R")
        SOURCE_DATASET = "raw georeferenced catches"
        DATA = iotc.data.reference.datasets.CE::RAW.CA
        REF = initialize_CA_reference_data(iotc.data.reference.datasets.CE::RAW.CA)
        ce_ca_server("ce-ca", SOURCE_DATASET, DATA, REF, input, output, session)
        ce_ca_ui("ce-ca", SOURCE_DATASET, REF)
      },
      # "ca-raised" = {
      #   LOAD("Load CA-RAISED module")
      #   #TODO
      # },
      "sf-raw" = {
        INFO("Load SF-RAW module")
        source("./modules/SF/SF_configuration.R")
        source("./modules/SF/SF_initialization.R")
        source("./modules/SF/SF_extras.R")
        SOURCE_DATASET = "raw georeferenced size-frequencies"
        DATA =
          rbind(
            iotc.data.reference.datasets.SF.raw::RAW.TROP,
            iotc.data.reference.datasets.SF.raw::RAW.TEMP,
            iotc.data.reference.datasets.SF.raw::RAW.BILL,
            iotc.data.reference.datasets.SF.raw::RAW.NERI,
            iotc.data.reference.datasets.SF.raw::RAW.SEER,
            iotc.data.reference.datasets.SF.raw::RAW.TNEI,
            iotc.data.reference.datasets.SF.raw::RAW.SHRK,
            iotc.data.reference.datasets.SF.raw::RAW.ETPS,
            iotc.data.reference.datasets.SF.raw::RAW.OTHR
          )
        DATA_TABLE = DATA      [, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(DATA),       c(C_MONTH_START, C_MONTH_END, C_FISH_COUNT))]
        DATA       = DATA_TABLE[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(DATA_TABLE), c(C_CLASS_LOW, C_CLASS_HIGH, C_FISH_COUNT))]
        
        source("./modules/SF/RAW/SF_RAW_configuration.R")
        REF = initialize_SF_reference_data(DATA)
        DATA = update_data(REF, DATA)
        DATA_TABLE = update_data(REF, DATA_TABLE)
        
        sf_raw_server("sf-raw", SOURCE_DATASET, DATA, DATA_TABLE, REF, input, output, session)
        sf_raw_ui("sf-raw", SOURCE_DATASET, REF)
      },
      "sf-std" = {
        INFO("Load load SF-STD module")
        source("./modules/SF/SF_configuration.R")
        source("./modules/SF/SF_initialization.R")
        source("./modules/SF/SF_extras.R")
        SOURCE_DATASET = "standardized georeferenced size-frequencies"
        
        DATA = rbind(
          iotc.data.reference.datasets.SF.std::STD.TROP,
          iotc.data.reference.datasets.SF.std::STD.TEMP,
          iotc.data.reference.datasets.SF.std::STD.BILL,
          iotc.data.reference.datasets.SF.std::STD.NERI,
          iotc.data.reference.datasets.SF.std::STD.SEER,
          iotc.data.reference.datasets.SF.std::STD.SHRK
        )
        
        DATA_TABLE = DATA      [, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(DATA),       c(C_MONTH_START, C_MONTH_END, C_FISH_COUNT))]
        DATA       = DATA_TABLE[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(DATA_TABLE), c(C_CLASS_LOW, C_CLASS_HIGH, C_FISH_COUNT))]
        
        DEFAULT_MEASURE_TYPE = "FL"
        
        source("./modules/SF/STD/SF_STD_configuration.R")
        source("./modules/SF/STD/SF_STD_extras.R")
        REF = initialize_SF_reference_data(DATA)
        DATA = update_data(REF, DATA)
        DATA_TABLE = update_data(REF, DATA_TABLE)
        
        sf_std_server("sf-std", SOURCE_DATASET, DATA, DATA_TABLE, REF, input, output, session)
        sf_std_ui("sf-std", SOURCE_DATASET, REF)
      }
    )
  })
  
  #events to update the model/page
  observeEvent(input$home,{ model$page <- "home"; updateURL(session, "#") }, ignoreInit = T)
  observeEvent(input$module_nc_raw,{ model$page <- "nc-raw"; updateURL(session, "#NC-RAW") }, ignoreInit = T)
  observeEvent(input$module_nc_sci,{ model$page <- "nc-sci"; updateURL(session, "#NC-SCI") }, ignoreInit = T)
  observeEvent(input$module_ce_ef,{ model$page <- "ce-ef" }, ignoreInit = T)
  observeEvent(input$module_ce_ca,{ model$page <- "ce-ca" }, ignoreInit = T)
  # observeEvent(input$module_ca_raised,{ model$page <- "ca-raised" }, ignoreInit = T)
  observeEvent(input$module_sf_raw,{model$page <- "sf-raw"}, ignoreInit = T)
  observeEvent(input$module_sf_std,{model$page <- "sf-std"}, ignoreInit = T)
  
  #mechanism to load a module page from the URL
  observe({
    hash = session$clientData$url_hash
    page = NULL
    print(hash)
    if(hash %in% c("#","")){
      page = "home"
    }else{
      page = tolower(substr(hash, 2, nchar(hash)))
    }
    model$page = page
  })

}
