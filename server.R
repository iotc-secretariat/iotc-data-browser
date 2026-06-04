
server = function(input, output, session) {
  
  #model <- reactiveValues(page = "home", load_from_url = TRUE)
  react_module <- reactiveVal("home")
  react_resolver <- reactiveVal(TRUE)
  react_nc_raw_activated <- reactiveVal(FALSE)
  react_nc_sci_activated <- reactiveVal(FALSE)
  react_ce_ef_activated <- reactiveVal(FALSE)
  react_ce_ca_activated <- reactiveVal(FALSE)
  react_sf_raw_activated <- reactiveVal(FALSE)
  react_sf_std_activated <- reactiveVal(FALSE)
  
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
                                     tags$span(DATASET_TITLES$NC_RAW)
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
                                     tags$span(DATASET_TITLES$NC_SCI)
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
                                     tags$span(DATASET_TITLES$CE_EF)
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
                                     tags$span(DATASET_TITLES$CE_CA)
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
                                     tags$span(DATASET_TITLES$SF_RAW)
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
                                     tags$span(DATASET_TITLES$SF_STD)
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
    DEBUG("Render MAIN UI")
    switch(react_module(),
      "home" = {
        homeUI()
      },
      "nc-raw" = {
        INFO("Load NC-RAW module")
        nc_raw_ui("nc-raw")
      },
      "nc-sci" = {
        INFO("Load NC-SCI module")
        nc_sci_ui("nc-sci")
      },
      "ce-ef" = {
        INFO("Load CE-EF module")
        ce_ef_ui("ce-ef")
      },
      "ce-ca" = {
        INFO("Load CE-CA module")
        ce_ca_ui("ce-ca")
      },
      "sf-raw" = {
        INFO("Load SF-RAW module")
        sf_raw_ui("sf-raw")
      },
      "sf-std" = {
        INFO("Load load SF-STD module")
        sf_std_ui("sf-std")
      }
    )
  })
  
  #events to update the model/page
  observeEvent(input$home,{ 
    react_module("home"); react_resolver(FALSE);
    updateURL(session, "#") 
  }, ignoreInit = T)
  observeEvent(input$module_nc_raw,{
    react_module("nc-raw"); react_resolver(FALSE);
    react_nc_raw_activated(TRUE)
    updateURL(session, "#NC-RAW")
  }, ignoreInit = T)
  observeEvent(input$module_nc_sci,{
    react_module("nc-sci"); react_resolver(FALSE);
    react_nc_sci_activated(TRUE)
    updateURL(session, "#NC-SCI")
  }, ignoreInit = T)
  observeEvent(input$module_ce_ef,{
    react_module("ce-ef"); react_resolver(FALSE);
    react_ce_ef_activated(TRUE)
    updateURL(session, "#CE-EF")
  }, ignoreInit = T)
  observeEvent(input$module_ce_ca,{
    react_module("ce-ca"); react_resolver(FALSE);
    react_ce_ca_activated(TRUE)
    updateURL(session, "#CE-CA")
  }, ignoreInit = T)
  observeEvent(input$module_sf_raw,{
    react_module("sf-raw"); react_resolver(FALSE);
    react_sf_raw_activated(TRUE)
    updateURL(session, "#SF-RAW")
  }, ignoreInit = T)
  observeEvent(input$module_sf_std,{
    react_module("sf-std"); react_resolver(FALSE);
    react_sf_std_activated(TRUE)
    updateURL(session, "#SF-STD")
  }, ignoreInit = T)
  
  #mechanism to load a module page from the URL
  observe({
    req(react_resolver())
    hash = session$clientData$url_hash
    module = NULL
    print(hash)
    if(hash %in% c("#","")){
      module = "home"
    }else{
      module = tolower(substr(hash, 2, nchar(hash)))
    }
    if(module != "home"){
      module_prefix = gsub("-","_", module)
      eval(parse(text = paste0("react_", module_prefix, "_activated(TRUE)")))
    }
    react_module(module)
  })

  #configure module servers
  nc_raw_server("nc-raw", react_nc_raw_activated)
  nc_sci_server("nc-sci", react_nc_sci_activated)
  ce_ef_server("ce-ef", react_ce_ef_activated)
  ce_ca_server("ce-ca", react_ce_ca_activated)
  sf_raw_server("sf-raw", react_sf_raw_activated)
  sf_std_server("sf-std", react_sf_std_activated)
}
