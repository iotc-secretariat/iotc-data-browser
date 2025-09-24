
server = function(input, output, session) {
  
  model <- reactiveValues(page = "home")
  
  homeUI <- function(){
    tagList(
      tags$head(includeHTML(("./assets/_resources/google-analytics.html"))),
      includeCSS("./assets/_resources/css/home.css"),
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
                                     "Nominal catches by year, IOTC area, fleet, fishery and species"
                                   )
                          ),
                          tags$div(class="panel-body",
                                   tags$h3(
                                     actionLink(inputId = "module_nc_raw", class="btn btn-primary text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "NC-RAW"
                                     ),
                                     tags$span("Nominal catches for all species, including bycatch ones")
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
                                     tags$span("Fully disaggregated, best scientific estimates for IOTC species only")
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
                                     "Catch-and-effort by year, quarter, grid, fleet, fishery and species")
                          ),
                          tags$div(class="panel-body",
                                   tags$h3(
                                     actionLink(inputId = "module_ce_ef",class="btn btn-info text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "CE-EF"
                                     ),
                                     tags$span("Reported georeferenced quarterly efforts")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.CE::METADATA$RAW.EF$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE),
                                     RAW(),
                                     UNRAISED("Not raised to total nominal efforts for the stratum"),
                                     QUARTERLY(),
                                     EFFORT(),
                                     REGULAR_GRIDS()
                                   ),
                                   tags$h3(
                                     actionLink(inputId = "module_ce_ca", class="btn btn-info text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "CE-CA"
                                     ),
                                     tags$span("Reported georeferenced quarterly catches (in weight and / or numbers)")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.CE::METADATA$CA$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.CE::METADATA$CA$LAST_UPDATE),
                                     RAW(),
                                     UNRAISED("Not raised to total nominal catches for the stratum"),
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
                 tags$div(class="panel panel-warning",
                          tags$div(class="panel-heading",
                                   tags$h2(
                                     faw_("balance-scale-left"),
                                     faw_("calendar-day"),
                                     faw_("map-marker-alt"),
                                     "Raised catches by year, quarter, 5x5 grid, fleet, fishery and species"
                                   ),
                          ),
                          tags$div(class="panel-body",
                                   tags$h3(
                                     tags$code(
                                       tags$span(class="glyphicon glyphicon-ban-circle", " "),
                                       tags$span("RESTRICTED ACCESS")
                                     )
                                   ),
                                   tags$h3(
                                     actionLink(inputId = "module_ca_raised", class="btn btn-warning text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "CA-RAISED"
                                     ),
                                     tags$span("Estimated, raised georeferenced quarterly catches (weight and numbers) for main species")
                                   ),
                                   tags$div(
                                     RECORDS(iotc.data.reference.datasets.CE::METADATA$RSD.CA$DATA),
                                     LAST_UPDATED(iotc.data.reference.datasets.CE::METADATA$RSD.CA$LAST_UPDATE),
                                     DISAGGREGATED(),
                                     RAISED("Raised to represent total catch for the stratum"),
                                     QUARTERLY(),
                                     MULTIPLE(
                                       CATCH_LIVE_WEIGHT(),
                                       CATCH_NUMBERS()
                                     ),
                                     REGULAR_5x5_GRIDS(),
                                     MAIN_IOTC_SPECIES()
                                   )
                          )
                 ),
                 tags$div(class="panel panel-success",
                          tags$div(class="panel-heading",
                                   tags$h2(
                                     faw_("sliders-h"),
                                     faw_("calendar-day"),
                                     faw_("map-marker-alt"),
                                     "Size-frequency by year, quarter, grid, fleet, fishery, species and measure"
                                   )
                          ),
                          tags$div(class="panel-body",
                                   tags$h3(
                                     actionLink(inputId = "module_sf_raw", class="btn btn-success text-monospace button-link", title="Click to explore this dataset",
                                                icon = faw_("search"),
                                                label = "SF-RAW"
                                     ),
                                     tags$span("Reported georeferenced size-frequency")
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
                                     tags$span("Standardized georeferenced size-frequency data for IOTC and relevant shark species")
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
        SOURCE_DATASET = "raw nominal catches"
        REF = initialize_NC_reference_data(iotc.data.reference.datasets.NC::RAW)
        nc_raw_server("nc-raw", SOURCE_DATASET, REF, input, output, session)
        nc_raw_ui("nc-raw", SOURCE_DATASET, REF)
      },
      "nc-sci" = {
        INFO("Load NC-SCI module")
        SOURCE_DATASET = "best scientific estimates of nominal catches"
        REF = initialize_NC_reference_data(iotc.data.reference.datasets.NC::SCI)
        nc_sci_server("nc-sci", SOURCE_DATASET, REF, input, output, session)
        nc_sci_ui("nc-sci", SOURCE_DATASET, REF)
      },
      "ce-ef" = {
        DEBUG("Should load CE-EF module")
      },
      "ce-ca" = {
        DEBUG("Should load CE-CA module")
      },
      "sf-raw" = {
        DEBUG("Should load SF-RAW module")
      },
      "sf-std" = {
        DEBUG("Should load SF-STD module")
      }
    )
  })
  
  #events to update the model/page
  observeEvent(input$module_nc_raw,{ model$page <- "nc-raw" }, ignoreInit = T)
  observeEvent(input$module_nc_sci,{ model$page <- "nc-sci" }, ignoreInit = T)
  observeEvent(input$module_ce_ef,{ model$page <- "ce-ef" }, ignoreInit = T)
  observeEvent(input$module_ce_ca,{ model$page <- "ce-ca" }, ignoreInit = T)
  observeEvent(input$module_sf_raw,{model$age <- "sf-raw"}, ignoreInit = T)
  observeEvent(input$module_sf_std,{model$age <- "sf-std"}, ignoreInit = T)

}