###################
# CA data browser #
###################

library(shiny)
library(periscope)
library(lubridate)
library(stringr)
library(iotc.base.common.plots)

#See https://statnmap.com/2016-04-19-a-nice-rshiny-onoff-switch-button/#
source("Rsource/SwitchButton.R")

CE_RAW_ENV = new.env()

load("./RAW.CA.rda",   envir = CE_RAW_ENV)
load("./METADATA.rda", envir = CE_RAW_ENV)

#load("./CA.RData")

DATA = CE_RAW_ENV$RAW.CA

source("../../scripts/constants.R")
source("../../scripts/data_management.R")
source("../../scripts/UI_management.R")

source("../../scripts/common_all.R")
source("../../scripts/common_georeferenced.R")

source("CA_initialization.R")

REF = initialize_CA_reference_data(DATA)

source("CA_configuration.R")

ui =
  fluidPage(
    tags$head(includeHTML(("../../_resources/google-analytics.html"))),
    theme = "button.css",
    includeCSS("../../_resources/css/common.css"),
    title = UI_window_title(SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.CA$LAST_UPDATE, REF),
    UI_main(
      SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.CA$LAST_UPDATE, REF, "info",
      fluidRow(
        UI_filters_CA(REF),
        column(
          width = 8,
          UI_filters_categories(CATCH_CATEGORIES),
          fluidRow(
            tabsetPanel(
              UI_summary_default(),
              UI_GEO()
            )
          )
        )
      ),
      "//data.iotc.org/browser/CE/CA"
    )
  )

server = function(input, output) {
  common_prepare_handlers(DATA, DATA, input, output, CA_RAW_PREFIX, SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.CA$LAST_UPDATE, current_configuration(REF))
  georeferenced_prepare_handlers(DATA, input, output, CA_RAW_PREFIX, SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.CA$LAST_UPDATE, current_configuration(REF))
}

# Run the application
shinyApp(ui = ui, server = server)
