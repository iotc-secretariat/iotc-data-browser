###################
# EF data browser #
###################

library(shiny)
library(periscope)
library(lubridate)
library(stringr)
library(iotc.base.common.plots)

#See https://statnmap.com/2016-04-19-a-nice-rshiny-onoff-switch-button/#
source("Rsource/SwitchButton.R")

CE_RAW_ENV = new.env()

load("./RAW.EF.rda",   envir = CE_RAW_ENV)
load("./METADATA.rda", envir = CE_RAW_ENV)

#load("./EF.RData")

DATA = CE_RAW_ENV$RAW.EF

source("../../scripts/constants.R")
source("../../scripts/data_management.R")
source("../../scripts/UI_management.R")

source("../../scripts/common_all.R")
source("../../scripts/common_georeferenced.R")

source("EF_initialization.R")

REF = initialize_EF_reference_data(DATA)
DATA = update_data(REF, DATA)

source("EF_configuration.R")

ui =
  fluidPage(
    tags$head(includeHTML(("../../_resources/google-analytics.html"))),
    theme = "button.css",
    includeCSS("../../_resources/css/common.css"),
    title = UI_window_title(SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.EF$LAST_UPDATE, REF),
    UI_main(
      SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.EF$LAST_UPDATE, REF, "info",
      fluidRow(
        UI_filters_EF(REF),
        column(
          width = 8,
          UI_filters_categories(EFFORT_CATEGORIES),
          fluidRow(
            tabsetPanel(
              UI_summary_default(),
              UI_GEO()
            )
          )
        )
      ),
      "//data.iotc.org/browser/CE/EF"
    )
  )

server = function(input, output) {
  common_prepare_handlers(DATA, DATA, input, output, EF_RAW_PREFIX, SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.EF$LAST_UPDATE, current_configuration(REF))
  georeferenced_prepare_handlers(DATA, input, output, EF_RAW_PREFIX, SOURCE_DATASET, CE_RAW_ENV$METADATA$RAW.EF$LAST_UPDATE, current_configuration(REF))
}

# Run the application
shinyApp(ui = ui, server = server)
