library(curl)

#######################
# NC RAW data browser #
#######################

library(shiny)
library(periscope)

library(lubridate)
library(stringr)
library(iotc.base.common.plots)

#See https://statnmap.com/2016-04-19-a-nice-rshiny-onoff-switch-button/#
source("Rsource/SwitchButton.R")

SOURCE_DATASET = "raw nominal catches"

NC_RAW_ENV = new.env()

load("./RAW.rda",      envir = NC_RAW_ENV)
load("./DQA.rda",      envir = NC_RAW_ENV)
load("./METADATA.rda", envir = NC_RAW_ENV)

#load("./NC.RData")

DATA   = NC_RAW_ENV$RAW
DATA_Q = NC_RAW_ENV$DQA

source("../../_common/constants.R")
source("../../_common/data_management.R")
source("../../_common/UI_management.R")

source("../NC_configuration.R")
source("../NC_initialization.R")
source("../NC_extras.R")

source("../../_common/common_all.R")

REF = initialize_NC_reference_data(NC_RAW_ENV$RAW)

ui =
  fluidPage(
    tags$head(includeHTML(("../../_resources/google-analytics.html"))),
    theme = "button.css",
    includeCSS("../../_resources/css/common.css"),
    title = UI_window_title(SOURCE_DATASET, NC_RAW_ENV$METADATA$RAW$LAST_UPDATE, REF),
    UI_main(
      SOURCE_DATASET, NC_RAW_ENV$METADATA$RAW$LAST_UPDATE, REF, "primary",
      fluidRow(
        UI_filters_NC(REF),
        column(
          width = 8,
          UI_filters_categories(NOMINAL_CATCH_CATEGORIES),
          fluidRow(
            tabsetPanel(
              UI_summary_default(),
              UI_NC_quality()
            )
          )
        )
      ),
      "//data.iotc.org/browser/NC/RAW"
    )
  )

server = function(input, output) {
  # Prepare output handlers
  prepare_NC_handlers(NC_RAW_ENV$RAW, NC_RAW_ENV$DQA, input, output, "RAW", SOURCE_DATASET, NC_RAW_ENV$METADATA$RAW$LAST_UPDATE, current_configuration(REF))
}

# Run the application
shinyApp(ui = ui, server = server)
