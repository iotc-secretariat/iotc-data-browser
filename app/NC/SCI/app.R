#######################
# NC SCI data browser #
#######################

library(shiny)
library(periscope)
library(lubridate)
library(stringr)
library(iotc.base.common.plots)

#See https://statnmap.com/2016-04-19-a-nice-rshiny-onoff-switch-button/#
source("Rsource/SwitchButton.R")

SOURCE_DATASET = "best scientific estimates of nominal catches"

NC_SCI_ENV = new.env()

load("./SCI.rda",       envir = NC_SCI_ENV)
load("./DQA.rda",       envir = NC_SCI_ENV)
load("./METADATA.rda",  envir = NC_SCI_ENV)

#load("./NC_SCI.RData")

source("../../_common/constants.R")
source("../../_common/data_management.R")
source("../../_common/UI_management.R")

source("../NC_configuration.R")
source("../NC_initialization.R")
source("../NC_extras.R")

source("../../_common/common_all.R")

REF = initialize_NC_reference_data(NC_SCI_ENV$SCI)

ui =
  fluidPage(
    tags$head(includeHTML(("../../_resources/google-analytics.html"))),
    theme = "button.css",
    includeCSS("../../_resources/css/common.css"),
    title = UI_window_title(SOURCE_DATASET, NC_SCI_ENV$METADATA$SCI$LAST_UPDATE, REF),
    UI_main(
      SOURCE_DATASET, NC_SCI_ENV$METADATA$SCI$LAST_UPDATE, REF, "primary",
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
      "//data.iotc.org/browser/NC/SCI"
    )
  )

server = function(input, output) {
  # Prepare output handlers
  prepare_NC_handlers(NC_SCI_ENV$SCI, NC_SCI_ENV$DQA, input, output, "SCI", SOURCE_DATASET, NC_SCI_ENV$METADATA$SCI$LAST_UPDATE, current_configuration(REF))
}

# Run the application
shinyApp(ui = ui, server = server)
