#######################
# SF RAW data browser #
#######################

library(shiny)
library(periscope)
library(lubridate)
library(stringr)
library(iotc.base.common.plots)

#See https://statnmap.com/2016-04-19-a-nice-rshiny-onoff-switch-button/#
source("Rsource/SwitchButton.R")

SOURCE_DATASET = "raw georeferenced size-frequencies"

SF_RAW_ENV = new.env()

load("./RAW.TROP.rda", envir = SF_RAW_ENV)
load("./RAW.TEMP.rda", envir = SF_RAW_ENV)
load("./RAW.BILL.rda", envir = SF_RAW_ENV)
load("./RAW.NERI.rda", envir = SF_RAW_ENV)
load("./RAW.SEER.rda", envir = SF_RAW_ENV)
load("./RAW.TNEI.rda", envir = SF_RAW_ENV)
load("./RAW.SHRK.rda", envir = SF_RAW_ENV)
load("./RAW.ETPS.rda", envir = SF_RAW_ENV)
load("./RAW.OTHR.rda", envir = SF_RAW_ENV)
load("./METADATA.rda", envir = SF_RAW_ENV)

data_SF_RAW =
  rbind(
    SF_RAW_ENV$RAW.TROP,
    SF_RAW_ENV$RAW.TEMP,
    SF_RAW_ENV$RAW.BILL,
    SF_RAW_ENV$RAW.NERI,
    SF_RAW_ENV$RAW.SEER,
    SF_RAW_ENV$RAW.TNEI,
    SF_RAW_ENV$RAW.SHRK,
    SF_RAW_ENV$RAW.ETPS,
    SF_RAW_ENV$RAW.OTHR
  )

#load("./SF_RAW.RData")

data_SF_RAW_table = data_SF_RAW      [, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(data_SF_RAW),       c(C_MONTH_START, C_MONTH_END, C_FISH_COUNT))]
data_SF_RAW       = data_SF_RAW_table[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(data_SF_RAW_table), c(C_CLASS_LOW, C_CLASS_HIGH, C_FISH_COUNT))]

DATA       = data_SF_RAW
DATA_TABLE = data_SF_RAW_table

source("../../_common/constants.R")
source("../../_common/data_management.R")
source("../../_common/UI_management.R")

source("../SF_configuration.R")
source("../SF_initialization.R")
source("../SF_extras.R")

source("../../_common/common_all.R")
source("../../_common/common_georeferenced.R")

source("./SF_RAW_configuration.R")

REF = initialize_SF_reference_data(DATA)

DATA = update_data(REF, DATA)
DATA_TABLE = update_data(REF, DATA_TABLE)

ui =
  fluidPage(
    tags$head(includeHTML(("../../_resources/google-analytics.html"))),
    theme = "button.css",
    includeCSS("../../_resources/css/common.css"),
    title = UI_window_title(SOURCE_DATASET, SF_RAW_ENV$METADATA$RAW.SF$LAST_UPDATE, REF),
    UI_main(
      SOURCE_DATASET, SF_RAW_ENV$METADATA$RAW.SF$LAST_UPDATE, REF, "success",
      fluidRow(
        UI_filters_SF(REF),
        column(
          width = 8,
          UI_filters_categories(SIZE_CATEGORIES, C_SPECIES_GROUP),
          fluidRow(
            tabsetPanel(
              UI_summary_default(),
              UI_GEO(),
              UI_samples_by_size(),
              UI_size_distribution()
            )
          )
        )
      ),
      "//data.iotc.org/browser/SF/RAW"
    )
  )

server = function(input, output, session) {
  cfg = current_configuration(REF)

  common_prepare_handlers       (DATA, DATA_TABLE, input, output, SF_RAW_PREFIX, SOURCE_DATASET, SF_RAW_ENV$METADATA$RAW.SF$LAST_UPDATE, cfg)
  georeferenced_prepare_handlers(DATA,             input, output, SF_RAW_PREFIX, SOURCE_DATASET, SF_RAW_ENV$METADATA$RAW.SF$LAST_UPDATE, cfg)
  SF_prepare_handlers           (DATA, DATA_TABLE, input, output, SF_RAW_PREFIX, SOURCE_DATASET, SF_RAW_ENV$METADATA$RAW.SF$LAST_UPDATE, cfg)

  observe({
    selected_measure_type = input$unit
    selected_size_bin = input$sizeBin

    measure_type = measure_type_by_code(selected_measure_type)

    values = c("1 cm" = 1,
               "2 cm" = 2,
               "5 cm" = 5,
               "10 cm" = 10)

    if(measure_type$MEASURE_UNIT == "KG")
      values = c("1 kg" = 1,
                 "2 kg" = 2,
                 "5 kg" = 5,
                 "10 kg" = 10)

    updateSelectInput(
      session,
      "sizeBin",
      "Bin size",
      values,
      selected = selected_size_bin
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
