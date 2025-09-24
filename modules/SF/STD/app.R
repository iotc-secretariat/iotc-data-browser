#######################
# SF STD data browser #
#######################

library(shiny)
library(periscope)
library(lubridate)
library(stringr)
library(iotc.base.common.plots)

#See https://statnmap.com/2016-04-19-a-nice-rshiny-onoff-switch-button/#
source("Rsource/SwitchButton.R")

SOURCE_DATASET = "standardized georeferenced size-frequencies"

SF_STD_ENV = new.env()

#load("./STD.TROP.BET.rda", envir = SF_STD_ENV)
#load("./STD.TROP.SKJ.rda", envir = SF_STD_ENV)
#load("./STD.TROP.YFT.rda", envir = SF_STD_ENV)
load("./STD.TROP.rda", envir = SF_STD_ENV)
load("./STD.TEMP.rda", envir = SF_STD_ENV)
load("./STD.BILL.rda", envir = SF_STD_ENV)
load("./STD.NERI.rda", envir = SF_STD_ENV)
load("./STD.SEER.rda", envir = SF_STD_ENV)
load("./STD.SHRK.rda", envir = SF_STD_ENV)
load("./METADATA.rda", envir = SF_STD_ENV)

data_SF_STD =
  rbind(
    SF_STD_ENV$STD.TROP,
   #SF_STD_ENV$STD.TROP.BET,
   #SF_STD_ENV$STD.TROP.SKJ,
   #SF_STD_ENV$STD.TROP.YFT,
    SF_STD_ENV$STD.TEMP,
    SF_STD_ENV$STD.BILL,
    SF_STD_ENV$STD.NERI,
    SF_STD_ENV$STD.SEER,
    SF_STD_ENV$STD.SHRK
  )

#load("./SF_SCI.RData")

data_SF_STD_table = data_SF_STD      [, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(data_SF_STD),       c(C_MONTH_START, C_MONTH_END, C_FISH_COUNT))]
data_SF_STD       = data_SF_STD_table[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(data_SF_STD_table), c(C_CLASS_LOW, C_CLASS_HIGH, C_FISH_COUNT))]

DATA       = data_SF_STD
DATA_TABLE = data_SF_STD_table

DEFAULT_MEASURE_TYPE = "FL"

source("../../scripts/constants.R")
source("../../scripts/data_management.R")
source("../../scripts/UI_management.R")

source("../SF_configuration.R")
source("../SF_initialization.R")
source("../SF_extras.R")

source("../../scripts/common_all.R")
source("../../scripts/common_georeferenced.R")

source("./SF_STD_configuration.R")
source("./SF_STD_extras.R")

REF = initialize_SF_reference_data(DATA)

ui =
  fluidPage(
    tags$head(includeHTML(("../../_resources/google-analytics.html"))),
    theme = "button.css",
    includeCSS("../../_resources/css/common.css"),
    title = UI_window_title(SOURCE_DATASET, SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE, REF),
    UI_main(
      SOURCE_DATASET, SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE, REF, "success",
      fluidRow(
        UI_filters_SF_STD(REF),
        column(
          width = 8,
          UI_filters_categories(SIZE_STD_CATEGORIES, C_SPECIES_GROUP),
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
      "//data.iotc.org/browser/SF/SCI"
    )
  )

server = function(input, output) {
  cfg = current_configuration(REF)

  common_prepare_handlers       (DATA, DATA_TABLE, input, output, SF_STD_PREFIX, SOURCE_DATASET, SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE, cfg)
  georeferenced_prepare_handlers(DATA,             input, output, SF_STD_PREFIX, SOURCE_DATASET, SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE, cfg)
  SF_prepare_handlers           (DATA, DATA_TABLE, input, output, SF_STD_PREFIX, SOURCE_DATASET, SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE, cfg)
}

# Run the application
shinyApp(ui = ui, server = server)
