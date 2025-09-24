##########################
# CA RAISED data browser #
##########################

library(shiny)
library(periscope)
library(lubridate)
library(stringr)
library(iotc.base.common.plots)

#See https://statnmap.com/2016-04-19-a-nice-rshiny-onoff-switch-button/#
source("Rsource/SwitchButton.R")

CE_RSD_ENV = new.env()

load("./RSD.CA.rda",   envir = CE_RSD_ENV)
load("./METADATA.rda", envir = CE_RSD_ENV)

#load("./CA_RAISED.RData")

DATA_ORIG = CE_RSD_ENV$RSD.CA

source("../../scripts/constants.R")
source("../../scripts/data_management.R")
source("../../scripts/UI_management.R")

source("../../scripts/common_all.R")
source("../../scripts/common_georeferenced.R")

source("CAR_initialization.R")

REF = initialize_CA_reference_data(DATA_ORIG)
DATA = update_data(REF, DATA_ORIG)

source("CAR_configuration.R")
source("CAR_extras.R")

ui =
  fluidPage(
    tags$head(includeHTML(("../../_resources/google-analytics.html"))),
    theme = "button.css",
    includeCSS("../../_resources/css/common.css"),
    title = UI_window_title(SOURCE_DATASET, CE_RSD_ENV$METADATA$RSD.CA$LAST_UPDATE, REF),
    UI_main(
      SOURCE_DATASET, CE_RSD_ENV$METADATA$RSD.CA$LAST_UPDATE, REF, "warning",
      fluidRow(
        UI_filters_CA(REF),
        column(
          width = 8,
          UI_filters_categories(CATCH_CATEGORIES),
          fluidRow(
            tabsetPanel(
              UI_summary_default(),
              UI_GEO(),
              tabPanel(
                icon = icon("scale", lib = "glyphicon"),
                "Average weights",
                tabsetPanel(
                  tabPanel(
                    icon = icon("fa-far fa-chart-line"),
                    "Time series", do_plot_output("avgWeightLineChart"),
                                   do_create_download_button("downloadAvgWeightLineChart")),
                  tabPanel(
                    icon = icon("th", lib = "glyphicon"),
                    "Heatmap",
                    fluidRow(
                      column(
                        width = 2,
                        selectizeInput("avgWeightQuadrant", "Quadrant", QUADRANTS, multiple = FALSE)
                      ),
                      column(
                      width = 2,
                        switchButton("avgWeightShowIO", "Show IOTC sub-areas", value = FALSE, col = "GB", type = "TF")
                      ),
                      column(
                        width = 2,
                        switchButton("avgWeightShowHS", "Show high seas", value = TRUE , col = "GB", type = "TF")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 9,
                        plotOutput(height = GEOREF_CHART_HEIGHT, "avgWeightGeoHeatmap"),
                        do_create_download_button("downloadAvgWeightGeoHeatmap")
                      ),
                      column(
                        width = 3
                      )
                    )
                  ),
                  tabPanel(
                    icon = icon("list-alt", lib = "glyphicon"),
                    "Dataset (yearly)",
                    do_create_download_button("downloadAvgWeightDataTable", DOWNLOAD_DATA_BUTTON_TEXT),
                    dataTableOutput("avgWeightTable")),
                  tabPanel(
                    icon = icon("list-alt", lib = "glyphicon"),
                    "Dataset (georeferenced)",
                    do_create_download_button("downloadAvgWeightGeoDataTable", DOWNLOAD_DATA_BUTTON_TEXT),
                    dataTableOutput("avgWeightGeoTable"))
                )
              )
            )
          )
        )
      ),
      "//data.iotc.org/browser/CA/raised"
    )
  )

server = function(input, output) {
  cfg = current_configuration(REF)

  common_prepare_handlers(DATA, DATA, input, output, CA_RAISED_PREFIX, SOURCE_DATASET, CE_RSD_ENV$METADATA$RSD.CA$LAST_UPDATE, cfg)
  georeferenced_prepare_handlers(DATA, input, output, CA_RAISED_PREFIX, SOURCE_DATASET, CE_RSD_ENV$METADATA$RSD.CA$LAST_UPDATE, cfg)

  avgWeightTable    = reactive_avgWeightTable(DATA_ORIG, input)
  avgWeightGeoTable = reactive_avgWeightGeoTable(DATA_ORIG, input)

  plotAvgWeightLineChart  = reactive_plotAvgWeightLineChart(DATA_ORIG, input, cfg)
  plotAvgWeightGeoHeatmap = reactive_plotAvgWeightGeoHeatmap(DATA_ORIG, input, cfg)

  # Output tables

  output$avgWeightTable =    { renderDataTable(options = list(pageLength = 10), avgWeightTable()) }
  output$avgWeightGeoTable = { renderDataTable(options = list(pageLength = 10), avgWeightGeoTable()) }

  # Output plots

  output$avgWeightLineChart  = { renderPlot(plotAvgWeightLineChart()) }
  output$avgWeightGeoHeatmap = { renderPlot(plotAvgWeightGeoHeatmap()) }

  # Download handlers for data tables

  output$downloadAvgWeightDataTable    = prepare_csv_file(input, paste0(CA_RAISED_PREFIX, "_AVG_WEIGHT"), NA, avgWeightTable)
  output$downloadAvgWeightGeoDataTable = prepare_csv_file(input, paste0(CA_RAISED_PREFIX, "_AVG_WEIGHT_GEO"), NA, avgWeightGeoTable)

  output$downloadAvgWeightLineChart  = prepare_png_file(input, SOURCE_DATASET, CE_RSD_ENV$METADATA$RSD.CA$LAST_UPDATE,
                                                        paste0(CA_RAISED_PREFIX, "_LINE_AVG_WEIGHT"), "Average yearly fish weight",
                                                        NA,
                                                        cfg$CATEGORIES,
                                                        plotAvgWeightLineChart)

  output$downloadAvgWeightGeoHeatmap = prepare_png_file(input, SOURCE_DATASET, CE_RSD_ENV$METADATA$RSD.CA$LAST_UPDATE,
                                                        paste0(CA_RAISED_PREFIX, "_GEO_HEATMAP_AVG_WEIGHT"), "Average fish weight",
                                                        NA,
                                                        NA,
                                                        plotAvgWeightGeoHeatmap)
}

# Run the application
shinyApp(ui = ui, server = server)
