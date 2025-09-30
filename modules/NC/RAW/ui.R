nc_raw_ui <- function(id, source_dataset, ref){
  tagList(
    tags$head(includeHTML(("www/google-analytics.html"))),
    includeCSS("www/css/common.css"),
    includeCSS("www/css/button.css"),
    title = UI_window_title(source_dataset, iotc.data.reference.datasets.NC::METADATA$RAW$LAST_UPDATE, ref),
    UI_main(
      source_dataset, iotc.data.reference.datasets.NC::METADATA$RAW$LAST_UPDATE, ref, "primary",
      fluidRow(
        UI_filters_NC(ref),
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
      "#NC-RAW"
    )
  )
}