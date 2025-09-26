ce_ca_ui <- function(id, source_dataset, ref){
  
  tagList(
    tags$head(includeHTML(("www/google-analytics.html"))),
    includeCSS("www/css/common.css"),
    includeCSS("www/css/button.css"),
    title = UI_window_title(source_dataset, iotc.data.reference.datasets.CE::METADATA$RAW.CA$LAST_UPDATE, ref),
    UI_main(
      source_dataset, iotc.data.reference.datasets.CE::METADATA$RAW.CA$LAST_UPDATE, ref, "info",
      fluidRow(
        UI_filters_CA(ref),
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
      "#CE-CA"
    )
  )
  
}