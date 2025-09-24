nc_sci_ui = function(id, source_dataset, ref){
  tagList(
    tags$head(includeHTML(("./assets/_resources/google-analytics.html"))),
    theme = "./assets/_resources/css/button.css",
    includeCSS("./assets/_resources/css/common.css"),
    title = UI_window_title(source_dataset, iotc.data.reference.datasets.NC::METADATA$SCI$LAST_UPDATE, REF),
    UI_main(
      source_dataset, iotc.data.reference.datasets.NC::METADATA$SCI$LAST_UPDATE, ref, "primary",
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
      "//data.iotc.org/browser/NC/SCI"
    )
  )
}