sf_raw_ui = function(id, source_dataset, ref){

  tagList(
    tags$head(includeHTML(("www/google-analytics.html"))),
    includeCSS("www/css/common.css"),
    includeCSS("www/css/button.css"),
    title = UI_window_title(source_dataset, iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE, ref),
    UI_main(
      source_dataset, iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE, ref, "success",
      fluidRow(
        UI_filters_SF(ref),
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
      "#SF-RAW"
    )
  )
  
}