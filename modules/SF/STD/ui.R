sf_std_ui <- function(id, source_dataset, ref){
  
  shiny::tagList(
    tags$head(includeHTML(("./www/google-analytics.html"))),
    includeCSS("./www/css/common.css"),
    includeCSS("./www/css/common.css"),
    title = UI_window_title(source_dataset, iotc.data.reference.datasets.SF.std::METADATA$STD.SF$LAST_UPDATE, ref),
    UI_main(
      source_dataset, iotc.data.reference.datasets.SF.std::METADATA$STD.SF$LAST_UPDATE, ref, "success",
      fluidRow(
        UI_filters_SF_STD(ref),
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
      "#SF-STD"
    )
  )
  
}