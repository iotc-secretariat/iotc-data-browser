sf_std_server <- function(id, activated){
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    #reactives
    react_source_dataset <- reactiveVal(NULL)
    react_ref <- reactiveVal(NULL)
    react_data <- reactiveVal(NULL)
    react_data_table <- reactiveVal(NULL)

    observe({
      req(activated())
      source("./modules/SF/SF_configuration.R")
      source("./modules/SF/SF_initialization.R")
      source("./modules/SF/SF_extras.R")
      
      react_source_dataset(DATASET_TITLES$SF_STD)
      
      current_data = rbind(
        iotc.data.reference.datasets.SF.std::STD.TROP,
        iotc.data.reference.datasets.SF.std::STD.TEMP,
        iotc.data.reference.datasets.SF.std::STD.BILL,
        iotc.data.reference.datasets.SF.std::STD.NERI,
        iotc.data.reference.datasets.SF.std::STD.SEER,
        iotc.data.reference.datasets.SF.std::STD.SHRK
      )
      current_data_table = current_data      [, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(current_data),       c(C_MONTH_START, C_MONTH_END, C_FISH_COUNT))]
      current_data       = current_data_table[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(current_data_table), c(C_CLASS_LOW, C_CLASS_HIGH, C_FISH_COUNT))]
      
      DEFAULT_MEASURE_TYPE = "FL"
      
      source("./modules/SF/STD/SF_STD_configuration.R")
      source("./modules/SF/STD/SF_STD_extras.R")
      react_ref(initialize_SF_reference_data(current_data))
      react_data(update_data(react_ref(), current_data));rm(current_data)
      react_data_table(update_data(react_ref(), current_data_table));rm(current_data_table)
      
      common_prepare_handlers(
        current_data = react_data(), 
        current_data_table = react_data_table(), 
        input = input, output = output, 
        prefix = "SF_STD", 
        source_dataset = react_source_dataset(), 
        last_update = iotc.data.reference.datasets.SF.std::METADATA$STD.SF$LAST_UPDATE, 
        configuration = current_configuration(react_ref())
      )
      georeferenced_prepare_handlers(
        current_data = react_data(),
        input = input, output = output, 
        prefix = "SF_STD", react_source_dataset(), SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE,
        configuration = current_configuration(react_ref())
      )
      SF_prepare_handlers(
        data = react_data(),
        tabular_data = react_data_table(),
        input = input, output = output,
        source_dataset = react_source_dataset(), 
        last_update = SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE,
        configuration = current_configuration(react_ref())
      )
    })
    
    output$sf_std_ui <- renderUI({
      shiny::tagList(
        tags$head(includeHTML(("./www/google-analytics.html"))),
        includeCSS("./www/css/common.css"),
        includeCSS("./www/css/common.css"),
        title = UI_window_title(react_source_dataset(), iotc.data.reference.datasets.SF.std::METADATA$STD.SF$LAST_UPDATE, react_ref()),
        UI_main(
          react_source_dataset(), iotc.data.reference.datasets.SF.std::METADATA$STD.SF$LAST_UPDATE, react_ref(), "success",
          fluidRow(
            UI_filters_SF_STD(ns, react_ref()),
            column(
              width = 8,
              UI_filters_categories(ns, SIZE_STD_CATEGORIES, C_SPECIES_GROUP),
              fluidRow(
                tabsetPanel(
                  UI_summary_default(ns),
                  UI_GEO(ns),
                  UI_samples_by_size(ns),
                  UI_size_distribution(ns)
                )
              )
            )
          ),
          "#SF-STD"
        )
      )
    })

  })  

}