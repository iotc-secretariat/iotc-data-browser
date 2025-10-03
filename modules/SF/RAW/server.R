sf_raw_server <- function(id, activated){
  
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
      
      react_source_dataset(DATASET_TITLES$SF_RAW)
      current_data =
        rbind(
          iotc.data.reference.datasets.SF.raw::RAW.TROP,
          iotc.data.reference.datasets.SF.raw::RAW.TEMP,
          iotc.data.reference.datasets.SF.raw::RAW.BILL,
          iotc.data.reference.datasets.SF.raw::RAW.NERI,
          iotc.data.reference.datasets.SF.raw::RAW.SEER,
          iotc.data.reference.datasets.SF.raw::RAW.TNEI,
          iotc.data.reference.datasets.SF.raw::RAW.SHRK,
          iotc.data.reference.datasets.SF.raw::RAW.ETPS,
          iotc.data.reference.datasets.SF.raw::RAW.OTHR
        )
      current_data_table = current_data      [, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(current_data),       c(C_MONTH_START, C_MONTH_END, C_FISH_COUNT))]
      current_data       = current_data_table[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(current_data_table), c(C_CLASS_LOW, C_CLASS_HIGH, C_FISH_COUNT))]
      
      source("./modules/SF/RAW/SF_RAW_configuration.R")
      react_ref(initialize_SF_reference_data(current_data))
      react_data(current_data);rm(current_data)
      react_data_table(current_data_table);rm(current_data_table)
      
      common_prepare_handlers(
        current_data = react_data(), 
        current_data_table = react_data_table(), 
        input = input, output = output,
        prefix = "SF_RAW", 
        source_dataset = react_source_dataset(),
        last_update = iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE, 
        configuration = current_configuration(react_ref())
      )
      georeferenced_prepare_handlers(
        current_data = react_data(),
        input = input, 
        output = output, 
        prefix = "SF_RAW",
        source_dataset = react_source_dataset(),
        last_update = iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE,
        configuration = current_configuration(react_ref())
      )
      SF_prepare_handlers(
        data = react_data(),
        tabular_data = react_data_table(),
        input = input, output = output,
        prefix = "SF_RAW",
        source_dataset = react_source_dataset(),
        last_update = iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE,
        configuration = current_configuration(react_ref())
      )
    })
    
    observe({
      req(activated())
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
    
    output$sf_raw_ui <- renderUI({
      tagList(
        tags$head(includeHTML(("www/google-analytics.html"))),
        includeCSS("www/css/common.css"),
        includeCSS("www/css/button.css"),
        title = UI_window_title(react_source_dataset(), iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE, react_ref()),
        UI_main(
          react_source_dataset(), iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE, react_ref(), "success",
          fluidRow(
            UI_filters_SF(ns, react_ref()),
            column(
              width = 8,
              UI_filters_categories(ns, SIZE_CATEGORIES, C_SPECIES_GROUP),
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
          "#SF-RAW"
        )
      )
    })
    
  })
}