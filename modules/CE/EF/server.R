ce_ef_server <- function(id, activated){
  
  moduleServer(id, function(input, output, session) {
  
    ns <- session$ns
    
    #reactives
    react_source_dataset <- reactiveVal(NULL)
    react_ref <- reactiveVal(NULL)
    react_data <- reactiveVal(NULL)
    
    observe({
      req(activated())
      source("./modules/CE/EF/EF_configuration.R")
      source("./modules/CE/EF/EF_initialization.R")
      
      react_source_dataset( DATASET_TITLES$CE_EF )
      react_ref( initialize_EF_reference_data(iotc.data.reference.datasets.CE::RAW.EF) )
      react_data( update_data(react_ref(), iotc.data.reference.datasets.CE::RAW.EF) )
      
      common_prepare_handlers(
        current_data = react_data(), 
        current_data_table = react_data(), 
        input = input, output = output, prefix = "EF_RAW", 
        source_dataset = react_source_dataset(), 
        last_update =  iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE, 
        configuration = current_configuration(react_ref()))
      
      georeferenced_prepare_handlers(
        current_data = react_data(), 
        input = input, output = output, prefix = "EF_RAW", 
        source_dataset = react_source_dataset(), 
        last_update = iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE, 
        configuration = current_configuration(react_ref())
      )
    })
    
    output$ce_ef_ui <- renderUI({
      tagList(
        tags$head(includeHTML(("www/google-analytics.html"))),
        includeCSS("www/css/common.css"),
        includeCSS("www/css/button.css"),
        title = UI_window_title(react_source_dataset(), iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE, react_ref()),
        UI_main(
          react_source_dataset(), iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE, react_ref(), "info",
          fluidRow(
            UI_filters_EF(ns, react_ref()),
            column(
              width = 8,
              UI_filters_categories(ns, EFFORT_CATEGORIES),
              fluidRow(
                tabsetPanel(
                  UI_summary_default(ns),
                  UI_GEO(ns)
                )
              )
            )
          ),
          "#CE-EF"
        )
      )
    })
    
  })
}