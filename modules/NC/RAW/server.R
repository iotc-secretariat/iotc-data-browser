nc_raw_server <- function(id, activated){
  
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    #reactives
    react_source_dataset <- reactiveVal(NULL)
    react_ref <- reactiveVal(NULL)
    
    observe({
      req(activated())
      
      source("./modules/NC/NC_configuration.R")
      source("./modules/NC/NC_initialization.R")
      source("./modules/NC/NC_extras.R")
      
      react_source_dataset(DATASET_TITLES$NC_RAW)
      react_ref( initialize_NC_reference_data(iotc.data.reference.datasets.NC::RAW) )
      
      prepare_NC_handlers(
        current_data = iotc.data.reference.datasets.NC::RAW, 
        data_quality = iotc.data.reference.datasets.NC::DQA, 
        input = input, output = output, prefix = "RAW", 
        source_dataset = react_source_dataset(), 
        last_update = iotc.data.reference.datasets.NC::METADATA$RAW$LAST_UPDATE, 
        current_configuration(react_ref())
      )
    })
    
    output$nc_raw_ui <- renderUI({
      tagList(
        tags$head(includeHTML(("www/google-analytics.html"))),
        includeCSS("www/css/common.css"),
        includeCSS("www/css/button.css"),
        title = UI_window_title(react_source_dataset(), iotc.data.reference.datasets.NC::METADATA$RAW$LAST_UPDATE, react_ref()),
        UI_main(
          react_source_dataset(), iotc.data.reference.datasets.NC::METADATA$RAW$LAST_UPDATE, react_ref(), "primary",
          fluidRow(
            UI_filters_NC(ns, react_ref()),
            column(
              width = 8,
              UI_filters_categories(ns, NOMINAL_CATCH_CATEGORIES),
              fluidRow(
                tabsetPanel(
                  UI_summary_default(ns),
                  UI_NC_quality(ns)
                )
              )
            )
          ),
          "#NC-RAW"
        )
      )
      
    })
  
  })

}