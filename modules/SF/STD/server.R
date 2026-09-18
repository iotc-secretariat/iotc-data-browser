sf_std_server <- function(id, activated){
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    initialized <- FALSE
    
    #reactives
    react_source_dataset <- reactiveVal(NULL)
    react_ref <- reactiveVal(NULL)
    react_data <- reactiveVal(NULL)
    react_data_table <- reactiveVal(NULL)

    #events
    handler_events = list()
    
    #helpers
    #destroy_observers
    destroy_observers <- function(observers) {
      if (length(observers) == 0L) {
        return(invisible(NULL))
      }
      
      for (observer in observers) {
        if (inherits(observer, "Observer")) {
          observer$destroy()
        }
      }
      
      invisible(NULL)
    }
    #cleanup_module
    cleanup_module <- function() {
      INFO("Cleanup module 'sf-std'")
      destroy_observers(handler_events)
      handler_events <<- list()
      
      rm(list = ls()[startsWith(ls(),"reactive_")])
      
      # Release references to potentially large objects
      react_ref(NULL)
      react_source_dataset(NULL)
      react_data_table(NULL)
      react_data(NULL)
      
      # Allow the module to be initialized again if it is activated later
      initialized <<- FALSE
      
      invisible(NULL)
    }
    
    initialize_module <- function() {
      if (initialized) {
        WARN("Module 'sf-std' already initialized")
        return(invisible(NULL))
      }
      INFO("Initialize module 'sf-std'")
      initialized <<- TRUE
      
      source("./modules/SF/SF_configuration.R")
      source("./modules/SF/SF_initialization.R")
      source("./modules/SF/SF_extras.R")
      
      react_source_dataset(DATASET_TITLES$SF_STD)
      
      DEFAULT_MEASURE_TYPE = "FL"
      
      source("./modules/SF/STD/SF_STD_configuration.R")
      source("./modules/SF/STD/SF_STD_extras.R")
      react_data_table(iotc.data.reference.datasets.SF.std::STD.ALL_AGG)
      react_data(react_data_table()[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = setdiff(names(react_data_table()), c(C_CLASS_LOW, C_CLASS_HIGH, C_FISH_COUNT))]);
      react_ref(initialize_SF_reference_data(react_data()))
      
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
    }
    
    # -------------------------------------------------------------------------
    # Activation lifecycle
    # -------------------------------------------------------------------------
    
    # This observer remains for the lifetime of the parent Shiny session.
    # It controls the child module lifecycle.
    activation_observer <- observeEvent(
      activated(),
      {
        if (isTRUE(activated())) {
          initialize_module()
        } else {
          cleanup_module()
          gc()
        }
      },
      ignoreInit = FALSE
    )
    
    # -------------------------------------------------------------------------
    # Module UI
    # -------------------------------------------------------------------------
    
    output$sf_std_ui <- renderUI({
      req(activated())
      req(!is.null(react_ref()))
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
    
    session$onSessionEnded(function() {
      message("Session ended for SF-STD module")
      
      cleanup_module()
      
      # The activation observer is owned by this module session.
      if (inherits(activation_observer, "Observer")) {
        activation_observer$destroy()
      }
      
      gc()
    })

  })  

}