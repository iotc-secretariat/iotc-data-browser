nc_sci_server <- function(id, activated){
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    initialized <- FALSE
    
    #reactives
    react_source_dataset <- reactiveVal(NULL)
    react_ref <- reactiveVal(NULL)
    
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
      INFO("Cleanup module 'nc-raw'")
      destroy_observers(handler_events)
      handler_events <<- list()
      
      rm(list = ls()[startsWith(ls(),"reactive_")])
      
      # Release references to potentially large objects
      react_ref(NULL)
      react_source_dataset(NULL)
      
      # Allow the module to be initialized again if it is activated later
      initialized <<- FALSE
      
      invisible(NULL)
    }


    initialize_module <- function() {
      if (initialized) {
        WARN("Module 'nc-sci' already initialized")
        return(invisible(NULL))
      }
      INFO("Initialize module 'nc-sci'")
      initialized <<- TRUE
      
      source("./modules/NC/NC_configuration.R")
      source("./modules/NC/NC_initialization.R")
      source("./modules/NC/NC_extras.R")
      
      react_source_dataset(DATASET_TITLES$NC_SCI)
      react_ref(initialize_NC_reference_data(iotc.data.reference.datasets.NC::SCI))
      
      prepare_NC_handlers(
        current_data = iotc.data.reference.datasets.NC::SCI,
        data_quality = iotc.data.reference.datasets.NC::DQA, 
        input = input, output = output, 
        prefix = "SCI", source_dataset = react_source_dataset(), 
        last_update = iotc.data.reference.datasets.NC::METADATA$SCI$LAST_UPDATE, 
        configuration = current_configuration(react_ref())
      )
      
      # Protect against helper functions returning NULL
      if (is.null(handler_events)) {
        handler_events <<- list()
      }
      
      invisible(NULL)
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
    
    output$nc_sci_ui <- renderUI({
      req(activated())
      req(!is.null(react_ref()))
      tagList(
        tags$head(includeHTML(("www/google-analytics.html"))),
        includeCSS("www/css/common.css"),
        includeCSS("www/css/button.css"),
        title = UI_window_title(react_source_dataset(), iotc.data.reference.datasets.NC::METADATA$SCI$LAST_UPDATE, react_ref()),
        UI_main(
          react_source_dataset(), iotc.data.reference.datasets.NC::METADATA$SCI$LAST_UPDATE, react_ref(), "primary",
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
          "#NC-SCI"
        )
      )
    })
    
    session$onSessionEnded(function() {
      message("Session ended for NC-SCI module")
      
      cleanup_module()
      
      # The activation observer is owned by this module session.
      if (inherits(activation_observer, "Observer")) {
        activation_observer$destroy()
      }
      
      gc()
    })
    
  })
  
}