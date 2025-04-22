library(shiny)

NC_ENV     = new.env()
CE_RAW_ENV = new.env()
CE_RSD_ENV = new.env()
SF_RAW_ENV = new.env()
SF_STD_ENV = new.env()

load("../NC/RAW/METADATA.rda",    envir = NC_ENV)
load("../CE/EF/METADATA.rda",     envir = CE_RAW_ENV)
load("../CA/raised/METADATA.rda", envir = CE_RSD_ENV)
load("../SF/RAW/METADATA.rda",    envir = SF_RAW_ENV)
load("../SF/STD/METADATA.rda",    envir = SF_STD_ENV)

records =
  list(
    NC = list(
      RAW = list(
        DATA        = NC_ENV$METADATA$RAW$DATA,
        QUALITY     = NC_ENV$METADATA$RAW$QUALITY,
        LAST_UPDATE = NC_ENV$METADATA$RAW$LAST_UPDATE
      ),
      SCI = list(
        DATA        = NC_ENV$METADATA$SCI$DATA,
        QUALITY     = NC_ENV$METADATA$SCI$QUALITY,
        LAST_UPDATE = NC_ENV$METADATA$SCI$LAST_UPDATE
      )
    ),
    CE = list(
      CA = list(
        DATA        = CE_RAW_ENV$METADATA$RAW.CA$DATA,
        LAST_UPDATE = CE_RAW_ENV$METADATA$RAW.CA$LAST_UPDATE
      ),
      EF = list(
        DATA        = CE_RAW_ENV$METADATA$RAW.EF$DATA,
        LAST_UPDATE = CE_RAW_ENV$METADATA$RAW.EF$LAST_UPDATE
      )
    ),
    CA = list(
      raised = list(
        DATA        = CE_RSD_ENV$METADATA$RSD.CA$DATA,
        LAST_UPDATE = CE_RSD_ENV$METADATA$RSD.CA$LAST_UPDATE
      )
    ),
    SF = list(
      RAW = list(
        DATA        = SF_RAW_ENV$METADATA$RAW.SF$DATA,
        LAST_UPDATE = SF_RAW_ENV$METADATA$RAW.SF$LAST_UPDATE
      ),
      STD = list(
        DATA        = SF_STD_ENV$METADATA$STD.SF$DATA,
        LAST_UPDATE = SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE
      )
    )
  )

#load("./HOME.RData")

glyph_ = function(icon) {
  return(icon(lib="glyphicon", name=icon))
}

faw_ = function(icon) {
  return(icon(name=icon))
}

tag_ = function(category, text, icon, hover, improved = FALSE) {
  additional = ""

  if(improved) additional = "improved"

  return(
    tags$span(
      title=hover,
      class=paste(
        "label",
        paste0("label-", category),
        "tag",
        additional
      ),
      icon, #tags$span(class=paste0("glyphicon glyphicon-", icon), " "),
      text
    )
  )
}

RECORDS = function(num_records) {
  formatted = format(num_records, big.mark = ",")
  return(
    tags$span(
      faw_("list"),
      title=paste0(formatted, " records in this dataset"),
      class="tag improved label label-danger",
      formatted
    )
  )
}

LAST_UPDATED = function(last_update_date) {
  return(
    tags$span(
      faw_("clock"),
      title=paste0("Last updated on ", last_update_date),
      class="tag improved label label-primary",
      last_update_date
    )
  )
}

MULTIPLE = function(first, second) {
  return(
    tags$span(class="multiple",
              first,
              second)
  )
}

data_ = function(text, icon, hover, improved = FALSE) {
  return(tag_("default", text, icon, hover, improved))
}

  RAW = function() {
    return(data_("Raw", faw_("th-list"), "Data as reported"))
  }

  DISAGGREGATED = function() {
    return(data_("Disaggregated", faw_("th"), "Data fully disaggregated by gear and species"))
  }

  CONVERTED = function(hover) {
    return(data_("Converted", faw_("exchange-alt"), hover, improved = TRUE))
  }

raise_ = function(text, icon, hover, improved = FALSE) {
  return(tag_("danger", text, icon, hover, improved))
}

  RAISED = function(hover = "Raised at the source") {
    return(raise_("Raised", faw_("align-justify"), hover, improved = TRUE))
  }

  UNRAISED = function(hover = "Not raised to totals for the stratum") {
    return(raise_("Unraised", faw_("align-left"), hover, improved = FALSE))
  }

time_ = function(text, icon, hover, improved = FALSE) {
  return(tag_("primary", text, icon, hover, improved))
}

  YEARLY = function() {
    return(time_("Yearly", faw_("calendar-alt"), "Aggregated by year"))
  }

  QUARTERLY = function() {
    return(time_("Quarterly", faw_("calendar-day"), "Aggregated by year and quarter", improved = TRUE))
  }

  MONTHLY = function() {
    return(time_("Monthly", faw_("clock"), "Aggregated by year and month", improved = TRUE))
  }

species_ = function(text, icon, hover, improved = FALSE) {
  return(tag_("info", text, icon, hover, improved))
}

  ALL_SPECIES = function() {
    return(species_("All species", faw_("fas fa-dragon"), "IOTC and bycatch species"))
  }

  MAIN_IOTC_SPECIES = function() {
    return(
      MULTIPLE(
        species_(      "Albacore",  faw_("fas fa-fish"), "ALB - Albacore",       improved = TRUE),
        MULTIPLE(
          species_(    "Bigeye",    faw_("fas fa-fish"), "BET - Bigeye tuna",    improved = TRUE),
          MULTIPLE(
            species_(  "Skipjack",  faw_("fas fa-fish"), "SKJ - Skipjack tuna",  improved = TRUE),
            MULTIPLE(
              species_("Yellowfin", faw_("fas fa-fish"), "YFT - Yellowfin tuna", improved = TRUE),
              species_("Swordfish", faw_("fas fa-fish"), "BET - Bigeye tuna",    improved = TRUE)
            )
          )
        )
      )
    )
  }

  IOTC_SPECIES = function() {
    return(species_("IOTC species", faw_("fas fa-fish"), "16 IOTC species only", improved = TRUE))
  }

  IOTC_AND_SHARK_SPECIES = function() {
    return(species_("IOTC species", faw_("fas fa-fish"), "16 IOTC and relevant shark species", improved = TRUE))
  }

area_ = function(text, icon, hover, improved = FALSE) {
  return(tag_("success", text, icon, hover, improved))
}

  IO_AREAS = function() {
    return(area_("IO main areas", faw_("globe-asia"), "Western / Eastern IO"))
  }

  REGULAR_GRIDS = function() {
    return(area_("Regular grids", faw_("th-large"), "Regular, gear-dependent CWP grids", improved = TRUE))
  }

  REGULAR_5x5_GRIDS = function() {
    return(area_("Regular grids", faw_("th-large"), "5x5 regular CWP grids", improved = TRUE))
  }

measure_ = function(text, icon, hover, improved = FALSE) {
  return(tag_("warning", text, icon, hover, improved))
}

  CATCH_LIVE_WEIGHT = function(improved = FALSE) {
    return(measure_("Live weight", faw_("balance-scale"), "Catches are in live-weight equivalent (t)", improved))
  }

  CATCH_NUMBERS = function(improved = FALSE) {
    return(measure_("Numbers", faw_("list-alt"), "Catches are in number of individuals", improved))
  }

  EFFORT = function() {
    return(measure_("Effort", faw_("cog"), "Gear-dependent effort unit"))
  }

  LENGTHS = function() {
    return(measure_("Lengths", faw_("arrows-alt-v"), "Species-specific length measurements"))
  }

  FL_LENGTHS = function() {
    return(measure_("Fork lengths", faw_("arrows-alt-v"), "Fork length only", improved = TRUE))
  }

  WEIGHTS = function() {
    return(measure_("Weights", faw_("balance-scale"), "Weight measurements"))
  }

  ESTIMATED_WEIGHTS = function() {
    return(measure_("Est. weights", faw_("balance-scale-left"), "Round weight, converted using species-specific equations", improved = TRUE))
  }

ui =
  fluidPage(
    tags$head(includeHTML(("../_resources/google-analytics.html"))),
    includeCSS("../_resources/css/home.css"),
    titlePanel(
      windowTitle = "IOTC data browser - explore the content of IOTC datasets",
      title = ""
    ),
    fluidRow(
      column = 12,
      tags$div(class="container",
        #tags$h1(
        #  tags$img(src="iotc-logo.png"),
        #  tags$span(
        #    "Data browser - Explore the current content of IOTC datasets"
        #  )
        #),
        tags$div(class="panel panel-primary",
          tags$div(class="panel-heading",
            tags$h2(
              faw_("balance-scale"),
              faw_("calendar-alt"),
              faw_("globe-asia"),
              "Nominal catches by year, IOTC area, fleet, fishery and species"
            )
          ),
          tags$div(class="panel-body",
            tags$h3(
              tags$a(class="btn btn-primary text-monospace button-link", href="/browser/NC/RAW", title="Click to explore this dataset",
                     faw_("search"),
                     "NC-RAW"
              ),
              tags$span("Nominal catches for all species, including bycatch ones")
            ),
            tags$div(
              RECORDS(records$NC$RAW$DATA),
              LAST_UPDATED(records$NC$RAW$LAST_UPDATE),
              RAW(),
              RAISED(),
              YEARLY(),
              CATCH_LIVE_WEIGHT(),
              IO_AREAS(),
              ALL_SPECIES()
            ),
            tags$h3(
              tags$a(class="btn btn-primary text-monospace button-link", href="/browser/NC/SCI", title="Click to explore this dataset",
                     faw_("search"),
                     "NC-SCI"
              ),
              tags$span("Fully disaggregated, best scientific estimates for IOTC species only")
            ),
            tags$div(
              RECORDS(records$NC$SCI$DATA),
              LAST_UPDATED(records$NC$SCI$LAST_UPDATE),
              DISAGGREGATED(),
              RAISED(),
              YEARLY(),
              CATCH_LIVE_WEIGHT(),
              IO_AREAS(),
              IOTC_SPECIES()
            )
          )
        ),
        tags$div(class="panel panel-info",
          tags$div(class="panel-heading",
            tags$h2(
              faw_("balance-scale"),
              faw_("calendar-day"),
              faw_("map-marker-alt"),
              "Catch-and-effort by year, quarter, grid, fleet, fishery and species")
          ),
          tags$div(class="panel-body",
            tags$h3(
              tags$a(class="btn btn-info text-monospace button-link", href="/browser/CE/EF", title="Click to explore this dataset",
                     faw_("search"),
                     "CE-EF"
              ),
              tags$span("Reported georeferenced quarterly efforts")
            ),
            tags$div(
              RECORDS(records$CE$EF$DATA),
              LAST_UPDATED(records$CE$EF$LAST_UPDATE),
              RAW(),
              UNRAISED("Not raised to total nominal efforts for the stratum"),
              QUARTERLY(),
              EFFORT(),
              REGULAR_GRIDS()
            ),
            tags$h3(
              tags$a(class="btn btn-info text-monospace button-link", href="/browser/CE/CA", title="Click to explore this dataset",
                     faw_("search"),
                     "CE-CA"
              ),
              tags$span("Reported georeferenced quarterly catches (in weight and / or numbers)")
            ),
            tags$div(
              RECORDS(records$CE$CA$DATA),
              LAST_UPDATED(records$CE$CA$LAST_UPDATE),
              RAW(),
              UNRAISED("Not raised to total nominal catches for the stratum"),
              QUARTERLY(),
              MULTIPLE(
                CATCH_LIVE_WEIGHT(),
                CATCH_NUMBERS()
              ),
              REGULAR_GRIDS(),
              ALL_SPECIES()
            )
          )
        ),
        tags$div(class="panel panel-warning",
          tags$div(class="panel-heading",
            tags$h2(
              faw_("balance-scale-left"),
              faw_("calendar-day"),
              faw_("map-marker-alt"),
              "Raised catches by year, quarter, 5x5 grid, fleet, fishery and species"
            ),
          ),
          tags$div(class="panel-body",
            tags$h3(
              tags$code(
                tags$span(class="glyphicon glyphicon-ban-circle", " "),
                tags$span("RESTRICTED ACCESS")
              )
            ),
            tags$h3(
              tags$a(class="btn btn-warning text-monospace button-link", href="/browser/CA/raised", title="Click to explore this dataset",
                     faw_("search"),
                     "CA-RAISED"
              ),
              tags$span("Estimated, raised georeferenced quarterly catches (weight and numbers) for main species")
            ),
            tags$div(
              RECORDS(records$CA$raised$DATA),
              LAST_UPDATED(records$CA$raised$LAST_UPDATE),
              DISAGGREGATED(),
              RAISED("Raised to represent total catch for the stratum"),
              QUARTERLY(),
              MULTIPLE(
                CATCH_LIVE_WEIGHT(),
                CATCH_NUMBERS()
              ),
              REGULAR_5x5_GRIDS(),
              MAIN_IOTC_SPECIES()
            )
          )
        ),
        tags$div(class="panel panel-success",
          tags$div(class="panel-heading",
            tags$h2(
              faw_("sliders-h"),
              faw_("calendar-day"),
              faw_("map-marker-alt"),
              "Size-frequency by year, quarter, grid, fleet, fishery, species and measure"
            )
          ),
          tags$div(class="panel-body",
            tags$h3(
             tags$a(class="btn btn-success text-monospace button-link", href="/browser/SF/RAW", title="Click to explore this dataset",
                    faw_("search"),
                    "SF-RAW"
             ),
             tags$span("Reported georeferenced size-frequency")
            ),
            tags$div(
              RECORDS(records$SF$RAW$DATA),
              LAST_UPDATED(records$SF$RAW$LAST_UPDATE),
              RAW(),
              UNRAISED(),
              QUARTERLY(),
              MULTIPLE(
                LENGTHS(),
                WEIGHTS()
              ),
              REGULAR_GRIDS(),
              ALL_SPECIES()
            ),
            tags$h3(
             tags$a(class="btn btn-success text-monospace button-link", href="/browser/SF/STD", title="Click to explore this dataset",
                    faw_("search"),
                    "SF-STD"
             ),
             tags$span("Standardized georeferenced size-frequency data for IOTC and relevant shark species")
            ),
            tags$div(
              RECORDS(records$SF$STD$DATA),
              LAST_UPDATED(records$SF$STD$LAST_UPDATE),
              CONVERTED("Converted using species-specific equations"),
              UNRAISED(),
              QUARTERLY(),
              MULTIPLE(
                FL_LENGTHS(),
                ESTIMATED_WEIGHTS()
              ),
              REGULAR_5x5_GRIDS(),
              IOTC_AND_SHARK_SPECIES()
            )
          )
        )
      )
    )
  )

# Define server logic required to draw a histogram
server = function(input, output) { }

# Run the application
shinyApp(ui = ui, server = server)
