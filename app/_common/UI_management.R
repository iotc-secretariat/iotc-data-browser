SD_NUM_COLS = 4
SD_ROW_YEAR_HEIGHT = 20

IOTC_LOGO = png::readPNG("../../_resources/assets/iotc-logo.png")
IOTC_LOGO_RASTER = grid::rasterGrob(IOTC_LOGO, interpolate = TRUE,
                                    #width = unit(75, "points"),
                                    width = unit(0.07, "npc"),
                                    #x = unit(.09, "npc"), y = unit(.98, "npc"),
                                    x = unit(0.08, "npc"), y = unit(.98, "npc"),
                                    hjust = 1, vjust = 1)

capitalize = function(string) {
  return(
    paste(toupper(substring(string, 1,1)), substring(string, 2), sep="", collapse=" ")
  )
}

calculate_width_for_size_distribution = function() {
  FILTERED =
    SF_filter_data(
      DATA_TABLE,
      input$period,
      DEFAULT_MEASURE_TYPE,
      NA,
      input$fisheryTypes, input$fisheryGroups, input$fisheries, input$gears,
      input$fleets,
      input$speciesWps, input$speciesGroups, input$speciesCategories, input$species,
      input$IUCNStatus
    )

  column = which(colnames(FILTERED) == input$category)

  num_categories = nrow(unique(FILTERED[, ..column]))

  if(input$numColumns == 1 | num_categories == 1) return(500)

  return(1200)
}

calculate_height_for_size_distribution = function() {
  FILTERED =
    SF_filter_data(
      DATA_TABLE,
      input$period,
      DEFAULT_MEASURE_TYPE,
      NA,
      input$fisheryTypes, input$fisheryGroups, input$fisheries, input$gears,
      input$fleets,
      input$speciesWps, input$speciesGroups, input$speciesCategories, input$species,
      input$IUCNStatus
    )

  column = which(colnames(FILTERED) == input$category)

  num_categories = nrow(unique(FILTERED[, ..column]))
  num_years = max(FILTERED$YEAR) - min(FILTERED$YEAR) + 1

  height = ceiling(num_categories / input$numColumns) * SD_ROW_YEAR_HEIGHT * num_years

  return(height)
}

do_create_download_button = function(button_id, button_text = DOWNLOAD_PLOT_BUTTON_TEXT) {
  return(downloadButton(button_id, button_text, class="btn btn-primary shiny-download-link"))
}

do_plot_output = function(input_id) {
  return (plotOutput(input_id, height = SUMMARY_CHART_HEIGHT))
}

prepare_csv_file = function(input, dataset = "NC_RAW", units, data_producer) {
  return (downloadHandler(
    filename = function() {
      paste0("IOTC_",
             filename_for(input, dataset, units, input$unit, NA, NA), #The CSV is not categorised
             ".csv.gz")
    },
    contentType = "application/gzip",
    content = function(filename) {
      tmpdir = tempdir()

      setwd(tmpdir)

      write.csv(data_producer(), gzfile(description = filename), row.names = FALSE)
    }
  ))
}

prepare_csv_file_with_fixed_unit = function(input, dataset = "NC_RAW", units, unit, data_producer) {
  return (downloadHandler(
    filename = function() {
      paste0("IOTC_",
             filename_for(input, dataset, units, unit, NA, NA), #The CSV is not categorised
             ".csv.gz")
    },
    contentType = "application/gzip",
    content = function(filename) {
      tmpdir = tempdir()

      setwd(tmpdir)

      write.csv(data_producer(), gzfile(description = filename), row.names = FALSE)
    }
  ))
}

watermark = function(add, plot) {
  if(add) return(plot + annotation_custom(IOTC_LOGO_RASTER))

  return(plot)
}

prepare_png_file = function(input,
                            source_dataset = "SOURCE_DATASET_NAME",
                            last_update = NA,
                            dataset = "NC_RAW_BAR",
                            plot_name,
                            units = NA,
                            categories,
                            plot_producer,
                            width = PLOT_DEFAULT_WIDTH,
                            height = PLOT_DEFAULT_HEIGHT,
                            add_watermark = TRUE) {
  return (downloadHandler(
    filename = function() { paste0("IOTC_", filename_for(input, dataset, units, input$unit, categories, input$category), ".png") },
    contentType = "image/png",
    content = function(file) {
      plot_title = title_for(input, plot_name, units, input$unit, categories, input$category)
      ggsave(file, plot = watermark(add_watermark, titolize(source_dataset, last_update, plot_producer(), plot_title)), device = "png", width = width, height = height)
    }
  ))
}

prepare_png_file_with_fixed_unit = function(input,
                                            source_dataset = "SOURCE_DATASET_NAME",
                                            last_update = NA,
                                            dataset = "NC_RAW_BAR",
                                            plot_name,
                                            units = NA,
                                            unit,
                                            categories,
                                            plot_producer,
                                            width = PLOT_DEFAULT_WIDTH,
                                            height = PLOT_DEFAULT_HEIGHT,
                                            add_watermark = TRUE) {
  return (downloadHandler(
    filename = function() { paste0("IOTC_", filename_for(input, dataset, units, unit, categories, input$category), ".png") },
    contentType = "image/png",
    content = function(file) {
      plot_title = title_for(input, plot_name, units, unit, categories, input$category)
      ggsave(file, plot = watermark(add_watermark, titolize(source_dataset, last_update, plot_producer(), plot_title)), device = "png", width = width, height = height)
    }
  ))
}

UI_window_title = function(source_dataset, last_update, references) {
  return(
    paste0("IOTC data browser for ", toupper(source_dataset), " (", references$YEARS$MIN, "-", references$YEARS$MAX, ")",
           "[ Last updated: ", last_update, " ]")
  )
}

UI_main = function(source_dataset, last_update, references, class = "default", body, external_URL) {
  return(
    tags$div(class = "main-container",
      UI_glasspane(),
      tags$div(class = paste0("custom panel panel-", class),
        tags$div(class = "custom panel-heading", UI_heading(source_dataset, last_update, references, external_URL)),
        tags$div(class = "custom panel-body custom", body)
      )
    )
  )
}

UI_heading = function(source_dataset, last_update, references, external_URL) {
  return (
    fluidRow(
      column(
        width = 9,
        tags$h2(
          tags$a(href = "/browser/home", class = "btn btn-link", title = "Back to data browser homepage",
            tags$img(src="../../_resources/assets/iotc-logo.png", height="32px")
          ),
          tags$span(class="far fa-caret-square-left", style="vertical-align: text-bottom;", " "),
          tags$b(tags$u(capitalize(source_dataset))),
          tags$span(paste0("(", references$YEARS$MIN, "-", references$YEARS$MAX, ")")),
          tags$a(target = str_to_upper(str_replace(source_dataset, " ", "_")),
                 href = external_URL,
                 title = "Open in a separate window / tab",
                 class = "header-link",
                 tags$span(class="fas fa-external-link-alt", " ")
          )
        )
      ),
      column(
        width = 3,
        class = "text-right",
        tags$h3(
          tags$span(
            class="label label-danger", paste0("Last updated: ", last_update)
          )
        )
      )
    )
  )
}

UI_glasspane = function() {
  return(
    conditionalPanel(
      condition="$('html').hasClass('shiny-busy')",
      tags$div(id="glasspane",
               tags$div(class="loading", "Please wait...")
      )
    )
  )
}

UI_filters_categories = function(categories, selected = C_FISHERY_GROUP) {
  return(
    fluidRow(
      column(
        width = 4,
        selectizeInput("category", "Categorize by",
                       width = "100%", categories, selected = selected, multiple = FALSE)
      ),
      column(
        width = 4,
        sliderInput("maxCats", "Max. categories",
                    width = "100%",
                    min = 1, max = 20,
                    value = 10, step = 1,
                    ticks = FALSE,
                    animate = FALSE)
      ),
      column(
        width = 4,
        switchButton("categoryColors", "Use default category colors", value = TRUE, col = "GB", type = "TF")
      )
    )
  )
}

UI_filters_NC = function(references) {
  return(
    column(width = 3,
           sliderInput("period", "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput("fishingGrounds", "IO major area",   references$FISHING_GROUNDS, multiple = TRUE,  width = "100%"),

           selectizeInput("fleets", "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput("species", "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput("fisheryTypes", "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheryGroups", "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheries", "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput("gears", "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput("speciesCategories", "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput("speciesGroups", "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput("speciesWps", "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput("IUCNStatus", "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_EF = function(references) {
  return(
    column(width = 3,
           sliderInput("period", "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),
           selectizeInput("unit", "Effort unit",            references$EFFORT_UNITS   , multiple = FALSE, width = "100%",
                          selected = "HOOKS"),

           selectizeInput("fleets", "Fleet",                references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput("fisheryTypes", "Fishery type",   references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheryGroups", "Fishery group", references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheries", "Fishery",           references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput("gears", "Gear",                  references$GEARS          , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_CA = function(references) {
  return(
    column(width = 3,
           sliderInput("period", "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput("unit", "Catch unit",                references$CATCH_UNITS    , multiple = FALSE, width = "100%"),

           selectizeInput("fleets", "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput("species", "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput("fisheryTypes", "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheries", "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheryGroups", "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),

           selectizeInput("gears", "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput("speciesCategories", "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput("speciesGroups", "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput("speciesWps", "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput("IUCNStatus", "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_SF = function(references) {
  return(
    column(width = 3,
           sliderInput("period", "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput("unit", "Measure type",              references$MEASURE_TYPES  , multiple = FALSE, width = "100%"),
           selectizeInput("raising", "Raising",                references$RAISINGS       , multiple = TRUE,  width = "100%"),

           selectizeInput("fleets", "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput("species", "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput("fisheryTypes", "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheryGroups", "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheries", "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput("gears", "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput("speciesCategories", "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput("speciesGroups", "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput("speciesWps", "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput("IUCNStatus", "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_SF_STD = function(references) {
  return(
    column(width = 3,
           sliderInput("period", "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput("fleets", "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput("species", "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput("fisheryTypes", "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheryGroups", "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput("fisheries", "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput("gears", "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput("speciesCategories", "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput("speciesGroups", "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput("speciesWps", "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput("IUCNStatus", "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_summary_default = function() {
  return(
    tabPanel(
      icon = icon("stats", lib = "glyphicon"),
      "Summary",
      tabsetPanel(
        tabPanel(icon = icon("far fa-chart-bar"),
                "Stacked barchart",            do_plot_output("barChart"),
                                               do_create_download_button("downloadBarChart")),
        tabPanel(icon = icon("far fa-chart-bar"),
                "Stacked barchart (relative)", do_plot_output("barChartExpanded"),
                                               do_create_download_button("downloadBarChartExpanded")),
        tabPanel(icon = icon("far fa-chart-line"),
                "Time series",                 do_plot_output("lineChart"),
                                               do_create_download_button("downloadLineChart")),
        tabPanel(icon = icon("far fa-th-large"),
                "Treemap chart",               do_plot_output("treemapChart"),
                                               do_create_download_button("downloadTreemapChart")),
        tabPanel(icon = icon("far fa-ship"),
                "By fleet",                    do_plot_output("paretoChart"),
                                               do_create_download_button("downloadParetoChart")),
        tabPanel(icon = icon("list-alt", lib = "glyphicon"),
                "Dataset",                     do_create_download_button("downloadDataTable",
                                                                         DOWNLOAD_DATA_BUTTON_TEXT),
                                               dataTableOutput("table")
        )
      )
    )
  )
}

UI_NC_quality = function() {
  return(
    tabPanel(
      icon = icon("equalizer", lib = "glyphicon"), "Data quality",
      tabsetPanel(
        tabPanel("Nominal catch",        do_plot_output("ncQualityChart"),
                                         do_create_download_button("downloadNCQualityChart")),
        tabPanel("Vs. Catch-and-Effort", do_plot_output("ceQualityChart"),
                                         do_create_download_button("downloadCEQualityChart")),
        tabPanel("Vs. Size-Frequency",   do_plot_output("sfQualityChart"),
                                         do_create_download_button("downloadSFQualityChart")),
        tabPanel(icon = icon("list-alt", lib = "glyphicon"),
                 "Dataset",              do_create_download_button("downloadDataQualityTable",
                                                                   DOWNLOAD_DATA_BUTTON_TEXT),
                                         dataTableOutput("qualityTable")
        )
      )
    )
  )
}

UI_controls_GEO = function() {
  return(
    fluidRow(
      column(
        width = 2,
        selectizeInput("resolution", "Grid resolution", GRID_RESOLUTIONS, selected = grid_5x5, multiple = FALSE)
      ),
      column(
        width = 2,
        selectizeInput("quadrant", "Quadrant", QUADRANTS, multiple = FALSE)
      ),
      column(
        width = 2,
        switchButton("showIO", "Show IOTC sub-areas", value = FALSE, col = "GB", type = "TF")
      ),
      column(
        width = 2,
        switchButton("showHS", "Show high seas", value = TRUE , col = "GB", type = "TF")
      )
    )
  )
}

UI_controls_GEO_pie = function() {
  return(
    column(
      width = 3,
      switchButton("fixSize", "Fixed-size pies"         , value = FALSE, col = "GB", type = "TF"),
      switchButton("opacity", "Opaque pies"             , value = TRUE , col = "GB", type = "TF"),
      switchButton("useCenter", "Use exact grid centers", value = TRUE , col = "GB", type = "TF")
    )
  )
}

UI_GEO = function() {
  return(
    tabPanel(
      icon = icon("globe", lib = "glyphicon"),
      "Georeferenced",
      UI_controls_GEO(),
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              tabPanel(
                icon = icon("far fa-chart-pie"),
                "Piemap",
                fluidRow(
                  column(
                    width = 9,
                    plotOutput(height = GEOREF_CHART_HEIGHT, "geoPiemap"),
                    do_create_download_button("downloadGeoPiemap")
                  ),
                  UI_controls_GEO_pie()
                )
              ),
              tabPanel(
                icon = icon("th", lib = "glyphicon"),
                "Heatmap",
                fluidRow(
                  column(
                    width = 9,
                    plotOutput(height = GEOREF_CHART_HEIGHT, "geoHeatmap"),
                    do_create_download_button("downloadGeoHeatmap")
                  ),
                  column(
                    width = 3
                  )
                )
              )
            )
          )
        )
      )
    )
}

UI_size_distribution = function() {
  tabPanel(
    icon = icon("chart-area"),
    "Size distribution",
    fluidRow(
      column(
        width = 3,
        sliderInput("numColumns", "Number of columns",
                    width = "100%",
                    min = 1, max = SD_NUM_COLS, value = SD_NUM_COLS,
                    step = 1, ticks = FALSE)
      ),
      column(
        width = 3,
        switchButton("showSampleProportion", "Show relative sample proportion", value = FALSE, col = "GB", type = "TF")
      ),
      column(
        width = 3,
        switchButton("showMedian", "Show median", value = TRUE, col = "GB", type = "TF")
      ),
      column(
        width = 3,
        switchButton("showMean", "Show mean", value = FALSE, col = "GB", type = "TF")
      )
    ),
    fluidRow(
      column(
        width = 12,
        plotOutput(inline=TRUE, "sizeDistribution")
      )
    ),
    fluidRow(
      column(
        width = 12,
        do_create_download_button("downloadSizeDistribution")
      )
    )
  )
}

UI_samples_by_size = function() {
  tabPanel(
    icon = icon("far fa-chart-bar"),
    "Samples by size",
    fluidRow(
      column(
        width = 3,
        selectInput("sizeBin", "Bin size",
                    c("1 cm" = 1,
                      "2 cm" = 2,
                      "5 cm" = 5,
                      "10 cm" = 10),
                    multiple = FALSE, selected = "5")
      )
    ),
    fluidRow(
      column(
        width = 12,
        tabsetPanel(
          tabPanel(icon = icon("far fa-chart-bar"), "Stacked barchart",
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, "samplesBySizeBar"),
                   do_create_download_button("downloadSamplesBySizeBar")),
          tabPanel(icon = icon("far fa-chart-bar"), "Stacked barchart (relative)",
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, "samplesBySizeBarRel"),
                   do_create_download_button("downloadSamplesBySizeBarRel")),
          tabPanel(icon = icon("far fa-chart-line"), "Line chart",
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, "samplesBySizeLine"),
                   do_create_download_button("downloadSamplesBySizeLine")),
          tabPanel(icon = icon("far fa-chart-line"), "Line chart (relative)",
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, "samplesBySizeLineRel"),
                   do_create_download_button("downloadSamplesBySizeLineRel"))
        )
      )
    )
  )
}
