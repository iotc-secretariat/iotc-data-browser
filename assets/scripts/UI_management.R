SD_NUM_COLS = 4
SD_ROW_YEAR_HEIGHT = 20

IOTC_LOGO = png::readPNG("www/iotc-logo.png")
IOTC_LOGO_RASTER = grid::rasterGrob(IOTC_LOGO, interpolate = TRUE,
                                    #width = unit(75, "points"),
                                    width = grid::unit(0.07, "npc"),
                                    #x = unit(.09, "npc"), y = unit(.98, "npc"),
                                    x = grid::unit(0.08, "npc"), y = grid::unit(.98, "npc"),
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
          actionLink(inputId = 'home', class = "btn btn-link", title = "Back to data browser homepage",
            label = tags$img(src="iotc-logo.png", height="32px")
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

UI_filters_categories = function(ns, categories, selected = C_FISHERY_GROUP) {
  return(
    fluidRow(
      column(
        width = 4,
        selectizeInput(ns("category"), "Categorize by",
                       width = "100%", categories, selected = selected, multiple = FALSE)
      ),
      column(
        width = 4,
        sliderInput(ns("maxCats"), "Max. categories",
                    width = "100%",
                    min = 1, max = 20,
                    value = 10, step = 1,
                    ticks = FALSE,
                    animate = FALSE)
      ),
      column(
        width = 4,
        switchButton(inputId = ns("categoryColors"), "Use default category colors", value = TRUE, col = "GB", type = "TF")
      )
    )
  )
}

UI_filters_NC = function(ns, references) {
  return(
    column(width = 3,
           sliderInput(inputId = ns("period"), "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput(ns("fishingGrounds"), "IO major area",   references$FISHING_GROUNDS, multiple = TRUE,  width = "100%"),

           selectizeInput(ns("fleets"), "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("species"), "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("fisheryTypes"), "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheryGroups"), "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheries"), "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("gears"), "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("speciesCategories"), "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesGroups"), "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesWps"), "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("IUCNStatus"), "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_EF = function(ns, references) {
  return(
    column(width = 3,
           sliderInput(ns("period"), "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),
           selectizeInput(ns("unit"), "Effort unit",            references$EFFORT_UNITS   , multiple = FALSE, width = "100%",
                          selected = "HOOKS"),

           selectizeInput(ns("fleets"), "Fleet",                references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("fisheryTypes"), "Fishery type",   references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheryGroups"), "Fishery group", references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheries"), "Fishery",           references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("gears"), "Gear",                  references$GEARS          , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_CA = function(ns, references) {
  return(
    column(width = 3,
           sliderInput(ns("period"), "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput(ns("unit"), "Catch unit",                references$CATCH_UNITS    , multiple = FALSE, width = "100%"),

           selectizeInput(ns("fleets"), "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("species"), "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("fisheryTypes"), "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheries"), "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheryGroups"), "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("gears"), "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("speciesCategories"), "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesGroups"), "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesWps"), "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("IUCNStatus"), "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_SF = function(ns, references) {
  return(
    column(width = 3,
           sliderInput(ns("period"), "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput(ns("unit"), "Measure type",              references$MEASURE_TYPES  , multiple = FALSE, width = "100%"),
           selectizeInput(ns("raising"), "Raising",                references$RAISINGS       , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("fleets"), "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("species"), "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("fisheryTypes"), "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheryGroups"), "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheries"), "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("gears"), "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("speciesCategories"), "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesGroups"), "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesWps"), "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("IUCNStatus"), "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_filters_SF_STD = function(ns, references) {
  return(
    column(width = 3,
           sliderInput(ns("period"), "Years",
                       width = "100%",
                       min = references$YEARS$MIN, max = references$YEARS$MAX, value = range(references$YEARS$MIN, references$YEARS$MAX),
                       step = 1, sep = "", animate = FALSE),

           selectizeInput(ns("fleets"), "Fleet",                   references$FLEETS         , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("species"), "Species",                references$SPECIES        , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("fisheryTypes"), "Fishery type",      references$FISHERY_TYPES  , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheryGroups"), "Fishery group",    references$FISHERY_GROUPS , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("fisheries"), "Fishery",              references$FISHERIES      , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("gears"), "Gear",                     references$GEARS          , multiple = TRUE,  width = "100%"),

           selectizeInput(ns("speciesCategories"), "Species category", references$SPECIES_CATEGORIES, multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesGroups"), "Species group",        references$SPECIES_GROUPS    , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("speciesWps"), "Species working party",   references$SPECIES_WPS       , multiple = TRUE,  width = "100%"),
           selectizeInput(ns("IUCNStatus"), "Species IUCN status",     references$IUCN_STATUS       , multiple = TRUE,  width = "100%")
    )
  )
}

UI_summary_default = function(ns) {
  return(
    tabPanel(
      icon = icon("stats", lib = "glyphicon"),
      "Summary",
      tabsetPanel(
        tabPanel(icon = icon("far fa-chart-bar"),
                "Stacked barchart",            do_plot_output(ns("barChart")),
                                               do_create_download_button(ns("downloadBarChart"))),
        tabPanel(icon = icon("far fa-chart-bar"),
                "Stacked barchart (relative)", do_plot_output(ns("barChartExpanded")),
                                               do_create_download_button(ns("downloadBarChartExpanded"))),
        tabPanel(icon = icon("far fa-chart-line"),
                "Time series",                 do_plot_output(ns("lineChart")),
                                               do_create_download_button(ns("downloadLineChart"))),
        tabPanel(icon = icon("far fa-th-large"),
                "Treemap chart",               do_plot_output(ns("treemapChart")),
                                               do_create_download_button(ns("downloadTreemapChart"))),
        tabPanel(icon = icon("far fa-ship"),
                "By fleet",                    do_plot_output(ns("paretoChart")),
                                               do_create_download_button(ns("downloadParetoChart"))),
        tabPanel(icon = icon("list-alt", lib = "glyphicon"),
                "Dataset",                     do_create_download_button(ns("downloadDataTable"),
                                                                         DOWNLOAD_DATA_BUTTON_TEXT),
                                               DT::dataTableOutput(ns("table"))
        )
      )
    )
  )
}

UI_NC_quality = function(ns) {
  return(
    tabPanel(
      icon = icon("equalizer", lib = "glyphicon"), "Data quality",
      tabsetPanel(
        tabPanel("Nominal catch",        do_plot_output(ns("ncQualityChart")),
                                         do_create_download_button(ns("downloadNCQualityChart"))),
        tabPanel("Vs. Catch-and-Effort", do_plot_output(ns("ceQualityChart")),
                                         do_create_download_button(ns("downloadCEQualityChart"))),
        tabPanel("Vs. Size-Frequency",   do_plot_output(ns("sfQualityChart")),
                                         do_create_download_button(ns("downloadSFQualityChart"))),
        tabPanel(icon = icon("list-alt", lib = "glyphicon"),
                 "Dataset",              do_create_download_button(ns("downloadDataQualityTable"),
                                                                   DOWNLOAD_DATA_BUTTON_TEXT),
                                         dataTableOutput(ns("qualityTable"))
        )
      )
    )
  )
}

UI_controls_GEO = function(ns) {
  return(
    fluidRow(
      column(
        width = 2,
        selectizeInput(ns("resolution"), "Grid resolution", GRID_RESOLUTIONS, selected = grid_5x5, multiple = FALSE)
      ),
      column(
        width = 2,
        selectizeInput(ns("quadrant"), "Quadrant", QUADRANTS, multiple = FALSE)
      ),
      column(
        width = 2,
        switchButton(ns("showIO"), "Show IOTC sub-areas", value = FALSE, col = "GB", type = "TF")
      ),
      column(
        width = 2,
        switchButton(ns("showHS"), "Show high seas", value = TRUE , col = "GB", type = "TF")
      )
    )
  )
}

UI_controls_GEO_pie = function(ns) {
  return(
    column(
      width = 3,
      switchButton(inputId = ns("fixSize"), "Fixed-size pies"         , value = FALSE, col = "GB", type = "TF"),
      switchButton(inputId = ns("opacity"), "Opaque pies"             , value = TRUE , col = "GB", type = "TF"),
      switchButton(inputId = ns("useCenter"), "Use exact grid centers", value = TRUE , col = "GB", type = "TF")
    )
  )
}

UI_GEO = function(ns) {
  return(
    tabPanel(
      icon = icon("globe", lib = "glyphicon"),
      "Georeferenced",
      UI_controls_GEO(ns),
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
                    plotOutput(height = GEOREF_CHART_HEIGHT, outputId = ns("geoPiemap")),
                    do_create_download_button(ns("downloadGeoPiemap"))
                  ),
                  UI_controls_GEO_pie(ns)
                )
              ),
              tabPanel(
                icon = icon("th", lib = "glyphicon"),
                "Heatmap",
                fluidRow(
                  column(
                    width = 9,
                    plotOutput(height = GEOREF_CHART_HEIGHT, ns("geoHeatmap")),
                    do_create_download_button(ns("downloadGeoHeatmap"))
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

UI_size_distribution = function(ns) {
  tabPanel(
    icon = icon("chart-area"),
    "Size distribution",
    fluidRow(
      column(
        width = 3,
        sliderInput(ns("numColumns"), "Number of columns",
                    width = "100%",
                    min = 1, max = SD_NUM_COLS, value = SD_NUM_COLS,
                    step = 1, ticks = FALSE)
      ),
      column(
        width = 3,
        switchButton(ns("showSampleProportion"), "Show relative sample proportion", value = FALSE, col = "GB", type = "TF")
      ),
      column(
        width = 3,
        switchButton(ns("showMedian"), "Show median", value = TRUE, col = "GB", type = "TF")
      ),
      column(
        width = 3,
        switchButton(ns("showMean"), "Show mean", value = FALSE, col = "GB", type = "TF")
      )
    ),
    fluidRow(
      column(
        width = 12,
        plotOutput(inline=TRUE, ns("sizeDistribution"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        do_create_download_button(ns("downloadSizeDistribution"))
      )
    )
  )
}

UI_samples_by_size = function(ns) {
  tabPanel(
    icon = icon("far fa-chart-bar"),
    "Samples by size",
    fluidRow(
      column(
        width = 3,
        selectInput(ns("sizeBin"), "Bin size",
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
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, ns("samplesBySizeBar")),
                   do_create_download_button(ns("downloadSamplesBySizeBar"))),
          tabPanel(icon = icon("far fa-chart-bar"), "Stacked barchart (relative)",
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, ns("samplesBySizeBarRel")),
                   do_create_download_button(ns("downloadSamplesBySizeBarRel"))),
          tabPanel(icon = icon("far fa-chart-line"), "Line chart",
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, ns("samplesBySizeLine")),
                   do_create_download_button(ns("downloadSamplesBySizeLine"))),
          tabPanel(icon = icon("far fa-chart-line"), "Line chart (relative)",
                   plotOutput(height = SIZE_LINE_CHART_HEIGHT, ns("samplesBySizeLineRel")),
                   do_create_download_button(ns("downloadSamplesBySizeLineRel")))
        )
      )
    )
  )
}
