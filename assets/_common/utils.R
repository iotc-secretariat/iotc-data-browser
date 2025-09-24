#General helpers
logger <- function(type, symbol, txt, ...){
  log_txt <- sprintf(txt, ...)
  cat(sprintf("[iotc-data-browser][%s] %s %s \n", type, symbol, log_txt), file = stderr())
}
INFO <- function(txt, ...){logger("INFO", "\u2139", txt, ...)}
WARN <- function(txt, ...){logger("WARN", "\u26A0", txt, ...)}
ERROR <- function(txt, ...){logger("ERROR", "\u274C", txt, ...)}
DEBUG <- function(txt, ...){logger("DEBUG", "\u203C", txt, ...)}

DEBUG_MODULE_PROCESSING_TIME <- function(module, start, end){
  module_time = end - start
  DEBUG("\u23F3 %s module loaded in %s %s", module, as(module_time, "numeric"), attr(module_time, "units"))
}



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