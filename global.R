#core packages
require(shiny)
require(periscope)
require(lubridate)
require(stringr)

#iotc software packages
require(iotc.core.utils.misc)
require(iotc.core.gis.maps) #standalone
require(iotc.core.gis.wkt) #standalone
require(iotc.core.gis.cwp.IO) #standalone

#IOTC core db packages
require(iotc.core.db.connections)
require(iotc.core.db.data)

#IOTC base common packages
require(iotc.base.common.data)
require(iotc.base.common.plots)

#IOTC data packages
require(iotc.data.reference.codelists)
require(iotc.data.reference.datasets.NC)
require(iotc.data.reference.datasets.CE)
require(iotc.data.reference.datasets.SF.raw)
require(iotc.data.reference.datasets.SF.std)

#global options
iotc.core.db.connections::setDefaultDBIHandler( function(){ return(NULL)} )

#assets
common_assets <- list.files("assets/scripts", pattern = ".R", full.names = T)
for(common_asset in common_assets) source(common_asset)

#thematic modules
source("./modules/NC/RAW/server.R")
source("./modules/NC/RAW/ui.R")
source("./modules/NC/SCI/server.R")
source("./modules/NC/SCI/ui.R")
source("./modules/CE/EF/server.R")
source("./modules/CE/EF/ui.R")
