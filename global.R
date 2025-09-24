#core packages
require(shiny)
require(periscope)
require(lubridate)
require(stringr)

#iotc software packages
require(iotc.core.db.connections)
require(iotc.core.db.data)
require(iotc.core.utils.misc)
require(iotc.core.gis.cwp.IO.standalone)
require(iotc.data.reference.codelists)
require(iotc.base.common.data)
require(iotc.base.common.plots)

#IOTC data packages
require(iotc.data.reference.datasets.NC)
require(iotc.data.reference.datasets.CE)
require(iotc.data.reference.datasets.SF.raw)
require(iotc.data.reference.datasets.SF.std)

#global options
iotc.core.db.connections::setDefaultDBIHandler( function(){ return(NULL)} )

#assets
common_assets <- list.files("assets/_common", pattern = ".R", full.names = T)
for(common_asset in common_assets) source(common_asset)
nc_assets <- list.files("modules/NC", pattern = ".R", full.names = T, recursive = T)
for(nc_asset in nc_assets) source(nc_asset)
