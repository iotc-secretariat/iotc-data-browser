#core packages
require(shiny)
require(periscope)
require(lubridate)
require(stringr)

#iotc software packages
#remotes::install_github("iotc-secretariat/iotc-lib-core-utils-constants")
require(iotc.core.utils.constants)
#remotes::install_github("iotc-secretariat/iotc-lib-core-utils-misc")
require(iotc.core.utils.misc)
#remotes::install_github("iotc-secretariat/iotc-lib-core-utils-aes")
require(iotc.core.utils.aes)
#remotes::install_github("iotc-secretariat/iotc-lib-core-gis-cwp-wkt", ref = "0.2.1-standalone")
require(iotc.core.gis.wkt) 
#remotes::install_github("iotc-secretariat/iotc-lib-core-gis-cwp")
#remotes::install_github("iotc-secretariat/iotc-lib-core-gis-cwp-io", ref = "0.2.1-standalone")
require(iotc.core.gis.cwp.IO)
#remotes::install_github("iotc-secretariat/iotc-lib-core-gis-maps", ref = "0.2.1-standalone")
require(iotc.core.gis.maps)

#IOTC core db packages
#remotes::install_github("iotc-secretariat/iotc-lib-core-db-connections")
require(iotc.core.db.connections)
#remotes::install_github("iotc-secretariat/iotc-lib-core-db-data")
require(iotc.core.db.data)

#IOTC base common packages
#remotes::install_github("iotc-secretariat/iotc-lib-base-common-data")
require(iotc.base.common.data)
#remotes::install_github("iotc-secretariat/iotc-lib-base-common-plots")
require(iotc.base.common.plots)

#IOTC data packages
#remotes::install_github("iotc-secretariat/iotc-data-reference-codelists")
require(iotc.data.reference.codelists)
#remotes::install_github("iotc-secretariat/iotc-reference-datasets-nc")
require(iotc.data.reference.datasets.NC)
#remotes::install_github("iotc-secretariat/iotc-reference-datasets-ce")
require(iotc.data.reference.datasets.CE)
#remotes::install_github("iotc-secretariat/iotc-reference-datasets-sf")
require(iotc.data.reference.datasets.SF.raw)
#remotes::install_github("iotc-secretariat/iotc-reference-datasets-sf-std")
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
source("./modules/CE/CA/server.R")
source("./modules/CE/CA/ui.R")
source("./modules/SF/RAW/server.R")
source("./modules/SF/RAW/ui.R")
source("./modules/SF/STD/server.R")
source("./modules/SF/STD/ui.R")