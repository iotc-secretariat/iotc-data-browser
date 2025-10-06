# Project Title
IOTC Data Browser (Shiny App)

# Overview
This Shiny application provides access to IOTC public domain data on catches, effort, and size frequencies.  

The app is hosted on the FAO ShinyApps account: <https://foodandagricultureorganization.shinyapps.io/iotc-data-browser>  

It is also accessible via the IOTC website: <https://iotc.org/data/browser>

# Features
The IOTC Data Browser provides an interactive interface to explore the IOTC public-domain datasets.  
Main features include:  

- **Data access and filtering**: Select and filter data by species, fishery, area, and time period
- **Visualisation**: Generate interactive charts (e.g. bar plots, maps, time series) to explore trends and patterns
- **Export options**: Save visualisations as PNG images for use in presentations or reports  
- **Interactive tables**: Browse data in responsive JavaScript tables for quick inspection
- **Data download**: Export the selected datasets as CSV files for further analysis

# Installation
The IOTC Shiny App is deployed by the IOTC Secretariat on the FAO shinyapps.io account, a managed hosting service provided by Posit.

# Data Sources

The datasets available through this application are the main public-domain datasets of the Indian Ocean Tuna Commission (IOTC).  

They are compiled from data submitted by IOTC Contracting Parties and Cooperating Non-Contracting Parties (CPCs), in accordance with IOTC Conservation and Management Measures (CMMs).  

The data are curated and consolidated by the IOTC Secretariat to ensure consistency and availability for scientific and management purposes.

# Dependencies

## IOTC Function Packages

- iotc.core.utils.constants from [iotc-lib-core-utils-constants](https://github.com/iotc-secretariat/iotc-lib-core-utils-constants) repository 
- iotc.core.utils.misc from [iotc-lib-core-utils-misc](https://github.com/iotc-secretariat/iotc-lib-core-utils-misc) repository 
- iotc.core.utils.aes from [iotc-lib-core-utils-aes](https://github.com/iotc-secretariat/iotc-lib-core-utils-aes) repository 
- iotc.core.gis.wkt from [iotc-lib-core-gis-cwp-wkt](https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp-wkt) repository 
- iotc.core.gis.cwp.IO from [iotc-lib-core-gis-cwp-io](https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp-io) repository
- iotc.core.gis.maps from [iotc-lib-core-gis-maps](https://github.com/iotc-secretariat/iotc-lib-core-gis-maps) repository
- iotc.core.db.connections from [iotc-lib-core-db-connections](https://github.com/iotc-secretariat/iotc-lib-core-db-connections) repository
- iotc.core.db.data from [iotc-lib-core-db-data](https://github.com/iotc-secretariat/iotc-lib-core-db-data) repository
- iotc.base.common.data from [iotc-lib-base-common-data](https://github.com/iotc-secretariat/iotc-lib-base-common-data) repository
- iotc.base.common.plots from [iotc-lib-base-common-plots](https://github.com/iotc-secretariat/iotc-lib-base-common-plots) repository

## IOTC Data Packages

- iotc.data.reference.codelists from [iotc-data-reference-codelists](https://github.com/iotc-secretariat/iotc-data-reference-codelists) repository
- iotc.data.reference.datasets.NC from [iotc-reference-datasets-nc](https://github.com/iotc-secretariat/) repository
- iotc.data.reference.datasets.CE from [iotc-reference-datasets-ce](https://github.com/iotc-secretariat/iotc-reference-datasets-ce) repository
- iotc.data.reference.datasets.SF.raw from [iotc-reference-datasets-sf](https://github.com/iotc-secretariat/iotc-reference-datasets-sf) repository
- iotc.data.reference.datasets.SF.std from [iotc-reference-datasets-sf](https://github.com/iotc-secretariat/iotc-reference-datasets-sf) repository

# License
Copyright &copy; Indian Ocean Tuna Commission (IOTC).

This Shiny App is released under the [MIT License](https://opensource.org/licenses/MIT).

You are free to use, modify, and distribute this software, provided that the copyright notice and license terms are included in any copies or substantial portions of the software.
The software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, and noninfringement.

# Contact
IOTC Secretariat - [IOTC-Statistics@fao.org](mailto:IOTC-Statistics@fao.org)
