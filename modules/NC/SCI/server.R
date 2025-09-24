nc_sci_server <- function(id, source_dataset, ref, input, output, session) {
  
  prepare_NC_handlers(
    current_data = iotc.data.reference.datasets.NC::SCI,
    data_quality = iotc.data.reference.datasets.NC::DQA, 
    input = input, output = output, 
    prefix = "SCI", source_dataset = source_dataset, 
    last_update = iotc.data.reference.datasets.NC::METADATA$SCI$LAST_UPDATE, 
    configuration = current_configuration(ref)
  )
  
}