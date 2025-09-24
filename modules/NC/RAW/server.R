nc_raw_server <- function(id, source_dataset, ref, input, output, session) {

  prepare_NC_handlers(
    current_data = iotc.data.reference.datasets.NC::RAW, 
    data_quality = iotc.data.reference.datasets.NC::DQA, 
    input = input, output = output, prefix = "RAW", 
    source_dataset = source_dataset, 
    last_update = iotc.data.reference.datasets.NC::METADATA$RAW$LAST_UPDATE, 
    current_configuration(ref)
  )

}