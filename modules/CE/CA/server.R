ce_ca_server <- function(id, source_dataset, current_data, ref, input, output, session) {
  common_prepare_handlers(
    current_data = current_data,
    current_data_table = current_data,
    input = input, output = output,
    prefix = "CA_RAW", 
    source_dataset = source_dataset, 
    last_update = iotc.data.reference.datasets.CE::METADATA$RAW.CA$LAST_UPDATE,
    configuration = current_configuration(ref)
  )
  georeferenced_prepare_handlers(
    current_data = current_data, 
    input = input, output = output,
    prefix = "CA_RAW", 
    source_dataset = source_dataset,
    last_update = iotc.data.reference.datasets.CE::METADATA$RAW.CA$LAST_UPDATE, 
    configuration = current_configuration(ref)
  )
}