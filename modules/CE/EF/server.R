ce_ef_server <- function(id, source_dataset, current_data, ref, input, output, session) {
  
  common_prepare_handlers(
    current_data = current_data, 
    current_data_table = current_data, 
    input = input, output = output, prefix = "EF_RAW", 
    source_dataset = SOURCE_DATASET, 
    last_update =  iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE, 
    configuration = current_configuration(ref))
  
  georeferenced_prepare_handlers(
    current_data = current_data, 
    input = input, output = output, prefix = "EF_RAW", 
    source_dataset = SOURCE_DATASET, 
    last_update = iotc.data.reference.datasets.CE::METADATA$RAW.EF$LAST_UPDATE, 
    configuration = current_configuration(ref)
  )
}