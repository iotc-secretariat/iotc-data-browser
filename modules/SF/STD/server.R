sf_std_server <- function(id, source_dataset, current_data, current_data_table, ref, input, output, session){
  
  cfg = current_configuration(ref)
  
  common_prepare_handlers(
    current_data = current_data, 
    current_data_table = current_data_table, 
    input = input, output = output, 
    prefix = "SF_STD", 
    source_dataset = source_dataset, 
    last_update = iotc.data.reference.datasets.SF.std::METADATA$STD.SF$LAST_UPDATE, 
    configuration = current_configuration(ref)
  )
  georeferenced_prepare_handlers(
    current_data = current_data,
    input = input, output = output, 
    prefix = "SF_STD", source_dataset, SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE,
    configuration = current_configuration(ref)
  )
  SF_prepare_handlers(
    data = current_data,
    tabular_data = current_data_table,
    input = input, output = output,
    source_dataset = SOURCE_DATASET, 
    last_update = SF_STD_ENV$METADATA$STD.SF$LAST_UPDATE,
    configuration = current_configuration(ref)
  )

}