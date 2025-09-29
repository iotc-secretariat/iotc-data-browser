sf_raw_server <- function(id, source_dataset, current_data, current_data_table, ref, input, output, session) {
  
  common_prepare_handlers(
    current_data = current_data, 
    current_data_table = current_data_table, 
    input = input, output = output,
    prefix = "SF_RAW", 
    source_dataset = source_dataset,
    last_update = iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE, 
    configuration = current_configuration(ref)
  )
  georeferenced_prepare_handlers(
    current_data = current_data,
    input = input, 
    output = output, 
    prefix = "SF_RAW",
    source_dataset = source_dataset,
    last_update = iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE,
    configuration = current_configuration(ref)
  )
  SF_prepare_handlers(
    data = current_data,
    tabular_data = current_data_table,
    input = input, output = output,
    prefix = "SF_RAW",
    source_dataset = source_dataset,
    last_update = iotc.data.reference.datasets.SF.raw::METADATA$RAW.SF$LAST_UPDATE,
    configuration = current_configuration(ref)
  )
  
  observe({
    selected_measure_type = input$unit
    selected_size_bin = input$sizeBin
    
    measure_type = measure_type_by_code(selected_measure_type)
    
    values = c("1 cm" = 1,
               "2 cm" = 2,
               "5 cm" = 5,
               "10 cm" = 10)
    
    if(measure_type$MEASURE_UNIT == "KG")
      values = c("1 kg" = 1,
                 "2 kg" = 2,
                 "5 kg" = 5,
                 "10 kg" = 10)
    
    updateSelectInput(
      session,
      "sizeBin",
      "Bin size",
      values,
      selected = selected_size_bin
    )
  })
}