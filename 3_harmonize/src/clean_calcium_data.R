#' @title Clean calcium data
#' 
#' @description 
#' Function to clean calcium data, currently does nothing 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a data record. [MORE HERE]
#' 
clean_calcium_data <- function(wqp_data){

  # Clean calcium data
  wqp_data_harmonized_units <- wqp_data %>% 
    # Convert values from various concentrations to mg/L & keep the original
    mutate(conversion_multiplier = case_when(
      is.na(ResultMeasure.MeasureUnitCode) ~ NA_integer_, # This will effectively, turn the result into an NA value
      tolower(ResultMeasure.MeasureUnitCode) == "mg/l" ~ 1,
      tolower(ResultMeasure.MeasureUnitCode) == "ug/l" ~ (1/1000),
      tolower(ResultMeasure.MeasureUnitCode) == "mmol/l" ~ 35.453, # THIS IS DIFFERENT
      tolower(ResultMeasure.MeasureUnitCode) == "mg/g" ~ 1000,
      tolower(ResultMeasure.MeasureUnitCode) == "mg/kg" ~ 1,
      tolower(ResultMeasure.MeasureUnitCode) == "ppm" ~ 1,
      # TODO: what about `mg/l CaCO3**`, `mg/l CaCO3`, `ueq/L`, and `%`
      TRUE ~ NA_integer_
    )) %>% 
    mutate(ResultMeasureValue_original = ResultMeasureValue,
           ResultMeasureValue = ResultMeasureValue * conversion_multiplier) %>% 
    # Now update units for those unit codes we converted, keep original value
    mutate(ResultMeasure.MeasureUnitCode_original = ResultMeasure.MeasureUnitCode) %>% 
    mutate(ResultMeasure.MeasureUnitCode = case_when(
      is.na(ResultMeasure.MeasureUnitCode) ~ as.character(NA),
      tolower(ResultMeasure.MeasureUnitCode) %in% c(
        "mg/l", "ug/l", 
        "mmol/l",
        "mg/g", "mg/kg", "ppm"
      ) ~ "mg/l",
      TRUE ~ ResultMeasure.MeasureUnitCode
    )) %>% 
    select(-conversion_multiplier)
  
  # Now filter out missing values (some became a missing value if their unit code was unknown)
  wqp_data_out <- wqp_data_harmonized_units %>% 
    filter(!is.na(ResultMeasureValue)) %>% 
    harmonize_output_columns() # Have columns match output of all other characteristics
  
  message(sprintf('`clean_calcium_data()` units harmonization resulted in %s records being dropped',
                  nrow(wqp_data_harmonized_units)-nrow(wqp_data_out)))
  
  return(wqp_data_out)
  
}
