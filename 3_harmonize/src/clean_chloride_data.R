#' @title Clean chloride data
#' 
#' @description 
#' Function to clean chloride data, currently does nothing 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a data record. [MORE HERE]
#' 
clean_chloride_data <- function(wqp_data){
  
  # Clean chloride data
  wqp_data_out <- wqp_data %>% 
    # Convert values from various concentrations to mg/L
    mutate(conversion_multiplier = case_when(
      is.na(ResultMeasure.MeasureUnitCode) ~ NA_integer_,
      tolower(ResultMeasure.MeasureUnitCode) == "mg/l" ~ 1,
      tolower(ResultMeasure.MeasureUnitCode) == "ug/l" ~ (1/1000),
      tolower(ResultMeasure.MeasureUnitCode) == "mmol/l" ~ 35.453,
      tolower(ResultMeasure.MeasureUnitCode) == "mg/g" ~ 1000,
      tolower(ResultMeasure.MeasureUnitCode) == "mg/kg" ~ 1,
      tolower(ResultMeasure.MeasureUnitCode) == "ppm" ~ 1,
      TRUE ~ NA_integer_
    )) %>% 
    mutate(ResultMeasureValue = ResultMeasureValue * conversion_multiplier) %>% 
    # Now update units for those unit codes we converted
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
  
  return(wqp_data_out)
  
}
