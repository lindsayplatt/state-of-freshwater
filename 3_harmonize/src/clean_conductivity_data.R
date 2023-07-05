#' @title Clean conductivity data
#' 
#' @description 
#' Function to clean conductivity data, including harmonizing diverse units. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a data record. Conductivity
#' units have been standardized to uS/cm where possible. 
#' 
clean_conductivity_data <- function(wqp_data){
  
  # Harmonize conductivity units when possible
  wqp_data_harmonized_units <- wqp_data %>%
    # convert values from mS/cm to uS/cm
    mutate(conversion_multiplier = case_when(
      is.na(ResultMeasure.MeasureUnitCode) ~ NA_integer_,
      tolower(ResultMeasure.MeasureUnitCode) == "us/cm" ~ 1,
      tolower(ResultMeasure.MeasureUnitCode) == "ms/cm" ~ 1000,
      tolower(ResultMeasure.MeasureUnitCode) == "s/m" ~ (10^6/10^3),
      tolower(ResultMeasure.MeasureUnitCode) == "mmol/l" ~ 35.453,
      # TODO: what about `ueq/L` and `umol`?
      TRUE ~ NA_integer_
    )) %>% 
    # Keep original unit code
    mutate(ResultMeasure.MeasureUnitCode_original = ResultMeasure.MeasureUnitCode) %>% 
    mutate(ResultMeasureValue = if_else(!is.na(ResultMeasureValue) &
                                          ResultMeasure.MeasureUnitCode == "mS/cm",
                                        (ResultMeasureValue * 1000), ResultMeasureValue),
           # update units in column "ResultMeasure.MeasureUnitCode"
           ResultMeasure.MeasureUnitCode = case_when(
             # convert mS/cm to uS/cm
             !is.na(ResultMeasureValue) & 
               ResultMeasure.MeasureUnitCode == "mS/cm" ~ "uS/cm",
             # convert equivalent units (umho/cm) to uS/cm
             !is.na(ResultMeasureValue) & 
               ResultMeasure.MeasureUnitCode == "umho/cm" ~ "uS/cm",
             # if result measure unit code is NA but units are reported for
             # the detection quantitation limit, assume that the quantitation
             # units also apply to the result value
             !is.na(ResultMeasureValue) & 
               is.na(ResultMeasure.MeasureUnitCode) &
               !is.na(DetectionQuantitationLimitMeasure.MeasureUnitCode) ~
               DetectionQuantitationLimitMeasure.MeasureUnitCode,
             # attempt to standardize units using temperature basis
             !is.na(ResultMeasureValue) & 
               ResultMeasure.MeasureUnitCode == "uS/cm" &
               grepl("25 deg C", ResultTemperatureBasisText, ignore.case = TRUE) ~
               paste0(ResultMeasure.MeasureUnitCode, " @25C"),
             TRUE ~ ResultMeasure.MeasureUnitCode)
           ) %>%
    # assume that uS/cm is equivalent to uS/cm at 25 degrees C
    mutate(ResultMeasure.MeasureUnitCode = if_else(!is.na(ResultMeasureValue) & 
                                                     ResultMeasure.MeasureUnitCode == "uS/cm @25C", 
                                                   "uS/cm", ResultMeasure.MeasureUnitCode)) 
  
  # Now filter out missing values (some became a missing value if their unit code was unknown)
  wqp_data_out <- wqp_data_harmonized_units %>% 
    filter(!is.na(ResultMeasureValue)) %>% 
    harmonize_output_columns() # Have columns match output of all other characteristics
  
  message(sprintf('`clean_conductivity_data()` units harmonization resulted in %s records being dropped',
                  nrow(wqp_data_harmonized_units)-nrow(wqp_data_out)))
  
  return(wqp_data_out)
}




                                    