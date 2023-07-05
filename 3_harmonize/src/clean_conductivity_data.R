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
    # Retain the original units for reference
    mutate(ResultMeasure.MeasureUnitCode_original = ResultMeasure.MeasureUnitCode) %>% 
    # Update units in column "ResultMeasure.MeasureUnitCode" BEFORE converting
    mutate(ResultMeasure.MeasureUnitCode = case_when(
      # If result measure unit code is NA but units are reported for
      # the detection quantitation limit, assume that the quantitation
      # units also apply to the result value
      !is.na(ResultMeasureValue) & 
        is.na(ResultMeasure.MeasureUnitCode) &
        !is.na(DetectionQuantitationLimitMeasure.MeasureUnitCode) ~
        DetectionQuantitationLimitMeasure.MeasureUnitCode,
      TRUE ~ ResultMeasure.MeasureUnitCode
    )) %>% 
    # convert values to uS/cm & keep the original value
    mutate(conversion_multiplier = case_when(
      is.na(ResultMeasure.MeasureUnitCode) ~ NA_integer_,
      tolower(ResultMeasure.MeasureUnitCode) == "us/cm" ~ 1,
      tolower(ResultMeasure.MeasureUnitCode) == "ms/cm" ~ 1000,
      tolower(ResultMeasure.MeasureUnitCode) == "s/m" ~ (10^6/10^3),
      tolower(ResultMeasure.MeasureUnitCode) == "ms/m" ~ 10,
      tolower(ResultMeasure.MeasureUnitCode) == "umho/cm" ~ 1,
      tolower(ResultMeasure.MeasureUnitCode) == "mho/cm" ~ 10^6,
      tolower(ResultMeasure.MeasureUnitCode) == "mmhos/cm" ~ 1000,
      tolower(ResultMeasure.MeasureUnitCode) == "mmol/l" ~ 35.453,
      TRUE ~ NA_integer_
    )) %>% 
    mutate(ResultMeasureValue_original = ResultMeasureValue,
           ResultMeasureValue = ResultMeasureValue * conversion_multiplier) %>% 
    # Now update units for those unit codes we converted
    mutate(ResultMeasure.MeasureUnitCode = case_when(
      is.na(ResultMeasure.MeasureUnitCode) ~ as.character(NA),
      tolower(ResultMeasure.MeasureUnitCode) %in% c(
        "us/cm", 
        "ms/cm", "s/m", "ms/m",
        "umho/cm", "mho/cm", "mmhos/cm",
        "mmol/l"
      ) ~ "uS/cm",
      tolower(ResultMeasure.MeasureUnitCode) %in% c(
        "none"
      ) ~ as.character(NA),
      TRUE ~ ResultMeasure.MeasureUnitCode
    )) %>% 
    # Attempt to standardize units using temperature basis
    mutate(ResultMeasure.MeasureUnitCode = ifelse(
      !is.na(ResultMeasureValue) & 
        ResultMeasure.MeasureUnitCode == "uS/cm" &
        grepl("25 deg C", ResultTemperatureBasisText, ignore.case = TRUE),
      yes = paste0(ResultMeasure.MeasureUnitCode, " @25C"),
      no = ResultMeasure.MeasureUnitCode
    ))
  
  # Now filter out missing values (some became a missing value if their unit code was unknown)
  wqp_data_out <- wqp_data_harmonized_units %>% 
    filter(!is.na(ResultMeasureValue)) %>% 
    harmonize_output_columns() # Have columns match output of all other characteristics
  
  message(sprintf('`clean_conductivity_data()` units harmonization resulted in %s records being dropped',
                  nrow(wqp_data_harmonized_units)-nrow(wqp_data_out)))
  
  return(wqp_data_out)
}




                                    