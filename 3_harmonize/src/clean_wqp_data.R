#' @title Clean WQP data
#' 
#' @description 
#' Function to harmonize WQP data in preparation for further analysis. Included
#' in this function are steps to unite diverse characteristic names by assigning
#' them to more commonly-used water quality parameter names; to flag missing
#' records as well as duplicate records; and to carry out parameter-specific
#' harmonization steps for temperature and conductivity data, including
#' harmonizing units where possible. 
#' 
#' @param file_in feather file containing the data downloaded from the WQP, 
#' where each row represents a data record. 
#' @param file_out character string representing the feather file path to
#' save the cleaned data 
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values. By 
#' default, the column "ResultCommentText" will be searched for the following 
#' strings: "analysis lost", "not analyzed", "not recorded", "not collected", 
#' and "no measurement taken", but other values may be added by passing in a new
#' vector with all values to be treated as missing.  
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`. By 
#' default, a record will be considered duplicated if it shares the same 
#' organization, site id, date, time, characteristic name, and sample fraction. 
#' However, these options can be customized by passing a vector of column names 
#' to the argument `duplicate_definition`.
#' @param remove_duplicated_rows logical; should duplicated records be omitted
#' from the cleaned dataset? Defaults to TRUE. 
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a unique data record.
#' 
clean_wqp_data <- function(file_in, file_out, 
                           commenttext_missing = c('analysis lost', 'not analyzed', 
                                                   'not recorded', 'not collected', 
                                                   'no measurement taken'),
                           duplicate_definition = c('OrganizationIdentifier',
                                                    'MonitoringLocationIdentifier',
                                                    'ActivityStartDate', 
                                                    'ActivityStartTime.Time',
                                                    'CharacteristicName', 
                                                    'ResultSampleFractionText'),
                           remove_duplicated_rows = TRUE){

  # Clean data and assign flags if applicable
  wqp_data <- read_feather(file_in)
  wqp_data_clean <- wqp_data %>%
    # flag true missing results
    flag_missing_results(., commenttext_missing) %>%
    # flag duplicate records
    flag_duplicates(., duplicate_definition) %>%
    {if(remove_duplicated_rows){
      remove_duplicates(., duplicate_definition)
    } else {.}
    }
  
  # Inform the user what we found for duplicated rows
  if(remove_duplicated_rows){
    message(sprintf(paste0("Removed %s duplicated records."), 
                    nrow(wqp_data) - nrow(wqp_data_clean)))
  }
  
  write_feather(wqp_data_clean, file_out)
  return(file_out)
  
}


#' @title Flag missing results
#' 
#' @description 
#' Function to flag true missing results, i.e. when the result measure value 
#' and detection limit value are both NA, when "not reported" is found in the
#' column "ResultDetectionConditionText", or when any of the strings from
#' `commenttext_missing` are found in the column "ResultCommentText".
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record. Must contain the columns
#' "DetectionQuantitationLimitMeasure.MeasureValue", "ResultMeasureValue", 
#' "ResultDetectionConditionText", and "ResultCommentText".
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values.
#' 
#' @returns
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a data record. New columns appended to the original
#' data frame include flags for missing results. 
#' 
flag_missing_results <- function(wqp_data, commenttext_missing){
  
  wqp_data_out <- wqp_data %>%
    mutate(flag_missing_result = 
             ( is.na(ResultMeasureValue) & is.na(DetectionQuantitationLimitMeasure.MeasureValue) ) |
             grepl("not reported", ResultDetectionConditionText, ignore.case = TRUE) |
             grepl(paste(commenttext_missing, collapse = "|"), ResultCommentText, ignore.case = TRUE)
    )
  
  return(wqp_data_out)
  
}



#' @title Flag duplicated records
#' 
#' @description 
#' Function to flag duplicated rows based on a user-supplied definition
#' of a duplicate record. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`.
#'
#' @returns 
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a data record. New columns appended to the original
#' data frame include flags for duplicated records. 
#' 
flag_duplicates <- function(wqp_data, duplicate_definition){
  
  # Flag duplicate records using the `duplicate_definition`
  wqp_data_out <- wqp_data %>%
    group_by(across(all_of(duplicate_definition))) %>% 
    # arrange all rows to maintain consistency in row order across users/machines
    arrange(across(c(all_of(duplicate_definition), everything()))) %>%
    mutate(n_duplicated = n(),
           flag_duplicated_row = n_duplicated > 1) %>% 
    ungroup() %>%
    select(-n_duplicated)
  
  return(wqp_data_out)
  
}


#' @title Remove duplicated records
#' 
#' @description
#' Function to append additional flags to sets of duplicate rows that are then 
#' used to drop duplicates from the dataset. Currently, we randomly retain the 
#' first record in a set of duplicated rows and drop all others.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`.
#' 
#' @returns 
#' Returns a data frame containing data downloaded from the Water Portal in which
#' duplicated rows have been removed. 
#' 
remove_duplicates <- function(wqp_data, duplicate_definition){

  wqp_data_out <- wqp_data %>%
    group_by(across(all_of(duplicate_definition))) %>% 
    # arrange all rows to maintain consistency in row order across users/machines;
    # the rows should be ordered the same way across machines so that when we 
    # "randomly" select the first duplicated row below, the output is consistent
    # for all users.
    arrange(across(c(all_of(duplicate_definition), everything()))) %>%
    # To help resolve duplicates, randomly select the first record
    # from each duplicated set and flag all others for exclusion.
    mutate(n_duplicated = n(),
           dup_number = seq(n_duplicated),
           flag_duplicate_drop_random = n_duplicated > 1 & dup_number != 1) %>%
    filter(flag_duplicate_drop_random == FALSE) %>%
    ungroup() %>%
    select(-c(n_duplicated, dup_number, flag_duplicate_drop_random))
  
  return(wqp_data_out)
  
}

#' @title Pull out useful columns
#' 
#' @description Select only the final columns we want to use in the 
#' harmonized output and re-order as appropriate.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' 
#' @returns 
#' Returns a data frame containing data downloaded from the Water Portal in which
#' only the desired columns have been retained. 
#' 
harmonize_output_columns <- function(wqp_data) {
  wqp_data %>% 
    select(
      # Basic sample info (who, what, when)
      ProviderName,
      OrganizationIdentifier,
      OrganizationFormalName,
      MonitoringLocationIdentifier,
      ADDED_GenericParameter = parameter, 
      CharacteristicName,
      USGSPCode,
      ActivityStartDate,
      ActivityStartTime.Time,
      ActivityStartDateTime,
      ActivityStartTime.TimeZoneCode,
      # Updated result columns
      ResultMeasureValue,
      ResultMeasure.MeasureUnitCode,
      DetectionQuantitationLimitMeasure.MeasureValue,
      DetectionQuantitationLimitMeasure.MeasureUnitCode,
      ADDED_flag_missing_result = flag_missing_result,
      ADDED_flag_duplicated_row = flag_duplicated_row,
      # Original result columns
      ResultMeasureValue_original,
      ResultMeasure.MeasureUnitCode_original,
      DetectionQuantitationLimitMeasure.MeasureValue_original, # This is just the non-numeric version
      # Detailed sample location/condition info
      ActivityDepthHeightMeasure.MeasureValue,
      ActivityDepthHeightMeasure.MeasureUnitCode,
      ActivityDepthAltitudeReferencePointText,
      ActivityTopDepthHeightMeasure.MeasureValue,
      ActivityTopDepthHeightMeasure.MeasureUnitCode,
      ActivityBottomDepthHeightMeasure.MeasureValue,
      ActivityBottomDepthHeightMeasure.MeasureUnitCode,
      ActivityConductingOrganizationText,
      ActivityIdentifier,
      ProjectIdentifier,
      ActivityTypeCode,
      ActivityCommentText,
      ActivityMediaName,
      ActivityMediaSubdivisionName,
      ResultDepthHeightMeasure.MeasureValue,
      ResultDepthHeightMeasure.MeasureUnitCode,
      ResultDepthAltitudeReferencePointText,
      SampleAquifer,
      HydrologicCondition,
      HydrologicEvent,
      # Detailed sample "how" info
      SampleCollectionMethod.MethodIdentifier,
      SampleCollectionMethod.MethodIdentifierContext,
      SampleCollectionMethod.MethodName,
      SampleCollectionEquipmentName,
      StatisticalBaseCode,
      ResultAnalyticalMethod.MethodIdentifier,
      ResultAnalyticalMethod.MethodIdentifierContext,
      ResultAnalyticalMethod.MethodName,
      MethodDescriptionText,
      LaboratoryName,
      AnalysisStartDate,
      ResultLaboratoryCommentText,
      PreparationStartDate,
      # Detailed sample result context
      ResultDetectionConditionText,
      DetectionQuantitationLimitTypeName,
      ResultSampleFractionText,
      MeasureQualifierCode,
      ResultStatusIdentifier,
      ResultValueTypeName,
      ResultWeightBasisText,
      ResultTimeBasisText,
      ResultTemperatureBasisText,
      ResultParticleSizeBasisText,
      PrecisionValue,
      ResultCommentText
    )
  # Things that were removed purposefully:
  #   ActivityEndDateTime
  #   ActivityEndDate
  #   ActivityEndTime.Time
  #   ActivityEndTime.TimeZoneCode
  #   SubjectTaxonomicName
  #   SampleTissueAnatomyName
}
