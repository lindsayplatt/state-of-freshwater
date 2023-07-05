# Source the functions that will be used to build the targets in p3_targets_list
source("3_harmonize/src/format_columns.R")
source("3_harmonize/src/add_cleaning_groups.R")
source("3_harmonize/src/clean_wqp_data.R")
source("3_harmonize/src/clean_calcium_data.R")
source("3_harmonize/src/clean_chloride_data.R")
source("3_harmonize/src/clean_conductivity_data.R")
source("3_harmonize/src/summarize_wqp_records.R")

p3_targets_list <- list(
  
  # All columns in p2_wqp_data_aoi are of class character. Coerce select 
  # columns back to numeric, but first retain original entries in new columns
  # ending in "_original". The default option is to format "ResultMeasureValue"
  # and "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but 
  # additional variables can be added using the `vars_to_numeric` argument in 
  # format_columns(). By default, format_columns() will retain all columns, but
  # undesired variables can also be dropped from the WQP dataset using the 
  # optional `drop_vars` argument. 
  tar_target(
    p3_wqp_data_aoi_formatted,
    format_columns(p2_wqp_data_aoi)
  ),
  
  # For this large-scale pull, one big data set is proving to be quite slow
  # and we should save intermediate files. Saving intermediate files based on 
  # the characteristic group and 1 million record max.
  tar_target(
    p3_wqp_data_aoi_formatted_cleaning_grps,
    p3_wqp_data_aoi_formatted %>%
      add_cleaning_groups() %>%
      group_by(cleaning_grp) %>%
      tar_group(),
    iteration = "group"
  ),
  tar_target(
    p3_wqp_data_aoi_formatted_cleaning_grps_feather, {
      data_in <- p3_wqp_data_aoi_formatted_cleaning_grps
      file_out <- sprintf('3_harmonize/tmp/formatted_data_%s.feather', unique(data_in$cleaning_grp))
      write_feather(data_in, file_out)
      return(file_out)
    },
    pattern = map(p3_wqp_data_aoi_formatted_cleaning_grps),
    format = "file"
  ),
  
  # Harmonize WQP data by uniting diverse characteristic names under more
  # commonly-used water quality parameter names, flagging missing records,
  # and flagging duplicate records. Duplicated rows are identified using 
  # the argument `duplicate_definition`. By default, a record will be 
  # considered duplicated if it shares the same organization, site id, date,
  # time, characteristic name and sample fraction, although a different 
  # vector of column names can be passed to `clean_wqp_data()` below. By 
  # default, duplicated rows are flagged and omitted from the dataset. To 
  # retain duplicate rows, set the argument `remove_duplicated_rows` to FALSE. 
  tar_target(
    p3_wqp_data_aoi_clean_feather, {
      file_in <- p3_wqp_data_aoi_formatted_cleaning_grps_feather
      file_out <- gsub("formatted_", "basic_cleaned_", file_in)
      clean_wqp_data(file_in, file_out)
    },
    pattern = map(p3_wqp_data_aoi_formatted_cleaning_grps_feather),
    format = "file"
  ),
  
  # Create a table that defines parameter-specific data cleaning functions.
  # Cleaning functions should be defined within a named list where the name
  # of each list element is the function name.
  tar_target(
    p3_wqp_param_cleaning_info,
    tibble(
      parameter = c('calcium', 'chloride', 'conductivity'),
      cleaning_fxn = c(clean_calcium_data, clean_chloride_data, clean_conductivity_data))
  ),
  
  # Harmonize WQP data by applying parameter-specific data cleaning steps,
  # including harmonizing units where possible. `p3_wqp_param_cleaning_info` 
  # is a {targets} dependency, so changes to any of the parameter-specific 
  # cleaning functions will trigger a rebuild of only those branches that 
  # correspond to the group of data impacted by the change.
  tar_target(
    p3_wqp_data_aoi_clean_param,
    {
      data_in <- read_feather(p3_wqp_data_aoi_clean_feather) %>% 
        left_join(y = p1_char_names_crosswalk, by = c("CharacteristicName" = "char_name"))
      
      # Decide which function to use
      fxn_to_use <- p3_wqp_param_cleaning_info %>%
        filter(parameter == unique(data_in$parameter)) %>%
        pull(cleaning_fxn) %>%
        {.[[1]]}
      
      # If applicable, apply parameter-specific cleaning function
      if(length(fxn_to_use) > 0){
        wqp_clean <- do.call(fxn_to_use, list(wqp_data = data_in))
      } else {
        wqp_clean <- data_in
      }
      
      file_out <- gsub('basic_cleaned_', 
                       sprintf('clean_%s_', unique(data_in$parameter)),
                       p3_wqp_data_aoi_clean_feather)
      write_feather(wqp_clean, file_out)
      return(file_out)
    },
    map(p3_wqp_data_aoi_clean_feather),
    format = "file"
  ),
  
  # Save the cleaned data out to a smaller number of files by grouping into 
  # a single file per parameter
  tar_target(
    p3_wqp_clean_file_info, 
    tibble(file_nm = p3_wqp_data_aoi_clean_param,
           file_hash = tools::md5sum(p3_wqp_data_aoi_clean_param)) %>% 
      mutate(parameter = gsub('clean_|_data_[0-9]*.feather', '', basename(file_nm))) %>% 
      group_by(parameter) %>% 
      tar_group(),
    iteration = "group"),
  tar_target(
    p3_wqp_data_aoi_clean_param_feather, {
      file_out <- sprintf('3_harmonize/out/wqp_clean_%s.feather',
                          unique(p3_wqp_clean_file_info$parameter))
      purrr::map(p3_wqp_clean_file_info$file_nm, read_feather) %>% 
        bind_rows() %>% 
        write_feather(file_out)
      return(file_out)
    },
    pattern = map(p3_wqp_clean_file_info),
    format = "file"
  ),
  
  # Summarize the number of records associated with each parameter,
  # characteristic name, and harmonized units. The harmonized dataset
  # can be summarized using any combination of columns by passing a
  # different vector of column names in `grouping_cols`.
  tar_target(
    p3_wqp_records_summary,
    read_feather(p3_wqp_data_aoi_clean_param_feather) %>% 
      summarize_wqp_records(grouping_cols = c(
        'parameter', 
        'CharacteristicName',
        'ResultMeasure.MeasureUnitCode')),
    pattern = map(p3_wqp_data_aoi_clean_param_feather)
  ),
  tar_target(
    p3_wqp_records_summary_csv, {
      file_out <- "3_harmonize/log/wqp_records_summary.csv"
      readr::write_csv(x = p3_wqp_records_summary, file = file_out) 
      return(file_out)
    },
    format = "file"
  )

)

