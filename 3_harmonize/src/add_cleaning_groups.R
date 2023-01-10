
#' @title Group sites for cleaning data without hitting a memory cap
#' 
#' @description 
#' Function to group downloaded data into reasonably sized chunks for
#' cleaning (specifically, for removing duplicates). The code here is 
#' borrowed almost entirely from the fxn `add_download_groups()` used 
#' during the download phase.
#' 
#' @param wqp_data data frame containing all of the downloaded data. Must 
#' contain columns `CharacteristicName` and `MonitoringLocationIdentifier`.
#' @param max_results integer indicating the maximum number of records allowed
#' in each download group. Defaults to 1,000,000.
#' 
#' @returns 
#' Returns the original data passed in as `wqp_data` and an additional column 
#' called `cleaning_grp` which is made up of unique groups that enable use of 
#' `group_by()` and then `tar_group()` for cleaning.
#' 

add_cleaning_groups <- function(wqp_data, max_results = 10^6) {
  
  site_counts <- wqp_data %>%
    select(CharacteristicName, MonitoringLocationIdentifier) %>% 
    group_by(CharacteristicName, MonitoringLocationIdentifier) %>%
    summarize(results_count = n(), .groups = "keep") %>% 
    arrange(desc(results_count), .by_group = TRUE) %>% 
    ungroup()
  
  sites_grouped <- site_counts %>%
    group_by(CharacteristicName) %>% 
    mutate(task_num_by_results = MESS::cumsumbinning(x = results_count, 
                                                     threshold = max_results)) %>%
    ungroup() %>% 
    # Each group from before (which represents a different characteristic 
    # name) will have task numbers that start with "1", so now we create 
    # a new column called `cleaning_grp` to create unique task numbers across
    # all characteristic names. For example, both "Specific conductance" and 
    # "Temperature" may have values for `task_num_by_results` of 1 and 2 but 
    # because they represent different characteristics that they have unique
    # values for `task_num` equaling 1, 2, 3, and 4.
    group_by(CharacteristicName, task_num_by_results) %>% 
    mutate(cleaning_grp = cur_group_id()) %>% 
    ungroup() %>% 
    arrange(cleaning_grp) %>%
    select(CharacteristicName, MonitoringLocationIdentifier, cleaning_grp) 
  
  # Add the `cleaning_grp` column to the actual data and return
  wqp_data %>% 
    left_join(sites_grouped, by = c('CharacteristicName', 'MonitoringLocationIdentifier'))
}
