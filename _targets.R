library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'lubridate', 'dataRetrieval', 
                            'sf', 'xml2', 'units', 'httr', 'MESS',
                            'arrow'),
               format = "qs")

source("1_inventory.R")
source("2_download.R")
source("3_harmonize.R")

# Define the temporal extent of our data pull
# set start_date or end_date to "" to query the earliest or latest available date
start_date <- "1900-01-01"
end_date <- "2022-12-31" 

# Define which parameter groups (and CharacteristicNames) to return from WQP. 
# Different options for parameter groups are represented in the first level of 
# 1_inventory/cfg/wqp_codes.yml. This yml file is meant to provide a starting 
# place for an analysis and does not represent a definitive list of characteristic 
# names. Which characteristic names to include for any given parameter group may 
# change depending on the user or application, so the yml file can be edited to 
# omit characteristic names or include others, to change top-level parameter names,
# or to customize parameter groupings. 
param_groups_select <- c('calcium', 'chloride', 'conductivity')

# Specify arguments to WQP queries
# see https://www.waterqualitydata.us/webservices_documentation for more information 
wqp_args <- list(sampleMedia = c("Water","water"),
                 siteType = c("Stream", "Lake, Reservoir, Impoundment"),
                 # return sites with at least one data record
                 minresults = 1, 
                 startDateLo = start_date,
                 startDateHi = end_date)

# Return the complete list of targets
c(p1_targets_list, p2_targets_list, p3_targets_list)
