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
  wqp_data_out <- wqp_data
  # TODO: come up with chloride cleaning steps
  
  return(wqp_data_out)
  
}
