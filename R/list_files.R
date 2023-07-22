get_list_line_may <- function() {

  list_line_may <- list.files("data/mayotte", recursive = TRUE, pattern = "line.xls", full.names = TRUE)
 
  return(list_line_may)
   
}

get_list_belt_may <- function() {
  
  list_belt_may <- list.files("data/mayotte", recursive = TRUE, pattern = "belt.xls", full.names = TRUE)
  
  return(list_belt_may)
  
}

get_list_line_run <- function() {
  
  list_line_run <- list.files("data/reunion", recursive = TRUE, pattern = "line.xls", full.names = TRUE)
  
  return(list_line_run)
  
}

get_list_belt_run <- function() {
  
  list_belt_run <- list.files("data/reunion", recursive = TRUE, pattern = "belt.xls", full.names = TRUE)
  
  return(list_belt_run)
  
}