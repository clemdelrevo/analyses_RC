# --- LIST FILES --------------------------------------------------------------- 

# get the character string vector of data files

get_list_pit_may <- function() {

  list_pit_may <- list.files("data/mayotte/pit", recursive = TRUE, full.names = TRUE)
 
  return(list_pit_may)
   
}

get_list_belt_may <- function() {
  
  list_belt_may <- list.files("data/mayotte/belt", recursive = TRUE, full.names = TRUE)
  
  return(list_belt_may)
  
}

get_list_pit_run <- function() {
  
  list_pit_run <- list.files("data/reunion/pit", recursive = TRUE, full.names = TRUE)
  
  return(list_pit_run)
  
}

get_list_belt_run <- function() {
  
  list_belt_run <- list.files("data/reunion/belt", recursive = TRUE, full.names = TRUE)
  
  return(list_belt_run)
  
}