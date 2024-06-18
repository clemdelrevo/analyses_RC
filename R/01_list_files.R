# --- LIST FILES --------------------------------------------------------------- 

# get the character string vector of data files

get_list <- function(path) {

  data_list <- list.files(path, recursive = TRUE, full.names = TRUE)
 
  return(data_list)
   
}


