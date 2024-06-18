
#' Mayotte reef type -----------------------------------------------------------
#'
#' @description
#' Assigns a reef type  to the Mayotte sites
#' 
#' @return a list 
#' 
#' @export

get_reef_type_mayotte <- function() {
  
  fringing_may <- c("bandrele", "mbouzi", "sakouli", "saziley", "tzoundzou", "ngouja", "tanaraki",
                    "mtsangamouji", "dzoumogne", "majikavo")
  barrier_may  <- c("pes.deux", "pes.onze", "boueni")
  
  intern_may   <- c("boa", "longoni")
  
  return(list(fringing_may = fringing_may, barrier_may = barrier_may, intern_may = intern_may))
  
}

# Create a reef_type column and inserts the reef type for each site ------------

# These functions are used in the 04_wrangle.R file 

add_reef_type_may <- function(data, fringing, barrier, intern) {
  
  data$reef_type <- NA
  
  data$reef_type[data$site %in% fringing] <- "fringing"
  data$reef_type[data$site %in% barrier]  <- "barrier"
  data$reef_type[data$site %in% intern]   <- "intern"
  
  return(data)
  
}
