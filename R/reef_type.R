get_reef_type_mayotte <- function() {
  
  fringing_may <- c("bandrele", "mbouzi", "sakouli", "saziley", "tzoundzou", "ngouja", "tanaraki",
                    "mtsangamouji", "dzoumogne", "majikavo")
  barrier_may  <- c("pes.deux", "pes.onze", "boueni")
  
  intern_may   <- c("boa", "longoni")
  
  return(list(fringing_may = fringing_may, barrier_may = barrier_may, intern_may = intern_may))
  
}

get_reef_type_run <- function() {
  
  slope_run <- c("tessier.pe")
  
  flat_run  <- NULL
  
  return(list(slope_run = slope_run, flat_run = flat_run))
  
}

add_reef_type_may <- function(data, fringing, barrier, intern) {
  
  data$reef_type <- NA
  
  data$reef_type[data$site %in% fringing] <- "fringing"
  data$reef_type[data$site %in% barrier]  <- "barrier"
  data$reef_type[data$site %in% intern]   <- "intern"
  
  return(data)
  
}

add_reef_type_run <- function(data, flat, slope) {
  
  data$reef_type <- NA
  
  data$reef_type[data$site %in% flat]  <- "flat"
  data$reef_type[data$site %in% slope] <- "slope"
  
  return(data)
  
}
