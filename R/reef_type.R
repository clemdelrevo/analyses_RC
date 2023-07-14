get_reef_type_mayotte <- function() {
  
  fringing_may <- c("bandrele", "mbouzi", "sakouli", "saziley", "tzoundzou", "ngouja", "tanaraki",
                    "mtsangamouji", "dzoumogne", "majikavo")
  barrier_may  <- c("pes.deux", "pes.onze", "boueni")
  
  intern_may   <- c("boa", "longoni")
  
  return(list(fringing_may = fringing_may, barrier_may = barrier_may, intern_may = intern_may))
  
}