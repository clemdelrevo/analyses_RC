
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

#' Reunion reef type -----------------------------------------------------------
#'
#' @description
#' Assigns a reef type  to the Reunion sites
#' 
#' @return a list
#' 
#' @export

get_reef_type_run <- function() {
  
  slope_run <- c("tessier.pe", "ermitage.passe.pe", "ermitage.centre.pe", "brisants.pe", "spot.pe", "cap.la.houssaye.pe", "trou.d.eau.pe",
                 "livingstone.pe", "plage.mns.pe", "roches.noires.pe", "boucan.maharani.pe", "banc.dore.pe",
                 "bleu.marine.pe", "coulee.1977.pe", "grand.bois.pe", "jardin.de.corail.pe", "la.corne.etimareco.pe",
                 "le.bowl.pe", "petit.tombant.pe", "ravine.des.poux.etimareco.pe", "roche.caesari.pe", "spot.perroquet.pe",
                 "trou.d.eau.etimareco.pe", "vallee.des.schtroumpfs.pe", "billy.pe")
  
  flat_run  <- c("tessier.pl", "ermitage.centre.pl", "roches.noires.pl", "trou.d.eau.pl", "bassin.pirogue.pl", "plage.mns.pl",
                 "boucan.maharani.pl", "ermitage.passe.pl", "livingstone.pl", "la.digue.pl", "beach.soccer.pl", "billy.pl",
                 "bleaching.pl", "la.bobine.pl", "grand.fond.pl", "la.closerie.pl", "la.corne.pl", "la.saline.nord.pl",
                 "planchalize.pl", "ravine.blanche.pl", "ravine.des.poux.pl", "trou.d.eau.etimareco.pl", "varangue.pl", "ermitage.ame.pl",
                 "gendarmerie.kiosque.pl", "livingstone.sud.pl") 

  return(list(slope_run = slope_run, flat_run = flat_run))
  
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

add_reef_type_run <- function(data, flat, slope) {
  
  data$reef_type <- NA
  
  data$reef_type[data$site %in% flat]  <- "flat"
  data$reef_type[data$site %in% slope] <- "slope"
  
  return(data)
  
}
