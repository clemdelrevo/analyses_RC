wrangle_mayotte_shp <- function(mayotte_shp) {
  
  #targets::tar_load(mayotte_shp)
  
  mayotte_bd <- sf::read_sf(mayotte_shp)
  mayotte_bd <- sf::st_transform(mayotte_bd, 4326)
  mayotte_bd <- sf::st_make_valid(mayotte_bd)
  
  return(mayotte_bd)
  
}
  
wrangle_may_reef <- function(millenium_reef_shp) {
  
  #targets::tar_load(millenium_reef_shp)
  
  millenium_reef <- sf::read_sf(millenium_reef_shp)
  millenium_reef <- sf::st_make_valid(millenium_reef)
  
  may_bbox <- sf::st_bbox(c(xmin = 44.850, xmax = 45.350, ymin = -13.1, ymax = -12.5))
  may_reef <- sf::st_crop(millenium_reef, may_bbox)
  may_reef <- sf::st_make_valid(may_reef)
  
  return(may_reef)
  
}

get_data_fish <- function(data_belt, reef_type_may) {
  
  #targets::tar_load(data_belt)
  
  data_fish <-  data_belt$data_fish
  data_fish$reef_type <- NA
  data_fish$reef_type[data_fish$site %in% reef_type_may$fringing_may] <- "fringing"
  data_fish$reef_type[data_fish$site %in% reef_type_may$barrier_may]  <- "barrier"
  data_fish$reef_type[data_fish$site %in% reef_type_may$intern_may]   <- "intern"
  
  return(data_fish)
  
}


get_data_invert <- function(data_belt, reef_type_may) {
  
  #targets::tar_load(data_belt)
  
  data_invert <-  data_belt$data_invert
  data_invert$reef_type <- NA
  data_invert$reef_type[data_invert$site %in% reef_type_may$fringing_may] <- "fringing"
  data_invert$reef_type[data_invert$site %in% reef_type_may$barrier_may]  <- "barrier"
  data_invert$reef_type[data_invert$site %in% reef_type_may$intern_may]   <- "intern"
  
  return(data_invert)
  
}
  