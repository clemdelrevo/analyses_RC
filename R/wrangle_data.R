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
  