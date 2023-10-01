wrangle_mayotte_shp <- function(mayotte_shp) {
  
  #targets::tar_load(mayotte_shp)
  
  mayotte_bd <- sf::read_sf(mayotte_shp)
  mayotte_bd <- sf::st_transform(mayotte_bd, 4326)
  mayotte_bd <- sf::st_make_valid(mayotte_bd)
  
  return(mayotte_bd)
  
}

wrangle_reunion_reef_gpkg <- function(reunion_reef_gpkg) {
  
  #targets::tar_load(reunion_reef_gpkg)
  
  reunion_reef <- sf::read_sf(reunion_reef_gpkg)
  reunion_reef <- sf::st_make_valid(reunion_reef)
  reunion_reef <- sf::st_as_sf(sf::st_union(reunion_reef))
  reunion_reef <- sf::st_transform(reunion_reef, 4326)
  
  return(reunion_reef)
  
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

wrangle_run_reef <- function(millenium_reef_shp) {
  
  #targets::tar_load(millenium_reef_shp)
  
  millenium_reef <- sf::read_sf(millenium_reef_shp)
  millenium_reef <- sf::st_make_valid(millenium_reef)
  
  run_bbox <- sf::st_bbox(c(xmin = 55.140, xmax = 55.900, ymin = -21.420, ymax = -20.820))
  run_reef <- sf::st_crop(millenium_reef, run_bbox)
  run_reef <- sf::st_make_valid(run_reef)
  
  return(run_reef)
  
}

wrangle_carmayotte_shp <- function(carmayotte_shp) {
  
  #targets::tar_load(carmayotte_shp)
  
  carmayotte <- sf::st_read(carmayotte_shp)
  carmayotte <- sf::st_make_valid(carmayotte)
  
  carmayotte$Geo_N3_mil[carmayotte$Geo_N3_mil == "Recif frangeant d ilots"] <- "Recif frangeant"
  carmayotte$Geo_N3_mil[carmayotte$Geo_N3_mil == "Recif barriere ennoyee"]  <- "Recif barriere"
  carmayotte$Geo_N3_mil[carmayotte$Geo_N3_mil == "Recif barriere immergee"] <- "Recif barriere"
  carmayotte$Geo_N3_mil[carmayotte$Geo_N3_mil == "Lagon ennoye"] <- "Lagon"
  
  carmayotte <-  carmayotte[carmayotte$Geo_N3_mil != "Terre emmergee", ]
  carmayotte <-  carmayotte[carmayotte$Geo_N3_mil != "Passe", ]
  carmayotte <-  carmayotte[carmayotte$Geo_N3_mil != "Fond de baie", ]
  
  carmayotte <- carmayotte[carmayotte$surface > 5000, ]
  carmayotte <- sf::st_transform(carmayotte, crs = sf::st_crs(4326))
  
  carto_union <- setNames(lapply(unique(carmayotte$Geo_N3_mil), function(geo_type) {
    
    #geo_type = "Recif barriere" 
    message(geo_type)
    geo_union <- sf::st_union(carmayotte[carmayotte$Geo_N3_mil == geo_type, ])
    geo_union <- sf::st_make_valid(geo_union)
    
    return(geo_union)

  }), unique(carmayotte$Geo_N3_mil))

  carto_n3 <- data.frame(geometry = do.call(rbind, carto_union))
  carto_n3$geo_n3 <- rownames(carto_n3)
  rownames(carto_n3) <- 1:nrow(carto_n3)
  carto_n3 <- sf::st_as_sf(carto_n3)
  sf::st_crs(carto_n3) <- 4326

  ggplot2::ggplot()+
    ggplot2::geom_sf(data = carto_n3, ggplot2::aes(color = geo_n3, fill = geo_n3))+
    ggplot2::scale_fill_manual(values = c("#FFFF99", "#edf8ff", "#024f14", "#FF0000", "#064d7d", "#50a5de"))+
    ggplot2::scale_color_manual(values = c("#FFFF99", "#edf8ff", "#024f14", "#FF0000", "#064d7d", "#50a5de"))+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank())
                                            
  
}

wrangle_bati_run_shp <- function(bati_run_shp) {
  
  #targets::tar_load(bati_run_shp)
  bati_run <- sf::st_read(bati_run_shp)
  bati_run <- sf::st_as_sf(sf::st_make_valid(sf::st_union(bati_run)))
  bati_run <- sf::st_transform(bati_run, 4326)
  
  return(bati_run)
  
}

# --- MAYOTTE ------------------------------------------------------------------

get_data_line_may <- function(data_line_may, reef_type_may) {
  
  #targets::tar_load(data_line_may)
  #targets::tar_load(reef_type_may)
  
  add_reef_type_may(data = data_line_may,
                   fringing = reef_type_may$fringing_may,
                   barrier = reef_type_may$barrier_may,
                   intern = reef_type_may$intern_may)
  
}

get_data_fish_may <- function(data_fish_may, reef_type_may) {
  
  #targets::tar_load(data_fish_may)
  #targets::tar_load(reef_type_may)
  
 add_reef_type_may(data = data_fish_may,
                   fringing = reef_type_may$fringing_may,
                   barrier = reef_type_may$barrier_may,
                   intern = reef_type_may$intern_may)
  
}

get_data_invert_may <- function(data_invert_may, reef_type_may) {
  
  #targets::tar_load(data_invert_may)
  #targets::tar_load(reef_type_may)
  
  add_reef_type_may(data = data_invert_may,
                    fringing = reef_type_may$fringing_may,
                    barrier = reef_type_may$barrier_may,
                    intern = reef_type_may$intern_may)
  
  
}

# --- RÉUNION ------------------------------------------------------------------

get_data_line_run <- function(data_line_run, reef_type_run) {
  
  #targets::tar_load(data_line_run)
  #targets::tar_load(reef_type_run
  
  add_reef_type_run(data = data_line_run,
                    slope = reef_type_run$slope_run,
                    flat = reef_type_run$flat_run)
  
}
  