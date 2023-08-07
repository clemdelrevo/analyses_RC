# Calculate the evolution of coral cover in each station -----------------------

get_cc_evol <- function(line_region, coord_site, fringing, barrier, intern, slope, flat) {
  
  cc_site <- line_region |>
    dplyr::group_by(annee, site) |>
    dplyr::summarise(mean_coral_cover = mean(HC),
                     sd_coral_cover = sd(HC))
  
  cc_site <- merge(cc_site, coord_site, by = "site")
  
  cc_evol <- data.frame(do.call(rbind, lapply(levels(as.factor(cc_site$site)), function(site) {
    
    #site <- "majikavo"
    message(site)
    sub_site <- cc_site[cc_site$site == site, ]
    
    if(nrow(sub_site) == 1) {
      
      diff_cc = NA
      etat    = NA
      
    }else{
      
      present_year_survey <- max(sub_site$annee)
      last_year_survey    <- max(sub_site$annee[sub_site$annee < present_year_survey])
    
      diff_cc <- sub_site$mean_coral_cover[sub_site$annee == present_year_survey] - sub_site$mean_coral_cover[sub_site$annee == last_year_survey]
  
      if(diff_cc > 5) { 
        
        etat <- "improvement"
        
      } else if (diff_cc < -5) {
          
        etat <- "degradation"
        
      } else {etat <- "stable"}
    
    }
    
    data.frame(site = site, diff_cc = diff_cc, etat = etat, x = unique(sub_site$x), y = unique(sub_site$y))
  
  })))
  
  cc_evol <- sf::st_as_sf(cc_evol, coords = c("x", "y"))
  sf::st_crs(cc_evol) <- 4326
  
  cc_evol$reef_type <- NA
  cc_evol$reef_type[cc_evol$site %in% fringing] <- "fringing"
  cc_evol$reef_type[cc_evol$site %in% barrier]  <- "barrier"
  cc_evol$reef_type[cc_evol$site %in% intern]   <- "intern"
  cc_evol$reef_type[cc_evol$site %in% slope]    <- "slope"
  cc_evol$reef_type[cc_evol$site %in% flat]     <- "flat"

  
  cc_evol$etat <- cc_evol$etat |>
    forcats::fct_relevel("improvement", "stable", "degradation")
  
  return(cc_evol)
  
}

# Plot a donut of the percentage of coral cover evolution ----------------------

get_dot_cc_evol<- function(cc_evol) {
  
  cc_devel <- table(cc_evol$etat)
  cc_devel <- data.frame(cc_devel)
  colnames(cc_devel) <- c("etat", "freq")
  cc_devel$freq <- (cc_devel$freq * 100) / sum(cc_devel$freq)
  
  cc_devel$etat_fr <- NA
  cc_devel$etat_fr[cc_devel$etat == "improvement"] <- "amélioration"
  cc_devel$etat_fr[cc_devel$etat == "stable"] <- "stable"
  cc_devel$etat_fr[cc_devel$etat == "degradation"] <- "dégradation"
  labs <- paste0(cc_devel$etat_fr, "\n (", round(cc_devel$freq), "%)")
  cc_devel$label <- labs
  cc_devel$ymax  <- cumsum(cc_devel$freq)
  cc_devel$ymin  <- c(0, head(cc_devel$ymax, n = -1))
  cc_devel$label_pos <- (cc_devel$ymax + cc_devel$ymin) / 2
  
  ggplot2::ggplot(cc_devel, ggplot2::aes(xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = etat))+
    ggplot2::geom_rect()+
    ggplot2::coord_polar(theta = "y")+
    ggplot2::theme_void()+
    ggplot2::xlim(c(-.5, 4.5))+
    ggplot2::geom_label(x = 3.5, ggplot2::aes(y = label_pos, label = label), size = 14, label.size = 0.5, nudge_x = 1)+
    ggplot2::scale_fill_manual(values = c("#05A9D1", "#05D13A", "#F8F804"))+
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(0, 0, -4, -2, "cm"))
  
}

# Map the evolution of coral cover in area region ------------------------------

get_map_cc_evol <- function(map_land, map_reef, cc_evol, color, labels, shape) {
  
  cc_evol <- na.omit(cc_evol)
  
  ggplot2::ggplot()+
    ggplot2::theme_classic()+
    ggplot2::geom_sf(data = map_reef, fill = "#FFCC66", color = "#FFCC66")+
    ggplot2::geom_sf(data = map_land, fill = "#bdb7aa")+
    ggplot2::geom_sf(data = cc_evol, ggplot2::aes(fill = reef_type, shape = etat), color = "#000000", size = 3)+
    ggplot2::scale_shape_manual(values = c(24, 21, 25), labels = c("amélioration", "stable", "dégradation"))+
    ggplot2::scale_fill_manual(values = color, labels = labels,
                               guide = ggplot2::guide_legend(override.aes = list(shape = shape, color = color)))+
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = c("#D1CECE", "#D1CECE", "#D1CECE"), colour = "#000000")))+
    ggplot2::labs(fill = "Complexe récifal", shape = "Modification du taux de \nrecouvrement depuis le dernier suivi")+
    ggplot2::xlab("")+
    ggplot2::ylab("")+
    ggspatial::annotation_north_arrow(location = "tr", height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.7, "cm"))+
    ggspatial::annotation_scale()+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#EBF5FB"),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size = 9, face = "bold"),
                   legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
                   legend.position = c(1.25, 0.2),
                   legend.margin = ggplot2::margin(0, 0, 5, 0, "cm"),
                   plot.margin = ggplot2::margin(0, -12, 0, -20, "cm"))
  
}

# --- MAYOTTE ------------------------------------------------------------------

get_cc_evol_may <- function(line_may, reef_type_may, coord_site_may, mayotte_bd, may_reef) {
  
  #targets::tar_load(line_may)
  #targets::tar_load(reef_type_may)
  #targets::tar_load(coord_site_may)
  #targets::tar_load(mayotte_bd)
  #targets::tar_load(may_reef)
  
  cc_evol_may <- get_cc_evol(line_region = line_may, coord_site = coord_site_may,
                             fringing = reef_type_may$fringing_may,
                             barrier = reef_type_may$barrier_may,
                             intern = reef_type_may$intern_may,
                             slope = NULL,
                             flat = NULL)
  
  dot <- get_dot_cc_evol(cc_evol = cc_evol_may)
  
  map <- get_map_cc_evol(map_land = mayotte_bd, map_reef = may_reef, cc_evol = cc_evol_may,
                         color = c("#0066CC", "#336666", "#66CCFF"), labels = c("barrière", "frangeant", "interne"),
                         shape = c(15, 15, 15))
  
  return(list(may_dot = dot, may_map = map))
  
}

# --- RÉUNION ------------------------------------------------------------------

get_cc_evol_run <- function(line_run, reef_type_run, coord_site_run, reunion_bd, run_reef) {
  
  #targets::tar_load(line_run)
  #targets::tar_load(reef_type_run)
  #targets::tar_load(coord_site_run)
  #targets::tar_load(reunion_bd)
  #targets::tar_load(run_reef)
  
  cc_evol_run <- get_cc_evol(line_region = line_run, coord_site = coord_site_run,
                             fringing = NULL,
                             barrier = NULL,
                             intern = NULL,
                             slope = reef_type_run$slope_run,
                             flat = NULL)
  
  dot <- get_dot_cc_evol(cc_evol = cc_evol_run)
  
  map <- get_map_cc_evol(map_land = reunion_bd, map_reef = run_reef, cc_evol = cc_evol_run,
                         color = c("#0066CC", "#66CCFF"), labels = c("pente externe", "platier récifal"),
                         shape = c(15, 15))
  
  return(list(run_dot = dot, run_map = map))
  
}