
#' Statement evolution of coral cover ------------------------------------------------
#'
#' @description
#' This function calculate the evolution of coral cover in each station
#' 
#' @param pit_region 
#' @param coord_site 
#' @param fringing 
#' @param barrier 
#' @param intern 
#' @param slope 
#' @param flat 
#'
#' @return a sf object 
#' 
#' @export

get_cc_evol <- function(pit_region, coord_site, fringing, barrier, intern, slope, flat) {
  
  # calculate the mean coral cover on each site for each year
  cc_site <- pit_region |>
    dplyr::group_by(annee, site) |>
    dplyr::summarise(mean_coral_cover = (mean(HC) * 100) / 40,
                     st_error_cc = (plotrix::std.error(HC) * 100) / 40)
  
  # Merge the site coordinates created in 06_coord_site.R file
  cc_site <- merge(cc_site, coord_site, by = "site")
  
  cc_evol <- data.frame(do.call(rbind, lapply(levels(as.factor(cc_site$site)), function(site) {
    
    #site = "bandrele"
    message(site)
    sub_site <- cc_site[cc_site$site == site, ]
    
    # Cannot evaluate coral cover evolution in site with one year survey
    if(nrow(sub_site) == 1) {
      
      diff_cc = NA
      etat    = NA
      
    } else { 
      
      present_year_survey <- max(sub_site$annee)
      #last_year_survey   <- min(sub_site$annee)
      last_year_survey    <- max(sub_site$annee[sub_site$annee < present_year_survey])
    
      data_test <- pit_region[pit_region$annee %in% c(present_year_survey, last_year_survey) & pit_region$site == site, ]
      t <- wilcox.test(HC ~ annee, data = data_test)
      
      diff_cc <- sub_site$mean_coral_cover[sub_site$annee == present_year_survey] - sub_site$mean_coral_cover[sub_site$annee == last_year_survey]
  
      if(diff_cc > 0 & t$p.value < 0.05) { 
        
        etat <- "amélioration"
        
      } else if (diff_cc < 0 & t$p.value < 0.05) {
          
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
    forcats::fct_relevel("improvement", "stable")
  
  return(cc_evol)
  
}

#' coral cover donut -----------------------------------------------------------
#'
#' @description
#' Plot a donut of the percentage of coral cover evolution
#' 
#' @param cc_evol 
#'
#' @return a ggplot object
#' 
#' @export

get_dot_cc_evol<- function(cc_evol, color) {
  
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
  
  ggplot2::ggplot(
    data = cc_devel, 
    ggplot2::aes(
      xmin = 2, 
      xmax = 3, 
      ymin = ymin, 
      ymax = ymax, 
      fill = etat
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void() +
    ggplot2::xlim(c(-.5, 4.5)) +
    ggplot2::geom_label(
      x = 4, 
      ggplot2::aes(y = label_pos, label = label), 
      size = 9, 
      label.size = 0.5, 
      nudge_x = 1
    ) +
    ggplot2::scale_fill_manual(values = color)+
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(0, 0, 0, -2, "cm")
    )
  
}

#' Coral cover evolution mapping -----------------------------------------------
#'
#' @description
#' Map the evolution of coral cover in a specific region
#' 
#' @param map_land 
#' @param map_reef 
#' @param cc_evol 
#' @param bati 
#' @param color 
#' @param labels 
#' @param shape 
#'
#' @return a ggplot object
#' 
#' @export

get_map_cc_evol <- function(map_land, map_reef, cc_evol, labels, shape) {
  
  cc_evol <- na.omit(cc_evol)
  
  ggplot2::ggplot()+
    ggplot2::geom_sf(data = map_reef, fill = "#FFCC66", color = "#FFCC66") +
    ggplot2::geom_sf(data = map_land, fill = "#bdb7aa") +
    ggplot2::geom_sf(
      data = cc_evol, 
      ggplot2::aes(fill = reef_type, shape = etat), 
      color = "#000000", 
      size = 3
    ) +
    ggplot2::scale_shape_manual(
      values = shape, 
      labels = labels
    ) +
    ggplot2::scale_fill_manual(
      values = c("#0066CC", "#336666", "#66CCFF"), 
      labels = c("barrière", "frangeant", "interne"),
      guide = ggplot2::guide_legend(override.aes = list(shape = c(15, 15, 15), color = c("#0066CC", "#336666", "#66CCFF")))
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(
      fill = rep("#D1CECE", length(unique(cc_evol$etat))), 
      colour = "#000000")
      )) +
    ggplot2::labs(
      fill = "Complexe récifal", 
      shape = "Modification du taux de \nrecouvrement depuis le premier suivi"
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggspatial::annotation_scale()

}

# --- MAYOTTE ------------------------------------------------------------------

get_cc_evol_may <- function(pit_may, reef_type_may, coord_site_may, mayotte_bd, may_reef) {
  
  #targets::tar_load(pit_may)
  #targets::tar_load(reef_type_may)
  #targets::tar_load(coord_site_may)
  #targets::tar_load(mayotte_bd)
  #targets::tar_load(may_reef)
  
  cc_evol_may <- get_cc_evol(
    pit_region = pit_may, 
    coord_site = coord_site_may,
    fringing   = reef_type_may$fringing_may,
    barrier    = reef_type_may$barrier_may,
    intern     = reef_type_may$intern_may,
    slope      = NULL,
    flat       = NULL
    )
  
  cat_recouvrement <- unique(cc_evol_may$etat)
  
  all_cat  <- c("amelioration", "stable", "degradation")
  cat_name <- c("amélioration", "stable", "dégradation")
  shape <- c(24, 21, 25)
  names(shape) <- all_cat
  names(cat_name) <- all_cat
  
  cat_plot_shape <- shape[names(shape) %in% cat_recouvrement]
  cat_plot_name  <- cat_name[names(cat_name) %in% cat_recouvrement]
  
  cat_color <- c("#05A9D1", "#05D13A", "#F8F804")
  names(cat_color) <- all_cat
  cat_plot_color <- cat_color[names(cat_color) %in% cat_recouvrement]
  
  dot <- get_dot_cc_evol(cc_evol = cc_evol_may, color = cat_plot_color)
  
  map <- get_map_cc_evol(
    map_land = mayotte_bd, 
    map_reef = may_reef, 
    cc_evol = cc_evol_may,
    labels = cat_plot_name,
    shape = cat_plot_shape
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr", 
      height = ggplot2::unit(0.7, "cm"), 
      width = ggplot2::unit(0.7, "cm")
      ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
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
      plot.margin = ggplot2::margin(0, -12, 0, -20, "cm")
      )
  
  return(list(may_dot = dot, may_map = map))
  
}
