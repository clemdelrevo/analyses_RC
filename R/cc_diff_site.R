get_may_cc_diff_site <- function(data_line_may, reef_type_may, coord_site_may) {
  
  #targets::tar_load(data_line)
  #targets::tar_load(reef_type_may)
  #targets::tar_load(coord_site_may)
  
  may_cc_site <- data_line_may |>
    dplyr::group_by(annee, site) |>
    dplyr::summarise(mean_coral_cover = mean(HC),
                     sd_coral_cover = sd(HC))
  
  may_cc_site <- merge(may_cc_site, coord_site_may, by = "site")
  
  may_cc_diff_site <- data.frame(do.call(rbind, lapply(levels(as.factor(may_cc_site$site)), function(site) {
    
    #site <- "majikavo"
    message(site)
    sub_site <- may_cc_site[may_cc_site$site == site, ]
    
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
  
  may_cc_diff_site <- sf::st_as_sf(may_cc_diff_site, coords = c("x", "y"))
  sf::st_crs(may_cc_diff_site) <- 4326
  
  may_cc_diff_site$reef_type <- NA
  may_cc_diff_site$reef_type[may_cc_diff_site$site %in% reef_type_may$fringing_may] <- "fringing"
  may_cc_diff_site$reef_type[may_cc_diff_site$site %in% reef_type_may$barrier_may]  <- "barrier"
  may_cc_diff_site$reef_type[may_cc_diff_site$site %in% reef_type_may$intern_may]  <- "intern"
  
  may_cc_diff_site$etat <- may_cc_diff_site$etat |>
    forcats::fct_relevel("improvement", "stable", "degradation")
  
  return(may_cc_diff_site)
  
}

get_may_diff_cc_dot<- function(may_cc_diff_site) {
  
  #targets::tar_load(may_cc_diff_site)
  
  cc_devel <- table(may_cc_diff_site$etat)
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
  
  may_diff_cc_dot <- ggplot2::ggplot(cc_devel, ggplot2::aes(xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = etat))+
    ggplot2::geom_rect()+
    ggplot2::coord_polar(theta = "y")+
    ggplot2::theme_void()+
    ggplot2::xlim(c(-.5, 4.5))+
    ggplot2::geom_label(x = 3.5, ggplot2::aes(y = label_pos, label = label), size = 14, label.size = 0.5, nudge_x = 1)+
    ggplot2::scale_fill_manual(values = c("#05A9D1", "#05D13A", "#F8F804"))+
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(0, 0, -4, -2, "cm"))
  
  return(may_diff_cc_dot)
  
}