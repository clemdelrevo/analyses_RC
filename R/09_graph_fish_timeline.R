
get_graph_fish <- function(fish_abondance_region) {
  
  graph_fish <- setNames(lapply(names(fish_abondance_region), function(level) {
    
    #level = "herbivore"
    if (level == "all") {
      
      subtitle = bquote("Abondance totale (nb/100"*m^2*")")
      
    } else if (level == "herbivore") {
      
      subtitle = bquote("Abondance de perroquets (nb/100"*m^2*")")
      
    } else if (level == "carnivore") {
      
      subtitle = bquote("Abondance de piscivores (nb/100"*m^2*")")
      
    } else if (level == "corallivore") {
      
      subtitle = bquote("Abondance de papilllons (nb/100"*m^2*")")
      
    }
    
    sub_level <- fish_abondance_region[[level]]
    reef <- unique(sub_level$mean_site_abondance$reef_type)
    
    setNames(lapply(reef, function(reef_type) {
      
      #reef_type = "barrier"
      reef_abondance  <- sub_level$mean_reef_abondance[sub_level$mean_reef_abondance$reef_type == reef_type, ]
      site_abondance  <- sub_level$mean_site_abondance[sub_level$mean_site_abondance$reef_type == reef_type, ]
      graph <- timeline_graph(data = site_abondance, x = "annee", y = "mean_site", color = "#4c6bb9", data2 = reef_abondance, x2 = "annee", y2 = "mean_reef", subtitle = subtitle)
      
      if (level == "all" | level == "herbivore" | level == "carnivore") {
        
        return(graph + ggplot2::theme(axis.text.x = ggplot2::element_blank()))
        
      } else {
        
        return(graph)
        
      }

    }), reef)
    
  }), names(fish_abondance_region))
  
  return(graph_fish)
  
}

get_final_fish_timeline <- function(graph_fish_region, nb_survey_fish_region) {
  
  final_graph <- setNames(lapply(names(graph_fish_region$all), function(reef_type) {
    
    #reef_type = "barrier"
    if (reef_type == "barrier") {
      
      labels = "RÉCIFS BARRIÈRES"
      
    } else if (reef_type == "fringing") {
      
      labels = "RÉCIFS FRANGEANTS"
      
    } else if (reef_type == "intern") {
      
      labels = "RÉCIFS INTERNES"
      
    }
  
    cowplot::plot_grid(nb_survey_fish_region[[reef_type]],
                       graph_fish_region$all[[reef_type]],
                       graph_fish_region$herbivore[[reef_type]],
                       graph_fish_region$carnivore[[reef_type]],
                       graph_fish_region$corallivore[[reef_type]],
                       nrow = 5, rel_heights = c(1.8, 2, 2, 2, 2), align = "hv", 
                       labels = labels, hjust = -0.2, vjust = -0.1)+
      ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  }), names(graph_fish_region$all))
  
  args      <- list(align = "hv", ncol = length(final_graph))
  all_graph <- c(final_graph, args)
  
  do.call(cowplot::plot_grid, all_graph)
  
}
  