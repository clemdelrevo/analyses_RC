get_graph_invert <- function(invert_abondance_region) {
  
  graph_invert <- setNames(lapply(names(invert_abondance_region), function(level) {
    
    #level = "all"
    if (level == "all") {
      
      subtitle = bquote("Abondance totale (nb/100"*m^2*")")
      
    } else if (level == "herbivore") {
      
      subtitle = bquote("Abondance d'oursins herbivores (nb/100"*m^2*")")
      
    } else if (level == "holothuria") {
      
      subtitle = bquote("Abondance d'holothuries (nb/100"*m^2*")")
      
    } else if (level == "remarkable") {
      
      subtitle = bquote("Abondance d'espèces remarquables (nb/100"*m^2*")")
      
    } 
    
    sub_level <- invert_abondance_region[[level]]
    reef <- unique(sub_level$mean_site_abondance$reef_type)
    
    setNames(lapply(reef, function(reef_type) {
      
      #reef_type = "barrier"
      reef_abondance  <- sub_level$mean_reef_abondance[sub_level$mean_reef_abondance$reef_type == reef_type, ]
      site_abondance  <- sub_level$mean_site_abondance[sub_level$mean_site_abondance$reef_type == reef_type, ]
      graph <- timeline_graph(data = site_abondance, x = "annee", y = "mean_site", color = "#87e06a", data2 = reef_abondance, x2 = "annee", y2 = "mean_reef", subtitle = subtitle)
      
      
      if (level == "all" | level == "herbivore" | level == "holothuria") {
        
        return(graph + ggplot2::theme(axis.text.x = ggplot2::element_blank()))
        
      } else {
        
        return(graph)
        
      }
      
    }), reef)
    
  }), names(invert_abondance_region))
  
}

get_final_invert_timeline <- function(graph_invert_region, nb_survey_invert_region) {
  
  final_graph <- setNames(lapply(names(graph_invert_region$all), function(reef_type) {
    
    #reef_type = "barrier"
    if (reef_type == "barrier") {
      
      labels = "RÉCIFS BARRIÈRES"
      
    } else if (reef_type == "fringing") {
      
      labels = "RÉCIFS FRANGEANTS"
      
    } else if (reef_type == "intern") {
      
      labels = "RÉCIFS INTERNES"
      
    }
    
    cowplot::plot_grid(nb_survey_invert_region[[reef_type]],
                       graph_invert_region$all[[reef_type]],
                       graph_invert_region$herbivore[[reef_type]],
                       graph_invert_region$holothuria[[reef_type]],
                       graph_invert_region$remarkable[[reef_type]],
                       nrow = 5, rel_heights = c(1.8, 2, 2, 2), align = "hv", 
                       labels = labels, hjust = -0.2, vjust = -0.1)+
      ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
    
  }), names(graph_invert_region$all))
  
  args      <- list(align = "hv", ncol = length(final_graph))
  all_graph <- c(final_graph, args)
  
  do.call(cowplot::plot_grid, all_graph)
  
}