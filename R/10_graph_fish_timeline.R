
fish_timeline_graph <- function(data, x, y, color, data2, x2, y2, subtitle) {
  
  ggplot2::ggplot()+
    ggplot2::geom_point(data = data, ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]]), size = 0.7, alpha = 0.5, color = color)+
    ggplot2::geom_smooth(data = data, method = "loess", ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]], group = 1), color = color, fill = color, alpha = 0.2)+
    ggplot2::geom_point(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), y = .data[[y2]]), color = color)+
    ggplot2::geom_linerange(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), ymin = mean_abondance - st_error_abondance, ymax = mean_abondance + st_error_abondance), color = color)+
    ggplot2::theme_classic()+
    ggplot2::labs(subtitle = subtitle)+
    ggplot2::scale_y_continuous(labels = scales::label_number(scale = 1, accuracy = 1))+
    ggplot2::theme(axis.title.x  = ggplot2::element_blank(), 
                   axis.title.y  = ggplot2::element_blank(),
                   legend.title  = ggplot2::element_blank(),
                   axis.text.x   = ggplot2::element_text(size = 12, face = "bold"),
                   axis.text.y   = ggplot2::element_text(size = 12, face = "bold"),
                   legend.text   = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 12, face = "bold"))
  
}

get_graph_fish <- function(fish_abondance_region) {
  
  graph_fish <- setNames(lapply(names(fish_abondance_region), function(level) {
    
    #level = "all"
    if (level == "all") {
      
      subtitle = bquote("Abondance totale (nb/100"*m^2*")")
      
    } else if (level == "herbivore") {
      
      subtitle = bquote("Abondance herbivore Scarinae (nb/100"*m^2*")")
      
    } else if (level == "carnivore") {
      
      subtitle = bquote("Abondance carnassier (nb/100"*m^2*")")
      
    } else if (level == "corallivore") {
      
      subtitle = bquote("Abondance papilllons (nb/100"*m^2*")")
      
    }
    
    sub_level <- fish_abondance_region[[level]]
    
    setNames(lapply(unique(sub_level$tot_abondance$reef_type), function(reef_type) {
      
      #reef_type = "barrier"
      tot_abondance  <- sub_level$tot_abondance[sub_level$tot_abondance$reef_type == reef_type, ]
      mean_abondance <- sub_level$mean_abondance[sub_level$mean_abondance$reef_type == reef_type, ]
      graph <- fish_timeline_graph(data = tot_abondance, x = "annee", y = "abondance", color = "orange", data2 = mean_abondance, x2 = "annee", y2 = "mean_abondance", subtitle = subtitle)
      
      if (level == "all" | level == "herbivore" | level == "carnivore") {
        
        return(graph + ggplot2::theme(axis.text.x = ggplot2::element_blank()))
        
      } else {
        
        return(graph)
        
      }

    }), unique(sub_level$tot_abondance$reef_type))
    
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
  
# --- MAYOTTE ------------------------------------------------------------------

get_graph_fish_may <- function(fish_abondance_may) {
  
  #targets::tar_load(fish_abondance_may)
  get_graph_fish(fish_abondance = fish_abondance_may)
  
}

get_final_fish_timeline_may <- function(graph_fish_may, nb_survey_fish_may) {
  
  #targets::tar_load(graph_fish_may)
  #targets::tar_load(nb_survey_fish_may)
  get_final_fish_timeline(graph_fish_region = graph_fish_may, nb_survey_fish_region = nb_survey_fish_may)
  
}
 