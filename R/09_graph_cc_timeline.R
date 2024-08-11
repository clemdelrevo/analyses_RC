
cc_timeline_graph <- function(color, data, x, y, data2, x2, y2, ymin, ymax) {
    
    ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]]), size = 0.7, color = color, alpha = 0.8) +
    ggplot2::geom_smooth(data = data, method = "loess", ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]], group = 1), fill = color, color = color, alpha = 0.4) +
    ggplot2::geom_point(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), y = .data[[y2]]), fill = color, color = color) +
    ggplot2::geom_linerange(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), ymin = mean_allcover - st_error, ymax = mean_allcover + st_error), color = color) +
    ggplot2::ylim(0, 100) +
    ggplot2::theme_classic() +
    ggplot2::labs(subtitle = "Corail vivant (%)") +
    ggplot2::theme(axis.title.x  = ggplot2::element_blank(), 
                   axis.title.y  = ggplot2::element_blank(),
                   legend.title  = ggplot2::element_blank(),
                   axis.text.x   = ggplot2::element_text(size = 9, face = "bold", angle = 30, vjust = 0.7),
                   axis.text.y   = ggplot2::element_text(size = 12, face = "bold"),
                   legend.text   = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 12, face = "bold"))

}

get_graph_cc <- function(pourc_cc_region) {
  
  graph_cc <- setNames(lapply(c("barrier", "intern", "fringing"), function(reef_type) {
    
    #reef_type = "barrier"
    message(reef_type)
    mean_site_cc <- pourc_cc_region$mean_site_cc[pourc_cc_region$mean_site_cc$reef_type == reef_type, ]
    mean_all_cc  <- pourc_cc_region$mean_all_cc[pourc_cc_region$mean_all_cc$reef_type == reef_type, ]
    
    if (reef_type == "barrier") {
      
      color = "#90d587"
      
    } else if (reef_type == "fringing") {
      
      color = "#7fc3df"
      
    } else if (reef_type == "intern") {
      
      color = "#fcd1a3"
      
    } else if (reef_type == "slope") {
      
      color = "#0066CC"
      
    } else if (reef_type == "flat") {
      
      color = "#66CCFF"
      
    }
    
    g <- cc_timeline_graph(color = color, data = mean_site_cc, x = "annee", y = "mean_cover", data2 = mean_all_cc, x2 = "annee", y2 = "mean_allcover")
    
    if (reef_type == "flat" | reef_type == "slope") { g + ggplot2::scale_x_discrete(breaks = c(2003, 2010, 2016, 2023))
      } else {
    return(g)
      }
    
  }), c("barrier", "intern", "fringing"))
  
  return(graph_cc)
                    
}

get_final_cc_timeline <- function(graph_cc_region, nb_survey_cc_region) {
  
  final_graph <- setNames(lapply(names(graph_cc_region), function(reef_type) {
    
    #reef_type = "barrier"
    if (reef_type == "barrier") {
      
      labels = "RÉCIFS BARRIÈRES"
      
    } else if (reef_type == "fringing") {
      
      labels = "RÉCIFS FRANGEANTS"
      
    } else if (reef_type == "intern") {
      
      labels = "RÉCIFS INTERNES"
      
    } else if (reef_type == "slope") {
      
      labels = "PENTE EXTERNE"
      
    } else if (reef_type == "flat") {
      
      labels = "PLATIER RÉCIFAL"
      
    }
    
    cowplot::plot_grid(nb_survey_cc_region[[reef_type]], graph_cc_region[[reef_type]], align = "v", nrow = 2, labels = labels, hjust = -0.2, vjust = -0.1, rel_heights = c(1,2))+
      ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
    
  }), names(graph_cc_region))
  
  args      <- list(align = "h", ncol = length(final_graph))
  all_graph <- c(final_graph, args)
  
  do.call(cowplot::plot_grid, all_graph)
  
}
