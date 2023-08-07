
cc_timeline_graph <- function(color, data, x, y, data2, x2, y2, ymin, ymax) {
    
  ggplot2::ggplot()+
    ggplot2::geom_point(data = data, ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]]), size = 0.7, color = color, alpha = 0.5)+
    ggplot2::geom_smooth(data = data, method = "loess", ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]], group = 1), fill = color, color = color, alpha = 0.4)+
    ggplot2::geom_point(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), y = .data[[y2]]), fill = color, color = color)+
    ggplot2::geom_linerange(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), ymin = mean_cover - st_error_cover, ymax = mean_cover + st_error_cover), color = color)+
    ggplot2::theme_classic()+
    ggplot2::labs(subtitle = "Corail vivant (%)")+
    ggplot2::theme(axis.title.x  = ggplot2::element_blank(), 
                   axis.title.y  = ggplot2::element_blank(),
                   legend.title  = ggplot2::element_blank(),
                   axis.text.x   = ggplot2::element_text(size = 12, face = "bold"),
                   axis.text.y   = ggplot2::element_text(size = 12, face = "bold"),
                   legend.text   = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 12, face = "bold"))

}

get_graph_cc <- function(pourc_cc_region) {
  
  graph_cc <- setNames(lapply(unique(pourc_cc_region$cc_line$reef_type), function(reef_type) {
    
    #reef_type = "barrier"
    cc_line_type <- pourc_cc_region$cc_line[pourc_cc_region$cc_line$reef_type == reef_type, ]
    mean_cc_type <- pourc_cc_region$mean_pourc_cc[pourc_cc_region$mean_pourc_cc$reef_type == reef_type, ]
    
    if (reef_type == "barrier") {
      
      color = "#0066CC"
      
    } else if (reef_type == "fringing") {
      
      color = "#336666"
      
    } else if (reef_type == "intern") {
      
      color = "#66CCFF"
      
    } else if (reef_type == "slope") {
      
      color = "#0066CC"
      
    } else if (reef_type == "flat") {
      
      color = "#66CCFF"
      
    }
    
    cc_timeline_graph(color = color, data = cc_line_type, x = "annee", y = "pourc_hc", data2 = mean_cc_type, x2 = "annee", y2 = "mean_cover")
    
  }), unique(pourc_cc_region$cc_line$reef_type))
  
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

# --- MAYOTTE ------------------------------------------------------------------

get_graph_cc_may <- function(pourc_cc_may) {
  
  #targets::tar_load(pourc_cc_may)
  get_graph_cc(pourc_cc_region = pourc_cc_may)
  
}

get_final_cc_timeline_may <- function(graph_cc_may, nb_survey_cc_may) {
  
  #targets::tar_load(graph_cc_may)
  #targets::tar_load(nb_survey_cc_may)
  get_final_cc_timeline(graph_cc_region = graph_cc_may, nb_survey_cc_region = nb_survey_cc_may)
  
}

# --- RÉUNION ------------------------------------------------------------------

get_graph_cc_run <- function(pourc_cc_run) {
  
  #targets::tar_load(pourc_cc_run)
  get_graph_cc(pourc_cc_region = pourc_cc_run)
  
}

get_final_cc_timeline_run <- function(graph_cc_run, nb_survey_cc_run) {
  
  #targets::tar_load(graph_cc_run)
  #targets::tar_load(nb_survey_cc_run)
  get_final_cc_timeline(graph_cc_region = graph_cc_run, nb_survey_cc_region = nb_survey_cc_run)
  
}