
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

get_may_graph_cc <- function(may_pourc_cc) {
  
  #targets::tar_load(may_pourc_cc)
  
  cc_line_barrier <-  may_pourc_cc$cc_line[may_pourc_cc$cc_line$reef_type == "barrier", ]
  mean_cc_barrier <-  may_pourc_cc$mean_pourc_cc[may_pourc_cc$mean_pourc_cc$reef_type == "barrier", ]
  cc_barrier      <- cc_timeline_graph(color = "#0066CC", data = cc_line_barrier, x = "annee", y = "pourc_hc", data2 = mean_cc_barrier, x2 = "annee", y2 = "mean_cover")
  
  cc_line_fringing <- may_pourc_cc$cc_line[may_pourc_cc$cc_line$reef_type == "fringing", ]
  mean_cc_fringing <-  may_pourc_cc$mean_pourc_cc[may_pourc_cc$mean_pourc_cc$reef_type == "fringing", ]
  cc_fringing      <- cc_timeline_graph(color = "#336666", data = cc_line_fringing, x = "annee", y = "pourc_hc", data2 = mean_cc_fringing, x2 = "annee", y2 = "mean_cover")
  
  cc_line_intern <- may_pourc_cc$cc_line[may_pourc_cc$cc_line$reef_type == "intern", ]
  mean_cc_intern <-  may_pourc_cc$mean_pourc_cc[may_pourc_cc$mean_pourc_cc$reef_type == "intern", ]
  cc_intern      <- cc_timeline_graph(color = "#66CCFF", data = cc_line_intern, x = "annee", y = "pourc_hc", data2 = mean_cc_intern, x2 = "annee", y2 = "mean_cover")

  return(list(cc_barrier = cc_barrier, cc_fringing = cc_fringing, cc_intern = cc_intern))
                    
}

get_may_final_cc_timeline <- function(may_graph_cc, n_survey_graph) {
  
  #targets::tar_load(may_graph_cc)
  #targets::tar_load(n_survey_graph)
  
  final_barrier <- cowplot::plot_grid(n_survey_graph$barrier, may_graph_cc$cc_barrier, align = "v", nrow = 2, labels = "RÉCIFS BARRIÈRES", hjust = -0.2, vjust = -0.1, rel_heights = c(1,2))+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  final_fringing <- cowplot::plot_grid(n_survey_graph$fringing, may_graph_cc$cc_fringing, align = "v", nrow = 2, labels = "RÉCIFS FRANGEANTS", hjust = -0.2, vjust = -0.1, rel_heights = c(1,2))+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  final_intern <- cowplot::plot_grid(n_survey_graph$intern, may_graph_cc$cc_intern, align = "v", nrow = 2, labels = "RÉCIFS INTERNES", hjust = -0.2, vjust = -0.1, rel_heights = c(1,2))+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
    
  
  cowplot::plot_grid(final_fringing, final_intern, final_barrier, align = "h", ncol = 3)
  
}
