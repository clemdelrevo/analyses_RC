
timeline_graph <- function(color, data, x, y, data2, x2, y2, ymin, ymax) {
    
  ggplot2::ggplot()+
    ggplot2::geom_point(data = data, ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]]), size = 0.7, color = color, alpha = 0.5)+
    ggplot2::geom_smooth(data = data, method = "loess", ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]], group = 1), fill = color, color = color, alpha = 0.4)+
    ggplot2::geom_point(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), y = .data[[y2]]), fill = color, color = color)+
    ggplot2::geom_linerange(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), ymin = mean_cover - sd_cover, ymax = mean_cover + sd_cover), color = color)+
    ggplot2::theme_classic()+
    ggplot2::labs(subtitle = "% corail vivant")+
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
  cc_barrier      <- timeline_graph(color = "#0066CC", data = cc_line_barrier, x = "annee", y = "pourc_hc", data2 = mean_cc_barrier, x2 = "annee", y2 = "mean_cover")
  
  cc_line_fringing <- may_pourc_cc$cc_line[may_pourc_cc$cc_line$reef_type == "fringing", ]
  mean_cc_fringing <-  may_pourc_cc$mean_pourc_cc[may_pourc_cc$mean_pourc_cc$reef_type == "fringing", ]
  cc_fringing      <- timeline_graph(color = "#336666", data = cc_line_fringing, x = "annee", y = "pourc_hc", data2 = mean_cc_fringing, x2 = "annee", y2 = "mean_cover")
  
  cc_line_intern <- may_pourc_cc$cc_line[may_pourc_cc$cc_line$reef_type == "intern", ]
  mean_cc_intern <-  may_pourc_cc$mean_pourc_cc[may_pourc_cc$mean_pourc_cc$reef_type == "intern", ]
  cc_intern      <- timeline_graph(color = "#66CCFF", data = cc_line_intern, x = "annee", y = "pourc_hc", data2 = mean_cc_intern, x2 = "annee", y2 = "mean_cover")

  return(list(cc_barrier = cc_barrier, cc_fringing = cc_fringing, cc_intern = cc_intern))
                    
}

get_n_survey <- function(may_pourc_cc) {
  
  #targets::tar_load(may_pourc_cc)
  
  n_survey <- data.frame(do.call(rbind, lapply(levels(as.factor(may_pourc_cc$cc_line$reef_type)), function(reef_type){
    
    #reef_type = "barrier"
    rf_type <-  may_pourc_cc$cc_line[may_pourc_cc$cc_line$reef_type == reef_type, ]
    
    n_year <- data.frame(do.call(rbind, lapply(levels(as.factor(rf_type$annee)), function(year){
      
      #year = "2018"
      year_survey <- rf_type[rf_type$annee == year, ]
      n <- length(levels(as.factor(year_survey$site)))
      data.frame(year = year, n = n)
      
    })))
    
    n_year$reef_type <- rep(reef_type, nrow(n_year))
    n_year$year <- as.integer(n_year$year)
    
    return(n_year)
    
  })))
  
  return(n_survey)
  
}

n_survey_graph_function <- function(data, x, y) {
  
  ggplot2::ggplot()+
    ggplot2::geom_col(data = data, ggplot2::aes(x = as.factor(.data[[x]]), y = as.integer(.data[[y]])))+
    ggplot2::theme_classic()+
    ggplot2::labs(subtitle = "nombre de stations")+
    ggplot2::scale_y_continuous(labels = scales::label_number(scale = 1, accuracy = 1))+
    ggplot2::theme(axis.title.x  = ggplot2::element_blank(), 
                   axis.title.y  = ggplot2::element_blank(),
                   legend.title  = ggplot2::element_blank(),
                   axis.text.x   = ggplot2::element_text(size = 12, face = "bold"),
                   axis.text.y   = ggplot2::element_text(size = 12, face = "bold"),
                   legend.text   = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 12, face = "bold"))
  
}

get_n_survey_graph <- function(n_survey) {
  
  n_survey_graph <- lapply(levels(as.factor(n_survey$reef_type)), function(reef_type) {
    
    #reef_type = "barrier"
    rf_type <- n_survey[n_survey$reef_type == reef_type, ]

    n_survey_graph_function(data = rf_type, x = "year", y = "n")
    
  })
  
  names(n_survey_graph) <- c("barrier", "fringing", "intern")

  return(n_survey_graph)
  
}

get_may_final_cc_timeline <- function(may_graph_cc, n_survey_graph) {
  
  #targets::tar_load(may_graph_cc)
  #targets::tar_load(n_survey_graph)
  
  final_barrier <- cowplot::plot_grid(n_survey_graph$barrier, may_graph_cc$cc_barrier, align = "v", nrow = 2, labels = "RÉCIF BARRIÈRE", hjust = -0.2, vjust = -0.1, rel_heights = c(1,2))+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  final_fringing <- cowplot::plot_grid(n_survey_graph$fringing, may_graph_cc$cc_fringing, align = "v", nrow = 2, labels = "RÉCIF FRANGEANT", hjust = -0.2, vjust = -0.1, rel_heights = c(1,2))+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  final_intern <- cowplot::plot_grid(n_survey_graph$intern, may_graph_cc$cc_intern, align = "v", nrow = 2, labels = "RÉCIF INTERNE", hjust = -0.2, vjust = -0.1, rel_heights = c(1,2))+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  cowplot::plot_grid(final_fringing, final_intern, final_barrier, align = "h")
  
}
