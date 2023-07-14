
timeline_graph <- function(data, x, y, reef_type_color, color, group, fill, moy, sd, labels, subtitle) {
    
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color]]))+
    ggplot2::theme_classic()+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(data = data, method = "loess", ggplot2::aes(group = .data[[group]], fill = .data[[fill]]), alpha = 0.2)+
    ggplot2::geom_linerange(ggplot2::aes(ymin = .data[[moy]] - .data[[sd]], ymax = .data[[moy]] + .data[[sd]]))+
    ggplot2::scale_color_manual(values = reef_type_color, labels = labels)+
    ggplot2::scale_fill_manual(values = reef_type_color)+
    ggplot2::labs(subtitle = subtitle, size = 12)+
    ggplot2::guides(fill = "none")+
    ggplot2::theme(axis.title.x  = ggplot2::element_blank(), 
                   axis.title.y  = ggplot2::element_blank(),
                   legend.title  = ggplot2::element_blank(),
                   axis.text.x   = ggplot2::element_text(size = 12, face = "bold"),
                   axis.text.y   = ggplot2::element_text(size = 12, face = "bold"),
                   legend.text   = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 12, face = "bold"),
                   legend.position  = c(0.9, 1),
                   legend.direction = "horizontal")


}


get_may_graph_cc <- function(may_coral_cover) {
  
  may_graph_cc <- timeline_graph(data = may_coral_cover, x = "annee", y = "mean_coral_cover", reef_type_color = c("#669999", "#66CCCC"),
                                    color = "reef_type", group = "reef_type", fill = "reef_type",
                                    moy = "mean_coral_cover", sd = "sd_coral_cover",
                                    labels = c("barrière", "frangeant"), subtitle = "corail vivant (%)")
  may_graph_cc
  
}
