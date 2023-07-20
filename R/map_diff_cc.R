get_map_diff_cc <- function(mayotte_bd, may_reef, may_cc_diff_site) {
  
  #targets::tar_load(mayotte_bd)
  #targets::tar_load(may_reef)
  #targets::tar_load(may_cc_diff_site)
  
  may_cc_diff_site <- na.omit(may_cc_diff_site)
  
  may_diff_cc_map <- ggplot2::ggplot()+
    ggplot2::theme_classic()+
    ggplot2::geom_sf(data = may_reef, fill = "#FFCC66", color = "#FFCC66")+
    ggplot2::geom_sf(data = mayotte_bd, fill = "#bdb7aa")+
    ggplot2::geom_sf(data = may_cc_diff_site, ggplot2::aes(fill = reef_type, shape = etat), color = "#000000", size = 3)+
    ggplot2::scale_shape_manual(values = c(24, 21, 25), labels = c("amélioration", "stable", "dégradation"))+
    ggplot2::scale_fill_manual(values = c("#0066CC", "#336666", "#66CCFF"), labels = c("barrière", "frangeant", "interne"),
                               guide = ggplot2::guide_legend(override.aes = list(shape = c(15, 15, 15), color = c("#006666", "#669999", "#66CCCC"))))+
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = c("#D1CECE", "#D1CECE", "#D1CECE"), colour = "#000000")))+
    ggplot2::labs(fill = "Complexe récifal", shape = "Modification du taux de \nrecouvrement depuis le dernier suivi")+
    ggplot2::xlab("")+
    ggplot2::ylab("")+
    ggspatial::annotation_north_arrow(location = "tr", height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.7, "cm"))+
    ggspatial::annotation_scale()+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#EBF5FB"),
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
                   plot.margin = ggplot2::margin(0, -12, 0, -20, "cm"))
  
  return(may_diff_cc_map)
  
}

