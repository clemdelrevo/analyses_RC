
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

get_may_graph_fish <- function(fish_abondance, fish_trophic_abondance) {
  
  #targets::tar_load(fish_abondance)
  #targets::tar_load( fish_trophic_abondance)
  
  tot_abondance <- setNames(lapply(levels(as.factor(fish_abondance$tot_abondance$reef_type)), function(reef_type) {
    
    #reef_type = "barrier"
    tot_abondance  <- fish_abondance$tot_abondance[fish_abondance$tot_abondance$reef_type == reef_type, ]
    mean_abondance <- fish_abondance$mean_abondance[fish_abondance$mean_abondance$reef_type == reef_type, ]
    fish_timeline_graph(data = tot_abondance, x = "annee", y = "abondance", color = "orange", data2 = mean_abondance, x2 = "annee", y2 = "mean_abondance", subtitle = bquote("Abondance totale (nb/100"*m^2*")"))+
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
    
  }), levels(as.factor(fish_abondance$tot_abondance$reef_type)))
  
  
  tot_herbivore <- setNames(lapply(levels(as.factor(fish_trophic_abondance$herbivore$reef_type)), function(reef_type) {
    
    tot_herbivore_reef <- fish_trophic_abondance$herbivore[fish_trophic_abondance$herbivore$reef_type == reef_type, ]
    mean_herbivore     <- fish_trophic_abondance$mean_herbivore[fish_trophic_abondance$mean_herbivore$reef_type == reef_type, ]
    fish_timeline_graph(data = tot_herbivore_reef, x = "annee", y = "parrotfish", color = "orange", data2 = mean_herbivore, x2 = "annee", y2 = "mean_abondance", subtitle = bquote("Abondance herbivore Scarinae (nb/100"*m^2*")"))+
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
    
  }), levels(as.factor(fish_trophic_abondance$herbivore$reef_type)))
  
  tot_carnivore <- setNames(lapply(levels(as.factor(fish_trophic_abondance$carnivore$reef_type)), function(reef_type) {
    
    tot_carnivore_reef <- fish_trophic_abondance$carnivore[fish_trophic_abondance$carnivore$reef_type == reef_type, ]
    mean_carnivore     <- fish_trophic_abondance$mean_carnivore[fish_trophic_abondance$mean_carnivore$reef_type == reef_type, ]
    fish_timeline_graph(data = tot_carnivore_reef, x = "annee", y = "abondance_carnivore", color = "orange", data2 = mean_carnivore, x2 = "annee", y2 = "mean_abondance", subtitle = bquote("Abondance carnassier (nb/100"*m^2*")"))+
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
    
  }), levels(as.factor(fish_trophic_abondance$carnivore$reef_type)))
  
  tot_corallivore <- setNames(lapply(levels(as.factor(fish_trophic_abondance$corallivore$reef_type)), function(reef_type) {
    
    tot_corallivore_reef <- fish_trophic_abondance$corallivore[fish_trophic_abondance$corallivore$reef_type == reef_type, ]
    mean_corallivore     <- fish_trophic_abondance$mean_corallivore[fish_trophic_abondance$mean_corallivore$reef_type == reef_type, ]
    fish_timeline_graph(data = tot_corallivore_reef, x = "annee", y = "butterflyfish", color = "orange", data2 = mean_corallivore, x2 = "annee", y2 = "mean_abondance", subtitle = bquote("Abondance papilllons (nb/100"*m^2*")"))
    
  }), levels(as.factor(fish_trophic_abondance$corallivore$reef_type)))
  
  return(list(tot_abondance = tot_abondance, tot_herbivore = tot_herbivore, tot_carnivore = tot_carnivore, tot_corallivore = tot_corallivore))
  
}
  
get_may_final_fish_timeline <- function(may_graph_fish) {
  
  #targets::tar_load(may_graph_fish)
  #targets::tar_load(survey_may_fish)
  
  fringing <- cowplot::plot_grid(survey_may_fish$fringing, may_graph_fish$tot_abondance$fringing, may_graph_fish$tot_herbivore$fringing, may_graph_fish$tot_carnivore$fringing, may_graph_fish$tot_corallivore$fringing,
                     nrow = 5, rel_heights = c(1.8, 2, 2, 2, 2), align = "hv", labels = "RÉCIFS FRANGEANTS", hjust = -0.2, vjust = -0.1)+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  intern <- cowplot::plot_grid(survey_may_fish$intern, may_graph_fish$tot_abondance$intern, may_graph_fish$tot_herbivore$intern, may_graph_fish$tot_carnivore$intern, may_graph_fish$tot_corallivore$intern,
                     nrow = 5, rel_heights = c(1.8, 2, 2, 2, 2), align = "hv", labels = "RÉCIFS INTERNES", hjust = -0.2, vjust = -0.1)+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  barrier <- cowplot::plot_grid(survey_may_fish$barrier, may_graph_fish$tot_abondance$barrier, may_graph_fish$tot_herbivore$barrier, may_graph_fish$tot_carnivore$barrier, may_graph_fish$tot_corallivore$barrier,
                     nrow = 5, rel_heights = c(1.8, 2, 2, 2, 2), align = "hv", labels = "RÉCIFS BARRIÈRES", hjust = -0.2, vjust = -0.1)+
    ggplot2::theme(plot.margin = ggplot2::margin(t = 13, unit = "pt"))
  
  cowplot::plot_grid(fringing, intern, barrier, ncol = 3, align = "hv")
  
}