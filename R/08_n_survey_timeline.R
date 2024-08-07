n_survey_function <- function(data, variable) {
  
  n_survey <- data.frame(do.call(rbind, lapply(levels(as.factor(variable)), function(reef_type){
    
    #reef_type = "barrier"
    rf_type <-  data[variable == reef_type, ]
    
    n_year <- data.frame(do.call(rbind, lapply(levels(as.factor(rf_type$annee)), function(year){
      
      #year = "2023-2024"
      year_survey <- rf_type[rf_type$annee == year, ]
      n <- length(levels(as.factor(year_survey$site)))
      data.frame(year = year, n = n)
      
    })))
    
    n_year$reef_type <- rep(reef_type, nrow(n_year))
    #n_year$year <- as.integer(n_year$year)
    
    return(n_year)
    
  })))
  
  return(n_survey)
  
}

n_survey_graph_function <- function(n_survey) {
  
  n_survey_graph <- setNames(lapply(levels(as.factor(n_survey$reef_type)), function(reef_type) {
  
    #reef_type = "inter"
    rf_type <- n_survey[n_survey$reef_type == reef_type, ]
    
    g <- ggplot2::ggplot()+
      ggplot2::geom_col(data = rf_type, ggplot2::aes(x = as.factor(year), y = as.integer(n)))+
      ggplot2::theme_classic()+
      ggplot2::labs(subtitle = "nombre de stations")+
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
      ggplot2::theme(axis.title.x  = ggplot2::element_blank(), 
                     axis.title.y  = ggplot2::element_blank(),
                     legend.title  = ggplot2::element_blank(),
                     axis.text.x   = ggplot2::element_text(size = 9, face = "bold", angle = 30, vjust = 0.7),
                     axis.text.y   = ggplot2::element_text(size = 10, face = "bold"),
                     legend.text   = ggplot2::element_text(size = 12, face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 12, face = "bold"))
    
    if (reef_type == "slope" | reef_type == "flat") { g + ggplot2::scale_x_discrete(breaks = c(2003, 2010, 2016, 2023)) 
    } else {
      return(g)
        }
    
  }), levels(as.factor(n_survey$reef_type)))
  
  return(n_survey_graph)
  
}

# --- MAYOTTE ------------------------------------------------------------------

get_nb_survey_cc_may <- function(pourc_cc_may) {
  
  #targets::tar_load(pourc_cc_may)
  n_survey <- n_survey_function(data = pourc_cc_may$mean_site_cc, variable = pourc_cc_may$mean_site_cc$reef_type)
  n_survey_graph_function(n_survey)
  
}

get_nb_survey_fish_may <- function(fish_abondance_may) {
  
  #targets::tar_load(fish_abondance_may)
  n_survey <- n_survey_function(data = fish_abondance_may$all$mean_site_abondance, variable = fish_abondance_may$all$mean_site_abondance$reef_type)
  n_survey_graph_function(n_survey)
  
}

get_nb_survey_invert_may <- function(invert_abondance_may) {
  
  #targets::tar_load(invert_abondance_may)
  n_survey <- n_survey_function(data = invert_abondance_may$all$mean_site_abondance, variable = invert_abondance_may$all$mean_site_abondance$reef_type)
  n_survey_graph_function(n_survey)
  
}
