# Generate barplot to have an overview of fish or 
# invert mean abondance in each station ---------------------------------------------

stat_bar_function <- function(data, taxon_name) {
  
  taxon_abondance <- tidyr::gather(data = data, taxon, abondance, -site, -annee, -transect)
  max_year        <- tapply(taxon_abondance$annee, taxon_abondance$site, max)
  taxon_abondance <- taxon_abondance[taxon_abondance$annee %in% max_year, ]
  
  taxon_abondance <- setNames(lapply(levels(as.factor(taxon_abondance$site)), function(site) {
  
    #site = "pes.onze"
    taxon_station <- taxon_abondance[taxon_abondance$site == site, ]  
    taxon_station <-  taxon_station |>
      dplyr::group_by(site, taxon) |>
      dplyr::summarise(mean_abondance = mean(abondance),
                       st_error_abondance = plotrix::std.error(abondance))
    
    taxon_station <- taxon_station[taxon_station$mean_abondance != 0, ]
    taxon_name    <- taxon_name 
    #color         <- color 
    taxon_station <- taxon_station[order(taxon_station$taxon), ]
    
    taxon_station$taxon_name <- taxon_name[names(taxon_name) %in% taxon_station$taxon]
    #taxon_station$color      <- color[names(color) %in% taxon_station$taxon]
    
    taxon_station <- taxon_station[order(taxon_station$mean_abondance), ]
    #taxon_station$color <- taxon_station$color |>
      #forcats::fct_inorder()|>
      #forcats::fct_rev()
    taxon_station$taxon_name<- taxon_station$taxon_name |>
      forcats::fct_inorder()|>
      forcats::fct_rev()
    
    ggplot2::ggplot()+
      ggplot2::geom_col(data= taxon_station, ggplot2::aes(x = taxon_name, y = mean_abondance, fill = taxon_name), linewidth = 0.7, color = "black")+
      ggplot2::geom_errorbar(data = taxon_station, ggplot2::aes(x = taxon_name, ymin = mean_abondance - st_error_abondance, ymax = mean_abondance + st_error_abondance),
                             linewidth = 0.5, width = 0.2)+
      ggplot2::coord_flip()+
      ggplot2::scale_fill_viridis_d()+
      ggplot2::theme_bw()+
      ggplot2::ylab(bquote("Abondance moyenne (nb/100"*m^2*")"))+
      ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "none",
                     plot.subtitle = ggplot2::element_text(face = "bold", size = 12),
                     axis.text = ggplot2::element_text(face = "bold", size = 12))
      
    
  }), levels(as.factor(taxon_abondance$site)))

  return(taxon_abondance)
  
}
  
# Generate camenbert to have an overview of mean substrat cover in each station 

camenbert_function <- function(data, color) {

  data_line <- tidyr::gather(data = data, substrat, cover, -site, -annee, -transect)
  max_year  <- tapply(data_line$annee, data_line$site, max)
  data_line <- data_line[data_line$annee %in% max_year, ]
  data_line$cover <- (data_line$cover * 100) / 40

  substrat <- setNames(lapply(levels(as.factor(data_line$site)), function(site) {
    
    #site = "tzoundzou"
    message(site)
    
    data_line_site <- data_line[data_line$site == site, ]
    data_line_site <- data_line_site |>
      dplyr::group_by(substrat) |>
      dplyr::summarise(mean_cover = mean(cover)) |>
      dplyr::arrange(dplyr::desc(substrat))
    data_line_site <- data_line_site[data_line_site$mean_cover != 0, ]
    
    data_line_site$lab.ypos <- cumsum(data_line_site$mean_cover) - 0.5 * data_line_site$mean_cover 
    
    color = color
    
    data_line_site <- data_line_site[order(data_line_site$substrat), ]
    data_line_site$color <- color[names(color) %in% data_line_site$substrat]
    data_line_site$color <- data_line_site$color |>
      forcats::fct_inorder()
  
    ggplot2::ggplot(data_line_site, ggplot2::aes(x = "", y = mean_cover, fill = substrat)) +
      ggplot2::geom_bar(width = 1, stat = "identity", color = "#000000") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::theme_classic()+
      ggplot2::labs(fill = "Substrat (%)")+
      ggplot2::scale_fill_manual(values = as.vector(data_line_site$color))+
      ggplot2::geom_text(ggplot2::aes(y = lab.ypos, label = round(mean_cover, 1)),
                         color = "black", size=5, fontface = "bold", check_overlap = TRUE)+
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.margin = ggplot2::margin(l = -25, unit = "pt"),
                     legend.title = ggplot2::element_text(face = "bold"))
    
  
    }), levels(as.factor(data_line$site)))
  
  return(substrat)
    
}

substrat_station_may <- function(data_line_may) {
  
  #targets::tar_load(data_line_may)
  
  camenbert_function(data = data_line_may, color = c(HC = "#FF6666", NIA = "#99FFCC", OT = "#333333", RB = "#CCCCCC", 
                                                     RC = "#CC6633", RKC = "#FFFFFF", SC = "#CC0033", SD = "#FFFF99", 
                                                     SI = "#999900", SP = "#3399CC"))
  
}

fish_station_may <- function(data_fish_may) {
  
  #targets::tar_load(data_fish_may)
  
  stat_bar_function(data = data_fish_may, 
                   taxon_name = c(bumphead_parrot = "Perroquet à bosse", butterflyfish = "Chaetodontidae", haemulidae = "Haemulidae",
                                  humphead_wrasse = "Napoléon", grouper = "Serranidae", moray_eel = "Muraenidae",
                                  parrotfish = "Scarinae", snapper = "Lutjanidae"))
                   #color =      c(bumphead_parrot = "#336633", butterflyfish ="#FFFF66", haemulidae = "#666666",
                                  #humphead_wrasse = "#66FF99", grouper = "#CC6666", moray_eel = "#6600CC",
                                  #parrotfish = "#006600", snapper = "#003366"))
                  
}

invert_station_may <- function(data_invert_may) {
  
  #targets::tar_load(invert_may)
  
  stat_bar_function(data = data_invert_may,
                    taxon_name = c(banded_coral_shrimp = "Crevette à bande", collector_urchin = "Oursin collecteur", 
                                   crown_of_thorns = "Acanthasteridae", diadema_urchin = "Oursin diadème", 
                                   giant_clam = "Bénitier", lobster = "Langouste", 
                                   pencil_urchin = "Oursin crayon", sea_cucumber = "Holothuroidea", 
                                   triton = "Triton"))
                    #color = c(banded_coral_shrimp = "#996633", collector_urchin = "#660099", 
                              #crown_of_thorns = "#CC0033", diadema_urchin = "#333333",
                              #giant_clam = "#6699FF", lobster = "#FFCCFF",  
                              #pencil_urchin = "#993333", sea_cucumber = "#FFCC99", 
                              #triton = "#CCCC33"))
  
}
