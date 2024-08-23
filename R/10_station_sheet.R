# Generate barplot to have an overview of fish or 
# invert mean abondance in each station ---------------------------------------------

stat_bar_function <- function(data, taxon_name, taxon) {
  
  taxon_abondance <- tidyr::gather(data = data, taxon, abondance, -site, -annee, -transect)
  taxon_abondance$taxon_name <- taxon_name[taxon_abondance$taxon]

  taxon_abondance <- setNames(lapply(levels(as.factor(taxon_abondance$site)), function(site) {
  
    #site = "mbouzi"
    taxon_station <- taxon_abondance[taxon_abondance$site == site, ]  
    taxon_station <-  taxon_station |>
      dplyr::group_by( annee, taxon) |>
      dplyr::summarise(mean_abondance = mean(abondance),
                       st_error_abondance = plotrix::std.error(abondance))
    
    taxon_station <- as.data.frame(taxon_station[taxon_station$taxon != "barramundi_cod", ])
    taxon_station <- taxon_station[order(taxon_station$taxon), ]
    taxon_station$taxon_name <- taxon_name[taxon_station$taxon]
    tax_off <- tapply(taxon_station$mean_abondance, taxon_station$taxon, sum) == 0
    tax_off <- names(tax_off[tax_off == TRUE])
    taxon_station <- taxon_station[!taxon_station$taxon %in% tax_off, ]
    # taxon_abondance_station <- taxon_abondance[taxon_abondance$site == site, ]
    # taxon_abondance_station <- taxon_abondance_station[!taxon_abondance_station$taxon %in% tax_off, ] #enlever les hashtag si besoin de modéliser

    if (taxon == "fish") color_bar <- "#4c6bb9" else color_bar <- "#b9674c"
    
    custom_breaks <- function(x, n = 4) {
      if (length(unique(x)) > n) {
        unique(x)[seq(1, length(unique(x)), length.out = n)]
      } else {
        unique(x)
      }
    }
    
    p <- ggplot2::ggplot()+
      ggplot2::geom_col(data = taxon_station, ggplot2::aes(x = as.factor(annee), y = mean_abondance), color = color_bar, fill = color_bar)+
      ggplot2::geom_errorbar(data = taxon_station, ggplot2::aes(x = as.factor(annee), ymin = mean_abondance - st_error_abondance, ymax = mean_abondance + st_error_abondance),
                              linewidth = 0.5, width = 0.2) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 4), labels = integer_labels) +
      ggplot2::scale_x_discrete(breaks = if (length(unique(taxon_station$annee)) > 8) custom_breaks(taxon_station$annee) else ggplot2::waiver()) +
      ggplot2::theme_bw() +
      ggplot2::labs(subtitle = bquote("Abondance moyenne (nb/100"*m^2*")")) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(), 
        axis.text.y   = ggplot2::element_text(size = 9, face = "bold"),
        axis.text.x   = ggplot2::element_text(size = 9, face = "bold", angle = 40, vjust = 0.7),
        axis.title.y = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold", size = 12)
        ) +
      ggplot2::facet_wrap(
        ~ taxon,
        labeller = ggplot2::labeller(taxon = taxon_name))
    
    if (all(taxon_station$mean_abondance < 12)) p else p + 
      ggplot2::geom_text(
      data = taxon_station, 
      ggplot2::aes(
        x = as.factor(annee), 
        y = mean_abondance + st_error_abondance + 1.5, 
        label = round(mean_abondance, 1), 
        group = taxon
      ), 
      size = 2
    )
      
  }), levels(as.factor(taxon_abondance$site)))

  return(taxon_abondance)
  
}
  
# Generate camenbert to have an overview of mean substrat cover in each station 

camenbert_function <- function(data, color) {

  data_pit <- tidyr::gather(data = data, substrat, cover, -site, -annee, -standard_date, -transect, -bleaching)
  max_year <- tapply(data_pit$annee, data_pit$site, max)
  data_pit <- data_pit[data_pit$annee %in% max_year, ]
  data_pit$cover <- (data_pit$cover * 100) / 40

  substrat <- setNames(lapply(levels(as.factor(data_pit$site)), function(site) {
    
    #site = "mtsangamouji"
    message(site)
    
    data_pit_site <- data_pit[data_pit$site == site, ]
    data_pit_site <- data_pit_site |>
      dplyr::group_by(substrat) |>
      dplyr::summarise(mean_cover = mean(cover)) |>
      dplyr::arrange(dplyr::desc(substrat))
    data_pit_site <- data_pit_site[data_pit_site$mean_cover != 0, ]
    
    data_pit_site$lab.ypos <- cumsum(data_pit_site$mean_cover) - 0.5 * data_pit_site$mean_cover 
    
    color = color
    
    data_pit_site <- data_pit_site[order(data_pit_site$substrat), ]
    data_pit_site$color <- color[names(color) %in% data_pit_site$substrat]
    data_pit_site$color <- data_pit_site$color |>
      forcats::fct_inorder()
  
    ggplot2::ggplot(data_pit_site, ggplot2::aes(x = "", y = mean_cover, fill = substrat)) +
      ggplot2::geom_bar(width = 5, stat = "identity", color = "#FFFFFF") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::theme_classic()+
      ggplot2::labs(fill = "Substrat (%)")+
      ggplot2::scale_fill_manual(values = as.vector(data_pit_site$color))+
      ggplot2::geom_text(ggplot2::aes(y = lab.ypos, label = round(mean_cover, 1)),
                         color = "#000000", size = 7, fontface = "bold", check_overlap = TRUE)+
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.margin = ggplot2::margin(l = -25, unit = "pt"),
                     legend.title = ggplot2::element_text(face = "bold", size = 23),
                     legend.text = ggplot2::element_text(size = 22),
                     plot.margin = ggplot2::margin(t = -3, b = -3, l = -1, r = 0, "cm")
                     
                     )
    
  
    }), levels(as.factor(data_pit$site)))
  
  return(substrat)
    
}

get_color_substrat <- function() {
  
  color_substrat <- as.character(paletteer::paletteer_d("basetheme::brutal"))
  names(color_substrat) <- c("SP","NIA", "HC", "SD","RC", "OT", "RB", "RKC", "SI", "SC")
  color_substrat <- color_substrat[order(names(color_substrat))]
  
  return(color_substrat)
  
}

get_french_fish_name <- function() {
  
  french_fish_name <- c(
    bumphead_parrot = "Perroquets à bosse", butterflyfish = "Papillons", haemulidae = "Gaterins",
    humphead_wrasse = "Napoléons", grouper = "Mérous", moray_eel = "Murènes",
    parrotfish = "Perroquets", snapper = "Lutjans"
    )
  
  return(french_fish_name)
  
}

get_french_invert_name <- function() {
  
  french_invert_name <- c(
    banded_coral_shrimp = "Crevette à bande", collector_urchin = "Oursin collecteur", 
    crown_of_thorns = "Acanthasteridae", diadema_urchin = "Oursin diadème", 
    giant_clam = "Bénitier", lobster = "Langouste", 
    pencil_urchin = "Oursin crayon", sea_cucumber = "Holothuroidea", 
    triton = "Triton"
    )
  
  return(french_invert_name)
  
}

coral_station_evolution <- function(data) {
  
  #targets::tar_load(data_pit_may)
  
  data$cc_pourc <- (data$HC * 100) / 40
  
  setNames(lapply(unique(data$site), function(s) {
  
    #s = "bandrele"
    coral_cover <- data[data$site == s, ]
    
    ggpubr::ggboxplot(
      data = coral_cover, 
      x = "annee", 
      y = "cc_pourc",
      add = "jitter",
      xlab = "",
      ylab = "",
      title = "Recouvrement corallien (%)"
      ) +
      ggplot2::ylim(0, 100) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9, face = "bold", angle = 30, vjust = 0.7))
    
    
  }), unique(data$site))
  
}
