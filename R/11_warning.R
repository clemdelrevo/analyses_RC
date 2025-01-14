get_bleaching <- function() {
  
  #targets::tar_load(data_pit_may)
  
  last_year <- max(data_pit_may$annee)
  
  last_survey <- data_pit_may[data_pit_may$annee == last_year, ]
  last_bleaching <- last_survey |>
    dplyr::group_by(site, transect) |>
    dplyr::mutate(
      pourc_bleaching = (bleaching * 100) / HC
    ) |>
    dplyr::group_by(site) |>
    dplyr::summarise(
      mean = mean(pourc_bleaching),
      ec_t = sd(pourc_bleaching),
      type = "bleaching"
    )
  
  last_mortality <- last_survey |>
    dplyr::group_by(site, transect) |>
    dplyr::mutate(
      pourc_rkc = (RKC * 100) / 40,
    ) |>
    dplyr::group_by(site) |>
    dplyr::summarise(
      mean = mean(pourc_rkc),
      ec_t = sd(pourc_rkc),
      type = "mortality"
    )
  
  bm <- rbind(last_bleaching, last_mortality)
  bm <- bm[order(bm$mean, decreasing = TRUE),]
  bm$site <- forcats::fct_inorder(bm$site)
  bm <- bm[bm$mean != 0, ]
  
  # Choose to remove site
  bm <- bm[bm$site != "dzoumogne", ]
  
  ggplot2::ggplot(bm, ggplot2::aes(x = site, y = mean, fill = type, group = type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = site, ymin = mean - ec_t, max = mean + ec_t), 
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 0.5,
      width = 0.2
      ) +
    ggplot2::labs(fill = "", y = "% moyenne ± écart type", x = "") +
    ggplot2::scale_fill_discrete(labels = c("% de corail dur blanchis", "% de corail mort récemment")) +
    ggplot2::coord_flip(ylim = c(0, NA), expand = 0) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "top"
    )

}

get_nia_cover <- function(data_pit_may) {

  #targets::tar_load(data_pit_may)
  
  site <- c(
    "bandrele", "boa", "boueni", "dzoumogne", "longoni", "majikavo", "mbouzi",       
    "mtsangamouji", "ngouja", "pes.deux", "pes.onze", "sakouli", "saziley",      
    "tanaraki", "tzoundzou"
  )
  
  correct_name <- stringr::str_to_upper(site)
  correct_name <- stringr::str_replace(correct_name, "PES.DEUX", "PASSE EN S BOUÉE 2")
  correct_name <- stringr::str_replace(correct_name, "PES.ONZE", "PASSE EN S BOUÉE 11")
  correct_name <- stringr::str_replace(correct_name, "MBOUZI", "M'BOUZI")
  correct_name <- stringr::str_replace(correct_name, "MTSANGAMOUJI", "M'TSANGAMOUJI")
  correct_name <- stringr::str_replace(correct_name, "BANDRELE", "BANDRÉLÉ")
  correct_name <- stringr::str_replace(correct_name, "NGOUJA", "N'GOUJA")
  
  names(correct_name) <- site
  
  data_pit_may$nia_pourc <- (data_pit_may$NIA * 100) / 40
  
  data_pit_may$annee[data_pit_may$annee == "2021-2022"] <- "21-22"
  data_pit_may$annee[data_pit_may$annee == "2023-2024"] <- "23-24"
  
  data_pit_may$color <- "black"
  data_pit_may$color[data_pit_may$annee == "23-24"] <- "red"
  
  ggplot2::ggplot(data = data_pit_may, ggplot2::aes(x = annee, y = nia_pourc, fill = color)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "", y = "", title = "Macroalgues %") +
    ggplot2::scale_fill_manual(values = c("#FFFFFF", "#ff0000")) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(
      ~ site,
      labeller = ggplot2::labeller(site = correct_name)
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#bdffd5"),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x   = ggplot2::element_text(size = 7.5, angle = 40, vjust = 0.7),
      legend.position = "none"
    ) 
}