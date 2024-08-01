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