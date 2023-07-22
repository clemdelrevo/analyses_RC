fish_station <- function(data_fish) {
  
  fish_station <- tidyr::gather(data = data_fish, fish, abondance, -site, -annee, -transect)
  
  
  max_year <- tapply(fish_station$annee, fish_station$site, max)
  
  fish_station <- fish_station[fish_station$annee %in% max_year, ]
  
  fish_station <-  fish_station |>
    dplyr::group_by(site, annee, fish) |>
    dplyr::summarise(mean_abondance = mean(abondance),
                     sd_abondance = sd(abondance))
  
  fish_station_pes.2 <- fish_station[fish_station$site == "pes.deux", ]
  
  
  ggplot2::ggplot()+
    ggplot2::geom_col(data= fish_station_pes.2, ggplot2::aes(x = fish, y = mean_abondance, fill = fish))+
    ggplot2::coord_flip()
  
}
