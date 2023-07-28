
get_fish_abondance <- function(fish_may) {
  
  #targets::tar_load(data_fish)

  fish <- c("bumphead_parrot", "butterflyfish", "grouper", "haemulidae", "humphead_wrasse", "moray_eel", "parrotfish", "snapper")
  
  abondance     <- apply(fish_may[, names(fish_may) %in% fish], 1, sum)
  tot_abondance <- cbind(fish_may[, names(fish_may) %in% c("site", "annee", "transect", "reef_type")], abondance)
  
  mean_abondance <- tot_abondance |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(abondance),
                     st_error_abondance = plotrix::std.error(abondance))
  
  return(list(tot_abondance = tot_abondance, mean_abondance = mean_abondance))
  
}

get_fish_trophic_abondance <- function(fish_may) {
  
  #targets::tar_load(data_fish)
  
  herbivore <- fish_may[, names(fish_may) %in% c("site", "annee", "transect", "reef_type", "parrotfish")]
  mean_herbivore <-  herbivore |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(parrotfish),
                     st_error_abondance = plotrix::std.error(parrotfish))
  
  carnivore <- c("haemulidae", "moray_eel", "snapper", "grouper")
  abondance_carnivore <- apply(fish_may[, names(fish_may) %in% carnivore], 1, sum)
  tot_abondance_carnivore <- cbind(fish_may[, names(fish_may) %in% c("site", "annee", "transect", "reef_type")], abondance_carnivore)
  mean_carnivore <- tot_abondance_carnivore |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(abondance_carnivore),
                     st_error_abondance = plotrix::std.error(abondance_carnivore))
  
  corallivore <- fish_may[, names(fish_may) %in% c("site", "annee", "transect", "reef_type", "butterflyfish")]
  mean_corallivore <-  corallivore |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(butterflyfish),
                     st_error_abondance = plotrix::std.error(butterflyfish))
  
  return(list(herbivore = herbivore, mean_herbivore = mean_herbivore,
              carnivore = tot_abondance_carnivore, mean_carnivore = mean_carnivore,
              corallivore = corallivore, mean_corallivore = mean_corallivore))
  
}
