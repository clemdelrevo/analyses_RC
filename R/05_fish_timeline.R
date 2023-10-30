
#' Trophic fish abundance ------------------------------------------------------
#'
#' @description
#' Calculate the mean trophic abundance for herbivore, carnivore and corallivore
#' 
#' @param fish_region 
#'
#' @return a list for each trophic level
#' 
#' @export

get_fish_abondance <- function(fish_region) {
  
  fish           <- c("bumphead_parrot", "butterflyfish", "grouper", "haemulidae", "humphead_wrasse", "moray_eel", "parrotfish", "snapper")
  abondance      <- apply(fish_region[, names(fish_region) %in% fish], 1, sum)
  all_abondance  <- cbind(fish_region[, names(fish_region) %in% c("site", "annee", "transect", "reef_type")], abondance)
  mean_abondance <- all_abondance |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(abondance),
                     st_error_abondance = plotrix::std.error(abondance))
  
  herbivore_abondance <- fish_region[, names(fish_region) %in% c("site", "annee", "transect", "reef_type", "parrotfish")]
  colnames(herbivore_abondance) <- c("site", "annee", "transect", "abondance", "reef_type")
  mean_herbivore      <-  herbivore_abondance |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(abondance),
                     st_error_abondance = plotrix::std.error(abondance))
  
  carnivore_names     <- c("haemulidae", "moray_eel", "snapper", "grouper")
  abondance           <- apply(fish_region[, names(fish_region) %in% carnivore_names], 1, sum)
  carnivore_abondance <- cbind(fish_region[, names(fish_region) %in% c("site", "annee", "transect", "reef_type")], abondance)
  mean_carnivore      <- carnivore_abondance |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(abondance),
                     st_error_abondance = plotrix::std.error(abondance))
  
  corallivore_abondance <- fish_region[, names(fish_region) %in% c("site", "annee", "transect", "reef_type", "butterflyfish")]
  colnames(corallivore_abondance) <- c("site", "annee", "transect", "abondance", "reef_type")
  mean_corallivore      <-  corallivore_abondance |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_abondance = mean(abondance),
                     st_error_abondance = plotrix::std.error(abondance))
  
  return(list(all = list(tot_abondance = all_abondance, mean_abondance = mean_abondance),
              herbivore = list(tot_abondance = herbivore_abondance, mean_abondance = mean_herbivore),
              carnivore = list(tot_abondance = carnivore_abondance, mean_abondance = mean_carnivore),
              corallivore = list(tot_abondance = corallivore_abondance, mean_abondance = mean_corallivore)))
  
}

# --- MAYOTTE ------------------------------------------------------------------

get_fish_abondance_may <- function(fish_may) {
  
  #targets::tar_load(fish_may)
  get_fish_abondance(fish_region = fish_may)
  
}
