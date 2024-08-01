
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
  mean_site_abondance <- all_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_abondance <- mean_site_abondance |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))
  
  herbivore_abondance <- fish_region[, names(fish_region) %in% c("site", "annee", "transect", "reef_type", "parrotfish")]
  colnames(herbivore_abondance) <- c("site", "annee", "transect", "abondance", "reef_type")
  mean_site_herbivore <- herbivore_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_herbivore <- mean_site_herbivore |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))
  
  carnivore_names     <- c("haemulidae", "moray_eel", "snapper", "grouper")
  abondance           <- apply(fish_region[, names(fish_region) %in% carnivore_names], 1, sum)
  carnivore_abondance <- cbind(fish_region[, names(fish_region) %in% c("site", "annee", "transect", "reef_type")], abondance)
  mean_site_carnivore <- carnivore_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_carnivore <- mean_site_carnivore |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))
  
  corallivore_abondance <- fish_region[, names(fish_region) %in% c("site", "annee", "transect", "reef_type", "butterflyfish")]
  colnames(corallivore_abondance) <- c("site", "annee", "transect", "abondance", "reef_type")
  mean_site_corallivore <- corallivore_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_corallivore <- mean_site_corallivore |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))
  
  return(list(all = list(mean_site_abondance = mean_site_abondance, mean_reef_abondance = mean_reef_abondance),
              herbivore = list(mean_site_abondance = mean_site_herbivore, mean_reef_abondance = mean_reef_herbivore),
              carnivore = list(mean_site_abondance = mean_site_carnivore, mean_reef_abondance = mean_reef_carnivore),
              corallivore = list(mean_site_abondance = mean_site_corallivore, mean_reef_abondance = mean_reef_corallivore)))
  
}
