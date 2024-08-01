
get_invert_abondance <- function(invert_region) {
  
  invert <- c(
    "banded_coral_shrimp", "collector_urchin", "crown_of_thorns",
    "diadema_urchin", "giant_clam", "lobster", "pencil_urchin",       
    "sea_cucumber", "triton"
    )
  abondance      <- apply(invert_region[, names(invert_region) %in% invert], 1, sum)
  all_abondance  <- cbind(invert_region[, names(invert_region) %in% c("site", "annee", "transect", "reef_type")], abondance)
  mean_site_abondance <- all_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_abondance <- mean_site_abondance |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))
  
  herbivore_names     <- c("collector_urchin", "diadema_urchin")
  abondance           <- apply(invert_region[, names(invert_region) %in% herbivore_names], 1, sum)
  herbivore_abondance <- cbind(invert_region[, names(invert_region) %in% c("site", "annee", "transect", "reef_type")], abondance)
  mean_site_herbivore <- herbivore_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_herbivore <- mean_site_herbivore |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))
  
  holothuria          <- "sea_cucumber"
  abondance           <- invert_region[, names(invert_region) %in% holothuria]
  holothuria_abondance <- cbind(invert_region[, names(invert_region) %in% c("site", "annee", "transect", "reef_type")], abondance)
  mean_site_holothuria <- holothuria_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_holothuria <- mean_site_holothuria |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))
  
  remarkable_names     <- c("giant_clam", "banded_coral_shrimp", "lobster", "pencil_urchin", "triton", "crown_of_thorns")
  abondance            <- apply(invert_region[, names(invert_region) %in% remarkable_names], 1, sum)
  remarkable_abondance <- cbind(invert_region[, names(invert_region) %in% c("site", "annee", "transect", "reef_type")], abondance)
  mean_site_remarkable <- remarkable_abondance |>
    dplyr::group_by(annee, reef_type, site) |>
    dplyr::summarise(mean_site = mean(abondance))
  
  mean_reef_remarkable <- mean_site_remarkable |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_reef = mean(mean_site),
                     st_error_abondance = plotrix::std.error(mean_site))

  return(list(all = list(mean_site_abondance = mean_site_abondance, mean_reef_abondance = mean_reef_abondance),
              herbivore = list(mean_site_abondance = mean_site_herbivore, mean_reef_abondance = mean_reef_herbivore),
              holothuria = list(mean_site_abondance = mean_site_holothuria, mean_reef_abondance = mean_reef_holothuria),
              remarkable = list(mean_site_abondance = mean_site_remarkable, mean_reef_abondance = mean_reef_remarkable)))
  
}
