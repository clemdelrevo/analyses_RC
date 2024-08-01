#' Percentage coral cover ------------------------------------------------------
#'
#' @description
#' This function calculate the percentage coral cover 
#' for each reef complex for each year
#' 
#' @param data 
#'
#' @return a list of two data frame with : 1- mean percentage coral cover
#' 2- percentage coral cover in each 20m transect 
#' 
#' @export

cc_timeline_function <- function(data) {
  
  data$pourc_hc <- (data$HC * 100) / 40
  
  cc_pit <- data[, names(data) %in% c("site", "annee", "pourc_hc", "reef_type")]
  
  mean_site_cc <- cc_pit |>
    dplyr::group_by(site, annee, reef_type) |>
    dplyr::summarise(mean_cover = mean(pourc_hc))
  
  mean_all_cc <- mean_site_cc |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(
      mean_allcover = mean(mean_cover),
      st_error = plotrix::std.error(mean_cover))
  
  return(list(mean_site_cc = mean_site_cc, mean_all_cc = mean_all_cc))
  
}

# --- MAYOTTE ------------------------------------------------------------------

get_cc_timeline_may <- function(pit_may) {
  
  #targets::tar_load(pit_may)
  
  cc_timeline_function(data = pit_may)
  
}
