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
  if(any(unique(cc_pit$reef_type) == "intern")) {
    cc_pit$annee[cc_pit$annee == "2021"] <- "2021-2022"
    cc_pit$annee[cc_pit$annee == "2022"] <- "2021-2022"
  }
  
  mean_pourc_cc <- cc_pit |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_cover = mean(pourc_hc),
                     st_error_cover = plotrix::std.error(pourc_hc))
  
  return(list(cc_pit = cc_pit, mean_pourc_cc = mean_pourc_cc))
  
}

# --- MAYOTTE ------------------------------------------------------------------

get_cc_timeline_may <- function(pit_may) {
  
  #targets::tar_load(pit_may)
  
  cc_timeline_function(data = pit_may)
  
}

# --- REUNION ------------------------------------------------------------------

get_cc_timeline_run <- function(pit_run) {
  
  #targets::tar_load(pit_run)
  
  cc_timeline_function(data = pit_run)
  
}