cc_timeline_function <- function(data, fringing, barrier, intern) {
  
  data$reef_type <- NA
  
  data$reef_type[data$site %in% fringing] <- "fringing"
  data$reef_type[data$site %in% barrier]  <- "barrier"
  data$reef_type[data$site %in% intern]   <- "intern"
  
  data$pourc_hc <-  (data$HC * 100) / 40
  
  cc_line <- data[, names(data) %in% c("site", "annee", "pourc_hc", "reef_type")]
  
  mean_pourc_cc <- data |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_cover = mean(pourc_hc),
                     st_error_cover = plotrix::std.error(pourc_hc))
  
  return(list(cc_line = cc_line, mean_pourc_cc = mean_pourc_cc))
  
}

get_mayotte_cc_timeline <- function(data_line_may, reef_type_may) {
  
  #targets::tar_load(data_line_may)
  #targets::tar_load(reef_type_may)
  
  cc_timeline_function(data = data_line_may,
                       fringing = reef_type_may$fringing_may,
                       barrier = reef_type_may$barrier_may,
                       intern = reef_type_may$intern_may)
  
}

get_reunion_cc_timeline <- function(data_line_run, reef_type_run) {
  
  #targets::tar_load(data_line_run)
  #targets::tar_load(reef_type_run)
  
  cc_timeline_function(data = data_line_run)
  
}