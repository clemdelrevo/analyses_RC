cc_timeline_function <- function(data) {
  
  data$pourc_hc <- (data$HC * 100) / 40
  
  cc_line <- data[, names(data) %in% c("site", "annee", "pourc_hc", "reef_type")]
  
  mean_pourc_cc <- data |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_cover = mean(pourc_hc),
                     st_error_cover = plotrix::std.error(pourc_hc))
  
  return(list(cc_line = cc_line, mean_pourc_cc = mean_pourc_cc))
  
}

get_mayotte_cc_timeline <- function(line_may) {
  
  #targets::tar_load(line_may)
  
  cc_timeline_function(data = line_may)
  
}

get_reunion_cc_timeline <- function(line_run) {
  
  #targets::tar_load(line_run)
  
  cc_timeline_function(data = line_run)
  
}