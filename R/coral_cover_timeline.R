get_mayotte_cc_timeline <- function(data_line, reef_type_may) {
  
  #targets::tar_load(data_line)
  #targets::tar_load(reef_type_may)
  
  data_line$reef_type <- NA
  
  data_line$reef_type[data_line$site %in% reef_type_may$fringing_may] <- "fringing"
  data_line$reef_type[data_line$site %in% reef_type_may$barrier_may]  <- "barrier"
  data_line$reef_type[data_line$site %in% reef_type_may$intern_may]  <- "intern"
  
  data_line$pourc_hc <-  (data_line$HC * 100) / 40
  
  cc_line <- data_line[, names(data_line) %in% c("site", "annee", "pourc_hc", "reef_type")]
  
  mean_pourc_cc <- data_line |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_cover = mean(pourc_hc),
                     sd_cover = sd(pourc_hc))
  
  return(list(cc_line = cc_line, mean_pourc_cc = mean_pourc_cc))
  
}
