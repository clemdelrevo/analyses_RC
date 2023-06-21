get_mayotte_coral_cover_timeline <- function(data_line, reef_type_may) {
  
  #targets::tar_load(data_line)
  #targets::tar_load(reef_type_may)
  
  data_line$reef_type <- NA
  
  data_line$reef_type[data_line$site %in% reef_type_may$fringing_may] <- "fringing"
  data_line$reef_type[data_line$site %in% reef_type_may$barrier_may]  <- "barrier"
  
  may_coral_cover <- data_line |>
    dplyr::group_by(annee, reef_type) |>
    dplyr::summarise(mean_coral_cover = mean(HC),
                     sd_coral_cover = sd(HC))
  
  may_coral_cover$mean_coral_cover <- (may_coral_cover$mean_coral_cover * 100) / 40
  may_coral_cover$sd_coral_cover   <- (may_coral_cover$sd_coral_cover * 100) / 40
  
  return(may_coral_cover)
  
}
