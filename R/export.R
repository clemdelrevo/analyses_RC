get_export_figure <- function(data_line_may, data_fish_may, data_invert_may,
                              may_cc_evol,  
                              may_final_cc_timeline, may_final_fish_timeline,
                              may_substrat_survey, may_fish_survey, may_invert_survey) {
  
  dir.create("outputs/mayotte/", showWarnings = FALSE)
  dir.create("outputs/mayotte/map/", showWarnings = FALSE)
  dir.create("outputs/mayotte/figure/", showWarnings = FALSE)
  dir.create("outputs/mayotte/data/", showWarnings = FALSE)
  
  # export dataframe ---
  write.csv(data_line_may, "outputs/mayotte/data/line_may.csv")
  write.csv(data_fish_may, "outputs/mayotte/data/fish_may.csv")
  write.csv(data_invert_may, "outputs/mayotte/data/invert_may.csv")
  
  # export map ---
  ggplot2::ggsave("outputs/mayotte/map/may_map.png", plot = may_cc_evol$may_map, dpi = 500,
                  width = 10, height = 8)
  
  ggplot2::ggsave("outputs/mayotte/map/may_dot.png", plot = may_cc_evol$may_dot, dpi = 500,
                  width = 10, height = 8)
  
  # export graph timeline survey ---
  ggplot2::ggsave("outputs/mayotte/figure/may_final_cc_timeline.png", plot = may_final_cc_timeline, dpi = 500,
                  width = 17, height = 8)
  
  ggplot2::ggsave("outputs/mayotte/figure/may_final_fish_timeline.png", plot = may_final_fish_timeline, dpi = 500,
                  width = 15, height = 9)
  
  # export graph last survey ---
  dir.create("outputs/mayotte/figure/station_survey", showWarnings = FALSE)
  dir.create("outputs/mayotte/figure/station_survey/substrat", showWarnings = FALSE)
  
  for (i in names(may_substrat_survey)) {
    #i = "bandrele"
    plot <-  may_substrat_survey[[i]]
    ggplot2::ggsave(paste0("outputs/mayotte/figure/station_survey/substrat/", i, "_substrat_survey.png"), plot = plot, dpi = 500,
                    width = 10, height = 9)
  }
  
  dir.create("outputs/mayotte/figure/station_survey/fish", showWarnings = FALSE)
  for (i in names(may_fish_survey)) {
    #i = "bandrele"
    plot <-  may_fish_survey[[i]]
    ggplot2::ggsave(paste0("outputs/mayotte/figure/station_survey/fish/", i, "_fish_survey.png"), plot = plot, dpi = 500,
                  width = 15, height = 5)
  }
  
  dir.create("outputs/mayotte/figure/station_survey/invert", showWarnings = FALSE)
  for (i in names(may_invert_survey)) {
    #i = "bandrele"
    plot <-  may_invert_survey[[i]]
    ggplot2::ggsave(paste0("outputs/mayotte/figure/station_survey/invert/", i, "_invert_survey.png"), plot = plot, dpi = 500,
                  width = 15, height = 5)
  }
  
}