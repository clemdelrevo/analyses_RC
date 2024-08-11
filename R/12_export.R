get_export_figure <- function(
    cc_evol_may,  
    final_cc_timeline_may, 
    final_fish_timeline_may,
    final_invert_timeline_may,
    cc_survey_may,
    substrat_survey_may,
    fish_survey_may,
    invert_survey_may
    ) {
  
  #targets::tar_load(cc_evol_may)
  #targets::tar_load(final_cc_timeline_may)
  #targets::tar_load(final_fish_timeline_may)
  #targets::tar_load(final_invert_timeline_may)
  #targets::tar_load(substrat_survey_may)
  #targets::tar_load(cc_survey_may)
  #targets::tar_load(fish_survey_may)
  #targets::tar_load(invert_survey_may)
  
  dir.create(here::here("outputs/mayotte/"), showWarnings = FALSE)
  dir.create(here::here("outputs/mayotte/substrat"), showWarnings = FALSE)
  dir.create(here::here("outputs/mayotte/coral_cover"), showWarnings = FALSE)
  dir.create(here::here("outputs/mayotte/fish"), showWarnings = FALSE)
  dir.create(here::here("outputs/mayotte/invert"), showWarnings = FALSE)
  
  # export map ---
  ggplot2::ggsave(here::here("outputs/mayotte/may_map.png"), plot = cc_evol_may$may_map, dpi = 400,
                  width = 8, height = 8)
  
  ggplot2::ggsave(here::here("outputs/mayotte/may_dot.png"), plot = cc_evol_may$may_dot, dpi = 400,
                  width = 10, height = 8)
  
  # export graph timeline survey ---
  ggplot2::ggsave(here::here("outputs/mayotte/may_cc_timeline.png"), plot = final_cc_timeline_may, dpi = 450,
                  width = 15, height = 8)
  
  ggplot2::ggsave(here::here("outputs/mayotte/may_fish_timeline.png"), plot = final_fish_timeline_may, dpi = 450,
                  width = 11, height = 10)
  
  ggplot2::ggsave(here::here("outputs/mayotte/may_invert_timeline.png"), plot = final_invert_timeline_may, dpi = 500,
                  width = 11, height = 10)
  
  message(cli::rule(center = "export des figures de substrat", col = "yellow"))
  
  for (i in names(substrat_survey_may)) {
    #i = "bandrele"
    plot <-  substrat_survey_may[[i]]
    ggplot2::ggsave(paste0("outputs/mayotte/substrat/", i, "_substrat_survey.png"), plot = plot, dpi = 350,
                    width = 8, height = 6)
  }
  
  message(cli::rule(center = "export des figures d'évolution du recouvrement corallien", col = "purple"))
  
  for (i in names(cc_survey_may)) {
    #i = "bandrele"
    plot <-  cc_survey_may[[i]]
    ggplot2::ggsave(paste0("outputs/mayotte/coral_cover/", i, "_coral_cover.png"), plot = plot, dpi = 400,
                    width = 8, height = 4.5)
  }
  
  message(cli::rule(center = "export des figures de d'évolutions de densité de poissons", col = "blue"))
  
  for (i in names(fish_survey_may)) {
    #i = "bandrele"
    plot <-  fish_survey_may[[i]]
    ggplot2::ggsave(paste0("outputs/mayotte/fish/", i, "_fish_survey.png"), plot = plot, dpi = 400,
                    width = 8, height = 6)
  }
  
  message(cli::rule(center = "export des figures de d'évolutions de densité d'invertébrés", col = "green"))
  
  for (i in names(invert_survey_may)) {
    #i = "bandrele"
    plot <-  invert_survey_may[[i]]
    ggplot2::ggsave(paste0("outputs/mayotte/invert/", i, "_invert_survey.png"), plot = plot, dpi = 400,
                    width = 8, height = 6)
  }
  
}
  