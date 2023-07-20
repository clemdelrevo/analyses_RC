get_export_figure <- function(may_diff_cc_map, may_diff_cc_dot) {
  
  
  dir.create("outputs/map/", showWarnings = FALSE)
  dir.create("outputs/figure/", showWarnings = FALSE)
  
  ggplot2::ggsave("outputs/map/diff_cc_may.png", plot = may_diff_cc_map, dpi = 500,
                  width = 10, height = 8)
  
  ggplot2::ggsave("outputs/figure/dot_diff_cc_may.png", plot = may_diff_cc_dot, dpi = 500,
                  width = 10, height = 8)
  
}