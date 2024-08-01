
timeline_graph <- function(data, x, y, color, data2, x2, y2, subtitle) {
  
  # Custom function to format y-axis labels as integers
  integer_labels <- function(x) {
    format(x, nsmall = 0, scientific = FALSE, trim = TRUE)
  }
  
  ggplot2::ggplot()+
    ggplot2::geom_point(data = data, ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]]), size = 1.5, alpha = 0.5, color = color) +
    ggplot2::geom_smooth(data = data, method = "loess", ggplot2::aes(x = as.factor(.data[[x]]), y = .data[[y]], group = 1), color = color, fill = color, alpha = 0.2) +
    ggplot2::geom_point(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), y = .data[[y2]]), color = color, size = 2.5) +
    ggplot2::geom_linerange(data = data2, ggplot2::aes(x = as.factor(.data[[x2]]), ymin = mean_reef - st_error_abondance, ymax = mean_reef + st_error_abondance), color = color) +
    ggplot2::theme_classic() +
    ggplot2::labs(subtitle = subtitle) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = integer_labels) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) + 
    ggplot2::theme(
      axis.title.x  = ggplot2::element_blank(), 
      axis.title.y  = ggplot2::element_blank(),
      legend.title  = ggplot2::element_blank(),
      axis.text.x   = ggplot2::element_text(size = 9, face = "bold", angle = 30, vjust = 0.7),
      axis.text.y   = ggplot2::element_text(size = 12, face = "bold"),
      legend.text   = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, face = "bold")
      )
  
}
