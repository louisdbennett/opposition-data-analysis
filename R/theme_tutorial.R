#' Custom themes
#' 
#' An example of how to extend and customise ggplot2 themes
#' 
#' @import ggplot2
theme_tutorial <- function(font = 'raleway') {
  ggplot2::theme_void() %+replace% ggplot2::theme(
    text = ggplot2::element_text(family = font),
    plot.title = ggplot2::element_text(face = 'bold', size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
  )
}
