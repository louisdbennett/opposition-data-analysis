
#' Passing Sonars
#' 
#' A function to plot receptions by the change in angle to next pass
#' 
#' @param receptions A dataframe of the passes, containing the column change_in_angle
#' @param nbins,colour,font Used to control the aesthetics 
#' 
#' @export
create_passing_sonars <- function(receptions, nbins = 10, colour = '#007a3e', font = 'raleway') {
  ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data = receptions,
      bins = nbins,
      fill = colour,
      colour = colour,
      alpha = 0.8,
      ggplot2::aes(x = change_in_angle)
    ) +
    ggplot2::xlim(-pi, pi) + 
    ggplot2::coord_polar() +
    theme_tutorial(font) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line()
    )
}
