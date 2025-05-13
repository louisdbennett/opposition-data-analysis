#' Gamestate Plots
#' 
#' A function to plot a comparison of team level metrics by different gamestates
#' 
#' @param gamestates A dataframe of the metrics by gamestate, with a column called GameState
#' @param xmetric,ymetric Names of metrics to use
#' @param font Used to control the aesthetics 
#' 
#' @export
create_gamestate_scatter <- function(gamestates, metrics, font = 'raleway') {
  ggplot2::ggplot() +
    ggiraph::geom_point_interactive(
      data = gamestates,
      ggplot2::aes(
        x = !!sym(metrics[1]),
        y = !!sym(metrics[2]),
        group = GameState,
        fill = GameState
      ),
      alpha = 0.9,
      colour = "#18191A",
      size = 20,
      shape = 21
    ) +
    ggplot2::scale_fill_manual(
      values = c(Winning = "#007a3e", Drawing = "#f1f1f1", Losing = "#b41313"),
      name = NULL
    ) +
    ggplot2::scale_x_reverse(
      limits = c(0.85, 0.8)
    ) +
    ggplot2::ylim(c(20, 25)) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(names(metrics[1])) +
    ggplot2::ylab(names(metrics[2])) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font),
      plot.title = ggplot2::element_text(face = 'bold', size = 16, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),  
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 20),
      legend.position = 'bottom'
    )
}
