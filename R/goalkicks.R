#' Goalkicks
#' 
#' A function to plot goalkicks & the most common zones hit
#' 
#' @param goalkicks A dataframe of the goalkicks, containing the columns x, y, end.x, end.y & success
#' @param colour,font Used to control the aesthetics 
#' 
#' @export
create_goalkick_chart <- function(goalkicks, colour = '#007a3e', font = 'raleway') {
  zones <- tibble::tibble(
    lane = rep(
      c('Left Wing', 'Left Half Space', 'Center', 'Right Half Space', 'Right Wing'), 3
    ),
    third = c(
      rep('First', 5), rep('Middle', 5), rep('Final', 5)
    ),
    xmin = c(
      rep(0, 5), rep(40, 5), rep(80, 5)
    ),
    xmax = c(
      rep(40, 5), rep(80, 5), rep(120, 5)
    ),
    ymin = rep(seq(0, 64, length.out = 5), 3),
    ymax = rep(seq(16, 80, length.out = 5), 3)
  )

  target_zones <- goalkicks |> 
    dplyr::left_join(
      zones, by = dplyr::join_by(
        end.x >= xmin,
        end.x <= xmax,
        end.y >= ymin,
        end.y <= ymax
      )
    ) |> 
    dplyr::group_by(lane, third) |> 
    dplyr::summarise(
      count = dplyr::n(), .groups = 'drop'
    ) |>
    dplyr::mutate(
      percentage = count / sum(count)
    ) |> 
    dplyr::left_join(zones, by = c('lane', 'third'))
  
  SBpitch::create_Pitch(grass_colour = '#ffffff', background_colour = '#ffffff', goaltype = 'box') +
    ggplot2::geom_rect(
      data = target_zones,
      ggplot2::aes(
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax,
        alpha = percentage,
        fill = percentage
      )
    ) +
    ggplot2::geom_segment(
      data = goalkicks,
      ggplot2::aes(
        x = x,
        y = y,
        xend = end.x,
        yend = end.y,
        colour = success
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")),
      alpha = 0.2
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c(colorRampPalette(c("#ffffff", colour))(10)),
      guide = "none"
    ) +
    ggplot2::scale_alpha(range = c(0, 0.7), guide = "none") +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = c('TRUE' = "#007a3e", 'FALSE' = "#b41313"),
      labels = c('TRUE' = "Successful", 'FALSE' = "Unsuccessful"),
      guide = ggplot2::guide_legend(override.aes = list(alpha = 1))
    ) +
    ggplot2::labs(
      caption = paste0(
        "*The <span style = 'color:",
        colour,
        "'>darker</span> the colour, the more goal kicks to that zone."
      )
    ) +
    theme_tutorial(font = font) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.spacing.x = ggplot2::unit(-0.1, "cm"),
      plot.caption = ggtext::element_markdown()
    )
}
