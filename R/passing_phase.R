#' Passing Clusters
#' 
#' A function to plot passes by their most common clusters
#' 
#' @param passes A dataframe of the passes, containing the columns x, y, end.x, end.y & cluster
#' @param n.clust Number of top clusters to take
#' @param nbins,colour,font Used to control the aesthetics 
#' 
#' @export
create_pass_clusters_chart <- function(passes, n.clust = 5, nbins = 1000, colour = '#007a3e', font = 'raleway') {
  top_clusters <- passes |>  
    dplyr::count(cluster) |> 
    dplyr::arrange(dplyr::desc(n)) |> 
    dplyr::slice(1:n.clust) |> 
    dplyr::pull(cluster)

  pass_clusters <- passes[passes$cluster %in% top_clusters, ]
  
  SBpitch::create_Pitch(grass_colour = '#ffffff', background_colour = '#ffffff', goaltype = 'box') +
    ggplot2::geom_density2d_filled(
      data = passes,
      ggplot2::aes(x = x, y = y),
      bins = nbins,
      breaks = seq(0.2, 1, length.out = nbins),
      alpha = 0.8,
      contour_var = "ndensity",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = colorRampPalette(c("#ffffff", colour))(nbins),
      guide = "none"
    ) +
    ggplot2::geom_segment(
      data = pass_clusters,
      ggplot2::aes(
        x = x,
        y = y,
        xend = end.x,
        yend = end.y,
        colour = colour
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")),
      alpha = 0.6
    ) +
    ggplot2::labs(
      caption = "The heatmap in the background represents the locations of all passes"
    ) +
    theme_tutorial(font = font) +
    ggplot2::theme(
      legend.position = 'none'
    )
}
