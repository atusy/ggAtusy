#' StatErr
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Stat
#' @export
StatErr <- ggproto(
  "StatErr",
  ggplot2::Stat,
  required_aes = c('xmin', 'x', 'xmax', 'ymin', 'y', 'ymax'),
  compute_group = function(data, scales) {
    #横長データを縦長にするイメージ
    data.frame(
      x    = c(data$xmin, data$x),
      xend = c(data$xmax, data$x),
      y    = c(data$y,    data$ymin),
      yend = c(data$y,    data$ymax)
    )[c(matrix(1:(2 * nrow(data)), nrow = 2, byrow = TRUE)), ]
  }
)
#' stat_error
#' A function to draw both vertical and horizontal
#' @param mapping NULL
#' @param data NULL
#' @param geom 'segment'
#' @param position 'identitiy'
#' @param na.rm FALSE
#' @param show.legend NA
#' @param inherit.aes TRUE
#' @param ... other arguments passed to ggplot2::layer
#' @importFrom ggplot2 layer
#' @example
#'
#' @export
stat_err <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {
  layer(
    stat = StatErr, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
