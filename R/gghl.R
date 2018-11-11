#' Return highlighted version of ggproto object a stat of geom_.
#' @importFrom ggplot2 ggproto
#' @importFrom purrr pmap
#' @param geom geom_* functions (e.g., geom_point)
#' @return ggproto object
ggproto_hl <- function(geom) {
  stat <- ggplot2:::check_subclass(formals(geom)$stat, "Stat", env = parent.frame())
  ggplot2::ggproto(
    paste0(class(stat)[1], 'HL'),
    stat,
    finish_layer = function(data, ...) stat$finish_layer(data = data[data$highlight, ], ...),
    required_aes = c(stat$required_aes, 'highlight')
  )
}
#' Return lowlighted version of ggproto object a stat of geom_.
#' @param LL aethentics for lowlights specified by list (e.g., list(colour = 'gray', alpha = '0.5'))
#' @inheritParams ggproto_hl
#' @return ggproto object
ggproto_ll <- function(geom, LL) {
  stat <- ggplot2:::check_subclass(formals(geom)$stat, "Stat", env = parent.frame())
  ggplot2::ggproto(
    paste0(class(stat)[1], 'HL'),
    stat,
    finish_layer = function(data, ...) {
      data_ll <- data[!data$highlight, ]
      data_ll[names(LL)] <-
        as.data.frame(
          c(LL, .n = list(numeric(sum(!data$highlight)))),
          stringsAsFactors = FALSE
        )[names(LL)]
      stat$finish_layer(data = data_ll, ...)
    },
    required_aes = c(stat$required_aes, 'highlight')
  )
}
#' Construct highlighted version of geom_*.
#'
#' This function can highlight geom functions who have 'stat' as a parameter.
#' For example, geom_point, geom_rect, are acceptable.
#' However, geom_abline isn't.
#' In addition, geom_smooth does not workds currently.
#'
#' @inheritParams ggproto_ll
#' @importFrom purrr pmap
#' @return function
#' @examples
#' library(ggplot2)
#' geom_point_hl2 <- gghl(geom_point)
#' d <- data.frame(x = 1:5, y = 1, hl = 1:5 == 3)
#' ggplot(
#'   d,
#'   aes(x, y, highlight = hl)
#' ) +
#'   geom_point_hl2()
#' ggplot(
#'   d,
#'   aes(x, y, highlight = hl, color = hl)
#' ) +
#'   gghl(geom_point)()
#' @export
gghl <- function(geom, LL = list(colour = NA)) {
  .LL <- LL
  function(..., LL = .LL) {
    pmap(
      list(stat = list(
        ggproto_ll(geom, LL),
        ggproto_hl(geom)
      )),
      geom,
      ...
    )
  }
}
#' highlighted version of ggplot2::geom_point
#' @param ... arguments passed to geom_point
#' @inheritParams ggproto_ll
#' @importFrom ggplot2 geom_point
#' @export
geom_point_hl <- gghl(geom_point)
#geom_point_hl <- function(...) gghl(geom_point)(...)
#' highlighted version of ggplot2::geom_line
#' @param ... arguments passed to geom_point
#' @inheritParams ggproto_ll
#' @importFrom ggplot2 geom_line
#' @export
geom_line_hl <- gghl(geom_line)

