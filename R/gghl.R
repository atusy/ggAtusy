#' wrapper function to extract method from existing Stat and assign data to be highlighted
#'
#' @importFrom ggplot2 ggproto
#' @importFrom purrr pmap
#' @param nm name of method in the ggproto object to extract
#' @param stat ggproto object of class Stat to inherit from
#' @return function
function_hl <- function(nm, stat) {
  function(data, ...) stat[[nm]](data = data[data$highlight, ], ...)
}
#' Return highlighted version of ggproto object a stat of geom_.
#' @param geom geom_* functions (e.g., geom_point)
#' @return ggproto object
ggproto_hl <- function(geom) {
  stat <- ggplot2:::find_subclass("Stat", formals(geom)$stat, parent.frame())
  ggplot2::ggproto(
    paste0(class(stat)[1], 'HL'),
    stat,
    compute_group = function_hl('compute_group', stat),
    compute_layer = function_hl('compute_layer', stat),
    compute_panel = function_hl('compute_panel', stat),
    required_aes = c(stat$required_aes, 'highlight')
  )
}
#' wrapper function to extract method from existing Stat and assign data to be lowlighted
#' @inheritParams function_hl
#' @param LL list of aethentics of lowlight objects (e.g., list(colour = NA))
#' @return function
function_ll <- function(nm, stat, LL) {
  function(data, ...) {
    data <- data[!data$highlight, ]
    data[names(LL)] <- LL
    stat[[nm]](data = data, ...)
  }
}
#' Return lowlighted version of ggproto object a stat of geom_.
#' @inheritParams function_ll
#' @inheritParams ggproto_hl
#' @return ggproto object
ggproto_ll <- function(geom, LL) {
  stat <- ggplot2:::find_subclass("Stat", formals(geom)$stat, parent.frame())
  ggplot2::ggproto(
    paste0(class(stat)[1], 'HL'),
    stat,
    compute_group = function_ll('compute_group', stat, LL),
    compute_layer = function_ll('compute_layer', stat, LL),
    compute_panel = function_ll('compute_panel', stat, LL),
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
#' @inheritParams function_ll
#' @inheritParams ggproto_hl
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
  function(...) {
    purrr::pmap(
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
#' @importFrom ggplot2 geom_point
#' @export
geom_point_hl <- gghl(geom_point)
#geom_point_hl <- function(...) gghl(geom_point)(...)
#' highlighted version of ggplot2::geom_line
#' @param ... arguments passed to geom_point
#' @importFrom ggplot2 geom_line
#' @export
geom_line_hl <- gghl(geom_line)

