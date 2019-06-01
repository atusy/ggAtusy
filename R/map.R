#' For post processing
#' @noRd
as_gglist <- function(x) structure(x, class = c('gglist', class(x)))

#' Apply `ggplot()` on a list of data frames or on a (grouped) data frame.
#'
#' @param data A list of data frames or a (grouped) data frame.
#' @param ... Layers to be added to ggplot
#'
#' @return A `gglist` class object which is a list of `gg` class objects
#'
#' @export
map_gg <- function(data = NULL, ...) UseMethod("map_gg")

#' @export
map_gg.default <- function(data = NULL, ...) {
  stop("x must be a (grouped) data frame or a list of data frames")
}

#' @rdname map_gg
#' @export
#' @importFrom dplyr group_map
#' @importFrom ggplot2 ggplot
map_gg.data.frame <- function(data = NULL, ...) {
  layers <- list(...)
  group_map(data, ~ ggplot(.x) + layers)
}

#' @rdname map_gg
#' @export
map_gg.list <- function(data = NULL, ...) {
  if (!any(vapply(data, is.data.frame, TRUE))) NextMethod()
  layers <- list(...)
  lapply(data, ~ ggplot(.x) + layers)
}
