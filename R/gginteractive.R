#' Convert ggplot2 to plotly more easily
#'
#' @param interactive Wheter to convert ggplot2 to plotly (default: `TRUE`).
#' @inheritDotParams plotly::ggplotly
#'
#' @importFrom purrr partial
#' @importFrom plotly ggplotly
#' @export
gginteractive <- function(interactive = TRUE, ...) {
  structure(
    if(interactive) partial(ggplotly, ...) else identity,
    class = c("gginteractive", "function")
  )
}

#' ggplot_add for gginteractive class
#' @inherit ggplot2::ggplot_add
#' @export
ggplot_add.gginteractive <- function (object, plot, object_name) object(plot)
