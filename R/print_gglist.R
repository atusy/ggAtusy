#' print list of ggplot objects
#' @param x List of ggplot objects
#' @param strategy See `strategy` in `...``
#' @inheritDotParams future::plan
#'
#' @importFrom future plan multiprocess
#' @importFrom furrr future_map
#' @importFrom ggplot2 ggplot_build ggplot_gtable
#' @importFrom grid grid.draw grid.newpage
#' @importFrom magrittr %>%
#' @importFrom purrr walk
print_gglist <- function(x, strategy = multiprocess, ...) {
  current_plan <- plan()
  plan(strategy, ...)
  future_map(x, ~ ggplot_gtable(ggplot_build(.x))) %>%
    walk(function(x) {
      grid.newpage()
      grid.draw(x)
    })
  plan(current_plan)
  invisible(x)
}

#' @export
print.gglist <- print_gglist
