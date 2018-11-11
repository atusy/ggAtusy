#' @importFrom ggplot2 geom_histogram
ggplot2::geom_histogram

#' Convert a function to calculate number of bins to a function to calculate width of bins.
#' @param x A numeric vector.
#' @param nclass A function that returns number of bins according to x.
#' @noRd
nclass2bw <- function(x, nclass) (max(x) - min(x)) / nclass(x)

#' Calculate binwidth from a numeric vector
#' @importFrom grDevices nclass.scott nclass.Sturges nclass.FD
#' @noRd
bw <- list(
  scott = function(x) nclass2bw(x, nclass.scott),
  Struges = function(x) nclass2bw(x, nclass.Sturges),
  FD = function(x) nclass2bw(x, nclass.FD)
)

#' A wrapper of geom_histogram with a support of choice of binwidth with emprical methods.
#'
#' Supported methods are "Sturges", "Scott", and "Freedman-Diaconis" s' choices.
#' Default is "Scott"
#' @param binwidth The width of binwidth.
#' Can be specified as a numeric value,
#' a function that calculates width from x,
#' or a character of either "scott", "Sturges", "FD".
#' Default is "scott"
#' See details in graphics::hist.
#' @inheritParams ggplot2::geom_histogram
#' @export
geom_hist <- function() {
  if(is.character(binwidth)) binwidth <- bw[[match.arg(binwidth)]]
  geom_histogram(
    mapping = mapping, data = data, stat = stat,
    position = position, ..., binwidth = binwidth, bins = bins,
    na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes
  )
}
formals(geom_hist) <- formals(ggplot2::geom_histogram)
formals(geom_hist)[["binwidth"]] <- names(bw)
