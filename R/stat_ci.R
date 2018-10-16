#' Confidence interval of smoothed density estimates
#' @param mapping NULL
#' @param ... other arguments described in stat_density
#' @param ci confidence interval. 0.95 in default
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = rnorm(100)), aes(x)) +
#' stat_density() +
#' stat_ci(fill = 'orange')
#' @export
stat_ci <- function(mapping = NULL, ..., ci = 0.95)
  stat_ci2(mapping = mapping, ..., ci = ci)

find_boundary <- function(data, bool, lim) {
  if(any(data$x == lim)) return(NULL)
  neighbors <- data[sum(bool) + c(0, 1), ]
  colSums(neighbors * rev(abs((lim - neighbors$x) / diff(neighbors$x))))
}

#' @importFrom ggplot2 ggproto
ggproto <- ggplot2::ggproto

#' Stat which shows CI of density plot
#' @importFrom ggplot2 StatDensity
#' @format NULL
#' @usage NULL
#' @export
StatCI <- ggproto(
  "StatCI",
  ggplot2::StatDensity,
  compute_group = function(data, self, ci = 0.95, bw, adjust, kernel, n, trim, ...) {
    # calculate density based on StatDensity
    res <- self$super()$compute_group(
        data = data,
        bw = bw,
        adjust = adjust,
        kernel =kernel,
        n = n,
        trim = trim,
        ...
      )

    # quantile intervals to trim res
    Q <- quantile(data$x, 0.5 + ci / c(-2, 2))
    L <- Q[1] <= res$x
    U <- res$x <= Q[2]

    # return trimmed res
    rbind(
      find_boundary(res, !L, Q[1]),
      res[L & U, ],
      find_boundary(res, U, Q[2])
    )
  }
)

#' @importFrom ggplot2 stat_density
stat_ci2 <- ggplot2::stat_density
formals(stat_ci2) <- c(
  formals(ggplot2::stat_density),
  StatDensity = StatCI
)

