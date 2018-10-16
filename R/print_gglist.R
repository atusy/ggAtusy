#' print list of ggplot objects
#' @param gglist list of ggplot objects
#' @param ... other arguments passed to pforeach
#' @importFrom purrr walk
#' @importFrom pforeach pforeach
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
print_gglist <- function(gglist, ...) {
  walk(
    pforeach(gglist = gglist, ..., .combine = c)({ # 計算を並列
      list(ggplot_gtable(ggplot_build(gglist)))
    }),
    function(gg) { # 順次出力
      grid.newpage()
      grid.draw(gg)
    }
  )
}
