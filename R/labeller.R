#' Labeller functions to parse TeX syntax
#' @inheritParams ggplot2::label_parsed
#' @inheritDotParams ggplot2::label_parsed -labels
#' @importFrom latex2exp TeX
#' @importFrom ggplot2 label_parsed
label_tex <- function(labels, ...) label_parsed(as.character(latex2exp::TeX(labels), ...))
