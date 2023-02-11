#' Generate ggplot2 colors
#' @description reference John Colby's stack overflow response
#' @import grDevices
#' @param n The number of colors to generate
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' gg_color(4)
gg_color <- function(n) {
  ## https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
