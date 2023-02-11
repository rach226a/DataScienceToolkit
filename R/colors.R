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


#' Generate distint colors
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @param n The number of colors to generate
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' distinct_color_ramp(4)
distinct_color_ramp <- function(n){
  ## Generate up to 74 distinct colors
  qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  if(n > 74){
    message("n > 74 colors, colors will repeat")
    return(sample(col_vector, n, replace = T))
  } else{
    return(sample(col_vector, n, replace = F))
  }
}
