#' Function to perform simple barplot

#' @param data1 A data frame
#' @param column the variable that will form the x-axis
#' @param title the title of the plot
#' @param xlablabel the x label of the plot
#' @param sizee the size of title
#' @param angle the of lables


#'
#' @examples
#' boxplot(df = diamonds, variable = price )
#'
#' @export
#'
histogram_summaries <- function(data1, column, title = "",xlablabel = "") {
  bw <-
    2 * IQR(as.numeric(as.matrix(data1[, column])), na.rm = T) / length(as.numeric(as.matrix(data1[, column]))) ^
    (1 / 3)

  p2 =  ggplot2::ggplot(data  =  data1, ggplot2::aes(x  =  !!rlang::sym(column))) + ggplot2::geom_histogram(
    ggplot2::aes(y = ..density..),
    binwidth = bw,
    colour  =  "#0033A1",
    fill  =  "#0033A1",
  ) + ggplot2::geom_density(col = "#0033A1") +
    ggthemes::theme_hc() + ggplot2::xlab(xlablabel) +
    ggplot2::ylab("Frequency") + ggplot2::ggtitle(title)


  return(list(p2))
}

#histogram_summaries(data1 =diamonds,column = "price")
