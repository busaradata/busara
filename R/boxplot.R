#' Function to perform simple boxplot

#' @param df A data frame
#' @param variable the variable that will form the x-axis
#' @param title the title of the plot
#' @param xlablabel the x label of the plot
#' @param ylablabel the y label of the plot
#' @param sizee the size of title
#' @param angle the of lables


#'
#' @examples
#' boxplot(df = diamonds, variable = price )
#'
#' @export
#'
boxplot <- function(df,variable, ylablabel  =  " ",xlablabel  =  "",title  =  "",angle  =  0, sizee  =  12){
  plot <- ggplot(df, aes(x  =  "",y  =  {{variable}})) +
    geom_boxplot(outlier.colour = "blue", outlier.shape = 16,
                 outlier.size = 2, notch = FALSE,fill  =  "#0071BC") +
    theme_hc() + theme(plot.title  =  element_text(size  =  sizee, hjust  =  0.5),
                       plot.subtitle  =  element_text(hjust  =  0.5),
                       text  =  element_text(size = 20, family = "Source Sans Pro Semibold"),
                       panel.border  =  element_blank(),
                       axis.text.x  =  element_text(angle  =  angle, hjust  =  0.5),
                       panel.grid.major  =  element_blank(),
                       panel.grid.minor  =  element_line(size  =  0.6,colour  = "gray"),
                       axis.line.x  =  element_line(color = "gray", size  =  1),
                       legend.position = "right",legend.title  =  element_blank()) +
    ggtitle(title) +
    ylab(ylablabel) +
    xlab(xlablabel)
  return(suppressWarnings(print(plot)))
}

