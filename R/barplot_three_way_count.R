#' A function  that plots a three way barplot with labels (counts) see example

#' @param df A data frame with x variable, y variable and the factor variable.
#' @param x_variable the variable that will form the x-axis
#' @param y_variable the variable that will form the y-axis
#' @param groupby_variable the factor variable that holds the groupings
#' @param title the title of the plot
#' @param xlablabel the x label of the plot
#'  @param ylablabel the y label of the plot
#'  @param scale_values Specify the factor groups with the colours you would like them to take eg "MMale"  =  "#66AAD7"
#'  @param sizee the size of title
#'  @param angle the of lables
#'  @param sizelabel the size of labels


#'
#' @examples
#'
#'
#' @export
#'
threeway_barplot_count <- function(df,x_variable, y_variable,groupby_variable, title, xlablabel  =  "",ylablabel  =  "Percent",scale_values  =  c("My spouse"  =  "#66AAD7", "My spouse & I"  =  "#0071BC","Myself"  =  "#00558D","Refuse to answer"  =  "#CCE3F2"), sizee  =  12,angle = 0,sizelabel = 3){
  ggplot2::ggplot(df, mapping  =  aes(x  =  {{x_variable}}, y  =  {{y_variable}}, fill  =  {{groupby_variable}}))  +
    ggplot2::geom_bar(stat  =  "identity", position  =  ggplot2::position_dodge(.85),width = 0.75)  +
    ggplot2:: geom_text(ggplot2::aes(label  = Total_count), family = "Source Sans Pro Semibold",vjust  = -0.25, size  =  sizelabel, position  =  ggplot2::position_dodge(0.85)) +
    ggplot2::scale_fill_manual("legend", values  =  scale_values) +
    ggthemes::theme_hc() + ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0.5),
                                          plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                                          text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
                                          panel.border  =  ggplot2::element_blank(),
                                          axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                                          panel.grid.major  =  ggplot2::element_blank(),
                                          panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
                                          axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                                          legend.position = "right",legend.title  =  ggplot2::element_blank()) +
    ggplot2::ggtitle(title) +
    ggplot2::ylab(ylablabel) +
    ggplot2::xlab(xlablabel)
}

#threeway_barplot_count(df  =  res ,x_variable  =  g1a4, y_variable  =  Total_count, groupby_variable  =  g1a6, title  =  "Number of respondents from  Kebelle by Gender", xlablabel  =  "",ylablabel  =  "Percent",scale_values  =  c( "Male"  =  "#0071BC","Female"  =  "#CCE3F2" ), angle = 0)

