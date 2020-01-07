
threeway_barplot_count <- function(df,x_variable, y_variable,groupby_variable, title, xlablabel  =  "",ylablabel  =  "Percent",scale_values  =  c("My spouse"  =  "#66AAD7", "My spouse & I"  =  "#0071BC","Myself"  =  "#00558D","Refuse to answer"  =  "#CCE3F2"), sizee  =  12,angle = 0,sizelabel = 3){
  ggplot2::ggplot(df, mapping  =  aes(x  =  df[,x_variable], y  =  df[,y_variable], fill  =  df[,groupby_variable]))  +
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
