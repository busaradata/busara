#' A function  that plots a a bar graph with different variations

#' @param df A data frame with x variable, y variable
#' @param xvar the variable that will form the x-axis
#' @param yvar the variable that will form the y-axis
#' @param ascending Boolean deciding the sorting order of the bars.Default:FALSE
#' @param ordered Boolean deciding whether bars should be sorted or not sorted.Default:FALSE
#' @param errorbar Boolean deciding whether to plot error bars of not. Default:FALSE
#' @param flip Boolean deciding whether to flip the bar plot or not. Default:FALSE
#' @param title the title of the plot
#' @param xlabel the x label of the plot
#' @param ylabel the y label of the plot
#' @param sizee the size of title
#' @param angle the angle position of the lables
#' @param sizelabel the size of labels
#' @param dcm the number of decimal numbers
#' @param ylimm Set the ylim scale



#'
#' @examples
#'
#'
#' @export
#'


bar_graph = function(df, xvar ,yvar, ascending = FALSE, ordered = FALSE, errorbar = FALSE,xlabel = "", ylabel = "", title = "", flip = FALSE, angle = 0, sizee = 12, sizelabel = 4, dcm = 0, ylimm = c(0,NA)){
  if(errorbar){
    if (ordered) {
      if (ascending) {
        if (flip) {
          p = ggplot2::ggplot(df, ggplot2::aes(x = reorder({{xvar}},{{yvar}}), {{yvar}})) +
            ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge",fill = "#0033A1") +
            ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0,vjust = 0),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major.y  =  ggplot2::element_blank(),
                           panel.grid.minor.x  =  ggplot2::element_line(size  =  0.1,colour  = "gray"),
                           axis.line.y  =  ggplot2::element_line(color = "gray", size  =  0.5),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::coord_flip() + ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) +
            ggplot2::ylim(ylimm)
        }else{
          p = ggplot2::ggplot(df, ggplot2::aes(x = reorder({{xvar}},{{yvar}}), {{yvar}})) +
            ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge",fill = "#0033A1") +
            ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major  =  ggplot2::element_blank(),
                           panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
                           axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) + ggplot2::ylim(ylimm) +
            ggplot2::ylim(ylimm)
        }
      }else{
        if (flip) {
          p = ggplot2::ggplot(df, ggplot2::aes(x = reorder({{xvar}},-{{yvar}}), {{yvar}})) +
            ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge",fill = "#0033A1") +
            ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0,vjust = 0),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major.y  =  ggplot2::element_blank(),
                           panel.grid.minor.x  =  ggplot2::element_line(size  =  0.1,colour  = "gray"),
                           axis.line.y  =  ggplot2::element_line(color = "gray", size  =  0.5),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::coord_flip() + ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) +
            ggplot2::ylim(ylimm)
        }else{
          p = ggplot2::ggplot(df, ggplot2::aes(x = reorder({{xvar}},-{{yvar}}), {{yvar}})) +
            ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge",fill = "#0033A1") +
            ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major  =  ggplot2::element_blank(),
                           panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
                           axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) + ggplot2::ylim(ylimm)
        }
      }
    }else{
      if (flip) {
        ggplot2::ggplot(df, ggplot2::aes(x = {{xvar}}, y = {{yvar}})) +
          ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge",fill = "#0033A1") +
          ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
          ggthemes::theme_hc() +
          ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0,vjust = 0),
                         plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                         text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
                         panel.border  =  ggplot2::element_blank(),
                         axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                         panel.grid.major.y  =  ggplot2::element_blank(),
                         panel.grid.minor.x  =  ggplot2::element_line(size  =  0.1,colour  = "gray"),
                         axis.line.y  =  ggplot2::element_line(color = "gray", size  =  0.5),
                         legend.position = "right",legend.title  =  ggplot2::element_blank()) +
          ggplot2::coord_flip() + ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) +
          ggplot2::ylim(ylimm)
      }else{
        p + ggplot2::ggplot(df, ggplot2::aes(x = {{xvar}}, y = {{yvar}})) +
          ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge",fill = "#0033A1") +
          ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
          ggthemes::theme_hc() +
          ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                         plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                         text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                         panel.border  =  ggplot2::element_blank(),
                         axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                         panel.grid.major  =  ggplot2::element_blank(),
                         panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
                         axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                         legend.position = "right",legend.title  =  ggplot2::element_blank()) +
          ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) + ggplot2::ylim(ylimm)
      }
    }
  }else{
    if (ordered) {
      if (ascending) {
        if (flip) {
          p = ggplot2::ggplot(df,ggplot2::aes(x = reorder({{xvar}},{{yvar}}),y = {{yvar}})) +
            ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(),fill = "#0033A1") +
            ggplot2::geom_text(ggplot2::aes({{xvar}},{{yvar}}, label  = paste(round({{yvar}},dcm),"%",sep = "")),position  =  ggplot2::position_dodge(0.85),
                      hjust  =  -0.5,vjust =  0.5,size  =  sizelabel) +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0,vjust = 0),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major.y  =  ggplot2::element_blank(),
                           panel.grid.minor.x  =  ggplot2::element_line(size  =  0.1,colour  = "gray"),
                           axis.line.y  =  ggplot2::element_line(color = "gray", size  =  0.5),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::coord_flip() + ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) +
            ggplot2::ylim(ylimm)
        }else{
          p = ggplot2::ggplot(df,ggplot2::aes(x = reorder({{xvar}},{{yvar}}),y = {{yvar}})) +
            ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(),fill = "#0033A1") +
            ggplot2::geom_text(ggplot2::aes({{xvar}},{{yvar}}, label  = paste(round({{yvar}},dcm),"%",sep = "")),position  =  ggplot2::position_dodge(0.85),
                               hjust  =  0.5,vjust =  -0.5,size  =  sizelabel) +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major  =  ggplot2::element_blank(),
                           panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
                           axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) + ggplot2::ylim(ylimm)
        }

      }else{
        if (flip) {
          p = ggplot2::ggplot(df,ggplot2::aes(x = reorder({{xvar}},-{{yvar}}),y = {{yvar}})) +
            ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(),fill = "#0033A1") +
            ggplot2::geom_text(ggplot2::aes({{xvar}},{{yvar}}, label  = paste(round({{yvar}},dcm),"%",sep = ""   )),position  =  ggplot2::position_dodge(0.85),
                               hjust  =  -0.5,vjust =  0.5,size  =  sizelabel) +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0,vjust = 0),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major.y  =  ggplot2::element_blank(),
                           panel.grid.minor.x  =  ggplot2::element_line(size  =  0.1,colour  = "gray"),
                           axis.line.y  =  ggplot2::element_line(color = "gray", size  =  0.5),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::coord_flip() + ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) +
            ggplot2::ylim(ylimm)
        }else{
          p = ggplot2::ggplot(df,ggplot2::aes(x = reorder({{xvar}},-{{yvar}}),y = {{yvar}})) +
            ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(),fill = "#0033A1") +
            ggplot2::geom_text(ggplot2::aes({{xvar}},{{yvar}}, label  = paste(round({{yvar}},dcm),"%",sep = "")),position  =  ggplot2::position_dodge(0.85),
                               hjust  =  0.5,vjust =  -0.5,size  =  sizelabel) +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                           plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                           text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                           panel.border  =  ggplot2::element_blank(),
                           axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                           panel.grid.major  =  ggplot2::element_blank(),
                           panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
                           axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                           legend.position = "right",legend.title  =  ggplot2::element_blank()) +
            ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) + ggplot2::ylim(ylimm)
        }

      }
    }else{
      if (flip) {
        p = ggplot2::ggplot(df,ggplot2::aes(x = {{xvar}},y = {{yvar}})) +
          ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(),fill = "#0033A1") +
          ggplot2::geom_text(ggplot2::aes({{xvar}},{{yvar}}, label  = paste(round({{yvar}},dcm),"%",sep = "")),position  =  ggplot2::position_dodge(0.85),
                             hjust  =  -0.5,vjust =  0.5,size  =  sizelabel) +
          ggthemes::theme_hc() +
          ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0,vjust = 0),
                         plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                         text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
                         panel.border  =  ggplot2::element_blank(),
                         axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                         panel.grid.major.y  =  ggplot2::element_blank(),
                         panel.grid.minor.x  =  ggplot2::element_line(size  =  0.1,colour  = "gray"),
                         axis.line.y  =  ggplot2::element_line(color = "gray", size  =  0.5),
                         legend.position = "right",legend.title  =  ggplot2::element_blank()) +
          ggplot2::coord_flip() + ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) +
          ggplot2::ylim(ylimm)
      }else{
        p = ggplot2::ggplot(df,ggplot2::aes(x = {{xvar}},y = {{yvar}})) +
          ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(),fill = "#0033A1") +
          ggplot2::geom_text(ggplot2::aes({{xvar}},{{yvar}}, label  = paste(round({{yvar}},dcm),"%",sep = "")),position  =  ggplot2::position_dodge(0.85),
                             hjust  =  0.5,vjust =  -0.5,size  =  sizelabel) +
          ggthemes::theme_hc() +
          ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                         plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                         text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                         panel.border  =  ggplot2::element_blank(),
                         axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
                         panel.grid.major  =  ggplot2::element_blank(),
                         panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
                         axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                         legend.position = "right",legend.title  =  ggplot2::element_blank()) +
          ggplot2::ylab(ylabel) + ggplot2::xlab(xlabel) + ggplot2::ggtitle(title) + ggplot2::ylim(ylimm)
      }
    }


  }

  return(p)
}


#Examples
# df =  diamonds %>% group_by(cut) %>%
#   summarise(average=  mean(y))
#
# bar_graph(df=diamonds,xvar = cut,yvar = y,errorbar = T,ascending = T, flip = T)
#
#bar_graph(df=df,xvar = cut,yvar = average,ascending = F,flip = F,ordered = T,dcm = 2,title = "")
#
#
# ggplot(df,aes(x=reorder(cut,-average),y=average)) +
#   geom_bar(stat = "identity",position = position_dodge())
#


