#' A Function that converts a table to data.frame.
#'
#' @param tab A table summary generated eg table of variable x
#' @param sort_values Sorts by Count column.Options;"normal","desc" and "asc". Default is "normal"
#' @param add_totals Adds additional row of Totals. Default : TRUE
#' @param dcm_place Decides the number of decimal places. Default: 0
#' @return A data frame structure of the table provided
#' @export

tab_to_df <- function(tab, sort_values = "normal", add_totals = TRUE, dcm_place = 0) {
  df <- as.data.frame(as.matrix(tab))
  df1  =  data.frame(
    Variable  =  row.names(df),
    Count  =  df[, 1],
    Percent  =  round((df[, 1] / sum(df[, 1])) * 100, dcm_place)
  )
  library(dplyr) # for the %>% symbols to be loaded
  if (sort_values == "desc") {
    df1  =  df1 %>% dplyr::arrange(desc(Count))
  } else if (sort_values == "asc") {
    df1  =  df1 %>% dplyr::arrange(Count)
  } else{
    df1 = df1
  }

  if (add_totals){
    df2 = data.frame(Variable = "Total",Count  = sum(df1$Count),Percent  =  sum(df1$Percent))
    df1 = rbind(df1,df2)
  }
  return(df1)
}




# Display tables neatly function
# input: Provide a base r tabulation as 'tab' and the fuction does the rest

tabplot <-
  function(df,
           x,
           title1 = "variable",
           dcm = 0,
           angle = 0,
           sizee = 12,
           flip = FALSE,
           filter_perc = 0,
           xlabell = "") {
    df <- as.data.frame(as.matrix(table(df[, x])))
    df1  =  data.frame(
      variable  =  row.names(df),
      values  =  df[, 1],
      Percent  =  round((df[, 1] / sum(df[, 1])) * 100, dcm)
    )
    res  =  df1 %>% dplyr::arrange(desc(values))

    #barplot_func(var1  =  res[,1], var2  =  res$values,title  =  title1,angle  =  0, xlablabel  =  "")
    tabb = kableExtra::kable_styling(
      knitr::kable(res, col.names  =  c(title1, "Count (N)", "Percentage (%)")),
      bootstrap_options  =  "striped",
      full_width  =  F
    )
    res = filter(res, Percent >= filter_perc)
    plot = ggplot2::ggplot(res, ggplot2::aes_string(
      x = paste0("reorder(", "variable", ",Percent)")  ,
      y = "Percent"
    )) + ggplot2::geom_bar(stat  =  "identity",
                  position  =  ggplot2::position_dodge(),
                  fill = "#0033A1") + ggplot2::geom_text(ggplot2::aes(variable, Percent, label  = paste(Percent)),
                                                hjust  =  0.5,
                                                vjust = -0.3) + ggplot2::ylab("Percent") + ggplot2::xlab("") + ggthemes::theme_hc() + ggplot2::theme(
                                                  plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                                                  plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                                                  text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                                                  panel.border  =  ggplot2::element_blank(),
                                                  axis.text.x  =  ggplot2::element_text(angle  =  0, hjust  =  0.5),
                                                  panel.grid.major  =  ggplot2::element_blank(),
                                                  panel.grid.minor  =  ggplot2::element_line(size  =  0.6, colour  = "gray"),
                                                  axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                                                  legend.position = "right",
                                                  legend.title  =  ggplot2::element_blank()
                                                )  +
      ggplot2::ggtitle(title1) +
      ggplot2::xlab(xlabell)

    if (flip) {
      plot = ggplot2::ggplot(res, aes_string(
        x = paste0("reorder(", "variable", ",Percent)")  ,
        y = "Percent"
      )) + ggplot2::geom_bar(stat  =  "identity",
                    position  =  ggplot2::position_dodge(),
                    fill = "#0033A1") + ggplot2::geom_text(ggplot2::aes(variable, Percent, label  = paste(Percent)),
                                                  hjust  =  -0.5,
                                                  vjust = -0.4) + ggplot2::ylab("Percent") + ggplot2::xlab("") + ggthemes::theme_hc() + ggplot2::theme(
                                                    plot.title  =  ggplot2::element_text(size  =  12, hjust  =  0.5),
                                                    plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
                                                    text  =  ggplot2::element_text(size = 10, family = "Source Sans Pro Semibold"),
                                                    panel.border  =  ggplot2::element_blank(),
                                                    axis.text.x  =  ggplot2::element_text(angle  =  0, hjust  =  0.5),
                                                    panel.grid.major  =  ggplot2::element_blank(),
                                                    panel.grid.minor  =  ggplot2::element_line(size  =  0.6, colour  = "gray"),
                                                    axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
                                                    legend.position = "right",
                                                    legend.title  =  ggplot2::element_blank()
                                                  )  +
        ggplot2::ggtitle(title1) +
        ggplot2::coord_flip() +
        ggplot2::ylim(c(0, max(res$Percent) + 6)) +
        ggplot2::xlab(xlabell)
    }
    return(list(tabb, plot))
  }





barplot_flip_filter_func <- function(data1,var1,var2, xlablabel  =  " ",title  =  "",angle  =  0, flip  =  FALSE, sizee  =  12,ylimm  =  c(0,100)){

  data1 <- transform(data1,
                     var1  =  reorder(var1, var2))

  plot <-
    ggplot2::ggplot(data1, ggplot2::aes(x  =  var1, y  =  var2, fill  =  "black")) + ggplot2::geom_bar(
      stat  =  "identity",
      position  =  ggplot2::position_dodge(),
      fill = "#0071BC"
    ) +
    ggplot2::geom_text(aes(var1, var2, label  = paste(var2, "%", sep = "")),

                       hjust  =  -0.5,
                       vjust =  -0.1) + ggplot2::ylab("Percent") + ggplot2::xlab(xlablabel) +
    ggthemes::theme_hc() + ggplot2::theme(
      plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0.5),
      plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
      text  =  element_text(size = 12, family = "Source Sans Pro Semibold"),
      panel.border  =  ggplot2::element_blank(),
      axis.text.y  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
      panel.grid.major  =  ggplot2::element_blank(),
      panel.grid.minor  =  ggplot2::element_line(size  =  0.6, colour  = "gray"),
      axis.line.y  =  ggplot2::element_line(color = "gray", size  =  1),
      legend.position = "right",
      legend.title  =  ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::coord_flip() +
    ggplot2::ylim(ylimm)

  return(suppressWarnings(print(plot)))
}






