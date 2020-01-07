
standard_theme <- ggplot2::theme(plot.title  =  ggplot2::element_text(size  =  sizee, hjust  =  0.5),
               plot.subtitle  =  ggplot2::element_text(hjust  =  0.5),
               text  =  ggplot2::element_text(size = 12, family = "Source Sans Pro Semibold"),
               panel.border  =  ggplot2::element_blank(),
               axis.text.x  =  ggplot2::element_text(angle  =  angle, hjust  =  0.5),
               panel.grid.major  =  ggplot2::element_blank(),
               panel.grid.minor  =  ggplot2::element_line(size  =  0.6,colour  = "gray"),
               axis.line.x  =  ggplot2::element_line(color = "gray", size  =  1),
               legend.position = "right",legend.title  =  ggplot2::element_blank())
