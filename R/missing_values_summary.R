#'
#' @param df A data frame to be examined the missingness level
#' @param perc Percentage of missing values to display
#' @param dcm Number of decimal places to be previewed
#' @param col_names Title names of the columns on the output



#'
#' @examples
#' missing_values_summary(mtcars)
#'
#' @export


missing_values_summary <- function(df,perc = 0,dcm = 2,col_names = c("Features","Count Missing (N)","Percentage Missing (%)")){
  missing_count <- sapply(df, function(x) sum(is.na(x)))
  if (any(missing_count > 0)){
    miss_df <- data.frame(Features = names(missing_count), count_missing = missing_count)%>%
      as_tibble() %>%
      arrange(desc(count_missing)) %>%
      mutate(percent_missing = round(count_missing/nrow(df),dcm)*100,
      percent_missing = cell_spec(percent_missing, "html", color = ifelse(percent_missing > 10, "red", "black"))) %>%
       dplyr::filter(percent_missing > perc)
  } else {
    cat("There is no Missing values in this data frame Enjoy :) No hustle!\n")
  }
}
