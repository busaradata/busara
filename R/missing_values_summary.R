missing_values_summary <- function(df,perc = 0,dcm = 2,col_names = c("Features","Count Missing (N)","Percentage Missing (%)", "Comment")){
  missing_count <- sapply(df, function(x) sum(is.na(x)))
  if (any(missing_count > 0)){
    miss_df <- data.frame(Features = names(missing_count), count_missing = missing_count)%>%
      as_tibble() %>%
      arrange(desc(count_missing)) %>%
      mutate(percent_missing = round(count_missing/nrow(df),dcm)*100,
             Comment = ifelse(percent_missing < 5,"Acceptable",ifelse(percent_missing < 15,"High","Very High!"))) %>%
      dplyr::filter(percent_missing > perc)

    knitr::kable(miss_df,col.names = col_names) %>%
      kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)
  }else{
    cat("There is no Missing values in this data frame Enjoy :) No hustle!\n")
  }

}
