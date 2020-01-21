#' A function to display level of missingness in a data frame provided
#' @param df A data frame containing the variables to be summarised
#' @param group_variable The grouping variable factor
#' @param dcm Number of decimal places to be previewed
#' @return A data frame structure showing the freq distribution
#'
#' @examples
#'  if(!require(ggplot2)){install.packages(ggplot2)};summary_group(diamonds, variable = clarity,group_variable = cut)

#'
#' @export
#'
summarise_by_group <- function(df,variable, group_variable, dcm = 0){
  if(!require(dplyr)){install.packages(dplyr)};if(!require(rlang)){install.packages(rlang)} # quick fix in case the packages do not exist
  res  =  df %>% select({{group_variable}},{{variable}}) %>%
    filter(!is.na({{group_variable}})) %>%
    group_by({{group_variable}}, {{variable}}) %>%
    summarise(Total_count  =  n()) %>%
    ungroup() %>%
    group_by({{group_variable}}) %>%
    mutate(Percentage  =  round(Total_count/sum(Total_count)*100, dcm)) %>%
    as.data.frame()
  return(res)
}

#summarise_by_group(diamonds, variable = clarity,group_variable = cut)


#devtools::install_bitbucket(repo = "stevenndungu/busara", username = "steven.ndungu@busaracenter.org", ref = "master", quiet = FALSE, auth_user = NULL, password = "****)


#devtools::install_bitbucket(repo = "stevenndungu/busara", username = "steven.ndungu@busaracenter.org", ref = "master", quiet = FALSE, auth_user = NULL, password = "****", force = T)
