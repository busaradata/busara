#' A function to display level of missingness in a data frame provided
#' @param tab A data frame or tibble table to be displayed on markdown
#' @param col_names A vector of column names as expected to be displayed

#' @examples
#' decorate_table(head(mtcars), names(mtcars))
#'
#' @export
#'

#summary_group(df = diamonds,variable = cut, group_variable = color,dcm = 2)

# Display tables neatly function
# Same as table_func but this allows you to change the variable names of the table.

decorate_table <- function(tab,col_names  =  col_names){
  kableExtra::kable_styling(knitr::kable(tab,col.names  =  col_names),bootstrap_options  =  "striped", full_width  =  F)
}
