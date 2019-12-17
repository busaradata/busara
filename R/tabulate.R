#
#--------------------------------------------------------------------------------------------------#

# Display tables neatly function
# Same as table_func but this allows you to change the variable names of the table.

table_func2 <- function(tab,col_names  =  col_names){
  kableExtra::kable_styling(knitr::kable(tab,col.names  =  col_names),bootstrap_options  =  "striped", full_width  =  F)
}


#devtools::install_bitbucket(repo = "stevenndungu/busara", username = "steven.ndungu@busaracenter.org", ref = "master", quiet = FALSE, auth_user = NULL, password = "machetho300#")


#devtools::install_bitbucket(repo = "stevenndungu/busara", username = "steven.ndungu@busaracenter.org", ref = "master", quiet = FALSE, auth_user = NULL, password = "machetho300#", force = T)
