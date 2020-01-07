univariate_summary <- function(df, variable){
  #continous variables
  if(class(df[,variable]) == "integer" | class(df[,variable]) == "numeric"){
    summary(df[,variable])
  }else{
    table(df[,variable])
  }
}
