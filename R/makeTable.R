# Function to create dataframe with uni and bivariate analysis ######
#' @import tidyr
makeTable<-function(quali.Univ_list.Global, group, pvalue, na.print,
                    parallel, mc.cores){
  ## Makes df out of the list
  df<-dplyr::bind_rows(parallelFun(parallel, X = quali.Univ_list.Global, FUN = function(x){
    tmp_df<-tibble::tibble("var" = c(x@parsed_name, x@missing.value.name),
                           "group_var" = c(x@value, x@missing.value), pvalue=c(x@pvalue, NA))

    names(tmp_df)[names(tmp_df) == "group_var"]<-x@group_var
    return(tmp_df)
  }, mc.cores = mc.cores))

  df<-subset(df, df[,"var"] != "")
  #Keep only full columns and merge
  for (n in colnames(df)){
    if (n == "var" || n == "pvalue")
      next
    tmp<-df[!is.na(df[, n]), ] %>% dplyr::select_if(~ !any(is.na(.)))
    if(!exists("tmp_df", inherits = FALSE))
      tmp_df<-tmp
    else
      tmp_df<-merge(tmp_df, tmp, by = "var")
  }
  if (!is.null(group) && pvalue == TRUE)
    tmp_df<-merge(tmp_df, unique(dplyr::select(df, "var", "pvalue"), by = "var"))
  else
    pvalue = FALSE
  if (na.print == FALSE)
    tmp_df<-tmp_df[!grepl(".*Missing values", tmp_df[,1]),]
  df<-tmp_df
  return (list("df" = df, "pvalue"= pvalue))
}
