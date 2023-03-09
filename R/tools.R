######### Checking tools ##############################"
##### Assess variables in DescTab
checkVarDescTab<-function(data, group, quanti, quali, na.print, pvalue, digits.p, digits.qt, digits.ql, normality){
  if(!is.data.frame(data)  && !tibble::is.tibble(data))
    stop(sprintf("data is not a data.frame: %s", class(data)))
  if (any(!c(class(quanti), class(quali), class(na.print), class(pvalue)) %in% "logical"))
    stop(sprintf("quanti, quali, na.print, pvalue not logical:
                 quanti is %s, quali is %s, na.print is %s, pvalu is %s",
                 class(quanti), class(quali), class(na.print), class(pvalue)))
  if(any(!c(class(digits.p), class(digits.qt), class(digits.ql)) %in% c('integer', 'numeric')))
    stop(sprintf("digits.p, digits.ql and digits.qt must be integer: %s, %s, %s",
                 class(digits.p), class(digits.qt), class(digits.ql)))
  if(!normality %in% c("assess", "normal", "manual", "non normal"))
    stop(sprintf('normality must be one of "assess",  "normal", "manual", "not normal" not "%s"', normality))
}

## Assess variables in parseClasseFun
checkVarParseClassFun<-function(levels_to_keep, col.order, group_rows_labels, table){
  if(!is.null(levels_to_keep)){
    if (is.null(names(levels_to_keep)))
      stop(sprintf("levels_to_keep needs to be a named list"))
    if (any(sapply(names(levels_to_keep), function(x){ return(!x %in% colnames(table@data))})))
      stop(sprintf("Check names of levels_to_keep, one is not a valid variable of
                   the dataset %s", paste(names(levels_to_keep), collapse = ' ')))
  }
  if (!is.null(col.order)){
    if(any(sapply(col.order, function(x){return (!x %in% colnames(table@table))})))
      stop(sprintf("col.order is invalid. %s not in %s", paste(col.order, collapse = ' '), paste(colnames(table@table), collapse=' ')))
    if(!is.vector(col.order))
      stop(sprintf("Invalid type for col.order. Must be a vector, is %s", class(col.order)))
    if(length(col.order) < nlevels(table@data[, table@group]))
      stop(sprintf("If set, col.order needs to contains at least all levels of group:
                   %s was given; expected %s", col.order, levels(table@data[, table@group])))
    if(length(col.order) > ncol(table@table))
      stop(sprintf("col.order is larger than nlevels of group + 2. Expected max of %d, got %d", ncol(table@table), length(col.order)))
  }
  if(!is.null(group_rows_labels)){
    if (is.null(names(group_rows_labels)))
      stop(sprintf("group_rows_labels needs to be a named list"))
    if (any(unlist(sapply(group_rows_labels, function(x){ return(!x %in% colnames(table@data))}))))
      stop(sprintf("Check variables specified into of group_rows_labels, one is not a valid variable of
                   the dataset: %s", paste(group_rows_labels, collapse = ' ')))
  }
}
#####################################################################
# Function to get rows indexes to make row labels
getGroupMinMax<-function(group_rows_labels, table){
  if (is.null(group_rows_labels))
    return(NULL)
  df<-sapply(names(group_rows_labels), function(group_rows_label, table, group_rows_labels){
    pat = paste(group_rows_labels[[group_rows_label]], collapse = "|")
    pat<-gsub('\\.', '\\\\.', pat)
    grep(pat, table[,1])
    return(group_rows_label = c(min(grep(pat, table[,1])),max(grep(pat, table[,1]))))
    }, table, group_rows_labels)
  return(df)
}

#### Function to create dataframe with uni and bivariate analysis ######
#' @import tidyr
makeTable<-function(quali.Univ_list.Global, group, pvalue, na.print){
  ## Makes df out of the list
  df<-dplyr::bind_rows(lapply(quali.Univ_list.Global, function(x){
    tmp_df<-tibble::tibble("var" = c(x@parsed_name, x@missing.value.name),
                   "group_var" = c(x@value, x@missing.value), pvalue=c(x@pvalue, NA))

    names(tmp_df)[names(tmp_df) == "group_var"]<-x@group_var
    return(tmp_df)
  }))

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
