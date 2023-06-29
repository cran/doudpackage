######### Checking tools ##############################
##### Remove NA in group var ####
checkData<-function(data, group){
  if (!is.null(group) && table(data[,group], useNA = "always")[nlevels(data[,group]) + 1] != 0)
    warning(paste(table(data[,group], useNA = "always")[nlevels(data[,group]) + 1], " rows have been deleted due to missing values in the defined group" ,sep = ""))
  data<-data[!is.na(data[,group]),]
  return(data)
}
##### Assess variables in DescTab
checkVarDescTab<-function(data, group, quanti, quali, na.print, pvalue, digits.p,
                          digits.qt, digits.ql, normality, parallel, mc.cores){
  if(!is.data.frame(data)  && !tibble::is.tibble(data))
    stop(sprintf("data is not a data.frame: %s", class(data)))
  if (any(!c(class(quanti), class(quali), class(na.print), class(pvalue), class(parallel)) %in% "logical"))
    stop(sprintf("quanti, quali, na.print, pvalue not logical, parallel:
                 quanti is %s, quali is %s, na.print is %s, pvalu is %s",
                 class(quanti), class(quali), class(na.print), class(pvalue)))
  if(any(!c(class(digits.p), class(digits.qt), class(digits.ql)) %in% c('integer', 'numeric')))
    stop(sprintf("digits.p, digits.ql and digits.qt must be integer: %s, %s, %s",
                 class(digits.p), class(digits.qt), class(digits.ql)))
  if(!normality %in% c("assess", "normal", "manual", "non normal"))
    stop(sprintf('normality must be one of "assess",
                 "normal", "manual", "non normal" not "%s"', normality))
  if (isTRUE(parallel))
    if (!class(mc.cores) %in% c('integer', 'numeric'))
      stop(sprintf("mc.cores must be integer or numeric, is %s", class(mc.cores)))
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
############### Parse tools ##################
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


####### Parallel tools ############################
### Function to send the pointer to function
#' @import parallel
parallelFun<-function(parallel, ...){
  args<-list(...)
  if (parallel == "TRUE")
    return(do.call(parallel::mclapply, args))
  else{
    args[["mc.cores"]]<-NULL
    return(do.call(lapply, args))
  }
}
