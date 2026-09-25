######### Checking tools ##############################
##### Remove NA in group var and drop its unused levels ####
checkData<-function(data, group){
  if (group == "")
    return(data)
  n.missing<-sum(is.na(data[, group]))
  if (n.missing != 0)
    warning(sprintf("%d rows have been deleted due to missing values in the defined group",
                    n.missing))
  data<-data[!is.na(data[, group]), , drop = FALSE]
  # Unused levels (typically left over by a subset) would otherwise produce a
  # whole column of NaN in the table
  empty.levels<-levels(data[, group])[table(data[, group]) == 0]
  if (length(empty.levels) != 0){
    warning(sprintf("Unused level(s) of %s dropped: %s",
                    group, paste(empty.levels, collapse = ", ")))
    data[, group]<-droplevels(data[, group])
  }
  return(data)
}
##### Assess variables in DescTab
checkVarDescTab<-function(data, group, quanti, quali, na.print, pvalue, digits.p,
                          digits.qt, digits.ql, normality, parallel, mc.cores){
  if(!is.data.frame(data) && !tibble::is_tibble(data))
    stop(sprintf("data is not a data.frame: %s", paste(class(data), collapse = "/")))
  if (ncol(data) == 0 || nrow(data) == 0)
    stop(sprintf("data is empty: %d rows, %d columns", nrow(data), ncol(data)))
  if (!is.character(group) || length(group) != 1 || is.na(group))
    stop(sprintf("group must be a single variable name, or \"\" for no group; got %s of length %d",
                 paste(class(group), collapse = "/"), length(group)))
  if (group != "" && !group %in% colnames(data))
    stop(sprintf("group is not a variable of data: \"%s\" not in %s",
                 group, paste(colnames(data), collapse = ", ")))
  if (group != "" && !is.factor(data[, group]))
    stop(sprintf("group needs to be a factor, %s is %s",
                 group, paste(class(data[, group]), collapse = "/")))
  if (group != "" && nlevels(droplevels(data[!is.na(data[, group]), group])) < 2)
    stop(sprintf("group needs at least two non empty levels, %s has %d",
                 group, nlevels(droplevels(data[!is.na(data[, group]), group]))))
  if (!isTRUE(quali) && !isTRUE(quanti))
    stop("quali and quanti cannot both be FALSE: there would be nothing to describe")
  if (any(!c(class(quanti), class(quali), class(na.print), class(pvalue), class(parallel)) %in% "logical"))
    stop(sprintf("quanti, quali, na.print, pvalue not logical, parallel:
                 quanti is %s, quali is %s, na.print is %s, pvalu is %s",
                 class(quanti), class(quali), class(na.print), class(pvalue)))
  if(any(!c(class(digits.p), class(digits.qt), class(digits.ql)) %in% c('integer', 'numeric')))
    stop(sprintf("digits.p, digits.ql and digits.qt must be integer: %s, %s, %s",
                 class(digits.p), class(digits.qt), class(digits.ql)))
  if(length(normality) != 1 || !normality %in% c("assess", "normal", "non normal"))
    stop(sprintf('normality must be one of "assess", "normal", "non normal", not "%s"',
                 paste(normality, collapse = ", ")))
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
    if(!is.vector(col.order))
      stop(sprintf("Invalid type for col.order. Must be a vector, is %s",
                   paste(class(col.order), collapse = "/")))
    if(any(duplicated(col.order)))
      stop(sprintf("col.order has duplicated columns: %s",
                   paste(col.order[duplicated(col.order)], collapse = ", ")))
    if(any(sapply(col.order, function(x){return (!x %in% colnames(table@table))})))
      stop(sprintf("col.order is invalid. %s not in %s", paste(col.order, collapse = ' '), paste(colnames(table@table), collapse=' ')))
    if(table@group != "" && !all(levels(table@data[, table@group]) %in% col.order))
      stop(sprintf("If set, col.order needs to contain all levels of group: %s was given; expected at least %s",
                   paste(col.order, collapse = ", "),
                   paste(levels(table@data[, table@group]), collapse = ", ")))
    if(length(col.order) > ncol(table@table))
      stop(sprintf("col.order is larger than nlevels of group + 2. Expected max of %d, got %d", ncol(table@table), length(col.order)))
  }
  if(!is.null(group_rows_labels)){
    if (is.null(names(group_rows_labels)) || any(names(group_rows_labels) == ""))
      stop(sprintf("group_rows_labels needs to be a named list"))
    described<-describedVars(table)
    unknown<-setdiff(unlist(group_rows_labels), described)
    if (length(unknown) != 0)
      stop(sprintf("group_rows_labels refers to variable(s) not described in the table: %s. Available: %s",
                   paste(unknown, collapse = ", "), paste(described, collapse = ", ")))
    duplicated.vars<-unlist(group_rows_labels)[duplicated(unlist(group_rows_labels))]
    if (length(duplicated.vars) != 0)
      stop(sprintf("group_rows_labels assigns the same variable to several labels: %s",
                   paste(unique(duplicated.vars), collapse = ", ")))
  }
}
#####################################################################
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

####### Type tools ############################
# Translate the class() of a variable into a type handled by the package.
# Returns "numeric", "factor" or NA_character_ if the class is not handled.
resolveType<-function(type, name = ""){
  if (any(type %in% c("numeric", "integer", "double")))
    return("numeric")
  if (any(type %in% c("factor", "ordered")))
    return("factor")
  return(NA_character_)
}

####### Row identity tools ############################
# Names of the variables actually described in the table (the group variable
# and the variables of an unhandled type are not)
describedVars<-function(table){
  vars<-unlist(lapply(table@var_list@List, function(x) x@name))
  return(vars[vars != table@group])
}

# Map every row label of the table back to the variable it describes.
# Labels are "<name>, <level>", "<name> mean (SD)"/"<name> median (IQR)" and
# "<name>.Missing values"; matching the longest name first keeps "age_group"
# from being mistaken for "age".
rowVarName<-function(labels, vars){
  vars<-vars[order(nchar(vars), decreasing = TRUE)]
  res<-vapply(labels, function(label){
    for (v in vars){
      if (identical(label, v))
        return(v)
      if (startsWith(label, paste0(v, ", ")) ||
          startsWith(label, paste0(v, ".Missing values")) ||
          startsWith(label, paste0(v, " mean (SD)")) ||
          startsWith(label, paste0(v, " median (IQR)")) ||
          startsWith(label, paste0(v, ", mean (SD)")) ||
          startsWith(label, paste0(v, ", median (IQR)")))
        return(v)
    }
    return(NA_character_)
  }, character(1), USE.NAMES = FALSE)
  return(res)
}

####### Statistical tools ############################
# Run a test and return NA rather than aborting the whole table when the test
# cannot be computed (constant variable, empty level, too few observations...)
safePvalue<-function(expr, name, where){
  failure<-NULL
  # The test's own warnings (approximations, ties...) are expected and muffled;
  # only a failure to compute is reported back to the user.
  p<-tryCatch(withCallingHandlers(expr(),
                                  warning = function(w) invokeRestart("muffleWarning")),
              error = function(e){
                failure<<-conditionMessage(e)
                return(NA_real_)
              })
  if (!is.null(failure))
    warning(sprintf("No p value for %s (%s): %s", name, where, failure), call. = FALSE)
  if (is.null(p) || length(p) != 1 || !is.numeric(p))
    return(NA_real_)
  return(as.numeric(p))
}
