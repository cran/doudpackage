##### Quali Univ Fun ###
qualiUnivFun<-function(x, data_sub, group, digits.ql){
  for (i in 1:nlevels(data_sub[, x@name])){
    tab = table(data_sub[, x@name], useNA = "always")
    n = tab[i]
    prop = round(prop.table(tab) * 100, digits.ql)[i]

    parsed_name = paste(x@name, levels(data_sub[, x@name])[i], sep = ", ")
    value = paste(n, " (", prop, ")", sep = "")
    group_var<-as.character(ifelse(is.null(group), "Total", group))

    if (i == nlevels(data_sub[, x@name])){
      n.missing.value = tab[(nlevels(data_sub[, x@name]) + 1)]
      prop.missing.value = round(prop.table(tab) * 100, digits = digits.ql)[(nlevels(data_sub[, x@name]) + 1)]
      missing.value = paste(n.missing.value, " (", prop.missing.value, ")", sep = "")
      missing.value.name = paste(x@name , "Missing values", sep = ".")
    }
    else{
      missing.value = ""
      missing.value.name = ""
    }
    var.group <-VarGroup(x = x,
                       group_var = group_var, pvalue = x@pvalue,
                       parsed_name = parsed_name, value = value,
                       missing.value = missing.value,
                       missing.value.name = missing.value.name)

    if(!exists("var.group_list", inherits = FALSE))
      var.group_list<-var.group
    else
      var.group_list<-c(var.group_list, var.group)
  }
  return(var.group_list)
}

lapplyQuali<-function(group, data, factor_list, digits.ql){
  if (!is.null(group)){
    for (i in 1:nlevels(data[,group])){
      data_sub<-data[data[,group] == levels(data[,group])[i],]
      quali.Univ_list.tmp<-purrr::compact(lapply(factor_list@List, qualiUnivFun, data_sub = data_sub,
                                  group = levels(data[,group])[i], digits.ql = digits.ql))
      if (!exists("quali.Univ_list.Group", inherits = FALSE))
        quali.Univ_list.Group<-quali.Univ_list.tmp
      else
        quali.Univ_list.Group<-c(quali.Univ_list.Group, quali.Univ_list.tmp)
    }
  }
  lst_VarGroup.Univ.Total<-purrr::compact(lapply(factor_list@List, qualiUnivFun, data_sub = data,
                                  group = NULL, digits.ql = digits.ql))
  if (exists("quali.Univ_list.Group", inherits = FALSE)){
    quali.Univ_list.Global<-unlist(c(quali.Univ_list.Group, lst_VarGroup.Univ.Total))
    return(unlist(quali.Univ_list.Global))
  }
  else
    return(unlist(lst_VarGroup.Univ.Total))
}
#########################################

######### QuantiFun ##################
quantiUnivFun<-function(x, data_sub, group, digits.qt, digits.ql){
  group_var<-as.character(ifelse(is.null(group), "Total", group))
  tab.missing = table(is.na(data_sub[, x@name]))
  n.missing.value = as.numeric(ifelse(is.na(tab.missing[2]), 0, tab.missing[2]))
  prop.missing.value<-as.numeric(ifelse(n.missing.value == 0, 0,
                                        round(prop.table(tab.missing)[2] * 100,
                                        digits = digits.ql)))
  missing.value<-paste(n.missing.value, " (", prop.missing.value, ")", sep = "")
  missing.value.name = paste(x@name, "Missing values", sep = ".")
  parsed_name = x@name
  if (x@normal == TRUE){
    "mean"<-round(mean(data_sub[,x@name], na.rm = T), digits = digits.qt)
    "sd"<-round(sd(data_sub[,x@name], na.rm = T), digits = digits.qt)
    value = paste(mean, " (", sd, ")", sep = "")
  }
  else{
    "median"<-round(stats::median(data_sub[,x@name], na.rm = T), digits = digits.qt)
    "iqr"<-round(stats::IQR(data_sub[,x@name], na.rm = T), digits = digits.qt)
    value = paste(median, " (", iqr, ")", sep = "")
  }

  var.group <-VarGroup(x = x,
                         group_var = group_var, pvalue = x@pvalue,
                         parsed_name = parsed_name, value = value,
                         missing.value = missing.value,
                         missing.value.name = missing.value.name)

  return(var.group)
}

lapplyQuanti<-function(group, data, numeric_list, digits.qt, digits.ql){
  if (!is.null(group)){
    for (i in 1:nlevels(data[,group])){
      data_sub<-data[data[,group] == levels(data[,group])[i],]
      quanti.Univ_list.tmp<-purrr::compact(lapply(numeric_list@List, quantiUnivFun,
                                   data_sub = data_sub,group = levels(data[,group])[i],
                                   digits.qt = digits.qt, digits.ql = digits.ql))
      if (!exists("quanti.Univ_list.Group", inherits = FALSE))
        quanti.Univ_list.Group<-quanti.Univ_list.tmp
      else
        quanti.Univ_list.Group<-c(quanti.Univ_list.Group, quanti.Univ_list.tmp)
    }
  }
  lst_VarGroup.Univ.Total<-purrr::compact(lapply(numeric_list@List, quantiUnivFun, data_sub = data,
                                  group = NULL, digits.qt = digits.qt))
  if (exists("quanti.Univ_list.Group", inherits = FALSE)){
    quanti.Univ_list.Global<-purrr::compact(unlist(c(quanti.Univ_list.Group, lst_VarGroup.Univ.Total)))
    return(unlist(quanti.Univ_list.Global))
  }
  else
    return(unlist(lst_VarGroup.Univ.Total))
}

#########################################

###### Method Class to dispatch vars ######
setGeneric("anaUniv", function(var, group, data, ...) {
  return(standardGeneric("anaUniv"))
})

setMethod("anaUniv", "listVar", function(var, group, data,
                                         digits.qt, digits.ql, quali, quanti){
  numeric_list<-purrr::compact(lapply(var@List, function(x){if("numeric" %in% x@type) return(x)}))
  if (!is.null(group))
    factor_list<-purrr::compact(lapply(var@List, function(x){if("factor" %in% x@type &&
                                                          x@name != group) return(x)}))
  else
    factor_list<-purrr::compact(lapply(var@List, function(x){if("factor" %in% x@type) return(x)}))
  numeric_list<-methods::new("listVar", List = numeric_list)
  factor_list<-methods::new("listVar", List = factor_list)
  if (quali == TRUE)
    lst_VarGroup.quali<-lapplyQuali(group, data, factor_list, digits.ql)
  if (quanti == TRUE)
  lst_VarGroup.quanti<-lapplyQuanti(group, data, numeric_list,
                                    digits.qt, digits.ql)
  if (!exists("lst_VarGroup.quali", inherits = FALSE) || is.null(lst_VarGroup.quali))
    return(lst_VarGroup.quanti)
  else if (!exists("lst_VarGroup.quanti") || is.null(lst_VarGroup.quanti))
    return(lst_VarGroup.quali)
  return(unlist(c(lst_VarGroup.quanti, lst_VarGroup.quali)))
})
