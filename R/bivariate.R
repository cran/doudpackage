
################ Quantitative #######################
### Function to calculate pvalue if group levels == 2
quantiTwoLevelsFun<-function(x, data, group){
  my_env<-environment()
  if (x@normal == TRUE){
    tryCatch({
      my_env$p<-stats::t.test(data[,x@name]~data[,group])$p.value
    },
    error=function(e){
      warning(paste(e), x@name,  "in quantiTwoLevelsFun")
    })
  }
  else if (x@normal == FALSE){
    tryCatch({
      my_env$p<-stats::wilcox.test(data[,x@name]~data[,group])$p.value
    },
    warning=function(w){
      my_env$p<-stats::wilcox.test(data[,x@name]~data[,group], exact = F)$p.value
    },
    error=function(e){
      warning(paste(e), x@name, " in quantiTwoLevelsFun")
    })
  }
  else
    stop(sprintf("Unknown normality %s for %s", x@normal, x@name))
  return(my_env$p)
}

quantiMoreLevelsFun<-function(x, data, group){
  my_env<-environment()
  if (x@normal == TRUE){
    tryCatch({
      aov_res<-unlist(summary(stats::aov(data[,x@name]~data[,group])))
      my_env$p<-aov_res["Pr(>F)1"]
    },
    warning=function(w){
      warning(paste(w), x@name, " in quantiMoreLevelsFun")
    },
    error=function(e){
      warning(paste(e), x@name, " in quantiMoreLevelsFun")
    })
  }
  else if (x@normal == FALSE){
    tryCatch({
      my_env$p<-stats::kruskal.test(x@name~group, data = data)$p.value
    },
    warning=function(w){
      warning(paste(w), x@name)
    },
    error=function(e){
      warning(paste(e), x@name, " in quantiMoreLevelsFun")
    })
  }
  else
    stop(sprintf("Unknown normality %s for %s", x@normal, x@name))
  return(my_env$p)
}

## Just a hub to assess nlevels
quantiBivFun<-function(x, group, data, digits.p){
  my_env<-environment()
  if (nlevels(data[, group]) == 2)
    p = quantiTwoLevelsFun(x, data, group)
  else if (nlevels(data[,group]) > 2)
    p = quantiMoreLevelsFun(x, data, group)
  else
    stop(sprintf("Group levels must be at least two: %d for %s", nlevels(data[,group]), group))

  var.p<-VarGroup(group_var = "Total", pvalue = round(p, digits.p),
                  x = x)
  return(var.p)
}
#############################################

################ Qualitative ################
### Function to calculate pvalue for qualitative variables
qualiBivFun<-function(x, group, data, digits.p){
  my_env<-environment()
  tryCatch({
    my_env$p<-stats::chisq.test(data[,x@name], data[,group], correct=FALSE)$p.value},
    error=function(e){
      warning(paste(e), x@name, " in qualiBivFun")
    },
    warning=function(w){
      tryCatch({
        my_env$p<-stats::fisher.test(data[,x@name], data[,group], simulate.p.value = TRUE)$p.value},
      error=function(e){
        warning(paste(e), x@name, " in qualiBivFun")})
    })
  var.p<-VarGroup(group_var = "Total", pvalue = round(my_env$p, digits.p),
                  x = x)
  return(var.p)
}

########### Class Methods #########################

#' anaBiv generic function
#'
#' Generic function of anaBiv which gives bivariate analysis according to group
#'
#' @param var listVar object or data.frame
#' @param group  Variable to make subgroups with
#' @param ... digits.p can be specified as descTab
#'
#' @return A list of VarGroup object or data.frame
setGeneric("anaBiv", function(var, group, ...) {
  return(standardGeneric("anaBiv"))
})

#' anaBiv data.frame function
#' @inherit anaBiv
setMethod("anaBiv", c(var = "listVar", group = "character"), function(var, group, ...){
  if (!is.null(group)){
    lst_VarGroup.Biv<-lapply(var@List, function(x, group, data, digits.p){
      if (x@type == "factor" && x@name != group)
        quali.Biv<-qualiBivFun(x, group, data, digits.p)
      else if (x@type == "numeric")
        quanti.Biv<-quantiBivFun(x, group, data, digits.p)
      else if (x@name == group)
        return(NULL)
      else
        stop(sprintf("Uknown type for %s", x@name))
    }, group, ...)
    lst_VarGroup.Biv<-purrr::compact(lst_VarGroup.Biv)
    return(lst_VarGroup.Biv)
  }
  else
    return(NULL)
})

#' anaBiv data.frame function
#'
#' @inherit anaBiv
#' @export
setMethod("anaBiv", c(var = "data.frame", group = "character"), function(var, group, ...) {
  if (!is.null(group) && !is.factor(var[, group]))
    stop(sprintf("group needs to be a factor, %s is %s", group, class(var[, group])))
  var_list<-varType(var, normality = "assess")
  ana.biv_list<-anaBiv(var_list, data = var, group = group, ...)
  return(ana.biv_list)
})
