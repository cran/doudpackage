
################ Quantitative #######################
### Function to calculate pvalue if group levels == 2
quantiTwoLevelsFun<-function(x, data, group){
  if (isTRUE(x@normal))
    return(safePvalue(function() stats::t.test(data[, x@name] ~ data[, group])$p.value,
                      x@name, "t.test"))
  if (isFALSE(x@normal))
    return(safePvalue(function() stats::wilcox.test(data[, x@name] ~ data[, group],
                                                    exact = FALSE)$p.value,
                      x@name, "wilcox.test"))
  stop(sprintf("Unknown normality %s for %s", x@normal, x@name))
}

quantiMoreLevelsFun<-function(x, data, group){
  if (isTRUE(x@normal))
    return(safePvalue(function(){
      aov_res<-unlist(summary(stats::aov(data[, x@name] ~ data[, group])))
      return(aov_res["Pr(>F)1"])
    }, x@name, "aov"))
  if (isFALSE(x@normal))
    return(safePvalue(function() stats::kruskal.test(data[, x@name] ~ data[, group])$p.value,
                      x@name, "kruskal.test"))
  stop(sprintf("Unknown normality %s for %s", x@normal, x@name))
}

## Just a hub to assess nlevels
quantiBivFun<-function(x, group, data, digits.p){
  if (nlevels(data[, group]) == 2)
    p<-quantiTwoLevelsFun(x, data, group)
  else if (nlevels(data[, group]) > 2)
    p<-quantiMoreLevelsFun(x, data, group)
  else
    stop(sprintf("Group levels must be at least two: %d for %s", nlevels(data[, group]), group))

  return(VarGroup(group_var = "Total", pvalue = round(p, digits.p), x = x))
}
#############################################

################ Qualitative ################
### Function to calculate pvalue for qualitative variables
qualiBivFun<-function(x, group, data, digits.p){
  # chisq.test warns when its approximation is unreliable: fall back on Fisher
  approximate<-FALSE
  p<-withCallingHandlers(
    tryCatch(stats::chisq.test(data[, x@name], data[, group], correct = FALSE)$p.value,
             error = function(e) NA_real_),
    warning = function(w){
      approximate<<-TRUE
      invokeRestart("muffleWarning")
    })
  if (isTRUE(approximate) || is.null(p) || length(p) != 1 || is.na(p))
    p<-safePvalue(function() stats::fisher.test(data[, x@name], data[, group],
                                                simulate.p.value = TRUE)$p.value,
                  x@name, "fisher.test")
  return(VarGroup(group_var = "Total", pvalue = round(p, digits.p), x = x))
}

########### Class Methods #########################

#' anaBiv generic function
#'
#' Generic function of anaBiv which gives bivariate analysis according to group
#'
#' @param var listVar object or data.frame
#' @param group  Name of the factor variable to make subgroups with
#' @param parallel Logical. Make analysis using parallel from [parallel::mclapply()].
#' @param ... Further arguments: `normality` and `digits.p`, as in [descTab()].
#'
#' @return A list of VarGroup object or data.frame
setGeneric("anaBiv", function(var, group, parallel,...) {
  return(standardGeneric("anaBiv"))
})

#' anaBiv data.frame function
#' @inherit anaBiv
setMethod("anaBiv", c(var = "listVar", group = "character"), function(var, group, parallel, ...){
  if (group != ""){
    lst_VarGroup.Biv<-parallelFun(parallel, X = var@List, FUN = function(x, group, data, digits.p){
      if (x@type == "factor" && x@name != group)
        quali.Biv<-qualiBivFun(x, group, data, digits.p)
      else if (x@type == "numeric")
        quanti.Biv<-quantiBivFun(x, group, data, digits.p)
      else if (x@name == group)
        return(NULL)
      else
        stop(sprintf("Unknown type for %s", x@name))
    }, group = group, ...)
    lst_VarGroup.Biv<-purrr::compact(lst_VarGroup.Biv)
    return(lst_VarGroup.Biv)
  }
  else
    return(NULL)
})

#' anaBiv data.frame function
#'
#' @inherit anaBiv
#' @param normality One of "normal", "non normal" or "assess", as in [descTab()].
#' @param digits.p Integer. Significant digits for p value.
#' @export
#' @examples
#' # A small simulated clinical trial
#' set.seed(42)
#' n <- 200
#' patients <- data.frame(
#'   arm      = factor(sample(c("Placebo", "Treatment"), n, replace = TRUE)),
#'   age      = round(rnorm(n, mean = 65, sd = 10)),
#'   crp      = round(rlnorm(n, meanlog = 2, sdlog = 1), 1),
#'   sex      = factor(sample(c("Female", "Male"), n, replace = TRUE)),
#'   diabetes = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3))),
#'   nyha     = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE),
#'                     ordered = TRUE)
#' )
#' patients$crp[sample(n, 15)] <- NA
#'
#' # p values only, without the descriptive table
#' res <- anaBiv(patients, group = "arm", parallel = FALSE, normality = "assess")
#' data.frame(variable = sapply(res, function(x) x["name"]),
#'            pvalue = sapply(res, function(x) x["pvalue"]))
setMethod("anaBiv", c(var = "data.frame", group = "character"),
          function(var, group, parallel, normality = "normal", digits.p = 3L, ...) {
  var<-as.data.frame(var, stringsAsFactors = FALSE)
  if (group != "" && !group %in% colnames(var))
    stop(sprintf("group is not a variable of var: \"%s\" not in %s",
                 group, paste(colnames(var), collapse = ", ")))
  if (group != "" && !is.factor(var[, group]))
    stop(sprintf("group needs to be a factor, %s is %s",
                 group, paste(class(var[, group]), collapse = "/")))
  var<-checkData(var, group)
  var_list<-varType(var, normality = normality)
  return(anaBiv(var_list, group = group, parallel = parallel, data = var,
                digits.p = digits.p, ...))
})
