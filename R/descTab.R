# Function to get var types in a custom object list
varType<-function(data, normality){
  list_names<-colnames(data)
  list_names<-list_names[order(list_names)]
  list_names<-as.list(list_names)
  type_list<-lapply(data, class)
  type_list<-type_list[order(names(type_list))]
  if (normality %in% c("manual", "assess"))
    stop(sprintf("Not ready yet for %s", normality))
  n<-ifelse(normality == "normal", TRUE, FALSE)

  list_res<-mapply(Var,list_names, type_list, normal = n)
  list_res<-methods::new("listVar", List = list_res)
  return(list_res)
}

#' Generic function to create a table of descriptive analysis of a dataset
#'
#' This function allows you to display all together all univariate analysis (median/mean; IQR/SD; proportions)
#' and bivariates analysis (Wilcoxon, Chi² or Fisher).
#' The univariate analysis can be sub-grouped by a variable of interest of n levels. Appropriate statistics test will be applied
#'
#' @param data A datasaset. Needs to be a data.frame/tibble object
#' @param group Optional. The name of the variable to make sub-groups comparisons.
#' @param quanti,quali,na.print,pvalue Logical. If false, won't display quantitative/qualitative/Missing values/pvalues variable results
#' @param digits.p Integer. Significant digits for p value
#' @param digits.qt Integer. Significant digits for mean/median, SD/IQR
#' @param digits.ql Integer. Significant digits for proportions
#' @param normality One of "assess", "normal", "manual", "non normal". See details
#' @param parallel Logical. Make analysis using parallel from [parallel::mclapply()].
#' @param mc.cores If parallel is TRUE, how many Cores to used.
#'
#' @return A S4 objects [parseClass()] containing the main table accessible by \["table"\] subscript.
#' @export
#'
#' @examples
#' data(iris)
#' library(stringi)
#' iris$fact_1<-as.factor(as.character(sample(1:5, 150, replace = TRUE)))
#' n_na<-sample(1:150, 30)
#' iris[n_na, "fact_1"]<-NA
#' iris$fact_2<-as.factor(as.character(sample(1:2, 150, replace = TRUE)))
#' n_na<-sample(1:150, 10)
#' iris[n_na, "fact_2"]<-NA
#' iris$fact_3<-as.factor(as.character(stri_rand_strings(150, 1, '[A-B]')))
#' iris$num<-runif(150, min = 0, max = 100)
#' n_na<-sample(1:150, 5)
#' iris[n_na, "num"]<-NA
#' iris_test<-descTab(iris, group = "Species", na.print = TRUE)
descTab<-function(data, group=NULL, quanti=TRUE, quali=TRUE, na.print = FALSE,
                      pvalue=TRUE, digits.p=3L, digits.qt = 1L,
                  digits.ql = 1L, normality = "normal", parallel = FALSE, mc.cores = 0)
{
  checkVarDescTab(data, group, quanti, quali, na.print, pvalue, digits.p, digits.qt,
           digits.ql, normality, parallel, mc.cores)
  if (!is.null(group) && !is.factor(data[, group]))
    stop(sprintf("group needs to be a factor, %s is %s", group, class(data[, group])))
  if (isTRUE(parallel) && mc.cores == 0)
    mc.cores = parallel::detectCores() - 1
  data<-checkData(data, group)
  var_list<-varType(data, normality)
  ana.biv_list<-anaBiv(var_list, parallel, data = data, group = group,
                       digits.p = digits.p, mc.cores = mc.cores)
  if (is.null(ana.biv_list))
    ana.biv_list<-lapply(var_list@List, methods::as, Class = "VarGroup")
  ana.biv_list<-methods::new("listVar", List = ana.biv_list)
  ana.univ_list<-anaUniv(ana.biv_list, parallel, group = group, data = data,
                         digits.qt = digits.qt,
                         digits.ql = digits.ql, quali = quali, quanti = quanti,
                         mc.cores = mc.cores)
  return<-makeTable(ana.univ_list, group, pvalue, na.print, parallel, mc.cores)

  return.table<-parseClass(table = return$df, group = group, quanti = quanti,
                           quali = quali, na.print = na.print,
                           pvalue = return$pvalue,
                           var_list = var_list, data = data,
                           digits.qt = digits.qt, digits.ql = digits.ql)
  return(return.table)
}
