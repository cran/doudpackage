## Var Class ###
#' S4 class
#'
#' A S4 class containing name, type and normality assessment of variable
#'
#' @slot name A character taking name of the variable
#' @slot type A character taking name of the variable type
#' @slot normal Logical, if variable, is numeric; is it normal
#' @export
methods::setClass("Var",slots = c(name = "character", type = "character",
           normal = "logical"))

#' S4 class initialization function
#'
#' Initialization function for Var [initialize,Var-method()]
#'
#' @param .Object Object to be initialized
#' @param name A character taking name of the variable
#' @param type A character taking name of the variable type
#' @param normal Logical, if variable, is numeric; is it normal
#'
#' @return Var Object
methods::setMethod("initialize",
          "Var",
          function(.Object, name, type, normal) {
            .Object@name <- name
            type<-as.character(ifelse(type == "numeric" | type == "integer", "numeric",
                                      ifelse(type == "factor", "factor",
                                             stop(sprintf("Type unrecognised %s of %s", type, name)))))
            .Object@type <- type
            .Object@normal <- normal
            return(.Object)
          })

#' S4 class initialization function
#'
#' Initialization function for Var [initialize,Var-method()]
#'
#' @param name A character taking name of the variable
#' @param type A character taking name of the variable type
#' @param normal Logical, if variable, is numeric; is it normal
#'
#' @return Var Object
Var <- function(name, type = "", normal = TRUE) {
  return(methods::new("Var", name = name, type = type, normal = normal))
}

#' Method to access S4 Var elements
#'
#' Method to access Var elements by name
#'
#' @param x : object
#' @param i : value
#' @return object of Var
methods::setMethod("[", "Var", function(x, i) {
  if (i == "name")
    return(x@name)
  else if (i == "type")
    return(x@type)
  else if (i == "normal")
    return(x@normal)
})

#' Method to access S4 Var elements
#'
#' Method to modify Var elements by name
#'
#' @param x : object
#' @param i : Element name
#' @param value : Value to be added
#'
#' @return object
setReplaceMethod("[", "Var", function(x, i, value) {
  if (i == "name")
    x@name <- value
  else if (i == "type")
    x@type <- value
  else if (i == "normal")
    x@normal <- value
  return(x)
})
#################

### List Var ###
#' S4 class
#'
#'A class of list of Var object
#'
#' @slot List a list of Var
#' @export
methods::setClass("listVar", slots = c(List = "list"))
#################

#### Var Group ###
#' S4 class
#'
#'A S4 class containing Var [initialize,Var-method()] It also contains the pvalue, the parsed value
#'the missing values and the group for which it was calculated
#'
#' @slot group_var The subgroup for which proportions, mean/sd were calculated
#' and missing values
#' @slot pvalue The calculated pvalue
#' @slot parsed_name The name of the variable parsed with the n (%), mean (SD)
#' @slot value The values calculated parsed
#' @slot missing.value Missing values numbers and proportions n (%)
#' @slot missing.value.name Missing values concatenate with the level
#' of the variable if it factor
#' @export
methods::setClass("VarGroup",contains = "Var",
         slots = c(group_var = "character", pvalue = "numeric",
                   parsed_name = "character", value = "character",
                   missing.value = "character", missing.value.name = "character"))

#' S4 class initialization function
#'
#' Initialization function for VarGroup [initialize,VarGroup-method()]
#'
#' @param .Object Object to be initialized
#' @param x A Var object
#' @param group_var The subgroup for which proportions, mean/sd were calculated
#' and missing values
#' @param pvalue The calculated pvalue
#' @param parsed_name The name of the variable parsed with the n (%), mean (SD)
#' @param value The values calculated parsed
#' @param missing.value Missing values numbers and proportions n (%)
#' @param missing.value.name Missing values concatenate with the level
#' of the variable if it factor
#'
#' @return VarGroup object
methods::setMethod("initialize",
          "VarGroup",
          function(.Object, x, group_var, pvalue, parsed_name,
                   value, missing.value, missing.value.name) {
            .Object@name<-x@name
            .Object@type<-x@type
            .Object@normal<-x@normal
            .Object@group_var <- group_var
            .Object@pvalue <- pvalue
            .Object@parsed_name<-parsed_name
            .Object@value <- value
            .Object@missing.value <- missing.value
            .Object@missing.value.name <- missing.value.name
            return(.Object)
          })

VarGroup <- function(x, group_var = "", pvalue = 0,  parsed_name = "",
                     value = "", missing.value = "", missing.value.name = "") {
  return(methods::new("VarGroup", x = x, group_var = group_var, pvalue = pvalue,
             parsed_name = parsed_name, value = value,
             missing.value = missing.value, missing.value.name = missing.value.name))
}

#' Method to access S4 Var elements
#'
#' Method to access VarGroup [initialize,VarGroup-method()] elements by name
#'
#' @param x : object
#' @param i : value
#'
#' @return object element
methods::setMethod("[", "VarGroup", function(x, i) {
  if (i == "group_var")
    return(x@group_var)
  else if (i == "pvalue")
    return(x@pvalue)
  else if (i == "parsed_name")
    return(x@parsed_name)
  else if (i == "value")
    return(x@value)
  else if (i == "missing.value")
    return(x@missing.value)
  else if (i == "missing.value.name")
    return(x@missing.value.name)
})

#' Method to access S4 Var elements
#'
#' Method to modify VarGroup [initialize,VarGroup-method()] elements by name
#'
#' @param x Object
#' @param i Element name
#' @param value Value to be added
#'
#' @return object
methods::setReplaceMethod("[", "VarGroup", function(x, i, value) {
  if (i == "group")
    x@group <- value
  else if (i == "pvalue")
    x@pvalue <- value
  else if (i == "parsed_name")
    x@parsed_name <- value
  else if (i == "value")
    x@value <- value
  else if (i == "missing.value")
    x@value <- x@missing.value
  else if (i == "missing.value.name")
    x@value <- x@missing.value.name
  return(x)
})

methods::setAs(
  from = "Var",
  to = "VarGroup",
  def = function(from) {
    return(VarGroup(x = from, group_var = "Total"))
  })

#################
#' S4 class
#'
#' A S4 class containing all the information needed for parsClassFun
#' the missing values and the group for which it was calculated
#'
#' @slot table The result of descTab
#' @slot group The variable from which to make subgroups
#' @slot pvalue,na.print,quanti,quali Values from descTab [descTab()]
#' @slot var_list An object of listVar [listVar-class()]
#' @slot data The dataset provided in descTab
#' @slot digits.qt,digits.ql As provided in descTab
methods::setClass("parseClass",
         slots = c(table = "data.frame", group = "character",
                   pvalue = "logical", na.print = "logical",
                   quanti = "logical", quali = "logical",
                   var_list = "listVar", data = "data.frame",
                   digits.qt = "numeric", digits.ql = "numeric"))

#' S4 class initialization function
#'
#' Initialization function for parseClass object [initialize,parseClass-method()]
#'
#' @param .Object The object to create
#' @param table The result of descTab
#' @param group The variable from which to make subgroups
#' @param pvalue,na.print,quanti,quali Values from descTab [descTab()]
#' @param var_list An object of listVar [listVar-class()]
#' @param data The dataset provided in descTab
#' @param digits.qt,digits.ql As provided in descTab
#'
#' @return parseClass object
methods::setMethod("initialize",
          "parseClass",
          function(.Object, table, group, pvalue, na.print,
                   quanti, quali, var_list, data, digits.qt, digits.ql) {
            .Object@table<-table
            .Object@group <-group
            .Object@pvalue<-pvalue
            .Object@na.print <- na.print
            .Object@quanti <- quanti
            .Object@quali<-quali
            .Object@var_list<-var_list
            .Object@data<-data
            .Object@digits.qt<-digits.qt
            .Object@digits.ql<-digits.ql

            return(.Object)
          })

#' S4 class initialization function
#'
#' Initialization function for parseClass object [initialize,parseClass-method()]
#'
#' @param table The result of descTab
#' @param group The variable from which to make subgroups
#' @param pvalue,na.print,quanti,quali Values from descTab [descTab()]
#' @param var_list An object of listVar [listVar-class()]
#' @param data The dataset provided in descTab
#' @param digits.qt,digits.ql As provided in descTab
#'
#' @return parseClass object
parseClass <- function(table, group, pvalue, na.print,
                       quanti, quali, var_list, data, digits.qt, digits.ql) {
  return(methods::new("parseClass", table = table, group = group, pvalue = pvalue,
             na.print = na.print, quanti = quanti, quali = quali,
             var_list = var_list, data = data, digits.qt = digits.qt, digits.ql = digits.ql))
}

#' Method to access S4 Var elements
#'
#' Method to access parseClass [initialize,parseClass-method()] elements by name
#'
#' @param x : Object
#' @param i : Element name
#'
#' @return object
#' @export
methods::setMethod("[", "parseClass", function(x, i) {
  if (i == "table")
    return(x@table)
  else if (i == "group")
    return(x@group)
  else if (i == "na.print")
    return(x@na.print)
  else if (i == "quanti")
    return(x@quanti)
  else if (i == "quali")
    return(x@quali)
  else if (i == "var_list")
    return(x@var_list)
  else if (i == "data")
    return(x@data)
  else if (i == "digits.qt")
    return(x@digits.qt)
  else if (i == "digits.ql")
    return(x@digits.ql)
  return(x)
})

#' Method to modify S4 Var elements
#'
#' Method to modify parseClass [initialize,parseClass-method()] elements by name
#'
#' @param x : Object
#' @param i : Element name
#' @param value : Value to be added
#' @return parseClass Object
#' @export
methods::setMethod("[<-", signature = "parseClass", function(x, i, value) {
  if (i == "table")
    x@table <- value
  else if (i == "group")
    x@group <- value
  else if (i == "na.print")
    x@na.print <- value
  else if (i == "quanti")
    x@quanti <- value
  else if (i == "quali")
    x@quali <- value
  else if (i == "var_list")
    x@var_list <- value
  else if (i == "data")
    x@data<- value
  else if (i == "digits.qt")
    x@digits.qt<-value
  else if (i == "digits.ql")
    x@digits.ql<-value
  return(x)
})
