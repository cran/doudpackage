######### Functions to parse with kableExtra ########
## Function to remove one level for binary variables
parseQuali<-function(table, levels_to_keep){
  factor_list<-purrr::compact(lapply(table@var_list@List,function(x){
    if("factor" %in% x@type) return(x@name)}))

  # Function to assemble row that will be removed from table
  var<-purrr::compact(lapply(factor_list, function(factor, data, table, levels_to_keep){
    var<-data[, factor]
    var_levels<-levels(var)

    if (nlevels(var) == 2){
      if(is.null(levels_to_keep) || !factor %in% names(levels_to_keep)){
        max_level<-var_levels[nlevels(var) - 1]
        ret<- paste(factor, ', ', max_level, sep = "")
        return(ret)
      }
      else {
        ret<-unlist(purrr::compact(lapply(names(levels_to_keep), function(level_to_k.name, var_levels, levels_to_keep, factor, table){
          if (factor == level_to_k.name){
            if (!levels_to_keep[[level_to_k.name]] %in% var_levels){
              stop(sprintf("%s is not a valid level of %s",
                           levels_to_keep[[level_to_k.name]], level_to_k.name))
            }
            var_level_to_delete<-var_levels[-which(var_levels == levels_to_keep[[level_to_k.name]])]
            ret<-paste(factor, ', ', var_level_to_delete, sep = "")
            return(ret)
          }
        }, var_levels, levels_to_keep, factor, table)))
        return(ret)
      }
    }
    else
      return(NULL)
  }, table@data, table@table, levels_to_keep))
  table@table<-table@table[-which(table@table[,"var"] %in% unlist(var)),]
  return(table@table)
}
########################################

########## Make Kable Extra ############
orderRowForGroupLabels<-function(table, group_rows_labels){
  if(is.null(group_rows_labels))
    return(table@table)
  tmp_df<-dplyr::bind_rows(purrr::compact(lapply(colnames(table@data), function(col_name, group_rows_labels, table){
    ret<-purrr::compact(lapply(names(group_rows_labels), function(group_label.name, col_name, group_rows_labels, table){
      if (col_name %in% group_rows_labels[[group_label.name]])
        return(table[grep(paste(col_name, ".*", sep = ""), table[, 1]), ])
       else
        return (NULL)
    }, col_name, group_rows_labels, table))
  }, group_rows_labels, table@table)))
  var<-dplyr::pull(tmp_df, "var")
  table@table<-table@table[-which(table@table[,"var"] %in% var),]
  table@table<-dplyr::bind_rows(tmp_df, table@table)
  return(table@table)
}

#' @import tidyr
#' @import kableExtra
makeKableExtra<-function(table, col.order, group_rows_labels){
  if (table@na.print == TRUE){
    col.names<-colnames(table@table) # Its easier to subset
    colnames(table@table)<-col.order
    table@table<-subset(table@table, !(table@table[,"Total"] %in% "0 (0)" & grepl(".*Missing values", table@table[,1])))
    colnames(table@table)<-col.names
    groupMinMax<-getGroupMinMax(group_rows_labels, table@table)
    table@table[,1]<-stringi::stri_replace_all(str = table@table[,1],
                                               replacement = "",
                                               regex  = '.*(?=Missing values)')

    ident<-grep("Missing values", table@table[,1])
  }
  else
    groupMinMax<-getGroupMinMax(group_rows_labels, table@table)

  rownames(table@table)<-NULL
  headers<-rep(1, length(col.order))
  names(headers)<-col.order
  names(headers)[names(headers) == "var"]<- ' '
  names(headers)[names(headers) == "pvalue"]<- ' '

  res_parsed<-kableExtra::kable(table@table) %>%
        kableExtra::kable_paper(html_font = "arial") %>% # Need to make it an option
        kableExtra::add_header_above(headers)
  if (!is.null(groupMinMax)){
    for (group in colnames(groupMinMax)){
      res_parsed<- res_parsed %>%
        kableExtra::pack_rows(group, groupMinMax[1,group], groupMinMax[2,group])
      }
    }

  if (table@na.print == TRUE){
    res_parsed<-res_parsed %>%
      kableExtra::add_indent(ident)
  }
  return(res_parsed)
}

## Function to rename columns according to the counts of each sub-group
getPopGroups<-function(table)
{
  if (is.null(table@group))
    return(table@table)

  col.names<-lapply(colnames(table@table), function(col, table){
    factor<-levels(table@data[,table@group])
    if (col %in% factor){
      t<-table(table@data[,table@group], useNA = "always")
      prop_table<-round(prop.table(t) * 100,
                        digits = table@digits.ql)
      col<-paste("n = ", t[col], " (", prop_table[col], ")" , sep = "")
    }
    else if (col == "Total")
      col<-paste("n = ", nrow(table@data), sep = "")
    else if (col == "var")
      col<-""
    return(col)

  }, table)
  colnames(table@table)<-unlist(col.names)
  return(table@table)
}

#### Main parsing Function #######
#' Make the LaTeX/HTML table. Generic function
#'
#' @param table The output of [descTab()] or [anaBiv()], an S4 object.
#' @param col.order Optional. A vector containing the column order. If set, must contains at least all levels of group. Three columns created are "var", "Total", and "pvalue" which can be present in the vector
#' @param levels_to_keep Optional, named list. If the variable is binary, which level to keep. Default is the last level of levels(variable). Must be as: list("variable name" = "level to keep").
#' @param group_rows_labels Optional, named list. Create row labels in order to regroup them. Must be as list("label" = c("var1", "var2), "label2" = c("var3", "var4")).
#'
#' @return An HTML/LaTex file which can be used directly in Rmarkdown and copy paste
#' @examples
#' data(iris)
#' library(stringi)
#' iris$fact_1<-as.factor(as.character(sample(1:5, 150, replace = TRUE)))
#' n_na<-sample(1:150, 30)
#' iris[n_na, "fact_1"]<-NA
#' iris$fact_2<-as.factor(as.character(stri_rand_strings(150, 1, '[A-B]')))
#' iris$num<-runif(150, min = 0, max = 100)
#' n_na<-sample(1:150, 5)
#' iris[n_na, "num"]<-NA
#' iris_test<-descTab(iris, group = "Species", na.print = TRUE)
#' testParse<-parseClassFun(iris_test, levels_to_keep = list("fact_2" =  "A"),
#' group_rows_labels = list("Size" = c("Petal.Length", "Petal.Width"),
#' "My_f" = c("num", "fact_2")))
methods::setGeneric("parseClassFun", function(table, col.order = NULL,
                                              levels_to_keep = NULL,
                                              group_rows_labels = NULL) {
  return(standardGeneric("parseClassFun"))
})

#' Make the LaTeX/HTML table
#'
#' This functions takes the S4 output of descTab to create an HTML parsed table
#' @inherit parseClassFun
#' @export
methods::setMethod("parseClassFun", "parseClass", function(table, col.order = NULL,
                                                  levels_to_keep = NULL,
                                                  group_rows_labels = NULL){
  checkVarParseClassFun(levels_to_keep, col.order, group_rows_labels, table)
  if (table@pvalue == TRUE)
    table@table$pvalue<-as.character(ifelse(is.na(table@table$pvalue), "",
                                                  ifelse(table@table$pvalue < 0.001,
                                            "< 0.001", table@table$pvalue)))
  table@table<-parseQuali(table, levels_to_keep)
  table@table<-orderRowForGroupLabels(table, group_rows_labels)
  if (!is.null(col.order)){
    if (!"var" %in% col.order)
      col.order<-c("var", col.order)
    if ("pvalue" %in% colnames(table@table) && !"pvalue" %in% col.order)
      col.order<-c(col.order, "pvalue")
    table@table<-table@table[, col.order]
  }
  else
    col.order<-colnames(table@table)
  table@table<-getPopGroups(table)
  parsed_table<-makeKableExtra(table, col.order, group_rows_labels)
  return(parsed_table)
})
