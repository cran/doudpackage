######### Functions to parse with kableExtra ########
## Function to remove one level for binary variables
parseQuali<-function(table, levels_to_keep){
  factor_list<-purrr::compact(lapply(table@var_list@List,function(x){
    if("factor" %in% x@type && !table@group %in% x@name) return(x@name)}))
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
  if (length(var) != 0)
    table@table<-table@table %>%
      dplyr::filter(!var %in% !!(unlist(var)))
  return(table@table)
}
########################################

########## Make Kable Extra ############
# Reorder the rows so that the variables of a same label are contiguous and the
# labels follow the order given by the user, then return the row spans that
# pack_rows needs. Returns NULL when there is nothing to group.
orderRowForGroupLabels<-function(table, group_rows_labels){
  if (is.null(group_rows_labels))
    return(list(table = table@table, spans = NULL))
  row_var<-rowVarName(table@table[, 1], describedVars(table))
  labelled<-integer(0)
  spans<-NULL
  for (label in names(group_rows_labels)){
    rows<-unlist(lapply(group_rows_labels[[label]], function(v) which(row_var == v)))
    rows<-setdiff(rows, labelled)
    if (length(rows) == 0){
      warning(sprintf("No row to group under \"%s\": %s not described in the table",
                      label, paste(group_rows_labels[[label]], collapse = ", ")))
      next
    }
    spans<-cbind(spans, stats::setNames(c(length(labelled) + 1,
                                          length(labelled) + length(rows)), NULL))
    colnames(spans)[ncol(spans)]<-label
    labelled<-c(labelled, rows)
  }
  if (length(labelled) == 0)
    return(list(table = table@table, spans = NULL))
  rest<-setdiff(seq_len(nrow(table@table)), labelled)
  return(list(table = table@table[c(labelled, rest), , drop = FALSE], spans = spans))
}

## Function to rename columns according to the counts of each sub-group
getPopGroups<-function(table)
{
  col.names<-lapply(colnames(table@table), function(col, table){
    if (table@group != "")
      factor<-levels(table@data[,table@group])
    else
      factor<-""
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

# With na.print, a missing value row that is empty in every column carries no
# information: drop it before the rows are grouped so the spans stay right.
dropEmptyMissingRows<-function(table){
  if (!"Total" %in% colnames(table@table))
    return(table@table)
  is.missing<-!is.na(rowVarName(table@table[, 1], describedVars(table))) &
    grepl("Missing values", table@table[, 1], fixed = TRUE)
  return(table@table[!(is.missing & table@table[, "Total"] %in% "0 (0)"), , drop = FALSE])
}

#' @import tidyr
#' @import kableExtra
makeKableExtra<-function(table, col.order, spans, font){
  if (table@na.print == TRUE){
    vars<-rowVarName(table@table[, 1], describedVars(table))
    missing.rows<-!is.na(vars) & grepl("Missing values", table@table[, 1], fixed = TRUE)
    table@table[missing.rows, 1]<-"Missing values"
    ident<-which(missing.rows)
  }

  rownames(table@table)<-NULL
  headers<-rep(1, length(col.order))
  names(headers)<-col.order
  names(headers)[names(headers) == "var"]<- ' '
  names(headers)[names(headers) == "pvalue"]<- ' '

  res_parsed<-kableExtra::kable(table@table) %>%
        kableExtra::kable_paper(html_font = font) %>%
        kableExtra::add_header_above(headers)
  if (!is.null(spans)){
    for (group in colnames(spans)){
      res_parsed<- res_parsed %>%
        kableExtra::pack_rows(group, spans[1, group], spans[2, group])
      }
    }

  if (table@na.print == TRUE && length(ident) != 0){
    res_parsed<-res_parsed %>%
      kableExtra::add_indent(ident, level_of_indent = 1)
  }
  return(res_parsed)
}

#### Main parsing Function #######
#' Make the LaTeX/HTML table. Generic function
#'
#' @param table The output of [descTab()] or [anaBiv()], an S4 object.
#' @param col.order Optional. A vector containing the column order. If set, must contain at least all levels of group, without duplicates. Three columns created are "var", "Total", and "pvalue" which can be present in the vector
#' @param levels_to_keep Optional, named list. If the variable is binary, which level to keep. Default is the last level of levels(variable). Must be as: list("variable name" = "level to keep").
#' @param group_rows_labels Optional, named list. Create row labels in order to regroup them. Must be as list("label" = c("var1", "var2"), "label2" = c("var3", "var4")). Variable names are matched exactly, and a variable with no row in the table is an error. Rows are reordered to follow the order of the labels and of the variables within each label.
#' @param font The HTML font of the table. Default "arial".
#'
#' @return An HTML/LaTex file which can be used directly in Rmarkdown and copy paste
#' @seealso [descTab()] which produces the object this function renders.
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
#' tab <- descTab(patients, group = "arm", normality = "assess", na.print = TRUE)
#'
#' # Default layout
#' parseClassFun(tab)
#'
#' # One line for the binary variables, rows grouped under labels, the Total
#' # column after the arms, and a custom font
#' parseClassFun(
#'   tab,
#'   levels_to_keep = list(sex = "Female", diabetes = "Yes"),
#'   group_rows_labels = list("Demographics" = c("age", "sex"),
#'                            "Clinical" = c("crp", "diabetes", "nyha")),
#'   col.order = c("var", "Placebo", "Treatment", "Total", "pvalue"),
#'   font = "Times New Roman"
#' )
methods::setGeneric("parseClassFun", function(table, col.order = NULL,
                                              levels_to_keep = NULL,
                                              group_rows_labels = NULL,
                                              font = "arial") {
  return(standardGeneric("parseClassFun"))
})

#' Make the LaTeX/HTML table
#'
#' This functions takes the S4 output of descTab to create an HTML parsed table
#' @inherit parseClassFun
#' @export
methods::setMethod("parseClassFun", "parseClass", function(table, col.order = NULL,
                                                          levels_to_keep = NULL,
                                                          group_rows_labels = NULL,
                                                          font = "arial"){

  checkVarParseClassFun(levels_to_keep, col.order, group_rows_labels, table)

  if (table@pvalue == TRUE)
    table@table$pvalue<-as.character(ifelse(is.na(table@table$pvalue), "",
                                            ifelse(table@table$pvalue < 0.001,
                                                   "< 0.001", table@table$pvalue)))

  table@table<-parseQuali(table, levels_to_keep)

  if (table@na.print == TRUE)
    table@table<-dropEmptyMissingRows(table)

  ordered<-orderRowForGroupLabels(table, group_rows_labels)
  table@table<-ordered$table

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
  parsed_table<-makeKableExtra(table, col.order, ordered$spans, font)
  return(parsed_table)
})
