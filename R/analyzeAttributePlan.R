#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

##---------------------------------------------------------------
##   Analyze single stage and multiple stage attribute plans   --
##---------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
#' @seealso .handleAttributePlan()
##---------------------------------------------------------------
AnalyzeAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  plan_vars <- c("lotSize", "distribution")
  pd_vars <- c("pd_lower", "pd_upper", "pd_step", "pd_unit")

  # Single sampling plan
  type <- "AnalyzeAttrSingle"    
  if ((options[[paste0("sampleSize", type)]] > 0) && (options[[paste0("acceptNumber", type)]] >= 0)) {
    plan_vars_single <- paste0(c(plan_vars, "sampleSize", "acceptNumber", "rejectNumber"), type)
    pd_vars_single <- paste0(pd_vars, type)

    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["singleContainer"]]) || jaspResults[["singleContainer"]]$getError()) {
      singleContainer <- createJaspContainer(title = gettext("Single Sampling Plan"))
      # Common dependencies for all single plans
      singleContainer$dependOn(c(plan_vars_single, pd_vars_single))
      jaspResults[["singleContainer"]] <- singleContainer
    } else {
      singleContainer <- jaspResults[["singleContainer"]]
    }
    .handleAttributePlan(singleContainer, pos=0, plan_vars_single, pd_vars_single, options, type)
  }

  # Multiple sampling plan
  type <- "AnalyzeAttrMult"    
  if (length(options[[paste0("stages", type)]]) > 1) {
    plan_vars_mult <- paste0(c(plan_vars, "stages", "numberOfStages"), type)
    pd_vars_mult <- paste0(pd_vars, type)
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["multContainer"]]) || jaspResults[["multContainer"]]$getError()) {
      multContainer <- createJaspContainer(title = gettext("Multiple Sampling Plan"))
      # Common dependencies for all multiple plans
      multContainer$dependOn(c(plan_vars_mult, pd_vars_mult))
      jaspResults[["multContainer"]] <- multContainer
    } else {
      multContainer <- jaspResults[["multContainer"]]
    }
    .handleAttributePlan(multContainer, pos=100, plan_vars_mult, pd_vars_mult, options, type)
  }
}

##---------------------------------------------------------------
##                   Analyze attribute plans                   --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param plan_vars {vector} Names of variables that determine a sampling plan. Used to set dependencies for outputs.
#' @param pd_vars {vector} Variables to generate a sequence of quality levels. Includes pd_unit, pd_lower, pd_upper, and pd_step.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @seealso AnalyzeAttributePlan()
##---------------------------------------------------------------
.handleAttributePlan <- function(jaspContainer, pos, plan_vars, pd_vars, options, type) {
  plan_table <- createJaspTable(title = gettext("Acceptance Sampling Plan"))
  if (type == "AnalyzeAttrSingle") {
    plan_table$dependOn(paste0(c("sampleSize", "acceptNumber", "rejectNumber"), type))
  } else {
    plan_table$dependOn(paste0("stages", type))
  }
  plan_table$showSpecifiedColumnsOnly <- TRUE
  plan_table$position <- pos
  jaspContainer[["plan_table"]] <- plan_table

  plan_values <- getPlanValues(jaspContainer, options, type)
  # Error handling for plan values
  if (jaspContainer$getError()) {
    return ()
  }

  # Error handling for hypergeometric distribution
  checkHypergeom(jaspContainer, options, type)
  if (jaspContainer$getError()) {
    return ()
  }

  n <- plan_values$n
  c <- plan_values$c
  r <- plan_values$r

  # Sampling plan table
  if (type == "AnalyzeAttrSingle") {
    # Single plan table
    plan_table$addColumnInfo(name = "table_1_col_1", title = "", type = "string")
    plan_table$addColumnInfo(name = "table_1_col_2", title = gettext("Value"), type = "integer")
    plan_table$addRows(list("table_1_col_1" = "Sample size", "table_1_col_2" = n))
    plan_table$addRows(list("table_1_col_1" = "Acceptance number", "table_1_col_2" = c))
  } else {
    # Multiple plan table
    stages <- options[[paste0("stages", type)]]
    plan_table$addColumnInfo(name = "table_1_col_1", title = gettext("Sample"), type = "integer")
    plan_table$addColumnInfo(name = "table_1_col_2", title = gettext("Sample Size"), type = "integer")
    plan_table$addColumnInfo(name = "table_1_col_3", title = gettext("Cum. Sample Size"), type = "integer")
    plan_table$addColumnInfo(name = "table_1_col_4", title = gettext("Acc. Number"), type = "integer")
    plan_table$addColumnInfo(name = "table_1_col_5", title = gettext("Rej. Number"), type = "integer")
    plan_table$setData(list(table_1_col_1 = 1:length(stages), table_1_col_2 = n, table_1_col_3 = cumsum(n),
                            table_1_col_4 = c, table_1_col_5 = r))
  }  
  # Assess plan options
  risk_vars <- paste0(c("aql", "prod_risk", "rql", "cons_risk"), type)
  # Output options
  output_vars <- paste0(c("assessPlan", "showSummary", "showOCCurve", "showAOQCurve", "showATICurve", "showASNCurve"), type)

  # Plan data
  plan <- getPlan(jaspContainer, options, type, n, c, r)
  if (jaspContainer$getError()) {
    return ()
  }
  df_plan <- plan$df_plan
  if (nrow(df_plan) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  oc_plan <- plan$oc_plan

  # Assess plan
  if (options[[output_vars[1]]]) {
    assessPlan(jaspContainer, pos=pos+1, c(output_vars[1], risk_vars), oc_plan, options, type)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # Summary table
  if (options[[output_vars[2]]]) {
    # Assess plan generates 2 output elements, so position of next element is previous + 2.
    getSummary(jaspContainer, pos=pos+3, output_vars[2], df_plan, options, type, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # OC Curve
  if (options[[output_vars[3]]]) {
    getOCCurve(jaspContainer, pos=pos+4, output_vars[3], df_plan, options, type)
  }

  # AOQ Curve (for plans with rectification)
  if (options[[output_vars[4]]]) {
    getAOQCurve(jaspContainer, pos=pos+5, output_vars[4], df_plan, options, type, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ATI Curve (for plans with rectification)
  if (options[[output_vars[5]]]) {
    getATICurve(jaspContainer, pos=pos+6, output_vars[5], df_plan, options, type, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ASN Curve (only for multiple sampling plan)
  if (options[[output_vars[6]]]) {
    getASNCurve(jaspContainer, pos=pos+7, output_vars[6], df_plan, options, type, n, c, r)
  }
}