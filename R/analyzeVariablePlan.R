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

##----------------------------------------------------------------
##                    Analyze variable plan.                    --
##----------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
##----------------------------------------------------------------
AnalyzeVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  type <- "AnalyzeVar"
  # Dependency variables
  plan_vars <- paste0(c("sampleSize", "kValue"), type)
  pd_vars <- paste0(c("pd_lower", "pd_upper", "pd_step", "pd_unit"), type)

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["analyzeVarContainer"]]) || jaspResults[["analyzeVarContainer"]]$getError()) {
    analyzeVarContainer <- createJaspContainer(title = gettext("Analyze Variable Plan"))
    analyzeVarContainer$dependOn(c(plan_vars, pd_vars)) # Common dependencies
    jaspResults[["analyzeVarContainer"]] <- analyzeVarContainer
  } else {
    analyzeVarContainer <- jaspResults[["analyzeVarContainer"]]
  }
  # Plan variables
  N <- options[[paste0("lotSize", type)]]
  n <- options[[paste0("sampleSize", type)]]
  k <- options[[paste0("kValue", type)]]
  sd <- if (options[[paste0("sd", type)]]) "known" else "unknown"

  # Initialize the plan table
  plan_table <- createJaspTable(title = gettextf("Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>)", sd))
  plan_table$transpose <- TRUE
  plan_table$transposeWithOvertitle <- FALSE
  plan_table$dependOn(c(plan_vars, "sd"))
  plan_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  plan_table$addColumnInfo(name = "col_1", title = gettext("Sample size"), type = "integer")
  plan_table$addColumnInfo(name = "col_2", title = gettext("Critical Distance (k)"), type = "number")
  plan_table$addRows(list("col_1" = n, "col_2" = k))
  plan_table$showSpecifiedColumnsOnly <- TRUE
  plan_table$position <- 1
  analyzeVarContainer[["plan_table"]] <- plan_table

  # Error check for n (sample size)
  if (sd == "unknown" && (n <= 1)) {
    analyzeVarContainer$setError(gettext("If historical standard deviation is unknown, sample size has to be > 1."))
    return ()
  }

  # Error check for N (lot size)
  if (N < n) {
    analyzeVarContainer$setError(gettextf("Lot size (N = %1$.0f) cannot be smaller than the sample size (n = %2$.0f) of the generated variable plan.", N, n))
    return ()
  }
  plan <- getPlan(analyzeVarContainer, options, type, n, k=k, sd=sd)
  if (analyzeVarContainer$getError()) {
    return ()
  }

  # Plan dataframe
  df_plan <- plan$df_plan
  oc_plan <- plan$oc_plan

  risk_vars <- paste0(c("aql", "prod_risk", "rql", "cons_risk"), type)
  output_vars <- paste0(c("assessPlan", "showSummary", "showOCCurve", "showAOQCurve", "showATICurve"), type)

  # 1. Assess plan
  if (options[[output_vars[1]]]) {
    assessPlan(analyzeVarContainer, pos=2, c(risk_vars, output_vars[1]), oc_plan, options, type)
    if (analyzeVarContainer$getError()) {
      return ()
    }
  }

  # 2. Plan summary
  if (options[[output_vars[2]]]) {
    # Assess plan generates 2 output elements, so position of next element is previous + 2.
    getSummary(analyzeVarContainer, pos=4, output_vars[2], df_plan, options, type, n)
  }
  # 3. OC Curve
  if (options[[output_vars[3]]]) {
    getOCCurve(analyzeVarContainer, pos=5, output_vars[3], df_plan)
  }
  # 4. AOQ Curve
  if (options[[output_vars[4]]]) {
    getAOQCurve(analyzeVarContainer, pos=6, c(output_vars[4], paste0("lotSize", type)), df_plan, options, type, n)
    if (analyzeVarContainer$getError()) {
      return ()
    }
  }
  # 5. ATI Curve
  if (options[[output_vars[5]]]) {
    getATICurve(analyzeVarContainer, pos=7, c(output_vars[5], paste0("lotSize", type)), df_plan, options, type, n)
  }
}