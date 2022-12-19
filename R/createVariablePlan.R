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
##                    Create variable plan.                    --
##---------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
##---------------------------------------------------------------
CreateVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Dependency variables
  risk_vars <- c("aql", "prod_risk", "rql", "cons_risk")
  pd_vars <- c("pd_lower", "pd_upper", "pd_step")
  
  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["createVarContainer"]]) || jaspResults[["createVarContainer"]]$getError()) {
    createVarContainer <- createJaspContainer(title = "Create Variable Plan")
    # Common dependencies
    createVarContainer$dependOn(risk_vars)
    jaspResults[["createVarContainer"]] <- createVarContainer
  } else {
    createVarContainer <- jaspResults[["createVarContainer"]]
  }
  
  # Description of the plan
  plan_op <- createJaspHtml(text = gettextf("%s\n\n%s", "Z.LSL = (mean - LSL) / historical standard deviation", "Accept lot if Z.LSL >= k, otherwise reject."),
                           dependencies=risk_vars, position=1)
  createVarContainer[["decision_info"]] <- plan_op

  sd <- "unknown"
  if (options$sd) {
    sd <- "known"
  }

  # Initialize the plan table
  plan_table <- createJaspTable(title = gettextf("Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>)", sd))
  plan_table$transpose <- TRUE
  plan_table$transposeWithOvertitle <- FALSE
  plan_table$dependOn(c(risk_vars, "sd"))
  plan_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  plan_table$addColumnInfo(name = "col_1", title = "Sample size", type = "integer")
  plan_table$addColumnInfo(name = "col_2", title = "Critical Distance (k)", type = "number")
  plan_table$showSpecifiedColumnsOnly <- TRUE
  plan_table$position <- 2
  createVarContainer[["plan_table"]] <- plan_table
  
  # Plan constraints
  aql <- round(options$aql, 3)
  rql <- round(options$rql, 3)
  pa_prod <- round(1 - options$prod_risk, 3)
  pa_cons <- round(options$cons_risk, 3)

  # Error handling for AQL/RQL
  if (aql >= rql) {
    createVarContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }
  # Error handling for Producer's and Consumer's Risk
  if (pa_prod <= pa_cons) {
    createVarContainer$setError(gettext("1 - \u03B1 (Producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }

  N <- options$lotSize
  # Quality levels
  pd_lower <- options$pd_lower
  pd_upper <- options$pd_upper
  pd_step <- options$pd_step
  checkPdErrors(createVarContainer, pd_lower, pd_upper, pd_step)
  if (createVarContainer$getError()) {
    return ()
  }
  pd <- seq(pd_lower, pd_upper, pd_step)
  # Add AQL and RQL to quality range
  pd <- c(pd, aql, rql)
  pd <- sort(pd)
  pd <- round(pd, 3)
  pd <- pd[!duplicated(pd)]
  
  # Sanity checks done. Let's find a plan that satisfies the constraints.
  var_plan <- tryCatch(AcceptanceSampling::find.plan(PRP = c(aql, pa_prod), CRP = c(rql, pa_cons), type = "normal", s.type = sd), error = function(x) "error")
  # find.plan can result in invalid sampling plans for certain quality constraints.
  # We want to check for such outputs.
  if (class(var_plan) == "character" && var_plan == "error") {
    createVarContainer$setError(gettext("Variable plan generated for the current quality constraints is invalid. Modify the quality constraints."))
    return ()
  }
  n <- round(var_plan$n, 3)
  k <- round(var_plan$k, 3)
  # Error checks for n
  if (is.null(n) || is.na(n) || is.infinite(n) || is.nan(n) || (n <= 0) || (!options$sd && (n <= 1))) {
    createVarContainer$setError(gettext("Variable plan generated for the current quality constraints has an invalid sample size (n). Modify the quality constraints."))
    return ()
  }
  # Error checks for k
  if (is.na(k) || is.null(k) || is.infinite(k) || is.nan(k) || k <= 0) {
    createVarContainer$setError(gettext("Variable plan generated for the current quality constraints has an invalid k value. Modify the quality constraints."))
    return ()
  }
  oc_var <- AcceptanceSampling::OCvar(n = n, k = k, type = "normal", s.type = sd, pd = pd)

  # Error check for lot size
  if (N < n) {
    createVarContainer$setError(gettextf("Lot size (N = %.0f) cannot be smaller than the sample size (n = %.0f) of the generated variable plan.", N, n))
    return ()
  }
  
  # Plan dataframe
  df_plan <- data.frame(PD = pd, PA = round(oc_var@paccept, 3))
  df_plan <- na.omit(df_plan)
  if (nrow(df_plan) == 0) {
    createVarContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  
  # Output options
  output_vars <- c("showSummary", "showOCCurve", "showAOQCurve", "showATICurve")

  # 0. Fill the variable plan table
  plan_table$addRows(list("col_1" = n, "col_2" = k))
  
  # 1. Plan summary
  if (options$showSummary) {
    getSummary(createVarContainer, pos=3, c(pd_vars, output_vars[1]), df_plan)
  }
  # 2. OC Curve
  if (options$showOCCurve) {
    getOCCurve(createVarContainer, pos=4, c(pd_vars, output_vars[2]), df_plan)
  }
  # 3. AOQ Curve
  if (options$showAOQCurve) {
    getAOQCurve(createVarContainer, pos=5, c(pd_vars, output_vars[3], "lotSize"), df_plan, options, "", n) 
    if (createVarContainer$getError()) {
      return ()
    }
  }
  # 4. ATI Curve
  if (options$showATICurve) {
    getATICurve(createVarContainer, pos=6, c(pd_vars, output_vars[4], "lotSize"), df_plan, options, "", n)
  }
}