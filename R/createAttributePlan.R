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
##                    Create attribute plan                    --
##---------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
#' @seealso .findPlan()
##---------------------------------------------------------------
CreateAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Constraints to create a plan
  type <- "CreateAttr"
  # Risk variables
  risk_vars <- paste0(c("aql", "prod_risk", "rql", "cons_risk"), type)
  # Quality levels
  pd_vars <- paste0(c("pd_lower", "pd_upper", "pd_step", "pd_unit"), type)
  depend_vars <- c(pd_vars, risk_vars, paste0(c("lotSize", "distribution"), type))

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["createContainer"]]) || jaspResults[["createContainer"]]$getError()) {
    createContainer <- createJaspContainer(title = "")
    createContainer$dependOn(depend_vars)
    jaspResults[["createContainer"]] <- createContainer
  } else {
    createContainer <- jaspResults[["createContainer"]]
  }

  # Plan table outline
  plan_table <- createJaspTable(title = gettext("Generated Sampling Plan"))
  plan_table$addColumnInfo(name = "col_1", title = "", type = "string")
  plan_table$addColumnInfo(name = "col_2", title = gettext("Value"), type = "integer")
  plan_table[["col_1"]] <- c("Sample size", "Acceptance number")
  plan_table$position <- 1
  createContainer[["findPlanTable"]] <- plan_table

  # Probability table outline
  prob_table <- createJaspTable(title = gettext("Acceptance probabilities at AQL and RQL"))
  prob_table$addColumnInfo(name = "col_1", title = "", type = "string")
  prob_table$addColumnInfo(name = "col_2", title = gettext("Proportion Non-conforming"), type = "number")
  prob_table$addColumnInfo(name = "col_3", title = gettext("Acceptance Probability"), type = "number")
  prob_table$addColumnInfo(name = "col_4", title = gettext("Rejection Probability"), type = "number")
  prob_table[["col_1"]] <- c("AQL", "RQL")
  prob_table$position <- 2
  createContainer[["findProbTable"]] <- prob_table

  # Error handling for hypergeometric distribution
  checkHypergeom(createContainer, options, type)
  if (createContainer$getError()) {
    return ()
  }

  # Error handling for AQL/RQL
  aql <- options[[paste0("aql", type)]]
  rql <- options[[paste0("rql", type)]]
  if (aql >= rql) {
    createContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }

  # Error handling for Producer's and Consumer's Risk
  pa_prod <- 1 - options[[paste0("prod_risk", type)]]
  pa_cons <- options[[paste0("cons_risk", type)]]
  if (pa_prod <= pa_cons) {
    createContainer$setError(gettext("1 - \u03B1 (Producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }
  # Sanity checks done. Let's find a plan that satisfies the constraints.
  .findPlan(createContainer, options, type, aql, rql, pa_prod, pa_cons)
}

##----------------------------------------------------------------------------------
##  Find the sampling plan that satisfies the specified AQL and RQL constraints.  --
##----------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Analysis type.
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param pa_prod {numeric} Minimum probability (0 to 1) of accepting the lot at Acceptable Quality Level.
#' @param pa_cons {numeric} Maximum probability (0 to 1) of accepting the lot at Rejectable Quality Level.
#' @seealso CreateAttributePlan()
##----------------------------------------------------------------------------------
.findPlan <- function(jaspContainer, options, type, aql, rql, pa_prod, pa_cons) {
  dist <- options[[paste0("distribution", type)]]
  plan_values <- NULL
  
  # # Create sampling plan with the specified values
  if (dist == "hypergeom") {
    # Need to provide the lot size (N) for hypergeometric distribution.
    N <- options[[paste0("lotSize", type)]]
    plan_values <- AcceptanceSampling::find.plan(PRP = c(aql, pa_prod), CRP = c(rql, pa_cons), type = dist, N = N)  
  } else {
    # Binomial and Poisson distributions don't require lot size (N) or standard deviation.
    plan_values <- AcceptanceSampling::find.plan(PRP = c(aql, pa_prod), CRP = c(rql, pa_cons), type = dist)    
  }
  n <- plan_values$n
  c <- plan_values$c
  r <- plan_values$r
  
  df_plan <- getPlan(jaspContainer, options, type, n, c, r)$df_plan
  if (jaspContainer$getError()) {
    return ()
  }
  df_plan <- na.omit(df_plan)
  if (nrow(df_plan) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  # Fill the output tables for the created plan.
  .attributePlanTable(jaspContainer, aql, rql, df_plan$PA[df_plan$PD_Prop == aql], df_plan$PA[df_plan$PD_Prop == rql], n, c, r)  

  output_vars <- paste0(c("showSummary", "showOCCurve", "showAOQCurve", "showATICurve"), type)
  # Plan summary
  if (options[[output_vars[1]]]) {
    getSummary(jaspContainer, pos=4, output_vars[1], df_plan, options, type, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # OC Curve
  if (options[[output_vars[2]]]) {
    getOCCurve(jaspContainer, pos=5, output_vars[2], df_plan, options, type)
  }

  # AOQ Curve (for plans with rectification)
  if (options[[output_vars[3]]]) {
    getAOQCurve(jaspContainer, pos=6, output_vars[3], df_plan, options, type, n)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ATI Curve (for plans with rectification)
  if (options[[output_vars[4]]]) {
    getATICurve(jaspContainer, pos=7, output_vars[4], df_plan, options, type, n)
    if (jaspContainer$getError()) {
      return ()
    }
  }
}

##----------------------------------------------------------------
##             Create and fill the output table(s).             --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param pa_prod {numeric} Minimum probability (0 to 1) of accepting the lot at Acceptable Quality Level.
#' @param pa_cons {numeric} Maximum probability (0 to 1) of accepting the lot at Rejectable Quality Level.
#' @param n {numeric} Sample size for the plan.
#' @param c {numeric} Acceptance number for the plan.
#' @param r {numeric} Rejection number for the plan.
##----------------------------------------------------------------
.attributePlanTable <- function(jaspContainer, aql, rql, pa_prod, pa_cons, n, c, r) {
  # Fill the table with sample size and acceptance number
  plan_table <- jaspContainer[["findPlanTable"]]
  plan_table[["col_2"]] <- c(n, c)

  # Fill the table with acceptance and rejection probabilities for AQL and RQL
  prob_table <- jaspContainer[["findProbTable"]]
  prob_table[["col_2"]] <- c(aql, rql)
  prob_table[["col_3"]] <- c(pa_prod, pa_cons)
  prob_table[["col_4"]] <- c(1-pa_prod, 1-pa_cons)

  # Description of the sampling plan:
  if (is.null(jaspContainer[["description"]])) {
    description <- createJaspHtml(text = gettextf("If the number of defective items out of %1$d sampled is <= %2$d, accept the lot. Reject otherwise.", n, c), position = 3)
    jaspContainer[["description"]] <- description
  }
}