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
  risk_vars <- c("aql", "prod_risk", "rql", "cons_risk")
  # Quality levels
  pd_vars <- c("pd_lower", "pd_upper", "pd_step")
  depend_vars <- c(pd_vars, risk_vars, "lotSize", "distribution")

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["createContainer"]]) || jaspResults[["createContainer"]]$getError()) {
    createContainer <- createJaspContainer(title = gettext(""))
    createContainer$dependOn(depend_vars)
    jaspResults[["createContainer"]] <- createContainer
  } else {
    createContainer <- jaspResults[["createContainer"]]
  }

  # Plan table outline
  plan_table <- createJaspTable(title = gettext("Generated Sampling Plan"))
  plan_table$addColumnInfo(name = "col_1", title = gettext(""), type = "string")
  plan_table$addColumnInfo(name = "col_2", title = gettext("Value"), type = "integer")
  plan_table[["col_1"]] <- c("Sample size", "Acceptance number")
  plan_table$position <- 1
  createContainer[["findPlanTable"]] <- plan_table

  # Probability table outline
  prob_table <- createJaspTable(title = gettext("Acceptance probabilities at AQL and RQL"))
  prob_table$addColumnInfo(name = "col_1", title = gettext(""), type = "string")
  prob_table$addColumnInfo(name = "col_2", title = gettext("Proportion Non-conforming"), type = "number")
  prob_table$addColumnInfo(name = "col_3", title = gettext("Acceptance Probability"), type = "number")
  prob_table$addColumnInfo(name = "col_4", title = gettext("Rejection Probability"), type = "number")
  prob_table[["col_1"]] <- c("AQL", "RQL")
  prob_table$position <- 2
  createContainer[["findProbTable"]] <- prob_table
  
  # Error handling for hypergeometric distribution
  aql <- options$aql
  rql <- options$rql
  checkHypergeom(createContainer, pd_vars, options, type="", aql, rql)
  if (createContainer$getError()) {
    return ()
  }
  
  # Error handling for AQL/RQL
  if (aql >= rql) {
    createContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }

  # Error handling for Producer's and Consumer's Risk
  pa_prod <- (1 - options$prod_risk)
  pa_cons <- options$cons_risk
  if (pa_prod <= pa_cons) {
    createContainer$setError(gettext("1 - \u03B1 (Producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }
  # Sanity checks done. Let's find a plan that satisfies the constraints.
  .findPlan(createContainer, options, depend_vars, aql, rql, pa_prod, pa_cons)
}

##----------------------------------------------------------------------------------
##  Find the sampling plan that satisfies the specified AQL and RQL constraints.  --
##----------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param pa_prod {numeric} Minimum probability (0 to 1) of accepting the lot at Acceptable Quality Level.
#' @param pa_cons {numeric} Maximum probability (0 to 1) of accepting the lot at Rejectable Quality Level.
#' @seealso CreateAttributePlan()
##----------------------------------------------------------------------------------
.findPlan <- function(jaspContainer, options, depend_vars, aql, rql, pa_prod, pa_cons) {
  pd_lower <- options$pd_lower
  pd_upper <- options$pd_upper
  pd_step <- options$pd_step
  pd <- seq(pd_lower, pd_upper, pd_step)
  # Add AQL and RQL to quality range.
  pd <- c(pd, aql, rql)
  pd <- pd[!duplicated(pd)]
  pd <- sort(pd)
  dist <- options$distribution
  plan_values <- NULL
  plan <- NULL
  
  # # Create sampling plan with the specified values
  if (dist == "hypergeom") {
    # Need to provide the lot size (N) for hypergeometric distribution.
    plan_values <- AcceptanceSampling::find.plan(PRP = c(aql, pa_prod), CRP = c(rql, pa_cons), type = dist, N = options$lotSize)
    plan <- AcceptanceSampling::OC2c(N = options$lotSize, n = plan_values$n, c = plan_values$c, r = plan_values$r, type = dist, pd = pd)
  } else {
    # Binomial and Poisson distributions don't require lot size (N) or standard deviation.
    plan_values <- AcceptanceSampling::find.plan(PRP = c(aql, pa_prod), CRP = c(rql, pa_cons), type = dist)
    plan <- AcceptanceSampling::OC2c(n = plan_values$n, c = plan_values$c, r = plan_values$r, type = dist, pd = pd)
  }
  n <- plan_values$n
  c <- plan_values$c
  r <- plan_values$r
  
  df_plan <- data.frame(PD = pd, PA = plan@paccept)
  df_plan <- na.omit(df_plan)
  if (nrow(df_plan) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  # Fill the output tables for the created plan.
  .attributePlanTable(jaspContainer, depend_vars, aql, rql, df_plan$PA[df_plan$PD == aql], df_plan$PA[df_plan$PD == rql], n, c, r)

  # Plan summary
  if (options$showSummary) {
    getSummary(jaspContainer, pos=4, c(depend_vars, "showSummary"), df_plan)
  }

  # OC Curve
  if (options$showOCCurve) {
    getOCCurve(jaspContainer, pos=5, c(depend_vars, "showOCCurve"), df_plan)
  }

  # AOQ Curve (for plans with rectification)
  if (options$showAOQCurve) {
    getAOQCurve(jaspContainer, pos=6, "showAOQCurve", df_plan, options, "", n)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ATI Curve (for plans with rectification)
  if (options$showATICurve) {
    getATICurve(jaspContainer, pos=7, "showATICurve", df_plan, options, "", n)
    if (jaspContainer$getError()) {
      return ()
    }
  }
}

##----------------------------------------------------------------
##             Create and fill the output table(s).             --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param pa_prod {numeric} Minimum probability (0 to 1) of accepting the lot at Acceptable Quality Level.
#' @param pa_cons {numeric} Maximum probability (0 to 1) of accepting the lot at Rejectable Quality Level.
#' @param n {numeric} Sample size for the plan.
#' @param c {numeric} Acceptance number for the plan.
#' @param r {numeric} Rejection number for the plan.
##----------------------------------------------------------------
.attributePlanTable <- function(jaspContainer, depend_vars, aql, rql, pa_prod, pa_cons, n, c, r) {
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