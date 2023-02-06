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
SequentialSampling <- function(jaspResults, dataset = NULL, options, ...) {
  # Constraints to create a plan
  type <- "Seq"
  risk_vars <- paste0(c("aql", "prod_risk", "rql", "cons_risk"), type)
  # Quality levels
  pd_vars <- paste0(c("pd_lower", "pd_upper", "pd_step"), type)
  depend_vars <- c(pd_vars, risk_vars, paste0("lotSize", type), paste0("max_n", type))

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["seqContainer"]]) || jaspResults[["seqContainer"]]$getError()) {
    seqContainer <- createJaspContainer(title = "")
    seqContainer$dependOn(depend_vars)
    jaspResults[["seqContainer"]] <- seqContainer
  } else {
    seqContainer <- jaspResults[["seqContainer"]]
  }  

  # Visual summary plot: create outline now, draw plot later.
  planCurve <- createJaspPlot(title = gettext("Sequential Plan Plot"),  width = 480, height = 320)
  planCurve$position <- 1
  seqContainer[["planCurve"]] <- planCurve

  # Error handling for hypergeometric distribution
  aql <- options[[paste0("aql", type)]]
  rql <- options[[paste0("rql", type)]]
  # checkHypergeom(seqContainer, pd_vars, options, type="", aql, rql)
  # if (seqContainer$getError()) {
  #   return ()
  # }

  # Error handling for AQL/RQL
  if (aql >= rql) {
    seqContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }

  # Error handling for Producer's and Consumer's Risk
  pa_prod <- (1 - options[[paste0("prod_risk", type)]])
  pa_cons <- options[[paste0("cons_risk", type)]]
  if (pa_prod <= pa_cons) {
    seqContainer$setError(gettext("1 - \u03B1 (Producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }
  # Sanity checks done. Let's find a plan that satisfies the constraints.
  .findSeqPlan(seqContainer, options, type, depend_vars, aql, rql, pa_prod, pa_cons)
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
.findSeqPlan <- function(jaspContainer, options, type, depend_vars, aql, rql, pa_prod, pa_cons) {
  pd <- getPDValues(jaspContainer, options, type)$PD_Prop
  # dist <- options$distribution
  N <- options[[paste0("lotSize", type)]]

  # # Create sampling plan with the specified values
  # if (dist == "hypergeom") {
  #   # Need to provide the lot size (N) for hypergeometric distribution.
  #   # Todo: Add support for hypergeometric distribution
  # } else if (dist == "poisson") {
  #   # Todo: Add support for poisson distribution
  # } else {
  # Binomial
  a <- log((1-pa_cons)/(1-pa_prod))
  b <- log((pa_prod)/pa_cons)
  g1 <- log(rql/aql)
  g2 <- log((1-aql)/(1-rql))
  G <- g1 + g2
  h1 <- b/G
  h2 <- a/G
  s <- g2/G
  fn <- function(h) {
    x <- ((1-rql)/(1-aql))^h
    y <- (rql/aql)^h
    (1- x)/(y-x) - pd    
  }
  h <- nleqslv::nleqslv(pd, fn)$x
  p <- (1-((1 - rql)/(1 - aql))^h )/(((rql/aql)^h)-(((1 - rql)/(1 - aql))^h))    
  # L <- round(2*h1/s)
  L <- options[[paste0("max_n", type)]]
  k <- seq(1, L, 1)
  accept <- -h1 + s*k
  reject <- h2 + s*k
  k1 <- ((1 - pa_cons)/(1 - pa_prod))^h
  k2 <- (pa_cons/(pa_prod))^h
  PA <- (k1 - 1) / (k1 - k2)
  AOQ <- p * PA
  k5 <- PA * log(pa_cons/(pa_prod)) 
  k3 <- k5 + (1 - PA) * log((1 - pa_cons)/(1 - pa_prod))
  k4 <- p * log(rql / aql) + (1 - p) * log((1 - rql) / (1 - aql))
  ASN <- k3/k4
  ATI <- k5/k4 + (1 - PA) * N
  # }
  
  # Calculate the defect probability levels in the specified unit
  pd_unit <- options[[paste0("pd_unit", type)]]
  factor <- 1
    if (pd_unit == "percent") {
      factor <- 10^2 
    } else if (pd_unit == "per_million") {
      factor <- 10^6
    }
  p_orig <- p * factor
  df_plan <- data.frame(PD_Prop=p, PD_Orig=p_orig, PA=PA, AOQ=AOQ, ATI=ATI, ASN=ASN)
  df_plan <- na.omit(df_plan)
  if (nrow(df_plan) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  # Draw the visual summary plot.
  # Todo: add legend for the accept and reject lines.
  planCurve <- jaspContainer[["planCurve"]]
  df_plot <- data.frame(n=k, accept=accept, reject=reject)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plot$n), max(df_plot$n)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plot$accept), max(df_plot$reject)))
  plt <- ggplot2::ggplot(data = df_plot, ggplot2::aes(x = n)) +
                  ggplot2::geom_line(ggplot2::aes(y = reject, colour = reject), colour = "red", linetype = "dashed") +
                  ggplot2::geom_line(ggplot2::aes(y = accept, colour = accept), colour = "green", linetype = "dashed") +
                  ggplot2::labs(x = gettext("Number of Items Sampled"), y = gettext("Acceptance/Rejection Criteria"), colour = gettext("Decision")) +
                  ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  planCurve$plotObject <- plt
  planCurve$position <- 1
  jaspContainer[["planCurve"]] <- planCurve

  output_vars <- paste0(c("showSummary", "showOCCurve", "showAOQCurve", "showATICurve", "showASNCurve"), type)

  # Plan summary
  if (options[[output_vars[1]]]) {
    getSummary(jaspContainer, pos=2, output_vars[1], df_plan, options, type)
    if (jaspContainer$getError()) {
      return ()
    } 
  }

  # OC Curve
  if (options[[output_vars[2]]]) {
    getOCCurve(jaspContainer, pos=3, output_vars[2], df_plan, options, type)
  }

  # AOQ Curve (for plans with rectification)
  if (options[[output_vars[3]]]) {
    getAOQCurve(jaspContainer, pos=4, output_vars[3], df_plan, options, type)
    if (jaspContainer$getError()) {
      return ()
    }    
  }

  # ATI Curve (for plans with rectification)
  if (options[[output_vars[4]]]) {
    getATICurve(jaspContainer, pos=5, output_vars[4], df_plan, options, type)
    if (jaspContainer$getError()) {
      return ()
    }    
  }

  # ASN Curve
  if (options[[output_vars[5]]]) {
    getASNCurve(jaspContainer, pos=6, output_vars[4], df_plan, options, type)
    if (jaspContainer$getError()) {
      return ()
    }    
  }
}