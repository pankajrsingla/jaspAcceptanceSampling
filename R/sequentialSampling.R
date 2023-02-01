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
  risk_vars <- c("aql", "prod_risk", "rql", "cons_risk")
  # Quality levels
  pd_vars <- c("pd_lower", "pd_upper", "pd_step")
  depend_vars <- c(pd_vars, risk_vars, "lotSize", "max_n")

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
  planCurve$dependOn(depend_vars)
  planCurve$position <- 1
  seqContainer[["planCurve"]] <- planCurve

  # Error handling for hypergeometric distribution
  aql <- options$aql
  rql <- options$rql
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
  pa_prod <- (1 - options$prod_risk)
  pa_cons <- options$cons_risk
  if (pa_prod <= pa_cons) {
    seqContainer$setError(gettext("1 - \u03B1 (Producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }
  # Sanity checks done. Let's find a plan that satisfies the constraints.
  .findSeqPlan(seqContainer, options, depend_vars, aql, rql, pa_prod, pa_cons)
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
.findSeqPlan <- function(jaspContainer, options, depend_vars, aql, rql, pa_prod, pa_cons) {
  pd_lower <- options$pd_lower
  pd_upper <- options$pd_upper
  pd_step <- options$pd_step
  pd <- seq(pd_lower, pd_upper, pd_step)
  # Add AQL and RQL to quality range.
  pd <- c(pd, aql, rql)
  pd <- pd[!duplicated(pd)]
  pd <- sort(pd)
  # dist <- options$distribution
  N <- options$lotSize

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
  L <- options$max_n
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
  
  df_plan <- data.frame(PD = p, PA = PA)
  df_plan <- na.omit(df_plan)
  if (nrow(df_plan) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  # Draw the visual summary plot.
  planCurve <- jaspContainer[["planCurve"]]
  df_plot <- data.frame(n=k, accept=accept, reject=reject)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plot$n), max(df_plot$n)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plot$accept), max(df_plot$reject)))
  plt <- ggplot2::ggplot(data = df_plot) +
                  # ggplot2::geom_point(ggplot2::aes(x = n, y = accept, colour = "blue", shape = 19)) +
                  ggplot2::geom_line(ggplot2::aes(x = n, y = accept, colour = "green", linetype = "dashed")) +
                  # ggplot2::geom_point(ggplot2::aes(x = n, y = reject, colour = "red", shape = 19)) +
                  ggplot2::geom_line(ggplot2::aes(x = n, y = reject, colour = "orange", linetype = "dashed")) +
                  ggplot2::labs(x = gettext("Number of Items Sampled"), y = gettext("Acceptance and Rejection Criteria")) +
                  ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  planCurve$plotObject <- plt
  planCurve$position <- 1
  jaspContainer[["planCurve"]] <- planCurve

  # Plan summary
  if (options$showSummary) {
    getSummary(jaspContainer, pos=2, c(depend_vars, "showSummary"), df_plan)
  }

  # OC Curve
  if (options$showOCCurve) {
    getOCCurve(jaspContainer, pos=3, c(depend_vars, "showOCCurve"), df_plan)
  }

  # AOQ Curve (for plans with rectification)
  if (options$showAOQCurve) {
    if (is.null(jaspContainer[["aoqCurve"]])) {
      aoqCurve <- createJaspPlot(title = gettext("AOQ (Average Outgoing Quality) Curve"), width = 480, height = 320)
      aoqCurve$dependOn(depend_vars)
      jaspContainer[["aoqCurve"]] <- aoqCurve
      # AOQL (Average Outgoing Quality Limit)
      df_plan$AOQ <- AOQ
      aoql <- max(df_plan$AOQ)
      # pd_aoql <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD), max(df_plan$PD)))
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$AOQ), 1.2*aoql))
      plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = AOQ)) +
            ggplot2::geom_point(colour = "black", shape = 19) + 
            ggplot2::labs(x = gettext("(Incoming) Proportion Non-conforming"), y = gettext("Average Outgoing Quality")) +
            ggplot2::geom_line(colour = "black", linetype = "dashed") +
            ggplot2::geom_hline(yintercept = aoql, linetype = "dotted") +
            ggplot2::annotate("text", label = gettextf("AOQL: %.3f", aoql),
                              x = (min(df_plan$PD) + max(df_plan$PD)) / 2, y = aoql*1.1, color = "black", size = 6) +
            ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
            ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
            # ggplot2::ylim(0.0,round(aoql*1.2, 2))
      plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
      plt$position <- 4
      aoqCurve$plotObject <- plt
    }
  }

  # ATI Curve (for plans with rectification)
  if (options$showATICurve) {
    if (is.null(jaspContainer[["atiCurve"]])) {
      atiCurve <- createJaspPlot(title = gettext("ATI (Average Total Inspection) Curve"), width = 480, height = 320)
      atiCurve$dependOn(depend_vars)
      jaspContainer[["atiCurve"]] <- atiCurve
      df_plan$ATI <- ATI
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(df_plan$PD)
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(df_plan$ATI)
      plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ATI)) +
            ggplot2::geom_point(colour = "black", shape = 19) +
            ggplot2::geom_line(colour = "black", linetype = "dashed") +
            ggplot2::labs(x = gettext("Proportion Non-conforming"), y = gettext("Average Total Inspection")) +
            ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
            ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
            # ggplot2::scale_y_continuous(breaks = pretty(df_plan$ATI))
      plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
      plt$position <- 5
      atiCurve$plotObject <- plt
    }
  }

  # ASN Curve
  if (options$showASNCurve) {
    if (is.null(jaspContainer[["asnCurve"]])) {    
      asnCurve <- createJaspPlot(title = gettext("ASN (Average Sample Number) Curve"), width = 480, height = 320)
      asnCurve$dependOn(depend_vars)
      jaspContainer[["asnCurve"]] <- asnCurve
      df_plan$ASN <- ASN
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD), max(df_plan$PD)))
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$ASN), max(df_plan$ASN)))
      plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ASN)) +
            ggplot2::geom_point(colour = "black", shape = 19) +
            ggplot2::geom_line(colour = "black", linetype = "dashed") +
            ggplot2::labs(x = gettext("Proportion Non-conforming"), y = gettext("Average Sample Number")) +
            ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
            ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
      plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
      plt$position <- 6
      asnCurve$plotObject <- plt
    }
  }
}

# library(ggplot2)
# df <- data.frame(a=c(1,2), b=c(3,4), c=c(50,60))
# ggplot2::ggplot(data=df) + geom_point(ggplot2::aes(x=a,y=b,color=c)) + geom_point(ggplot2::aes(x=a,y=c,color=b))
