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
BayesianSampling <- function(jaspResults, dataset = NULL, options, ...) {
  max_n <- options$max_n
  min_bf <- options$min_bf
  aql <- options$aql
  rql <- options$rql
  alpha <- beta <- 0
  prior <- options$prior
  if (prior == "impartial") {
    alpha <- (3 * rql) - aql * (4 * rql - 1) / (3 * (rql - aql))
    beta <- (2 + rql * (4 * aql - 1) - 5 * aql) / (3 * (rql - aql))
  } else if (prior == "uniform") {
    alpha <- beta <- 1
  } else if (prior == "custom") {
    alpha <- options$alpha
    beta <- options$beta
  }
  plans <- iso_bf_plans(aql, rql, max_n, min_bf, alpha, beta)
  plans$log_bf <- log(plans$bf)
  plans <- na.omit(plans)
  depend_vars <- c("max_n", "min_bf", "aql", "rql", "prior")

  if (nrow(plans) == 0) {
    explanation <- createJaspHtml(text = "No valid plans found satisfying the specified constraints")
    jaspResults[["explanation"]] <- explanation
    return ()
  }
  
  # 1. Create a table of the plans from above
  if (options$showPlans) {
    planTable <- createJaspTable(title = gettext("Sampling Plans"))
    planTable$dependOn(c(depend_vars, "showPlans"))
    # Data for the table
    planTable$addColumnInfo(name = "col_1", title = gettext("Sample size (n)"), type = "integer")
    planTable$addColumnInfo(name = "col_2", title = gettext("Acceptance number (c)"), type = "integer")
    planTable$addColumnInfo(name = "col_3", title = gettext("Bayes factor (BF)"), type = "number")
    row = list(col_1 = plans$n, col_2 = plans$c, col_3 = plans$bf)  
    planTable$setData(row)
    planTable$showSpecifiedColumnsOnly <- TRUE
    planTable$position <- 1
    jaspResults[["planTable"]] <- planTable
  }

  # 2. Plot of n vs c
  if (options$ncCurve) {
    ncCurve <- createJaspPlot(title = gettext("Sample Size and Acceptance Number"),  width = 570, height = 320)
    ncCurve$dependOn(c(depend_vars, "ncCurve"))
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$n), max(plans$n)))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$c), max(plans$c)))
    ncplt <- ggplot2::ggplot(data = plans) +
                    ggplot2::geom_point(ggplot2::aes(x = n, y = c, color = log_bf, show.legend=TRUE), shape = 19, size = 5) +
                    ggplot2::geom_line(ggplot2::aes(x = n, y = c), color = "black", linetype = "dotted") +
                    ggplot2::labs(x = gettext("Sample size (n)"), y = gettext("Acceptance Number (c)")) +
                    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
                    ggplot2::scale_color_gradient(low="blue", high="red")
    ncplt <- ncplt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(legend.position = "right")
    ncCurve$plotObject <- ncplt
    ncCurve$position <- 2
    jaspResults[["ncCurve"]] <- ncCurve
  }

  # 3. Plot of n vs BF
  if (options$bfCurve) {
    # plans$c <- factor(plans$c)
    bfCurve <- createJaspPlot(title = gettext("Sample Size and Bayes Factor"),  width = 570, height = 320)
    bfCurve$dependOn(c(depend_vars, "bfCurve"))
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$n), max(plans$n)))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$bf), max(plans$bf)))
    bfplt <- ggplot2::ggplot(data = plans) +
                    ggplot2::geom_point(ggplot2::aes(x = n, y = bf, color = c, show.legend=TRUE), shape = 19, size = 5) +
                    ggplot2::geom_line(ggplot2::aes(x = n, y = bf), color = "black", linetype = "dotted") +
                    ggplot2::labs(x = gettext("Sample size (n)"), y = gettext("Bayes Factor (BF)")) +
                    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
                    ggplot2::scale_color_gradient(low="blue", high="red")                                   
    bfplt <- bfplt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(legend.position = "right")
    bfCurve$plotObject <- bfplt
    bfCurve$position <- 3
    jaspResults[["bfCurve"]] <- bfCurve
  }
}

bf <- function(rql, n, c, alpha, beta, prior_odds) {
    posterior_in_favor <- pbeta(rql, alpha + c, beta + n - c)
    posterior_odds <- posterior_in_favor / (1 - posterior_in_favor)
    bf <- posterior_odds / prior_odds
    return (bf)
}

iso_bf_plans <- function(aql, rql, max_n, min_bf, alpha, beta) {
    prior_odds <- pbeta(rql, alpha, beta) / (1 - pbeta(rql, alpha, beta))
    plans <- data.frame(n=c(), c=c(), bf=c())
    for (n in seq(1,max_n,1)) {
        c <- n
        while (c >= 0) {
            bf <- bf(rql, n, c, alpha, beta, prior_odds)
            if (bf >= min_bf) {
                plans <- rbind(plans, list(n=n, c=c, bf=bf))
                break
            } else {
                if (c == 0) {                    
                    break
                } else {
                    c <- c - 1
                }
            }
        }
    }
    return (plans)
}