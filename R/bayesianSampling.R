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
  # 1. Planning section  
  analysis <- "plan"
  depend_vars_plan <- c("max_n", "min_bf", "aql", "rql", "prior")  
  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["planContainer"]]) || jaspResults[["planContainer"]]$getError()) {
    planContainer <- createJaspContainer(title = "")
    planContainer$dependOn(depend_vars_plan)
    jaspResults[["planContainer"]] <- planContainer
  } else {
    planContainer <- jaspResults[["planContainer"]]
  }

  aql_plan <- options[[paste0("aql", analysis)]]
  rql_plan <- options[[paste0("rql", analysis)]]
  max_n_plan <- options[[paste0("max_n", analysis)]]
  min_bf_plan <- options[[paste0("min_bf", analysis)]]
  prior_plan <- options[[paste0("prior", analysis)]]
  alpha_plan <- beta_plan <- 0
  if (prior_plan == "impartial") {
    alpha_plan <- (3 * rql_plan) - aql_plan * (4 * rql_plan - 1) / (3 * (rql_plan - aql_plan))
    beta_plan <- (2 + rql_plan * (4 * aql_plan - 1) - 5 * aql_plan) / (3 * (rql_plan - aql_plan))
  } else if (prior == "uniform") {
    alpha_plan <- beta_plan <- 1
  } else if (prior_plan == "custom") {
    alpha_plan <- options[[paste0("alpha", analysis)]]
    beta_plan <- options[[paste0("beta", analysis)]]
  }
  plans <- iso_bf_plans(aql_plan, rql_plan, max_n_plan, min_bf_plan, alpha_plan, beta_plan)
  plans$log_bf <- log(plans$bf)
  plans <- na.omit(plans)  
  
  if (nrow(plans) == 0) {
    planContainer$setError(gettext("No valid plans found satisfying the specified constraints."))
  }

  pos <- 1
  output_vars_plan <- paste0(c("showPlans", "priorPlot", "ncPlot", "bfPlot"), analysis)

  # 1.1 Create a table of the plans from above
  if (options[[output_vars_plan[1]]]) {
    makePlanTable(planContainer, pos, output_vars_plan[1], plans)
  }
  
  # 1.2 Prior distribution plot
  if (options[[output_vars_plan[2]]]) {
    makePriorPlot(planContainer, pos+1, output_vars_plan[2], aql_plan, rql_plan, alpha_plan, beta_plan)
  }

  # 1.3 Plot of n vs c
  if (options[[output_vars_plan[3]]]) {
    makeNCPlot(planContainer, pos+2, output_vars_plan[3], plans)
  }

  # 1.4 Plot of n vs BF
  if (options[[output_vars_plan[4]]]) {
    makeBFPlot(planContainer, pos+3, output_vars_plan[4], plans)
  }

  # 2. Inference for posterior
  segment <- "infer"
  depend_vars_inf <- paste0(c("choosePrior", "data_n", "data_d"), segment)
  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["infContainer"]]) || jaspResults[["infContainer"]]$getError()) {
    infContainer <- createJaspContainer(title = "")
    infContainer$dependOn(depend_vars_inf)
    jaspResults[["infContainer"]] <- infContainer
  } else {
    infContainer <- jaspResults[["infContainer"]]
  }

  alpha_prior_inf <- beta_prior_inf <- 0
  aql_inf <- rql_inf <- 0
  if (options[[paste0("choosePrior", segment)]] == "usePrev") {
    alpha_prior_inf <- alpha_plan
    beta_prior_inf <- beta_plan
    aql_inf <- aql_plan
    rql_inf <- rql_plan
  } else if (options[[paste0("choosePrior", segment)]] == "useNew") {
    prior_inf <- options[[paste0("prior", segment)]]
    aql_inf <- options[[paste0("aql", segment)]]
    rql_inf <- options[[paste0("rql", segment)]]
    if (prior_inf == "impartial") {
      alpha_prior_inf <- (3 * rql_inf) - aql_inf * (4 * rql_inf - 1) / (3 * (rql_inf - aql_inf))
      beta_prior_inf <- (2 + rql_inf * (4 * aql_inf - 1) - 5 * aql_inf) / (3 * (rql_inf - aql_inf))
    } else if (prior_inf == "uniform") {
      alpha_prior_inf <- beta_prior_inf <- 1
    } else if (prior_inf == "custom") {
      alpha_prior_inf <- options[[paste0("alpha", segment)]]
      beta_prior_inf <- options[[paste0("beta", segment)]]    
    }
  }
  n_inf <- options[[paste0("data_n", segment)]]
  d_inf <- options[[paste0("data_d", segment)]]
  alpha_post_inf <- alpha_prior_inf + d_inf
  beta_post_inf <- beta_prior_inf + n_inf - d_inf

  pos_inf <- pos+4
  output_vars_inf <- paste0(c("priorPlot", "posteriorPlot"), segment)
  
  # 2.1 Prior distribution plot for inference
  if (options[[output_vars_inf[1]]]) {
    makePriorPlot(infContainer, pos_inf, output_vars_inf[1], aql_inf, rql_inf, alpha_prior_inf, beta_prior_inf)
  }
  
  # 2.1 Posterior distribution plot for inference
  # if (options[[output_vars[2]]]) {
    # makePosteriorPlot(infContainer, pos_inf, output_vars[1], aql_inf, rql_inf, alpha_prior_inf, beta_prior_inf)
  # }
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

makePlanTable <- function(jaspContainer, pos, depend_vars, plans) {
  if (!is.null(jaspContainer[["planTable"]])) {
    return()
  }
  planTable <- createJaspTable(title = gettext("Sampling Plans"))
  planTable$dependOn(depend_vars)
  # Data for the table
  planTable$addColumnInfo(name = "col_1", title = gettext("Sample size (n)"), type = "integer")
  planTable$addColumnInfo(name = "col_2", title = gettext("Acceptance number (c)"), type = "integer")
  planTable$addColumnInfo(name = "col_3", title = gettext("Bayes factor (BF)"), type = "number")
  row = list(col_1 = plans$n, col_2 = plans$c, col_3 = plans$bf)  
  planTable$setData(row)
  planTable$showSpecifiedColumnsOnly <- TRUE
  planTable$position <- pos
  jaspContainer[["planTable"]] <- planTable
}

makePriorPlot <- function(jaspContainer, pos, depend_vars, aql, rql, alpha, beta) {
  if (!is.null(jaspContainer[["priorPlot"]])) {
    return()
  }
  # Todo: add labels for the highlighted part of the graph.  
  priorPlot <- createJaspPlot(title = gettext(paste0("Prior Distribution: Beta(", round(alpha,1), ", ", round(beta,1), ")")), width = 570, height = 320)
  priorPlot$dependOn(depend_vars)
  xValue <- seq(0,1,0.005)
  propPrior <- dbeta(xValue, alpha, beta)
  df_prior <- data.frame(x=xValue, y=propPrior)
  # xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  # yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_prior$y), max(df_prior$y)))
  plt <- ggplot2::ggplot(data = df_prior, ggplot2::aes(x=x, y=y)) +
                  ggplot2::geom_line(color = "black", linetype = "solid") +
                  ggplot2::labs(x = gettext("Lot Proportion Defective"), y = gettext("Density")) +
                  ggplot2::geom_ribbon(data = subset(df_prior, x <= aql), ggplot2::aes(x=x, ymin=0, ymax=y), fill="green", inherit.aes=FALSE, alpha=0.2, outline.type="both") +
                  ggplot2::geom_ribbon(data = subset(df_prior, x >= aql & x <= rql), ggplot2::aes(x=x, ymin=0, ymax=y), fill="blue", inherit.aes=FALSE, alpha=0.2, outline.type="both") +
                  ggplot2::geom_ribbon(data = subset(df_prior, x >= rql), ggplot2::aes(x=x, ymin=0, ymax=y), fill="red", inherit.aes=FALSE, alpha=0.2, outline.type="both")
                  # ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  # ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))                    
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  priorPlot$plotObject <- plt
  priorPlot$position <- pos
  jaspContainer[["priorPlot"]] <- priorPlot
}

makeNCPlot <- function(jaspContainer, pos, depend_vars, plans) {
  if (!is.null(jaspContainer[["ncPlot"]])) {
    return()
  }
  ncPlot <- createJaspPlot(title = gettext("Sample Size and Acceptance Number"), width = 570, height = 320)
  ncPlot$dependOn(depend_vars)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$n), max(plans$n)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$c), max(plans$c)))
  plt <- ggplot2::ggplot(data = plans) +
                  ggplot2::geom_point(ggplot2::aes(x = n, y = c, color = log_bf, show.legend=TRUE), shape = 19, size = 3) +
                  ggplot2::geom_line(ggplot2::aes(x = n, y = c), color = "black", linetype = "dotted") +
                  ggplot2::labs(x = gettext("Sample size (n)"), y = gettext("Acceptance Number (c)"), color = gettext("Log BF")) +
                  ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
                  ggplot2::scale_color_gradient(low="blue", high="red")
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(legend.position = "right")
  ncPlot$plotObject <- plt
  ncPlot$position <- pos
  jaspContainer[["ncPlot"]] <- ncPlot
}

makeBFPlot <- function(jaspContainer, pos, depend_vars, plans) {
  if (!is.null(jaspContainer[["bfPlot"]])) {
    return()
  }
  bfPlot <- createJaspPlot(title = gettext("Sample Size and Bayes Factor"),  width = 570, height = 320)
  bfPlot$dependOn(depend_vars)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$n), max(plans$n)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$bf), max(plans$bf)))
  plt <- ggplot2::ggplot(data = plans) +
                  ggplot2::geom_point(ggplot2::aes(x = n, y = bf, color = factor(c), show.legend=TRUE), shape = 19, size = 3) +
                  ggplot2::geom_line(ggplot2::aes(x = n, y = bf), color = "black", linetype = "dotted") +
                  ggplot2::labs(x = gettext("Sample size (n)"), y = gettext("Bayes Factor (BF)"), color = gettext("Acceptance Number (c)")) +
                  ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
                  # ggplot2::scale_color_gradient(low="blue", high="red")                                   
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(legend.position = "right")
  bfPlot$plotObject <- plt
  bfPlot$position <- pos
  jaspContainer[["bfPlot"]] <- bfPlot
}