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
##       Create Bayesian sampling plans and do inference.       --
##----------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
##---------------------------------------------------------------
BayesianSampling <- function(jaspResults, dataset = NULL, options, ...) {    
  # 1. Planning: use constraints, generate plans
  ##############################################
  section_plan <- "_plan"
  depend_vars_plan <- paste0(c("max_n", "min_bf", "aql", "rql", "prior"), section_plan)
  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["planContainer"]]) || jaspResults[["planContainer"]]$getError()) {
    planContainer <- createJaspContainer(title = "Planning")
    planContainer$dependOn(depend_vars_plan)
    jaspResults[["planContainer"]] <- planContainer
  } else {
    planContainer <- jaspResults[["planContainer"]]
  }

  aql_plan <- options[[paste0("aql", section_plan)]]
  rql_plan <- options[[paste0("rql", section_plan)]]
  max_n_plan <- options[[paste0("max_n", section_plan)]]
  min_bf_plan <- options[[paste0("min_bf", section_plan)]]
  prior_plan <- options[[paste0("prior", section_plan)]]
  alpha_plan <- beta_plan <- 0
  if (prior_plan == "impartial") {
    alpha_plan <- ((3 * rql_plan) - aql_plan * (4 * rql_plan + 1)) / (3 * (rql_plan - aql_plan))
    beta_plan <- ((2 + rql_plan * (4 * aql_plan - 1) - 5 * aql_plan)) / (3 * (rql_plan - aql_plan))    
  } else if (prior == "uniform") {
    alpha_plan <- beta_plan <- 1
  } else if (prior_plan == "custom") {
    alpha_plan <- options[[paste0("alpha", section_plan)]]
    beta_plan <- options[[paste0("beta", section_plan)]]
  }
  plans <- iso_bf_plans(aql_plan, rql_plan, max_n_plan, min_bf_plan, alpha_plan, beta_plan)
  plans$log_bf <- log(plans$bf)
  plans <- na.omit(plans)  
  
  if (nrow(plans) == 0) {
    planContainer$setError(gettext("No valid plans found satisfying the specified constraints."))
  }

  pos_plan <- 0
  output_vars_plan <- paste0(c("showPlans", "priorPlot", "ncPlot", "bfPlot"), section_plan)

  # 1.1 Create a table of the plans from above
  if (options[[output_vars_plan[1]]]) {
    makePlanTable(planContainer, pos_plan+1, output_vars_plan[1], plans)
  }
  
  # 1.2 Prior distribution plot
  if (options[[output_vars_plan[2]]]) {
    makeDistributionPlot(planContainer, pos_plan+2, output_vars_plan[2], aql_plan, rql_plan, alpha_plan, beta_plan, "Prior")
  }

  # 1.3 Plot of n vs c
  if (options[[output_vars_plan[3]]]) {
    makeNCPlot(planContainer, pos_plan+3, output_vars_plan[3], plans)
  }

  # 1.4 Plot of n vs BF
  if (options[[output_vars_plan[4]]]) {
    makeBFPlot(planContainer, pos_plan+4, output_vars_plan[4], plans)
  }

  # 2. Inference: take data, create posterior
  ##############################################
  section_infer <- "_infer"
  if (options[[paste0("inferPosterior", section_infer)]]) {
    depend_vars_inf <- paste0(c("inferPosterior", "choosePrior", "aql", "rql", "prior", "alpha", "beta"), section_infer)
    pos_inf <- 10
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["infContainer"]]) || jaspResults[["infContainer"]]$getError()) {
      infContainer <- createJaspContainer(title = "Inference")
      infContainer$dependOn(depend_vars_inf)
      jaspResults[["infContainer"]] <- infContainer
    } else {
      infContainer <- jaspResults[["infContainer"]]
    }

    alpha_prior_inf <- beta_prior_inf <- 0
    aql_inf <- rql_inf <- 0
    if (options[[paste0("choosePrior", section_infer)]] == "usePrev") {
      alpha_prior_inf <- alpha_plan
      beta_prior_inf <- beta_plan
      aql_inf <- aql_plan
      rql_inf <- rql_plan
    } else if (options[[paste0("choosePrior", section_infer)]] == "useNew") {
      prior_inf <- options[[paste0("prior", section_infer)]]
      aql_inf <- options[[paste0("aql", section_infer)]]
      rql_inf <- options[[paste0("rql", section_infer)]]
      if (prior_inf == "impartial") {
        alpha_prior_inf <- ((3 * rql_inf) - aql_inf * (4 * rql_inf + 1)) / (3 * (rql_inf - aql_inf))
        beta_prior_inf <- ((2 + rql_inf * (4 * aql_inf - 1) - 5 * aql_inf)) / (3 * (rql_inf - aql_inf))    
      } else if (prior_inf == "uniform") {
        alpha_prior_inf <- beta_prior_inf <- 1
      } else if (prior_inf == "custom") {
        alpha_prior_inf <- options[[paste0("alpha", section_infer)]]
        beta_prior_inf <- options[[paste0("beta", section_infer)]]    
      }
    }
    n_inf <- options[[paste0("data_n", section_infer)]]
    d_inf <- options[[paste0("data_d", section_infer)]]
    alpha_post_inf <- alpha_prior_inf + d_inf
    beta_post_inf <- beta_prior_inf + n_inf - d_inf
    posterior_odds_inf <- pbeta(rql_inf, alpha_post_inf, beta_post_inf) / (1 - pbeta(rql_inf, alpha_post_inf, beta_post_inf))
    prior_odds_inf <- pbeta(rql_inf, alpha_prior_inf, beta_prior_inf) / (1 - pbeta(rql_inf, alpha_prior_inf, beta_prior_inf))
    bf_inf <- posterior_odds_inf / prior_odds_inf
    
    output_vars_inf <- paste0(c("priorPlot", "posteriorPlot"), section_infer)
    
    # 2.1 Prior distribution plot for inference
    if (options[[output_vars_inf[1]]]) {
      makeDistributionPlot(infContainer, pos_inf+1, output_vars_inf[1], aql_inf, rql_inf, alpha_prior_inf, beta_prior_inf, "Prior")
    }
    
    # 2.2 Posterior distribution plot for inference
    if (options[[output_vars_inf[2]]]) {
      makeDistributionPlot(infContainer, pos_inf+2, c(output_vars_inf[2], paste0(c("data_n", "data_d"), section_infer)), aql_inf, rql_inf, alpha_post_inf, beta_post_inf, "Posterior", alpha_prior_inf, beta_prior_inf)
      # 2.2.1 Report the Bayes factor for the observed data
      bf_text <- createJaspHtml(text = gettextf("<u>Bayes factor</u> in favor of proportion of defects < <u>%1$.2f</u> is <b>%2$.2f</b>.", rql_inf, bf_inf), position=pos_inf+3, dependencies=paste0(c("data_n", "data_d"), section_infer))
      infContainer[["bf_text"]] <- bf_text
    }
  }

  # 3. Update: use posterior data, generate new plans
  ##############################################
  section_update <- "_update"
  if (options[[paste0("inferPosterior", section_infer)]] && options[[paste0("updatePlan", section_update)]]) {
    pos_update <- 100
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["updateContainer"]]) || jaspResults[["updateContainer"]]$getError()) {
      updateContainer <- createJaspContainer(title = "Update")
      updateContainer$dependOn(c(depend_vars_inf, paste0(c("updatePlan", "min_bf"), section_update)))
      jaspResults[["updateContainer"]] <- updateContainer
    } else {
      updateContainer <- jaspResults[["updateContainer"]]
    }

    min_bf_inf <- options[[paste0("min_bf", section_update)]]
    inf_plans <- iso_bf_plans(aql_inf, rql_inf, n_inf, min_bf_inf, alpha_post_inf, beta_post_inf)
    inf_plans$log_bf <- log(inf_plans$bf)
    inf_plans <- na.omit(inf_plans)
    if (nrow(inf_plans) == 0) {
      updateContainer$setError(gettext("No valid plans found satisfying the specified constraints."))
    }
    
    output_vars_update <- paste0(c("showPlans", "priorPlot", "ncPlot", "bfPlot"), section_update)

    # 3.1 Create a table of the plans from above
    if (options[[output_vars_update[1]]]) {
      makePlanTable(updateContainer, pos_update+1, output_vars_update[1], inf_plans)
    }
    
    # 3.2 Prior distribution plot
    if (options[[output_vars_update[2]]]) {
      makeDistributionPlot(updateContainer, pos_update+2, output_vars_update[2], aql_inf, rql_inf, alpha_post_inf, beta_post_inf, "Prior")
    }

    # 3.3 Plot of n vs c
    if (options[[output_vars_update[3]]]) {
      makeNCPlot(updateContainer, pos_update+3, output_vars_update[3], inf_plans)
    }

    # 3.4 Plot of n vs BF
    if (options[[output_vars_update[4]]]) {
      makeBFPlot(updateContainer, pos_update+4, output_vars_update[4], inf_plans)
    }
  }

  # 4. Projection: predict the number of defects and BF for future stages
  ##############################################
  section_project <- "_projection"
  if (options[[paste0("projectPlan", section_project)]]) {
    pos_project <- 1000
    depend_vars_proj <- paste0(c("projectPlan", "alpha", "beta", "proj_n", "proj_m", "rql"), section_project)
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["projectContainer"]]) || jaspResults[["projectContainer"]]$getError()) {
      projectContainer <- createJaspContainer(title = "Projection")
      projectContainer$dependOn(depend_vars_proj)
      jaspResults[["projectContainer"]] <- projectContainer
    } else {
      projectContainer <- jaspResults[["projectContainer"]]
    }
    alpha_proj <- options[[paste0("alpha", section_project)]]
    beta_proj <- options[[paste0("beta", section_project)]]
    proj_n <- options[[paste0("proj_n", section_project)]]
    proj_m <- options[[paste0("proj_m", section_project)]]
    rql_proj <- options[[paste0("rql", section_project)]]
    
    future_proj <- project_bf(alpha_proj, beta_proj, proj_n, proj_m, rql_proj)
    
    # 4.1 Plot of d vs n
    if (options[[paste0("showDefects", section_project)]]) {      
      makeProjectedDPlot(projectContainer, pos_project+1, paste0("showDefects", section_project), future_proj)
    }

    # 3.4 Plot of BF vs n
    if (options[[paste0("showBF", section_project)]]) {
      makeProjectedBFPlot(projectContainer, pos_project+2, paste0("showBF", section_project), future_proj)
    }
  }
}

##---------------------------------------------------------------
##                   Calculate Bayes factor.                   --
##---------------------------------------------------------------
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param n {numeric} Sample size for the plan.
#' @param c {numeric} Acceptance number for the plan.
#' @param alpha {numeric} First shape parameter for the Beta distribution.
#' @param beta {numeric} Second shape parameter for the Beta distribution.
#' @param prior_odds {numeric} Prior odds for the competing models.
##---------------------------------------------------------------------------------------
bf <- function(rql, n, c, alpha, beta, prior_odds) {
    posterior_in_favor <- pbeta(rql, alpha + c, beta + n - c)
    posterior_odds <- posterior_in_favor / (1 - posterior_in_favor)
    bf <- posterior_odds / prior_odds
    return (bf)
}

##-------------------------------------------------------------------------
##  Generate sampling plans with Bayes factor greater than a threshold.  --
##-------------------------------------------------------------------------
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param max_n {numeric} Maximum sample size for the plan.
#' @param min_bf {numeric} Minimum Fayes factor for the plan.
#' @param alpha {numeric} First shape parameter for the Beta distribution.
#' @param beta {numeric} Second shape parameter for the Beta distribution.
##---------------------------------------------------------------------------------------
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

##---------------------------------------------------------------
##            Project Bayes factor into the future.            --
##---------------------------------------------------------------
#' @param alpha {numeric} First shape parameter for the Beta distribution.
#' @param beta {numeric} Second shape parameter for the Beta distribution.
#' @param n_current {numeric} Current sample size for the plan.
#' @param m_future {numeric} Number of stages to project into the future.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
##---------------------------------------------------------------------------------------
project_bf <- function(alpha, beta, n_current, m_future, rql) {
    data <- data.frame(n=c(), d=c(), d_prob=c(), bf=c(), log_bf=c())
    prior_odds <- pbeta(rql, alpha, beta) / (1 - pbeta(rql, alpha, beta))
    for (m in 1:m_future) {
        d_vals <- 0:m
        # cum_d_vals <- alpha + d_vals
        # d_vals <- alpha:(alpha+m)    
        d_probs <- VGAM::dbetabinom.ab(d_vals, m, alpha, beta)
        # d_probs <- VGAM::dbetabinom.ab(d_vals, n_val, alpha, beta)
        posterior_in_favor <- pbeta(rql, d_vals, beta + m - d_vals)
        # posterior_in_favor <- pbeta(rql, d_vals, beta + n_val - d_vals)
        posterior_odds <- posterior_in_favor / (1 - posterior_in_favor)
        bf_vals <- posterior_odds / prior_odds
        log_bf_vals <- log(bf_vals)
        m_vals <- rep(m, times=m_future+1)
        d_vals <- append(d_vals, rep(NA, times = m_future - m))
        # cum_d_vals <- append(cum_d_vals, rep(NA, times = m_future - m))
        d_probs <- append(d_probs, rep(NA, times = m_future - m))
        bf_vals <- append(bf_vals, rep(NA, times = m_future - m))
        log_bf_vals <- append(log_bf_vals, rep(NA, times = m_future - m))
        data <- rbind(data, list(n=m_vals, d=d_vals, d_prob=d_probs, bf=bf_vals, log_bf=log_bf_vals))
    }
    data <- data[complete.cases(data), ]
    data$prob_level <- floor((data$d_prob * 10))
    return (data)
}

##-------------------------------------------------------------------------
##  Generate sampling plans with Bayes factor greater than a threshold.  --
##-------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param alpha {numeric} First shape parameter for the Beta distribution.
#' @param beta {numeric} Second shape parameter for the Beta distribution.
#' @param type {string} Analysis type.
#' @param alpha_prior {numeric} First shape parameter for the Beta prior distribution.
#' @param beta_prior {numeric} Second shape parameter for the Beta prior distribution.
##---------------------------------------------------------------------------------------
makeDistributionPlot <- function(jaspContainer, pos, depend_vars, aql, rql, alpha, beta, type, alpha_prior=NULL, beta_prior=NULL) {
  if (!is.null(jaspContainer[[paste0("distPlot", type)]])) {
    return()
  }
  # Todo: add labels for the highlighted part of the graph.  
  distPlot <- createJaspPlot(title = gettext(paste0(type, " Distribution: Beta(", round(alpha,1), ", ", round(beta,1), ")")), width = 570, height = 320)
  distPlot$dependOn(depend_vars)
  xValue <- seq(0,1,0.005)
  propDist <- dbeta(xValue, alpha, beta)
  df_dist <- data.frame(x=xValue, y=propDist)
  # xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  # yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_dist$y), max(df_dist$y)))
  plt <- ggplot2::ggplot(data = df_dist, ggplot2::aes(x=x, y=y)) +
                  ggplot2::geom_line(color = "black", linetype = "solid") +
                  ggplot2::labs(x = gettext("Lot Proportion Defective"), y = gettext("Density")) +
                  ggplot2::geom_ribbon(data = subset(df_dist, x <= aql), ggplot2::aes(x=x, ymin=0, ymax=y), fill="green", inherit.aes=FALSE, alpha=0.2, outline.type="both") +
                  ggplot2::geom_ribbon(data = subset(df_dist, x >= aql & x <= rql), ggplot2::aes(x=x, ymin=0, ymax=y), fill="blue", inherit.aes=FALSE, alpha=0.2, outline.type="both") +
                  ggplot2::geom_ribbon(data = subset(df_dist, x >= rql), ggplot2::aes(x=x, ymin=0, ymax=y), fill="red", inherit.aes=FALSE, alpha=0.2, outline.type="both")
                  # ggplot2::scale_color_manual(name = "Regions", breaks = c("green", "blue", "red"), labels = c("<AQL", "AQL-RQL", ">RQL"), guide = "legend")
                  # ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  # ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  if (type == "Posterior") {
    df_prior = data.frame(xp=xValue, yp=dbeta(xValue, alpha_prior, beta_prior))
    plt <- plt + ggplot2::geom_line(data=df_prior, ggplot2::aes(x=xp, y=yp), linetype="dashed")
  }
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  distPlot$plotObject <- plt
  distPlot$position <- pos
  jaspContainer[[paste0("distPlot", type)]] <- distPlot
}

##-------------------------------------------------------------------------
##  Generate sampling plans with Bayes factor greater than a threshold.  --
##-------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param plans {data.frame} Dataframe for the plans.
##---------------------------------------------------------------------------------------
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

##-------------------------------------------------------------------------
##  Generate sampling plans with Bayes factor greater than a threshold.  --
##-------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param plans {data.frame} Dataframe for the plans.
##---------------------------------------------------------------------------------------
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

##-------------------------------------------------------------------------
##  Generate sampling plans with Bayes factor greater than a threshold.  --
##-------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param plans {data.frame} Dataframe for the plans.
##---------------------------------------------------------------------------------------
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

##---------------------------------------------------------------
##        Generate plot of projected number of defects.        --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param plans {data.frame} Dataframe for the plans.
##---------------------------------------------------------------------------------------
makeProjectedDPlot <- function(jaspContainer, pos, depend_vars, plans) {
  if (!is.null(jaspContainer[["projectedDPlot"]])) {
    return()
  }
  projectedDPlot <- createJaspPlot(title = gettext("Projected Number of Defects"),  width = 570, height = 320)
  projectedDPlot$dependOn(depend_vars)
  # xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$n), max(plans$n)))
  # yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$d), max(plans$d)))
  plt <- ggplot2::ggplot(data = plans, mapping = ggplot2::aes(x = factor(n), y = factor(d), fill = factor(prob_level))) + 
                  ggplot2::geom_tile() + ggplot2::labs(x = gettext("Future Stage"), y = gettext("Projected number of defects"), fill = gettext("Probability Level")) +
                  ggplot2::scale_fill_grey(start = 0, end = .9)
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(legend.position = "right")
  projectedDPlot$plotObject <- plt
  projectedDPlot$position <- pos
  jaspContainer[["projectedDPlot"]] <- projectedDPlot
}

##----------------------------------------------------------------
##           Generate plot of projected Bayes Factor.           --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param plans {data.frame} Dataframe for the plans.
##---------------------------------------------------------------------------------------
makeProjectedBFPlot <- function(jaspContainer, pos, depend_vars, plans) {
  if (!is.null(jaspContainer[["projectedBFPlot"]])) {
    return()
  }
  projectedBFPlot <- createJaspPlot(title = gettext("Projected log(BF)"),  width = 570, height = 320)
  projectedBFPlot$dependOn(depend_vars)
  # xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(plans$n), max(plans$n)))
  # yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(floor(plans$log_bf)+10), max(floor(plans$log_bf)+10)))
  plt <- ggplot2::ggplot(data = plans, mapping = ggplot2::aes(x = factor(n), y = floor(log_bf)+10, fill = factor(prob_level))) + 
                  ggplot2::geom_tile() + ggplot2::labs(x = gettext("Stage"), y = gettext("Projected log(BF)+10"), fill = gettext("Probability Level"))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(legend.position = "right")
  projectedBFPlot$plotObject <- plt
  projectedBFPlot$position <- pos
  jaspContainer[["projectedBFPlot"]] <- projectedBFPlot
}