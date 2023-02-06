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
##         Check if values specified for PD are valid.         --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pd_lower {numeric} lower limit for the proportion of non-conforming items.
#' @param pd_upper {numeric} upper limit for the proportion of non-conforming items.
#' @param pd_step {numeric} step size for the sequence from pd_lower to pd_upper.
#' @seealso checkHypergeom()
#' @examples
#' checkPdErrors(jaspContainer, 0.1, 0.5, 0.01)
##---------------------------------------------------------------
checkPdErrors <- function(jaspContainer, pd_lower, pd_upper, pd_step) {
  if (pd_lower > pd_upper) {
    jaspContainer$setError(gettext("Lower limit for proportion non-conforming items needs to be smaller than the upper limit."))
    return ()
  }
  if (pd_step > (pd_upper - pd_lower)) {
    jaspContainer$setError(gettext("Step size for proportion non-conforming items needs to be smaller than the difference between the upper and limits."))
    return ()
  }
  is.zero <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - 0) < tol
  }
  if (is.zero(pd_step) && (pd_upper != pd_lower)) {
    jaspContainer$setError(gettext("Step size of 0 is allowed only if the lower and upper limits of proportion non-conforming items are identical."))    
  }
}

##--------------------------------------------------------------------------------
##  Check if D*pd values for the hypergeomtric distribution are whole numbers.  --
##--------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Analysis type.
#' @seealso checkPdErrors()
#' @examples
#' checkHypergeom(jaspContainer, options, "CreateAttr")
##--------------------------------------------------------------------------------
checkHypergeom <- function(jaspContainer, options, type) {
  pd_prop <- getPDValues(jaspContainer, options, type)$PD_Prop
  if (jaspContainer$getError()) {
    return ()
  }
  # This check is only required for hypergeometric distribution.
  if (options[[paste0("distribution", type)]] == "hypergeom") {
    # Function to check for whole numbers. From R package AcceptanceSampling.
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
      abs(x - round(x)) < tol
    }

    N <- options[[paste0("lotSize", type)]]
    D <- N * pd_prop
    if (!all(is.wholenumber(N), is.wholenumber(D))) {
      jaspContainer$setError(gettextf("%s\n\n%s", "For hypergeometric distribution, N * proportion non-conforming should all be integer values.", "Check the values of N and proportion non-conforming."))
    }
  }
}

##---------------------------------------------------------------
##  Check for errors in single stage attribute sampling plan.  --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param N {numeric} Total size of the lot.
#' @param n {numeric} Sample size for the plan.
#' @param c {numeric} Acceptance number for the plan.
#' @param r {numeric} Rejection number for the plan.
#' @seealso checkErrorsMultiplePlan()
#' @examples
#' checkErrorsSinglePlan(jaspContainer, 1000, 100, 4, 5)
##---------------------------------------------------------------
checkErrorsSinglePlan <- function(jaspContainer, N, n, c, r) {
  if (n > N) {
    jaspContainer$setError(gettext("Sample size (n) cannot be larger than the lot size (N)."))
    return ()
  }
  if (c > n) {
    jaspContainer$setError(gettext("Acceptance number (c) cannot be larger than the sample size (n)."))
    return ()
  }
  if (r > n) {
    jaspContainer$setError(gettext("Rejection number (r) cannot be larger than the sample size (n)."))
    return ()
  }
  if (r <= c) {
    jaspContainer$setError(gettext("Rejection number (r) has to be larger than the acceptance number (c)."))
  }
}

##-----------------------------------------------------------------
##  Check for errors in multiple stage attribute sampling plan.  --
##-----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param N {numeric} Total size of the lot.
#' @param n {vector} Sample sizes for the plan.
#' @param c {vector} Acceptance numbers for the plan.
#' @param r {vector} Rejection numbers for the plan.
#' @seealso checkErrorsSinglePlan()
#' @examples
#' checkErrorsMultiplePlan(jaspContainer, 1000, c(10,20), c(0,4), c(2,5))
##-----------------------------------------------------------------
checkErrorsMultiplePlan <- function(jaspContainer, N, n, c, r) {
  cum_n <- sum(n) # Total sample size for all stages combined
  cumsum_n <- cumsum(n) # Cumulative sample size at each stage
  num_stages <- length(n)
  if (cum_n > N) {
    jaspContainer$setError(gettext("Cumulative sample size (n1+n2+...) cannot be larger than the lot size (N)."))
    return ()
  }
  if (r[num_stages] != c[num_stages] + 1) {
    jaspContainer$setError(gettext("Final rejection number (r) needs to be 1 more than the final acceptance number (c)."))
    return ()
  }
  if (any(c > cumsum_n)) {
    jaspContainer$setError(gettext("Acceptance number (c) cannot be larger than the sample size (n)."))
    return ()
  }
  if (any(r > cumsum_n)) {
    jaspContainer$setError(gettext("Rejection number (r) cannot be larger than the sample size (n)."))
    return ()
  }
  if (any(r <= c)) {
    jaspContainer$setError(gettext("Rejection number (r) for every stage has to be larger than the corresponding acceptance number (c)."))
    return ()
  }
  if (any(c[1:(num_stages-1)] > r[1:(num_stages-1)] - 2)) {
    # Check for r[i] > c[i] + 1 for i in 1:stages-1
    jaspContainer$setError(gettextf("%s\n%s", "For all stages except the last stage, rejection numbers (r) have to be at at least 2 greater than the acceptance numbers (c).",
                                   "Else, subsequent stages become redundant."))
    return ()
  }
  if (any(c != sort(c))) {
    # Check for non-decreasing seqeuence of c
    jaspContainer$setError(gettext("Acceptance numbers (c) are cumulative, so they need to be in a non-decreasing sequence."))
    return ()
  }
  if (any(r != sort(r))) {
    # Check for non-decreasing seqeuence of r
    jaspContainer$setError(gettext("Rejection numbers (r) are cumulative, so they need to be in a non-decreasing sequence."))
  }
}

##----------------------------------------------------------------
##            Get the plan variables - n, c, and r.             --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @returns list
#' @seealso getPlan()
#' @examples
#' getPlanValues(jaspContainer, options, "Mult")
##----------------------------------------------------------------
getPlanValues <- function(jaspContainer, options, type) {
  n <- c <- r <- NULL
  N <- options[[paste0("lotSize", type)]]
  if (type != "AnalyzeAttrMult") {
    # Single sampling plan
    n <- options[[paste0("sampleSize", type)]]
    c <- options[[paste0("acceptNumber", type)]]
    r <- options[[paste0("rejectNumber", type)]]
    # Error checking for single stage plan
    checkErrorsSinglePlan(jaspContainer, N, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  } else {
    # Multiple sampling plan
    stages <- options[[paste0("stages", type)]]
    for (i in 1:length(stages)) {
      n[i] <- stages[[i]][[paste0("sampleSize", type)]]
      c[i] <- stages[[i]][[paste0("acceptNumber", type)]]
      r[i] <- stages[[i]][[paste0("rejectNumber", type)]]
    }
    # Error checking for multiple stage plan
    checkErrorsMultiplePlan(jaspContainer, N, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }
  return (list(n=n,c=c,r=r))
}

##---------------------------------------------------------------
##            Create and return a plan and its data            --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} (optional) Acceptance number(s) for the plan.
#' @param r {vector} (optional) Rejection number(s) for the plan.
#' @param k {numeric} (optional) The required distance in terms of standard deviation, between acceptance limits and the sample mean for a variable sampling plan.
#' @param sd {string} (optional) Status of the historical standard deviation. Possible values are "known" and "unknown".
#' @returns list
#' @seealso getPlanValues()
#' @examples
#' getPlan(jaspContainer, options, "Single", n=20, c=2, r=4)
#' getPlan(jaspContainer, options, "Single", n=8, k=1.2, sd="known")
##---------------------------------------------------------------

getPDTitle <- function(jaspContainer, options, type) {
  if (is.null(jaspContainer[["pd_title"]])) {
    pd_title <- createJaspState()
    pd_title$dependOn(paste0("pd_unit", type))
    jaspContainer[["pd_title"]] <- pd_title
    pd_unit <- options[[paste0("pd_unit", type)]]
    title = "Lot Proportion Defective"
    if (pd_unit == "percent") {
      title <- "Lot Percent Defective"    
    } else if (pd_unit == "per_million") {
      title <- "Lot Defectives Per Million"
    }
    pd_title$object <- title
  }
  return (jaspContainer[["pd_title"]]$object)
}

getPDValues <- function(jaspContainer, options, type) {
  if (is.null(jaspContainer[["pdValues"]])) {
    pdValues <- createJaspState()
    pd_vars <- paste0(c("pd_lower", "pd_upper", "pd_step", "pd_unit"), type)
    pdValues$dependOn(pd_vars)
    jaspContainer[["pdValues"]] <- pdValues
    pd_lower <- options[[pd_vars[1]]]
    pd_upper <- options[[pd_vars[2]]]
    pd_step <- options[[pd_vars[3]]]
    pd_unit <- options[[pd_vars[4]]]
    # Check for errors in the quality range
    checkPdErrors(jaspContainer, pd_lower, pd_upper, pd_step)
    if (jaspContainer$getError()) {
      return ()
    }
    pd_orig <- seq(pd_lower, pd_upper, pd_step)
    pd_prop <- pd_orig
    factor <- 1
    if (pd_unit == "percent") {
      factor <- 10^2 
    } else if (pd_unit == "per_million") {
      factor <- 10^6
    }
    pd_prop <- pd_orig / factor
    
    # Add the AQL and RQL values to quality range.
    aql <- tryCatch(options[[paste0("aql", type)]], error = function(x) "error")
    add_risk_points <- (!is.null(aql) && aql != "error")
    if (add_risk_points) {
      rql <- options[[paste0("rql", type)]]
      pd_prop <- c(pd_prop, aql, rql)
      pd_orig <- c(pd_orig, aql*factor, rql*factor)            
    }

    # If PD has only <= 1 point(s), add 0 and 1 to the range to make the output more interpretable.
    if (length(pd_prop) <= 1) {
      pd_prop <- c(pd_prop, 0, 1)
      pd_orig <- c(pd_orig, 0, factor)      
    }
    pd_prop <- sort(pd_prop)
    pd_orig <- sort(pd_orig)
    pd_prop <- pd_prop[!duplicated(pd_prop)]
    pd_orig <- pd_orig[!duplicated(pd_orig)]
    df_PD <- data.frame(PD_Prop = pd_prop, PD_Orig = pd_orig)
    pdValues$object <- df_PD    
  }
  return (jaspContainer[["pdValues"]]$object)
}

getPlan <- function(jaspContainer, options, type, n, c=NULL, r=NULL, k=NULL, sd=NULL) {
  df_plan <- getPDValues(jaspContainer, options, type)
  if (jaspContainer$getError()) {
    return ()
  }  
  # For variable plans, sd (whether historical stdev is known) will have a non-null value. Use normal distribution then.
  if (!is.null(sd)) {
    dist <- "normal"
  } else {
    dist <- options[[paste0("distribution", type)]]
  }
  # Create an OC2c or OCvar object for the plan using the AcceptanceSampling package
  oc_plan <- NULL
  if (dist == "hypergeom") {
    N <- options[[paste0("lotSize", type)]]
    oc_plan <- AcceptanceSampling::OC2c(N = N, n = n, c = c, r = r, type = dist, pd = df_plan$PD_Prop)
  } else if (dist == "binom" || dist == "poisson") {
    oc_plan <- AcceptanceSampling::OC2c(n = n, c = c, r = r, type = dist, pd = df_plan$PD_Prop)
  } else if (dist == "normal") {
    oc_plan <- AcceptanceSampling::OCvar(n = n, k = k, type = dist, s.type = sd, pd = df_plan$PD_Prop)
  }
  # df_plan <- data.frame(PD_Orig = df_PD$PD_Orig, PD_Prop = df_PD$PD_Prop, PA = oc_plan@paccept)
  df_plan$PA <- oc_plan@paccept
  df_plan <- na.omit(df_plan)
  if (nrow(df_plan) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  return (list(oc_plan=oc_plan, df_plan=df_plan))
}

##---------------------------------------------------------------------------------------
##  Check if the plan can satisfy the AQL and RQL constraints. Create tabular output.  --
##---------------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param oc_plan {object} An object from the AcceptanceSampling::OC2c class family (OCbinomial / OChypergeom / OCpoisson).
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
##---------------------------------------------------------------------------------------
assessPlan <- function(jaspContainer, pos, depend_vars, oc_plan, options, type) {
  aql <- options[[paste0("aql", type)]]
  pa_prod <- 1 - options[[paste0("prod_risk", type)]]
  rql <- options[[paste0("rql", type)]]
  pa_cons <- options[[paste0("cons_risk", type)]]

  # Error handling for AQL/RQL
  if (aql >= rql) {
    jaspContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }

  # Error handling for Producer's and Consumer's Risk
  if (pa_prod <= pa_cons) {
    jaspContainer$setError(gettext("1 - \u03B1 (Producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }

  # Assessment of the sampling plan
  assess <- AcceptanceSampling::assess(oc_plan, PRP = c(aql, pa_prod), CRP = c(rql, pa_cons))
  pa_prod_actual <- assess$PRP[3]
  pa_cons_actual <- assess$CRP[3]
  # Table with the specified and actual acceptance probabilities
  if (!is.null(jaspContainer[["riskTable"]])) {
    return ()
  }
  table <- createJaspTable(title = gettextf("Current plan <b>CAN %s</b> meet the specified risk point(s).", ifelse(assess$OK, "", "NOT")))
  table$dependOn(depend_vars)
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = gettext("Proportion Non-conforming"), type = "number")
  table$addColumnInfo(name = "col_3", title = gettext("Required P(accept)"), type = "number")
  table$addColumnInfo(name = "col_4", title = gettext("Actual P(accept)"), type = "number")
  table$addRows(list("col_1" = "AQL", "col_2" = aql, "col_3" = pa_prod, "col_4" = pa_prod_actual))
  table$addRows(list("col_1" = "RQL", "col_2" = rql, "col_3" = pa_cons, "col_4" = pa_cons_actual))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position <- pos
  jaspContainer[["riskTable"]] <- table

  # If the current plan CANNOT satisfy the constraints, provide explanation.
  if (!assess$OK) {
    if (pa_prod_actual < pa_prod) {
      text <- gettextf("Probability of acceptance (%.3f) at AQL (%.3f) is <b>lower</b> than the required probability of acceptance (%.3f) at AQL.", pa_prod_actual, aql, pa_prod)
    } else if (pa_cons_actual > pa_cons) {
      text <- gettextf("Probability of acceptance (%.3f) at RQL (%.3f) is <b>higher</b> than the required probability of acceptance (%.3f) at RQL.", pa_cons_actual, rql, pa_cons)
    }
    explanation <- createJaspHtml(text = text, position=pos+1, dependencies=depend_vars)
    if (is.null(jaspContainer[["explanation"]])) {
      jaspContainer[["explanation"]] <- explanation
    }
  }
}

##---------------------------------------------------------------
##            Generate a summary table for the plan            --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
##---------------------------------------------------------------
getSummary <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n=NULL, c=NULL, r=NULL) {
  if (!is.null(jaspContainer[["summaryTable"]])) {
    return()
  }
  summaryTable <- createJaspTable(title = gettext("Acceptance Probabilities"))
  summaryTable$dependOn(depend_vars)
  if (!"AOQ" %in% colnames(df_plan)) {
    df_plan <- getAOQ(jaspContainer, depend_vars, df_plan, options, type, n, c, r)
  }
  if (!"ATI" %in% colnames(df_plan)) {
    df_plan <- getATI(jaspContainer, depend_vars, df_plan, options, type, n, c, r)
  }
  pd_title <- getPDTitle(jaspContainer, options, type)
  if (jaspContainer$getError()) {
    return ()
  }
  # Data for the table
  row = list(col_1 = df_plan$PD_Orig, col_2 = df_plan$PA, col_3 = df_plan$AOQ, col_4 = df_plan$ATI)  

  summaryTable$addColumnInfo(name = "col_1", title = gettext(pd_title), type = "number")
  summaryTable$addColumnInfo(name = "col_2", title = gettext("P(accept)"), type = "number")
  summaryTable$addColumnInfo(name = "col_3", title = gettext("AOQ"), type = "number")
  summaryTable$addColumnInfo(name = "col_4", title = gettext("ATI"), type = "number")
  if (type == "AnalyzeAttrMult" || type == "Seq") {
    # Append data for ASN.
    summaryTable$addColumnInfo(name = "col_5", title = gettext("ASN"), type = "number")
    if (!"ASN" %in% colnames(df_plan)) {
      df_plan <- getASN(jaspContainer, df_plan, options, n, c, r)
    }
    row <- append(row, list("col_5" = df_plan$ASN))
  }
  summaryTable$setData(row)
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- pos
  jaspContainer[["summaryTable"]] <- summaryTable
}

##----------------------------------------------------------------
##  Generate the operating characteristics curve for the plan.  --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
##----------------------------------------------------------------
getOCCurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type) {
  if (!is.null(jaspContainer[["ocCurve"]])) {
    return()
  }
  pd_title <- getPDTitle(jaspContainer, options, type)
  # df_plan$PD <- round(df_plan$PD, 3)
  # df_plan$PA <- round(df_plan$PA, 3)
  ocCurve <- createJaspPlot(title = gettext("OC (Operating Characteristics) Curve"),  width = 570, height = 320)
  ocCurve$dependOn(depend_vars)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD_Orig), max(df_plan$PD_Orig)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PA), max(df_plan$PA)))
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD_Orig, y = PA)) +
                  ggplot2::geom_point(color = "black", shape = 19) +
                  ggplot2::geom_line(color = "black", linetype = "dashed") +
                  ggplot2::labs(x = gettext(pd_title), y = gettext("Probability of Acceptance")) +
                  ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  ocCurve$plotObject <- plt
  ocCurve$position <- pos
  jaspContainer[["ocCurve"]] <- ocCurve
}

##------------------------------------------------------------------------------
##  Generate the average outgoing quality curve for plan with rectification.  --
##------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} (optional) Acceptance number(s) for the plan.
#' @param r {vector} (optional) Rejection number(s) for the plan.
#' @seealso getATICurve()
##------------------------------------------------------------------------------

getAOQ <- function(jaspContainer, depend_vars, df_plan, options, type, n, c=NULL, r=NULL) {
  if (is.null(jaspContainer[["aoqValues"]])) {
    aoqValues <- createJaspState()
    aoqValues$dependOn(depend_vars)
    jaspContainer[["aoqValues"]] <- aoqValues

    N <- options[[paste0("lotSize", type)]]
    pd_prop <- df_plan$PD_Prop
    AOQ <- numeric(length(pd_prop))

    # Straightforward calculation for single stage plan.
    if (type != "AnalyzeAttrMult") {
      AOQ <- df_plan$PA * pd_prop * (N-n) / N    
    } else {
      # Multiple plan - need to compute stagewise probabilities.
      dist <- options[[paste0("distribution", type)]]
      stages <- length(n) # Number of sampling stages
      cum_n <- cumsum(n)
      stage_probs <- getStageProbability(pd_prop, n, c, r, dist, N)
      if (is.null(stage_probs)) {
        aoqCurve$setError(gettext("Can not calculate AOQ. Check the plan parameters."))
        return ()
      }
      stage_probs <- stage_probs[[1]] # We only need acceptance probability.
      for (i in 1:stages) {
        pAcc_i <- stage_probs[i,] # Acceptance probability for ith stage
        AOQ <- AOQ + (pAcc_i * (N - cum_n[i])) # Incremental AOQ till ith stage
      }
      # This part is common for every stage, so do it at the end.
      AOQ <- AOQ * pd_prop / N
    }
    AOQ <- round(AOQ, 3)
    df_plan$AOQ <- AOQ
    # df_plan$PD <- round(df_plan$PD, 3)
    df_plan <- na.omit(df_plan)
    if (nrow(df_plan) == 0) {
      jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
      return ()
    }
    aoqValues$object <- df_plan
  }
  return (jaspContainer[["aoqValues"]]$object)
}

getAOQCurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n=NULL, c=NULL, r=NULL) {
  if (!is.null(jaspContainer[["aoqCurve"]]) || jaspContainer$getError()) {
    return ()
  }
  aoqCurve <- createJaspPlot(title = gettext("AOQ (Average Outgoing Quality) Curve"), width = 570, height = 320)
  aoqCurve$dependOn(depend_vars)
  jaspContainer[["aoqCurve"]] <- aoqCurve
  
  if (!"AOQ" %in% colnames(df_plan)) {
    df_plan <- getAOQ(jaspContainer, depend_vars, df_plan, options, type, n, c, r)
  }
  if (jaspContainer$getError()) {
    return ()
  }
  pd_title <- getPDTitle(jaspContainer, options, type)
  # AOQL (Average Outgoing Quality Limit)
  aoql <- max(df_plan$AOQ)
  # pd_aoql <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD_Orig), max(df_plan$PD_Orig)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$AOQ), 1.2*aoql))
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD_Orig, y = AOQ)) +
         ggplot2::geom_point(color = "black", shape = 19) + 
         ggplot2::labs(x = gettext(paste0("(Incoming) ", pd_title)), y = gettext("Average Outgoing Quality")) +
         ggplot2::geom_line(color = "black", linetype = "dashed") +
         ggplot2::geom_hline(yintercept = aoql, linetype = "dotted") +
         ggplot2::annotate("text", label = gettextf("AOQL: %.3f", aoql),
                           x = (min(df_plan$PD_Orig) + max(df_plan$PD_Orig)) / 2, y = aoql*1.1, color = "black", size = 6) +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
        # ggplot2::ylim(0.0,round(aoql*1.2, 2))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  aoqCurve$plotObject <- plt
}

##---------------------------------------------------------------
##  Generate the average total inspection curve for the plan.  --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} (optional) Acceptance number(s) for the plan.
#' @param r {vector} (optional) Rejection number(s) for the plan.
#' @seealso getAOQCurve()
##---------------------------------------------------------------

getATI <- function(jaspContainer, depend_vars, df_plan, options, type, n, c=NULL, r=NULL) {
  if (is.null(jaspContainer[["atiValues"]])) {
    atiValues <- createJaspState()
    atiValues$dependOn(depend_vars)
    jaspContainer[["atiValues"]] <- atiValues

    N <- options[[paste0("lotSize", type)]]
    pd_prop <- df_plan$PD_Prop
    ATI <- numeric(length(pd_prop))

    # Straightforward calculation for single stage plan.
    if (type != "AnalyzeAttrMult" ) {
      ATI <- df_plan$PA * n + (1 - df_plan$PA) * N
    } else {
      # Multiple plan - need to compute stagewise probabilities.
      dist <- options[[paste0("distribution", type)]]
      stages <- length(n) # Number of sampling stages
      cum_n <- cumsum(n)
      stage_probs <- getStageProbability(pd_prop, n, c, r, dist, N)
      if (is.null(stage_probs)) {
        atiCurve$setError(gettext("Can not calculate ATI. Check the plan parameters."))
        return ()
      }
      acc_probs <- stage_probs[[1]] # Acceptance probabilities
      rej_probs <- stage_probs[[2]] # Rejection probabilities
      for (i in 1:stages) {
        pAcc_i <- acc_probs[i,] # Acceptance probability for ith stage
        ATI <- ATI + (pAcc_i * cum_n[i]) # Incremental ATI till ith stage
      }
      # This part is common for every stage, so do it at the end.
      # For any stage, if lot gets rejected, all N items are inspected under rectification plan.
      ATI <- ATI + N * colSums(rej_probs)
    }
    ATI <- round(ATI, 3)
    df_plan$ATI <- ATI
    # df_plan$PD <- round(df_plan$PD, 3)
    df_plan <- na.omit(df_plan)
    if (nrow(df_plan) == 0) {
      jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
      return ()
    }
    atiValues$object <- df_plan
  }
  return (jaspContainer[["atiValues"]]$object)
}

getATICurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n=NULL, c=NULL, r=NULL) {
  if (!is.null(jaspContainer[["atiCurve"]])) {
    return ()
  }
  atiCurve <- createJaspPlot(title = gettext("ATI (Average Total Inspection) Curve"), width = 570, height = 320)
  atiCurve$dependOn(depend_vars)
  jaspContainer[["atiCurve"]] <- atiCurve
  if (!"ATI" %in% colnames(df_plan)) {
    df_plan <- getATI(jaspContainer, depend_vars, df_plan, options, type, n, c, r)
  }
  if (jaspContainer$getError()) {
    return ()
  }
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df_plan$PD_Orig)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df_plan$ATI)
  pd_title <- getPDTitle(jaspContainer, options, type)
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD_Orig, y = ATI)) +
         ggplot2::geom_point(color = "black", shape = 19) +
         ggplot2::geom_line(color = "black", linetype = "dashed") +
         ggplot2::labs(x = gettext(pd_title), y = gettext("Average Total Inspection")) +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
         # ggplot2::scale_y_continuous(breaks = pretty(df_plan$ATI))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  atiCurve$plotObject <- plt
}

##---------------------------------------------------------------------------------------------------------
##  Generate the average sample number curve for the plan. Only applicable for multiple sampling plans.  --
##---------------------------------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
#' @param options {list} A named list of interface options selected by the user.
#' @param n {vector} Sample sizes for the plan.
#' @param c {vector} Acceptance numbers for the plan.
#' @param r {vector} Rejection numbers for the plan.
##---------------------------------------------------------------------------------------------------------

getASN <- function(jaspContainer, depend_vars, df_plan, options, type, n, c, r) {
  if (is.null(jaspContainer[["asnValues"]])) {
    asnValues <- createJaspState()
    asnValues$dependOn(depend_vars)
    jaspContainer[["asnValues"]] <- asnValues

    # Parse option values
    dist <- options[[paste0("distribution", type)]]
    N <- options[[paste0("lotSize", type)]]
    pd_prop <- df_plan$PD_Prop
    stages <- length(n)
    num_values <- length(pd_prop)
    ASN <- numeric(num_values)
    cum_n <- cumsum(n)
    stage_probs <- getStageProbability(pd_prop, n, c, r, dist, N)
    if (is.null(stage_probs)) {
      jaspContainer$setError(gettext("Can not calculate ASN. Check the plan parameters."))
      return ()
    }
    stage_probs <- stage_probs[[1]] + stage_probs[[2]] # Decision prob = p_acc + p_rej
    for (i in 1:stages) {
      pDecide_i <- stage_probs[i,]
      ASN <- ASN + pDecide_i * cum_n[i]
    }
    ASN <- round(ASN, 3)
    df_plan$ASN <- ASN
    # df_plan$PD <- round(df_plan$PD, 3)
    df_plan <- na.omit(df_plan)
    if (nrow(df_plan) == 0) {
      jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    }
    asnValues$object <- df_plan
  }
  return (jaspContainer[["asnValues"]]$object)
}

getASNCurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n=NULL, c=NULL, r=NULL) {
  if (!is.null(jaspContainer[["asnCurve"]])) {
    return ()
  }
  asnCurve <- createJaspPlot(title = gettext("ASN (Average Sample Number) Curve"), width = 570, height = 320)
  asnCurve$dependOn(depend_vars)
  jaspContainer[["asnCurve"]] <- asnCurve

  if (!"ASN" %in% colnames(df_plan)) {
    df_plan <- getASN(jaspContainer, depend_vars, df_plan, options, type, n, c, r)
  }
  if (jaspContainer$getError()) {
    return ()
  }
  # Draw ASN plot
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD_Orig), max(df_plan$PD_Orig)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$ASN), max(df_plan$ASN)))
  pd_title <- getPDTitle(jaspContainer, options, type)
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD_Orig, y = ASN)) +
         ggplot2::geom_point(color = "black", shape = 19) +
         ggplot2::geom_line(color = "black", linetype = "dashed") +
         ggplot2::labs(x = pd_title, y = "Average Sample Number") +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  asnCurve$plotObject <- plt
}

##-------------------------------------------------------------------------------------------
##  Helper function to get stagewise acceptance and rejection probabilities for the plan.  --
##-------------------------------------------------------------------------------------------
#' @param pd {vector} Range of quality levels over which to evaluate the plan, each value indicating a proportion (0 to 1) of non-conforming items.
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} Acceptance number(s) for the plan.
#' @param r {vector} Rejection number(s) for the plan.
#' @param dist {string} The type of distribution of non-conforming items in the sample. Possible values are "binom", "hypergeom", and "poisson".
#' @param N {numeric} (optional) Total size of the lot.
#' @returns list
#' @seealso getStageProbability()
#' @examples
#' getStageProbabilityHelper(pd, n=c(20,30), c=c(4,10), r=c(7,11), dist="hypergeom", N=500)
#' @notes
#' Part of the code in this function has been re-used from the R package AcceptanceSampling written by Andreas Kiermeier.
#' Specifically, from the following functions:
#' 1) calc.OCbinomial.pdi 2) calc.OChypergeom.pdi 3) calc.OCpoisson.pdi
#' https://github.com/cran/AcceptanceSampling/blob/master/R/code_twoclass.R
#' @importFrom stats dbinom pbinom dpois ppois dhyper phyper
##-------------------------------------------------------------------------------------------
getStageProbabilityHelper <- function(pd, n, c, r, dist, N=10000) {
  D <- pd * N # Number of defects = quality level * lot size
  num_stages <- length(n)
  acc_probs <- matrix(nrow = num_stages, ncol = length(pd))
  rej_probs <- matrix(nrow = num_stages, ncol = length(pd))
  k.s <- num_stages # number of stages in this sampling

  # Function to calculate acceptance probability
  prob.acc <- function(x, n, p, dist, N=1000) {
    k <- length(x)
    k1 <- k-2
    if (dist == "binom") {
      # k = number of this stage + 2.
      # x[1:k1] will give all values until the last stage before this one.
      # n[1:k1] does the same for n.
      prob <- prod(dbinom(x[1:k1], n[1:k1], p)) * pbinom(x[k-1], n[k-1], p)
    } else if (dist == "poisson") {
      prob <- prod(dpois(x[1:k1], n[1:k1]*p)) * ppois(x[k-1], n[k-1]*p)
    } else if (dist == "hypergeom") {
      x.cum <- cumsum(x[1:(k-1)])
      n.cum <- cumsum(n)
      N.cum <- N - c(0, n.cum[1:(k-1)])
      D.cum <- D - c(0, x.cum[1:(k-1)])

      prob <- prod(dhyper(x=x[1:k1], m=pmax(D.cum[1:k1], 0), n = N.cum[1:k1] - pmax(D.cum[1:k1], 0), k=n[1:k1])) *
                   phyper(q=x[k-1], m=pmax(D.cum[k-1], 0), n = N.cum[k-1] - pmax(D.cum[k-1], 0), k=n[k-1])
    }
    return (prob)
  }
  # Function to calculate rejection probability
  prob.rej <- function(x, n, p, dist, N=1000) {
    k <- length(x)
    k1 <- k-2
    if (dist == "binom") {
      prob <- prod(dbinom(x[1:k1], n[1:k1], p)) * pbinom(x[k], n[k-1], p, lower.tail = FALSE)
    } else if (dist == "poisson") {
      prob <- prod(dpois(x[1:k1], n[1:k1]*p)) * ppois(x[k], n[k-1]*p, lower.tail = FALSE)
    } else if (dist == "hypergeom") {
      x.cum <- cumsum(x[c(1:(k-2), k)])
      n.cum <- cumsum(n)
      N.cum <- N - c(0,n.cum[1:(k-2)])
      D.cum <- D - c(0,x.cum[1:(k-2)])
      prob <- prod(dhyper(x=x[1:k1], m=pmax(D.cum[1:k1], 0), n = N.cum[1:k1] - pmax(D.cum[1:k1], 0), k=n[1:k1])) *
                   phyper(q=x[k], m=pmax(D.cum[k-1], 0), n = N.cum[k-1] - pmax(D.cum[k-1], 0), k=n[k-1], lower.tail = FALSE)
    }
    return (prob)
  }

  for (k in 1:k.s) {
    # For each stage, find out all the possibilities which could lead to still not having made a decision,
    # and then calculate the appropriate probabilities.

    if (k == 1) {
      # Only a single sampling stage
      p.acc <- sapply(pd, FUN = function(el) {
        if (dist == "binom") {
          return (pbinom(q=c[1], size=n[1], prob=el))
        } else if (dist == "poisson") {
          return (ppois(q=c[1], lambda=n[1]*el))
        } else if (dist == "hypergeom") {
          el <- el * N
          return (phyper(q=c[1], m=el, n=N-el, k=n[1]))
        }
      })
      acc_probs[k,] = p.acc
      p.rej <- sapply(pd, FUN = function(el) {
        if (dist == "binom") {
          return (pbinom(q=r[1]-1, size=n[1], prob=el, lower.tail = FALSE))
        } else if (dist == "poisson") {
          return (ppois(q=r[1]-1, lambda = n[1]*el, lower.tail = FALSE))
        } else if (dist == "hypergeom") {
          el <- el * N
          return (phyper(q=r[1]-1, m=el, n=N-el, k=n[1], lower.tail = FALSE))
        }
      })
      rej_probs[k,] <- p.rej
      # p.acc and p.rej now exist and can be used in the following stages.
    }
    else if (k == 2) {
      # Two sampling stages. Needs to be handled separately from
      # more stages due to matrix dimensions
      c.s <- c+1 # Use to calculate limits
      r.s <- r-1 # Use to calculate limits
      # The possibilities which lead to a decision to be made at the second stage
      x <- data.frame(X1 = seq(c.s[1], r.s[1], by=1),
                      X.acc = c[2]-seq(c.s[1], r.s[1], by=1),
                      X.rej = r[2]-1-seq(c.s[1], r.s[1], by=1))
      p.acc_2 <- sum(apply(x, 1, FUN=prob.acc, n=n, p=pd, dist=dist, N=N))
      p.acc <- p.acc + p.acc_2
      acc_probs[k,] <- p.acc_2
      p.rej_2 <- sum(apply(x, 1, FUN=prob.rej, n=n, p=pd, dist=dist, N=N))
      p.rej <- p.rej + p.rej_2
      rej_probs[k,] <- p.rej_2
    }
    else {
      # More than two sampling stages.
      c.s <- c+1 # Use to calculate limits
      r.s <- r-1 # Use to calculate limits
      expand.call <- "expand.grid(c.s[k-1]:r.s[k-1]"
      for (i in 2:(k-1)) {
        expand.call <- paste(expand.call, paste("c.s[k-",i,"]:r.s[k-",i,"]", sep=""), sep=",")
      }
      expand.call <- paste(expand.call,")", sep="")
      x <- eval(parse(text=expand.call)[[1]])
      x <- x[,(k-1):1] # Reverses the order of columns in dataframe x
      names(x) <- paste("X", 1:(k-1), sep="")

      for (i in ncol(x):2) {
        x[,i] <- x[,i] - x[,i-1]
      }
      x <- cbind(x, X.acc = c[k] - rowSums(x[,1:(k-1)]))
      x <- cbind(x, X.rej = r[k]-1 - rowSums(x[,1:(k-1)]))
      p.acc_k <- sum(apply(x, 1, FUN=prob.acc, n=n, p=pd, dist=dist, N=N))
      p.acc <- p.acc + p.acc_k
      acc_probs[k,] <- p.acc_k # Acceptance probability for the kth stage
      p.rej_k <- sum(apply(x, 1, FUN=prob.rej, n=n, p=pd, dist=dist, N=N))
      p.rej <- p.rej + p.rej_k
      rej_probs[k,] <- p.rej_k # Rejection probability for the kth stage
    }
  }
  return(list(acc_probs,rej_probs))
}

##------------------------------------------------------------------------
##  Get stagewise acceptance and rejection probabilities for the plan.  --
##------------------------------------------------------------------------
#' @param pd {vector} Range of quality levels over which to evaluate the plan, each value indicating a proportion (0 to 1) of non-conforming items.
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} Acceptance number(s) for the plan.
#' @param r {vector} Rejection number(s) for the plan.
#' @param dist {string} The type of distribution of non-conforming items in the sample. Possible values are "binom", "hypergeom", and "poisson".
#' @param N {numeric} (optional) Total size of the lot.
#' @returns list
#' @seealso getStageProbabilityHelper()
#' @examples
#' getStageProbability(pd, n=c(10,20,30), c=c(2,5,8), r=c(4,7,9), dist="binom", N=1000)
#' @notes
#' The calculation of stagewise probability is done independently for every value in the vector 'pd', which has a range of quality levels.
##------------------------------------------------------------------------
getStageProbability <- function(pd, n, c, r, dist, N=10000) {
  stage_probs <- vapply(pd, FUN=getStageProbabilityHelper, n=n, c=c, r=r, dist=dist, N=N, FUN.VALUE=list(matrix,matrix))
  acc <- matrix(unlist(stage_probs[1,]), byrow=FALSE, nrow=length(n))
  rej <- matrix(unlist(stage_probs[2,]), byrow=FALSE, nrow=length(n))
  return (list(acc,rej))
}