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
##            Decision table for variable plan lots.            --
##----------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
#' @importFrom stats pnorm pbeta
##----------------------------------------------------------------
DecideVariableLots <- function(jaspResults, dataset = NULL, options, ...) {
  type <- "DecideVar"
  depend_vars <- paste0(c("vars", "sampleStats", "sampleSize", "sampleMean", "sampleSD", "kValue", "lsl", "lower_spec", "usl", "upper_spec", "sd", "stdev"), type)
  risk_vars <- paste0(c("aql", "rql"), type)

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["lotContainer"]]) || jaspResults[["lotContainer"]]$getError()) {
    lotContainer <- createJaspContainer(title = "")
    lotContainer$dependOn(c(depend_vars, risk_vars)) # Common dependencies
    jaspResults[["lotContainer"]] <- lotContainer
  } else {
    lotContainer <- jaspResults[["lotContainer"]]
  }

  # Plan variables
  n <- NULL
  mean_sample <- NULL
  sd_sample <- NULL
  method <- options[[paste0("method", type)]]
  historical_sd_known <- options[[paste0("sd", type)]]  
  k <- options[[paste0("kValue", type)]]
  var_name <- NULL

  vars <- unlist(options[[paste0("variables", type)]])
  if (options[[paste0("sampleStats", type)]]) {
    # Take sample size, sample mean and sample SD directly from sample statistics option values.
    n <- options[[paste0("sampleSize", type)]]
    mean_sample <- options[[paste0("sampleMean", type)]]
    sd_sample <- options[[paste0("sampleSD", type)]]
  } else {
    if (length(vars) == 1) {
      # Dataset is available. Process it.
      if (is.null(dataset)) {
        dataset <- .readDataSetToEnd(columns.as.numeric=vars)
        # Proceed with calculations for the data sample.
        data <- na.omit(dataset[[.v(vars)]])
        # Error checking for infinite/missing values.
        .hasErrors(dataset=dataset, type = c("infinity", "missingValues"), target = vars, exitAnalysisIfErrors = TRUE)
        n <- length(data)
        mean_sample <- mean(data)
        sd_sample <- sd(data)
        var_name <- vars[1]
      }
    }
  }

  # Initializing the lot decision table
  decision_table <- createJaspTable(title = gettextf("Accept or Reject Lot %s", ifelse(!is.null(var_name), paste0("(Variable: <b>", var_name, "</b>)"), "")))
  decision_table$transpose <- TRUE
  decision_table$transposeWithOvertitle <- FALSE
  decision_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row for title. Add title if needed.
  decision_table$addColumnInfo(name = "col_1", title = gettext("Sample Size"), type = "integer")
  decision_table$addColumnInfo(name = "col_2", title = gettext("Sample Mean"), type = "number")
  decision_table$addColumnInfo(name = "col_3", title = gettext("Sample Standard Deviation"), type = "number")

  # The below rows (tranposed columns) are to be added only if the corresponding options have been selected.
  if (historical_sd_known) {
    decision_table$addColumnInfo(name = "col_4", title = gettext("Historical Standard Deviation"), type = "number")
  }
  if (options[[paste0("lsl", type)]]) {
    decision_table$addColumnInfo(name = "col_5", title = gettext("Lower Specification Limit (LSL)"), type = "number")
  }
  if (options[[paste0("usl", type)]]) {
    decision_table$addColumnInfo(name = "col_6", title = gettext("Upper Specification Limit (USL)"), type = "number")
  }
  if (options[[paste0("lsl", type)]]) {
    decision_table$addColumnInfo(name = "col_7", title = gettext("Z.LSL"), type = "number")
  }
  if (options[[paste0("usl", type)]]) {
    decision_table$addColumnInfo(name = "col_8", title = gettext("Z.USL"), type = "number")
  }
  decision_table$addColumnInfo(name = "col_9", title = gettext("Critical distance (k)"), type = "number")  
  if (method == "M") {
    decision_table$addColumnInfo(name = "col_10", title = gettext("Maximum allowable proportion (M)"), type = "number")
  }

  lotContainer[["decision_table"]] <- decision_table
  # Sanity checks for sample statistics
  if (sd_sample <= 0) {
    lotContainer$setError(gettext("Sample standard deviation has to be greater than 0."))
    return ()
  }

  if (is.null(mean_sample)) {
    lotContainer$setError(gettext("Sample mean is invalid."))
    return ()
  }
  # We always have a sample standard deviation.
  # We might or might not have a historical standard deviation, so comparison is to be done accordingly.
  sd_compare <- sd_sample
  sd_historical <- 0
  if (historical_sd_known) {
    # Historical standard deviation is known. We'll use it for comparison.
    sd_historical <- options[[paste0("stdev", type)]]
    sd_compare <- sd_historical
  }

  lsl <- usl <- NULL
  z.lsl <- NULL
  z.usl <- NULL
  decision <- NULL

  if (!options[[paste0("lsl", type)]] && !options[[paste0("usl", type)]]) {
    decision <- NULL
  }

  # Decision for the lot is made based on the available specification limits.  
  M <- PL <- PU <- NULL    
  # 1. LSL is available.
  if (options[[paste0("lsl", type)]]) {
    lsl <- options[[paste0("lower_spec", type)]]
    z.lsl <- ((mean_sample - lsl) / sd_compare)      
    if (method == "M") {
      if (historical_sd_known) {
        M <- pnorm(k * sqrt(n/(n-1)), lower.tail=F)      
        PL <- pnorm(z.lsl * sqrt(n/(n-1)), lower.tail=F)
      } else {
        x <- max(0, 0.5 - 0.5 * z.lsl * (sqrt(n)/(n-1)))
        a <- b <- n/2 - 1
        PL <- pbeta(x, a, b)
        Bm <- 0.5 * (1 - k * (sqrt(n)/(n-1)))
        M <- pbeta(Bm, a, b)
      }
    }
    if (!options[[paste0("usl", type)]]) {
      # Only LSL available. Accept/reject lot based on LSL.
      if (method == "k") {
        decision <- z.lsl >= k
      } else {
        decision <- PL <= M
      } 
    }    
  }
  # 2. USL is available.
  if (options[[paste0("usl", type)]]) {
    usl <- options[[paste0("upper_spec", type)]]
    z.usl <- (usl - mean_sample) / sd_compare
    if (method == "M") {            
      if (historical_sd_known) {
        M <- pnorm(k * sqrt(n/(n-1)), lower.tail=F)      
        PU <- pnorm(z.usl * sqrt(n/(n-1)), lower.tail=F)
      } else {
        x <- max(0, 0.5 - 0.5 * z.usl * (sqrt(n)/(n-1)))
        a <- b <- n/2 - 1
        PU <- pbeta(x, a, b)
        Bm <- 0.5 * (1 - k * (sqrt(n)/(n-1)))
        M <- pbeta(Bm, a, b)
      }
    }
    if (!options[[paste0("lsl", type)]]) {
      # Only USL available. Accept/reject lot based on USL.
      if (method == "k") {
        decision <- z.usl >= k
      } else {
        decision <- PU <= M
      }
    }
  }
  # 3. Both LSL and USL are available.
  if (options[[paste0("lsl", type)]] && options[[paste0("usl", type)]]) {
    if (method == "k") {
      # When both LSL and USL are specified, we need to decide based on standard deviation.
      # Historical standard deviation known
      if (historical_sd_known) {
          # Error handling for AQL/RQL
          aql <- options[[paste0("aql", type)]]
          rql <- options[[paste0("rql", type)]]
          if (aql >= rql) {
            lotContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
            return ()
          }
          z.p <- (options[[paste0("lower_spec", type)]] - options[[paste0("upper_spec", type)]]) / (2 * sd_historical)
          p <- pnorm(z.p)
          if (2*p >= rql) {
            decision <- FALSE
          } else if (2*p <= aql) {
            decision <- (z.lsl >= k) && (z.usl >= k)
          } else {
            if (n <= 1) {
              lotContainer$setError(gettext("Can not accept or reject lot: sample size has to be greater than 1."))
              return ()
            } else {
              q.l <- z.lsl * sqrt(n/(n-1))
              p.l <- pnorm(q.l, lower.tail = FALSE)
              q.u <- z.usl * sqrt(n/(n-1))
              p.u <- pnorm(q.u, lower.tail = FALSE)
              p.combined <- p.l + p.u
              z.m <- k * sqrt(n/(n-1))
              m <- pnorm(z.m, lower.tail = FALSE)
              decision <- p.combined <= m
            }
          }      
      } else {
        # Historical standard deviation unknown
        if (n <= 1) {
          lotContainer$setError(gettextf("Sample size has to be <b>> 1</b> if <b>both</b> LSL and USL are provided, and historical standard deviation is <b>unknown</b>."))
          return ()
        } else {
          a <- (n - 2) / 2
          b <- (n - 2) / 2
          x.l <- max(0, 0.5 - (0.5 * z.lsl * sqrt(n)/(n-1)))
          p.l <- pbeta(x.l, a, b)
          x.u <- max(0, 0.5 - (0.5 * z.usl * sqrt(n)/(n-1)))
          p.u <- pbeta(x.u, a, b)
          p.combined <- p.l + p.u
          b.m <- 0.5 * (1 - k * sqrt(n)/(n-1))
          m <- pbeta(b.m, a, b)
          decision <- p.combined <= m
        }
      }
    } else {
      # M method
      P <- PL + PU
      decision <- P <= M
    }
  }
  # Fill up the decision table
  row <- list("col_1" = n, "col_2" = mean_sample, "col_3" = sd_sample, "col_4" = sd_compare)
  # The below values are to filled only when the corresponding options have been selected.
  if (options[[paste0("lsl", type)]]) {
    row <- append(row, list("col_5" = options[[paste0("lower_spec", type)]]))
  }
  if (options[[paste0("usl", type)]]) {
    row <- append(row, list("col_6" = options[[paste0("upper_spec", type)]]))
  }
  if (options[[paste0("lsl", type)]]) {
    row <- append(row, list("col_7" = z.lsl))
  }
  if (options[[paste0("usl", type)]]) {
    row <- append(row, list("col_8" = z.usl))
  }
  row <- append(row, list("col_9" = k))
  if (method == "M") {
    row <- append(row, list("col_10" = M))
  }

  decision_table$addRows(row)
  decision_table$showSpecifiedColumnsOnly <- TRUE
  decision_table$position <- 1

  # Show the decision for the lot.
  if (!is.null(decision)) {
    if (is.null(lotContainer[["decision_output"]])) {
      decision_output <- createJaspHtml(text = gettextf("<u>Decision:</u> <b>%s</b> lot.", ifelse(decision == TRUE, "Accept", "Reject")), position = 2)
      decision_output$position <- 2
      lotContainer[["decision_output"]] <- decision_output
    }
  }
}