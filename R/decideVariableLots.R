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
  depend_vars <- c("vars", "sampleStats", "sampleSize", "sampleMean", "sampleSD", "kValue", "lsl", "lower_spec", "usl", "upper_spec", "sd", "stdev")
  risk_vars <- c("aql", "rql")

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
  k <- round(options$kValue, 3)
  var_name <- NULL

  vars <- unlist(options$variables)
  if (options$sampleStats) {
    # Take sample size, sample mean and sample SD directly from sample statistics option values.
    n <- options$sampleSize
    mean_sample <- options$sampleMean
    sd_sample <- options$sampleSD
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
  decision_table$addColumnInfo(name = "col_1", title = "Sample Size", type = "integer")
  decision_table$addColumnInfo(name = "col_2", title = "Sample Mean", type = "number")
  decision_table$addColumnInfo(name = "col_3", title = "Sample Standard Deviation", type = "number")
  
  # The below rows (tranposed columns) are to be added only if the corresponding options have been selected.
  if (options$sd) {
    decision_table$addColumnInfo(name = "col_4", title = "Historical Standard Deviation", type = "number")
  }
  if (options$lsl) {
    decision_table$addColumnInfo(name = "col_5", title = "Lower Specification Limit (LSL)", type = "number")
  }
  if (options$usl) {
    decision_table$addColumnInfo(name = "col_6", title = "Upper Specification Limit (USL)", type = "number")
  }
  if (options$lsl) {
    decision_table$addColumnInfo(name = "col_7", title = "Z.LSL", type = "number")
  }
  if (options$usl) {
    decision_table$addColumnInfo(name = "col_8", title = "Z.USL", type = "number")
  }
  decision_table$addColumnInfo(name = "col_9", title = "Critical Distance (k)", type = "number")

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
  sd <- "unknown"
  sd_historical <- 0
  if (options$sd) {
    # Historical standard deviation is known. We'll use it for comparison.
    sd <- "known"
    sd_historical <- options$stdev
    sd_compare <- sd_historical
  }

  z.lsl <- NULL
  z.usl <- NULL
  decision <- NULL

  if (!options$lsl && !options$usl) {
    decision <- NULL
  }

  # Decision for the lot is made based on the available specification limits.
  # 1. LSL is available.
  if (options$lsl) {
    z.lsl <- ((mean_sample - options$lower_spec) / sd_compare)
    z.lsl <- round(z.lsl, 3)
    if (!options$usl) {
      # Only LSL available. Accept/reject lot based on Z.LSL.
      decision <- z.lsl >= k
    }
  }
  # 2. USL is available.
  if (options$usl) {
    z.usl <- ((options$upper_spec - mean_sample) / sd_compare)
    z.usl <- round(z.usl, 3)
    if (!options$lsl) {
      # Only USL available. Accept/reject lot based on Z.USL.
      decision <- z.usl >= k
    }
  }
  # 3. Both LSL and USL are available.
  if (options$lsl && options$usl) {
    # When both LSL and USL are specified, we need to decide based on standard deviation.
    # Historical standard deviation known
    if (options$sd) {
      # Error handling for AQL/RQL
      aql <- round(options$aql, 3)
      rql <- round(options$rql, 3)
      if (aql >= rql) {
        lotContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
        return ()
      }
      z.p <- (options$lower_spec - options$upper_spec) / (2 * sd_historical)
      p <- pnorm(z.p)
      p <- round(p, 3)
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
          p.combined <- round(p.combined, 3)
          z.m <- k * sqrt(n/(n-1))
          m <- pnorm(z.m, lower.tail = FALSE)
          m <- round(m, 3)
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
        p.combined <- round(p.combined, 3)
        b.m <- 0.5 * (1 - k * sqrt(n)/(n-1))
        m <- pbeta(b.m, a, b)
        m <- round(m, 3)
        decision <- p.combined <= m
      }
    }
  }
  # Fill up the decision table
  row = list("col_1" = n, "col_2" = mean_sample, "col_3" = sd_sample, "col_4" = sd_compare)
  # The below values are to filled only when the corresponding options have been selected.
  if (options$lsl) {
    row = append(row, list("col_5" = options$lower_spec))
  }
  if (options$usl) {
    row = append(row, list("col_6" = options$upper_spec))
  }
  if (options$lsl) {
    row = append(row, list("col_7" = z.lsl))
  }
  if (options$usl) {
    row = append(row, list("col_8" = z.usl))
  }
  row = append(row, list("col_9" = k))

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