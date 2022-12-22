# Tests for Decide variable lots

##----------------------------------------------------------------
##                      1. No dataset, LSL                      --
##----------------------------------------------------------------
test_that("Decide lots - no dataset, lsl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 24
  options$sampleMean <- 1.5
  options$sampleSD <- 1
  options$kValue <- 1.309
  options$lsl <- TRUE
  options$usl <- FALSE
  
  # 1.1 Historical SD unknown
  options$sd <- FALSE
  options$lower_spec <- 0.1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(24, 1.5, 1, 0.1, 1.4, 1.309))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 1.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 2.4
  options$lower_spec <- 0.5
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(24, 1.5, 1, 2.4, 0.5, 0.4167, 1.309))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
})

##----------------------------------------------------------------
##                      2. No dataset, USL                      --
##----------------------------------------------------------------
test_that("Decide lots - no dataset, usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 125
  options$sampleMean <- 172.3
  options$sampleSD <- 23.7
  options$kValue <- 2.5
  options$lsl <- FALSE
  options$usl <- TRUE
  
  # 2.1 Historical SD unknown
  options$sd <- FALSE

  # 2.1.1 Reject
  options$upper_spec <- 230
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 230, 2.435, 2.5))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 2.1.2 Accept
  options$upper_spec <- 232
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 232, 2.519, 2.5))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 2.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 19.5

  # 2.2.1 Reject
  options$upper_spec <- 221
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(125, 172.3, 23.7, 19.5, 221, 2.497, 2.5))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 2.2.2 Accept
  options$upper_spec <- 222
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 19.5, 222, 2.549, 2.5))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

##---------------------------------------------------------------
##               3. No dataset, both LSL and USL               --
##---------------------------------------------------------------
test_that("Decide lots - no dataset, lsl & usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 785
  options$sampleMean <- 35.2
  options$sampleSD <- 8.92
  options$kValue <- 2
  options$lsl <- TRUE
  options$usl <- TRUE
  options$upper_spec <- 60
  
  # 3.1 Historical SD unknown
  options$sd <- FALSE
  
  # 3.1.1 Reject
  options$lower_spec <- 17
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 17, 60, 2.04, 2.78, 2))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.1.2 Accept
  options$lower_spec <- 16
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 16, 60, 2.152, 2.78, 2))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 3.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 10.12
  options$aql <- 0.05
  options$rql <- 0.15
  options$lower_spec <- 10
  
  # 3.2.1 Reject
  options$upper_spec <- 55
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(785, 35.2, 8.92, 10.12, 10, 55, 2.49, 1.956, 2))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.2.2 Accept
  options$upper_spec <- 56
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 10.12, 10, 56, 2.49, 2.055, 2))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

##---------------------------------------------------------------
##                       4. Dataset, LSL                       --
##---------------------------------------------------------------
test_that("Decide lots - dataset, lsl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$variables <- "contNormal"
  options$sampleStats <- FALSE
  options$kValue <- 1.309
  options$lsl <- TRUE
  options$usl <- FALSE
  
  # 4.1 Historical SD unknown
  options$sd <- FALSE
  options$lower_spec <- 0.1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, -0.1887, 1.058, 0.1, -0.2728, 1.309))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 4.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 0.1
  options$lower_spec <- 0
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(100, -0.1887, 1.058, 0.1, 0, -1.888, 1.309))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Reject</b> lot."))
})

##---------------------------------------------------------------
##                       5. Dataset, USL                       --
##---------------------------------------------------------------
test_that("Decide lots - no dataset, usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$variables <- "contOutlier"
  options$sampleStats <- FALSE
  options$kValue <- 2.5
  options$lsl <- FALSE
  options$usl <- TRUE
  
  # 5.1 Historical SD unknown
  options$sd <- FALSE

  # 5.1.1 Reject
  options$upper_spec <- 7
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 0.0355, 2.903, 7, 2.399, 2.5))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 5.1.2 Accept
  options$upper_spec <- 8
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 0.0355, 2.903, 8, 2.744, 2.5))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 5.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 2.5
  options$aql <- 0.05
  options$rql <- 0.15

  # 5.2.1 Reject
  options$upper_spec <- 6
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(100, 0.0355, 2.903, 2.5, 6, 2.386, 2.5))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 5.2.2 Accept
  options$upper_spec <- 7
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 0.0355, 2.903, 2.5, 7, 2.786, 2.5))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

##----------------------------------------------------------------
##                 6. Dataset, both LSL and USL                 --
##----------------------------------------------------------------
test_that("Decide lots - dataset, lsl & usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$variables <- "contGamma"
  options$sampleStats <- FALSE
  options$kValue <- 1.309
  options$lsl <- TRUE
  options$usl <- TRUE
  options$lower_spec <- 0
  
  # 6.1 Historical SD unknown
  options$sd <- FALSE
  
  # 6.1.1 Reject
  options$upper_spec <- 6
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 2.033, 1.532, 0, 6, 1.327, 2.589, 1.309))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 6.1.2 Accept
  options$upper_spec <- 7
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 2.033, 1.532, 0, 7, 1.327, 3.241, 1.309))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 6.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 1.42
  options$aql <- 0.05
  options$rql <- 0.15
  options$upper_spec <- 6
  
  # 6.2.1 Reject
  options$lower_spec <- 1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(100, 2.033, 1.532, 1.42, 1, 6, 0.7274, 2.794, 1.309))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 6.2.2 Accept
  options$lower_spec <- 0
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 2.033, 1.532, 1.42, 0, 6, 1.432, 2.794, 1.309))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})