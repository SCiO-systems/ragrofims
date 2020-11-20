library(ragapi)
library(ragrofims)
context("Test for clean and get metadata from personnel metadata")

test_that("Test get_personnel_metadata for testq7 - API ver. 233 - no personnel", {
  
  out <-  get_personnel_metadata(studyId = 3,format="data.frame",
                                 serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                 version ="/0233/r")
  
  testthat::expect_equal(ncol(out), 0)
  testthat::expect_equal(nrow(out), 0)
  
})


test_that("Test get_personnel_metadata for testq7 - API ver. 233 - 3 personnel", {
  
  out <-  get_personnel_metadata(studyId = 9,format="data.frame",
                                 serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                 version ="/0233/r")
  
  testthat::expect_equal(ncol(out), 2)
  testthat::expect_equal(nrow(out), 21)
  
})