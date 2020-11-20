library(ragapi)
library(ragrofims)


context("Test for clean and get metadata from project lead")

test_that("Test get_projlead_metadata for testq0 - API ver. 233 - no combos", {
  
  out <- get_projlead_metadata(studyId  = 3,format = "data.frame",
                              serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                              version ="/0233/r")
  
  testthat::expect_equal(ncol(out), 0)
  testthat::expect_equal(nrow(out), 0)
  
})

test_that("Test get_projlead_metadata for testq5 - API ver. 233 - 1 other combo", {
  
  out <- get_projlead_metadata(studyId  = 7,format = "data.frame",
                               serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                               version ="/0233/r")
  
  testthat::expect_equal(ncol(out), 2)
  testthat::expect_equal(nrow(out), 2)
  
})


test_that("Test get_projlead_metadata for testq6 - API ver. 233 - 1 filled combo", {
  
  out <- get_projlead_metadata(studyId  = 8,format = "data.frame",
                               serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                               version ="/0233/r")
  
  testthat::expect_equal(ncol(out), 2)
  testthat::expect_equal(nrow(out), 2)
  
})