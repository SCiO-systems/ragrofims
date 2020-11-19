
library(ragapi)
library(ragrofims)
library(gsheet)

context("test of calculation nutrient in fertlizer or products")

test_that("Test  with empty table API v0291-ID=25- factorId=2", {
  
  out <- get_agrofims_designprod(expsiteId= 25,
                                   format = "data.frame",
                                   serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                   version = "/0291/r"
  )
  fertilizer <- calc_fert_design(fertilizer = out, 2)
  
  testthat::expect_equal(ncol(fertilizer), 0)
  testthat::expect_equal(nrow(fertilizer), 0)  
  
})


test_that("Test calculation of fertilizer - API v0291-ID=25- factorId=3", {
  
  out <- get_agrofims_designprod(expsiteId= 25,
                                 format = "data.frame",
                                 serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                 version = "/0291/r"
  )
  fertilizer <- calc_fert_design(fertilizer = out, factorId =  3)
  
  testthat::expect_equal(ncol(fertilizer), 19)
  testthat::expect_equal(nrow(fertilizer), 12)  
  
})
