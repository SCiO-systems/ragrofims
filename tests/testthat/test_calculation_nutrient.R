
library(ragapi)
library(ragrofims)
library(gsheet)

context("test of calculation nutrient in products")

test_that("Test Calculation of Nutrient Pipeline with empty table API v0291", {

  out <- get_agrofims_fertproducts(expsiteId= 6,
                                   format = "data.frame",
                                   serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                   version = "/0291/r"
  )
  fertilizer <- get_fertproducts_crop(fertproducts = out, crop = "Field")
  fertilizer <- calc_nutamount(fertilizer) 
  
  testthat::expect_equal(ncol(fertilizer), 0)
  testthat::expect_equal(nrow(fertilizer), 0)  
  
})

test_that("Test calculation of nutrient amount - Empty table with API v0291", {
  
  out <- get_agrofims_fertproducts(expsiteId= 6,
                                   format = "data.frame",
                                   serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                   version = "/0291/r"
  )
  
  fertilizer <- calc_nutamount(fertilizer = out) 
  
  testthat::expect_equal(ncol(fertilizer), 0)
  testthat::expect_equal(nrow(fertilizer), 0)
  
})




test_that("Test calculation of nutrient amount - fertilizer product submodule - ID=8 - API v0291", {
  
  out <- get_agrofims_fertproducts(expsiteId=8,
                                        format = "data.frame",
                                        serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                        version = "/0291/r"
                                        )

  fertilizer <- calc_nutamount(fertilizer = out) 
  
  testthat::expect_equal(ncol(fertilizer), 17)
  testthat::expect_equal(nrow(fertilizer), 7)
  
})




test_that("Test calculation of nutrient amount - with no products - ID=9 - API v0291", {
  
  out <- get_agrofims_fertproducts(expsiteId=9,
                                   format = "data.frame",
                                   serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                   version = "/0291/r"
  )
  
  fertilizer <- calc_nutamount(fertilizer = out) 
  
  testthat::expect_equal(ncol(fertilizer), 0)
  testthat::expect_equal(nrow(fertilizer), 0)
  
})


