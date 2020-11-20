
library(ragapi)
library(ragrofims)

context("test of calculation of nutrient content in products")

test_that("Test Calculation of Nutrient Pipeline with empty table API v0291", {
  
  nutrient <- get_agrofims_nutrients(expsiteId= 6,
                                   format = "data.frame",
                                   serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                   version = "/0291/r"
  )
  nutrient <- get_fertproducts_crop(fertproducts = nutrient, crop = "Field")
  nutrient <- calc_prodamount(nutrient)
  
  testthat::expect_equal(ncol(nutrient), 0)
  testthat::expect_equal(nrow(nutrient), 0)  
  
})


test_that("Test calculation of nutrient amount - fertilizer product submodule - ID=18 - API v0291", {
  
  out <- get_agrofims_nutrients(expsiteId=18,
                                   format = "data.frame",
                                   serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                   version = "/0291/r"
  )
  nutrient <- get_fertproducts_crop(fertproducts = out, crop = "Barley")
  
  nutrient <- calc_prodamount(nutrient = nutrient) 
  
  testthat::expect_equal(ncol(nutrient), 17)
  testthat::expect_equal(nrow(nutrient), 2)
  
})
