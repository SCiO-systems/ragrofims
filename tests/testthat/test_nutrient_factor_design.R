library(ragapi)
library(ragrofims)

context("test of calculation in nutrient type and amount factor - experimental design")

test_that("test of calculation of fertilizer product", {
  
serverURL <- "https://research.cip.cgiar.org/agrofims/api/dev"
version = "/0291/r"
expsiteId <- 25
factorId <- 4
nutrient <-get_agrofims_designnut(expsiteId)


factors_data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId = expsiteId,
                                                    format = "data.frame",
                                                    serverURL = serverURL, version = version)

fert_factors <- filter_factors_factorId(factors_data, factorId = factorId)

out <- calc_fertprod_design(fertilizer = nutrient, fert_factors, factorId = factorId)

testthat::expect_equal(nrow(out), 12)  

})


library(ragapi)
library(ragrofims)

context("test of calculation in nutrient type and amount factor - experimental design")

test_that("test of calculation of fertilizer product", {
  
  serverURL <- "https://research.cip.cgiar.org/agrofims/api/dev"
  version = "/0291/r"
  expsiteId <- 25
  factorId <- 5
  nutrient <-get_agrofims_designnut(expsiteId)
  
  
  factors_data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId = expsiteId,
                                                      format = "data.frame",
                                                      serverURL = serverURL, version = version)
  
  fert_factors <- filter_factors_factorId(factors_data, factorId = factorId)
  
  out <- calc_fertprod_design(fertilizer = nutrient,fert_factors, factorId = factorId)
  
  testthat::expect_equal(nrow(out), 0)  
  testthat::expect_equal(ncol(out), 0)  
  
})


test_that("test of calculation of fertilizer product in nutrient factor", {
  
  serverURL <- "https://research.cip.cgiar.org/agrofims/api/dev"
  version = "/0345/r"
  expsiteId <- 90
  factorId <- 2
  nutrient <-get_agrofims_designnut(format = "data.frame",
                                    serverURL = serverURL, expsiteId = expsiteId,
                                    version = version
                                      )
  
  
  factors_data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId = expsiteId,
                                                      format = "data.frame",
                                                      serverURL = serverURL, version = version)
  
  fert_factors <- filter_factors_factorId(factors_data, factorId = factorId)
  
  out <- calc_fertprod_design(fertilizer = nutrient,fert_factors, factorId = factorId)
  
  testthat::expect_equal(nrow(out), 0)  
  testthat::expect_equal(ncol(out), 0)  
  
})