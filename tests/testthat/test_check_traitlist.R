library(dplyr)
library(ragapi)
library(ragrofims)

context("test check_traitlist")

test_that("Test check duplicates in testq28 - traitlist v0345 - expsite=82- studyId=173", {
  
  library(ragapi)
  library(ragrofims)
  library(dplyr)
  serverURL <- "https://research.cip.cgiar.org/agrofims/api/dev"
  version = "/0345/r"
  expsiteId <- 82
  factorId <- 173
  
  traitlist <- get_agrofims_traitlist(expsiteId = expsiteId, format= "data.frame", 
                                      serverURL= serverURL,  version = version)
  
  out <- check_duplicates_traitlist(traitlist)
  
  testthat::expect_true(out)
  
})

test_that("Test remove duplicates in trailist in testq28 - v.345  - expsite=82- studyId=173", {
  
  library(ragapi)
  library(ragrofims)
  library(dplyr)
  serverURL <- "https://research.cip.cgiar.org/agrofims/api/dev"
  version = "/0345/r"
  expsiteId <- 82
  factorId <- 173
  
  traitlist <- get_agrofims_traitlist(expsiteId = expsiteId, format= "data.frame", 
                                      serverURL= serverURL,  version = version)
  
  out <- nrow(remove_duplicates_traitlist(traitlist))
  
  testthat::expect_equal(out,9)
  
})



