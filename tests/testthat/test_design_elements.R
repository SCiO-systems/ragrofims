
library(ragapi)
library(ragrofims)
library(gsheet)

meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")


context("test clean and convert xlsx project mgmt entity")


test_that("Test no factors in interface FRCBD", {
  
env <- "dev"
studyId <- "" 
expsiteId <- 12
serverURL <- paste0("https://research.cip.cgiar.org/agrofims/api/", env)
version <- paste0("/0253/r")
format <- "data.frame"; 
.data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId=expsiteId, 
                                             format=format,
                                             serverURL =  serverURL,
                                             version = version)
factor_names <- ck_factor_names(.data)

testthat::expect_equal(factor_names, expected = "There are missing factors in your experiment. Check the design tab.")

})



test_that("Test no levels in interface FRCBD", {
  
  env <- "dev"
  studyId <- "" 
  expsiteId <- 12
  serverURL <- paste0("https://research.cip.cgiar.org/agrofims/api/", env)
  version <- paste0("/0253/r")
  format <- "data.frame"; 
  .data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId=expsiteId, 
                                               format=format,
                                               serverURL =  serverURL,
                                               version = version)
  .levels <- ck_level_values(.data)
  testthat::expect_false(.levels)#expected = "There are missing factors in your experiment. Check the design tab.")
  testthat::expect_equal(.levels, expected = "There are missing levels in your experimetal design. Check the design tab.")
  
})