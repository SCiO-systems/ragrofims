
library(ragapi)
library(ragrofims)
library(gsheet)

meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")

context("test clean and convert xlsx project mgmt entity")


test_that("Test clean and convert xlsx project mgmt entity testq7 - API ver. 233", {
  
.data <- ag_get_projentity_studyId(studyDbId = 9, 
                                   format = "data.frame",
                                   serverURL = "https://research.cip.cgiar.org/agrofims/api/dev", 
                                   version ="/0233/r")
.data <- clean_projentity(.data)

out <- convert_to_xlsx_projentity(.data, meta_dbattributes)


testthat::expect_equal(ncol(out), 2)
testthat::expect_equal(nrow(out), 7)

})

test_that("Test clean and convert xlsx project mgmt entity testq7 - API ver. 233", {

  meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
  
  .data <- ag_get_projentity_studyId(studyDbId = 9, 
                                     format = "data.frame",
                                     serverURL = "https://research.cip.cgiar.org/agrofims/api/dev", 
                                     version ="/0233/r")
  out <- clean_projentity(.data)
  
  testthat::expect_equal(ncol(out), 3)
  testthat::expect_equal(nrow(out), 3)
  
})


test_that("test ag_get_projentity_studyId-testq0-api_ver-233", {
  
  meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
  
  out <- get_projentity_metadata(studyId = 3, 
                                 format = "data.frame",
                                 serverURL = "https://research.cip.cgiar.org/agrofims/api/dev", 
                                 version ="/0233/r",
                                 meta_dbattributes=meta_dbattributes)
  
  testthat::expect_equal(ncol(out), 0L)
  testthat::expect_equal(nrow(out), 0L)
  
})



test_that("test ag_get_projentity_studyId-testq7-api_ver-233", {
  
  meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
  
  out <- get_projentity_metadata(studyId = 9, 
                                     format = "data.frame",
                                     serverURL = "https://research.cip.cgiar.org/agrofims/api/dev", 
                                     version ="/0233/r",
                                     meta_dbattributes=meta_dbattributes)
  
  testthat::expect_equal(ncol(out), 2)
  testthat::expect_equal(nrow(out), 7)
  
})

