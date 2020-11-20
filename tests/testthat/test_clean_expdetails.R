library(ragapi)
library(gsheet)

context("test clean and convert xlsx funding agency")


test_that("Test clean and convert xlsx funding agency api version 233", {
  
  meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
  .data <- ragapi::ag_get_expdetails_studyId(studyDbId = 6,format = "data.frame",
                                                       serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                                       version ="/0233/r")  
  
  .data <- clean_expdetails(.data, "experimenttype", "experimenttypeother")
  .data <- convert_to_xlsx_expdetails(.data, meta_dbattributes)
  
  testthat::expect_equal(object = nrow(.data),expected = 9)
  testthat::expect_equal(object = ncol(.data),expected = 2)
  
})

