library(ragapi)
library(gsheet)

context("test clean funding agency")

test_that("Test clean funding agency api version 233", {

  meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
  

.data <- ag_get_fundagency_studyId(studyDbId = 6,format = "data.frame",
                                     serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                     version ="/0233/r")
.data <- .replace_other_attribute_fundagency(.data, "fundagencytypeid", "fundagencytypeother")
.data <- clean_fundagency(.data)
  
testthat::expect_equal(object = nrow(.data),expected = 2)
testthat::expect_equal(object = ncol(.data),expected = 2)

})