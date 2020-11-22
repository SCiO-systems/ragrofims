library(ragapi)
library(ragrofims)

context("Test for clean and get metadata from crop information metadata")

test_that("Test get_cropdesc_metadata for testq24 - API ver. 0291 - monocrop ", {
  
  meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
  
  out <- get_cropdesc_metadata(expsiteId = 25, format= "data.frame", 
                                    serverURL="https://research.cip.cgiar.org/agrofims/api/dev",  
                                    version = "/0291/r",
                                    meta_dbattributes=meta_dbattributes)
  
  testthat::expect_equal(ncol(out), 2)
  testthat::expect_equal(nrow(out), 4)
  
})