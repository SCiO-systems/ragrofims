
library(dplyr)
library(ragapi)
library(ragrofims)

context("test add_season_numplot_prefix")

test_that("Test cases for add_season_numplot_prefix", {
  
  dt <- data.frame(variableName =c("chipkpea_height","chipkea_weigth"), samplesperseason = c(2,10), samplesperplot=c(10,5))
  out <-add_season_numplot_prefix(dt)
  
  dt2 <- data.frame(variableName =c("chipkpea_height","chipkea_weigth"), samplesperseason = c(1,1), samplesperplot=c(1,1))
  out2 <-add_season_numplot_prefix(dt2)
  
  dt3 <- data.frame()
  out3 <-add_season_numplot_prefix(dt3)
  
  testthat::expect_equal(length(out), 70)
  testthat::expect_equal(length(out2), 2)
  testthat::expect_null(object =out3) 
  
})

