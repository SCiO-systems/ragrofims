context("test checkers")


library(ragapi)
library(checkmate)

test_that("test has_agronomic_metadata with all missing data with NA", {
  
  .data <- data.frame(a1=c(NA,NA,NA),b1=c(NA,NA,NA), c1= c("","",""))
  out_test <- has_agronomic_metadata(.data) 
  testthat::expect_false(object = out_test)
  
})


test_that("test has_agronomic_metadata with all missing data with double quotes ", {
  
  .data <- data.frame(a1=c("","",""), b1=c("","",""))
  out_test <- has_agronomic_metadata(.data) 
  testthat::expect_false(object = out_test)
  
})



test_that("test has_agronomic_metadata-api0233-testq0-no combos ", {
  
  .data <- ragapi::ag_get_projentity_studyId(studyDbId = 3, format = "data.frame")
  out_test <- has_agronomic_metadata(.data) 
  testthat::expect_false(object = out_test)
  
})

test_that("test has_agronomic_metadata-api0233-testq1-empty combos ", {
  
  resp <- ragapi::ag_get_projentity_studyId(studyDbId = 4, format = "data.frame")
  out_test <- has_agronomic_metadata(resp) 
  testthat::expect_false(object = out_test)
  
})




test_that("test has_agronomic_metadata-api0233-testq2- 1 row-filled combos and 1 row-empty combos", {
  
  resp <- ragapi::ag_get_projentity_studyId(studyDbId = 5, format = "data.frame")
  out_test <- has_agronomic_metadata(resp) 
  testthat::expect_true(object = out_test)
  
})