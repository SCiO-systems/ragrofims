context("test clean funding agency")
library(ragapi)

test_that("Test clean funding agency ", {
  
.data <- ag_get_fundagency_studyId(studyDbId = 28,format = "data.frame",
                                     serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                     version ="/0212/r")
.data <- .replace_other_attribute_funding(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
.data <- clean_fundagency(.data)
  
testthat::expect_equal(object = nrow(.data),expected = 3)


})