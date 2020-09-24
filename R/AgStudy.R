#' #' RAgroFIMS Class
#' #'
#' #' @title Response Class
#' #'
#' #' @description
#' #' The Response object is a generic object returned by any query
#' #' function of one of the nbaR client classes.
#' 
#' #'
#' #' @details
#' #' This class contains two fields (see also section 'Fields' below):
#' #' The field \code{content} contains the query result parsed from
#' #' the JSON response of the request. Depending on the query, this
#' #' can be an object such as e.g. a \code{ResultSet}, or a \code{Specimen} or
#' #' \code{Taxon} or a primitive data type. The field \code{reponse}
#' #' is the actual response from the \code{httr} package that is used
#' #' to perform the http request. It contains information such as the request and response headers,
#' #' status code, URL, and the raw response.
#' #'
#' #' @seealso https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html for further details.
#' #'
#' #'
#' #' @field content Parsed response of the query call
#' #' @field response Response object from package \code{httr}
#' #'
#' #' @export
#' Agrofims <- R6::R6Class(
#'   "Agrofims",
#'   public = list(
#'     studyinfo = NULL, 
#'     studydesign= NULL, 
#'     studymea = NULL,
#'     
#'     initialize = function(studyinfo, studydesign, studymea) {
#'       self$studyinfo <- studyinfo
#'       self$studydesign <- studydesign
#'       self$studymea <- studymea
#'     },
#'     print = function(...) {
#'       ## print class name
#'       cat("<Response>\n")
#'       cat("Fields:\n")
#'       cat("\tstudyinfo: Research Management Information: ", paste0("<", class(self$studyinfo)[1]), "\n")
#'       cat("\tstudydesign: Agronomic study design: ", paste0("<", class(self$studydesign)[1] ), "\n")
#'       cat("\tstudydesign: Agronomic measurements", paste0("<", class(self$studymea)[1]), "\n")
#'       invisible(self)
#'     }
#'   )
#' )
#' 
#' 
#' 
#' 
#' 
#' 
#' 
