#' Replace attributes that has others values in expdetails
#'
#' @param .data data to clean up \code{Other} values
#' @param attribute attribute of the database. Attribute name from AgroFIMS database where a user input is stored 
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @examples \dontrun{
#' .data <- ag_get_expdetails_studyId(studyDbId = 21,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data. <- .replace_other_attribute_expdetails(.data, "experimentType" ,"experimentTypeOther")
#' }
#'   
.replace_other_attribute_expdetails <- function(.data, attribute, other_attribute){
    
    value <- .data[.data$DbAttribute == attribute,"Value"]
    print(value)
    #add checkers
    if(value=="Other"){
      other <- .data[.data$DbAttribute == other_attribute,"Value"]  
      .data[.data$DbAttribute == attribute,]$Value <- other
    } else if(grepl(pattern = "Other", value)) {
      other <- .data[.data$DbAttribute == other_attribute,"Value"] 
      value <- gsub(pattern = "Other", replacement = other, x = value)
      .data[.data$DbAttribute == attribute,]$Value <- value
    }  
    .data
}

#' Get other row position in expdetails data
#' 
#' @param .data data to clean up \code{Other} values
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @examples \dontrun{
#' .data <- ag_get_expdetails_studyId(studyDbId = 21,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data. <- .get_other_position_expdetails(.data, "experimentTypeOther")
#' }
#'  
.get_other_rowpos_expdetails <- function(.data, other_attribute){
    which(.data$DbAttribute==other_attribute)
}

#' Remove other values in expdtails data
#' 
#' @param .data data to clean up \code{Other} values
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @examples \dontrun{
#' .data <- ag_get_expdetails_studyId(studyDbId = 21,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data. <- .remove_other_exdetails(.data, "experimentTypeOther")
#' }
.remove_other_exdetails <- function(.data, other_attribute){
  pos <- .get_other_rowpos_expdetails(.data, other_attribute)
  .data <- .data[-pos, ] 
}

#' Clean other values in experiment details 
#' 
#' @description Tipically values users fill forms or combos with standarized inputs in AgroFIMS. However, sometimes ontologized inputs are not enough.
#' For this reason, they select the \code{Other} value in order to fill options that have not been maped in the Agronomic Ontology in AgroFIMS
#' @param .data data to clean up \code{Other} values
#' @param attribute attribute of the database. Attribute name from AgroFIMS database where a user input is stored 
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @export
#' @examples \dontrun{
#' .data <- ag_get_expdetails_studyId(studyDbId = 21,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data. <- clean_other_expdetails(.data, "experimentType", "experimentTypeOther")
#' }
#' 
clean_other_expdetails <- function(.data, attribute, other_attribute){
  
  .data <- .replace_other_attribute_expdetails(.data, attribute, other_attribute) 
  .data <- .remove_other_exdetails(.data, other_attribute)
  
}




#' Modify labels for metadata table
#' @param .data data.frame table with 
#' @param attribute character vector of databases attributes
#' @param meta_dbattributes metadata attributes and labels in AgroFIMS
#' @description 
#' @author Omar Benites
#' @description set and modify databases attributes by AgroFIMS label in order to export the fieldbook file
#' @export
#' 
.modify_labels_expdetails <- function(.data, meta_dbattributes){
  
  out <- dplyr::left_join(.data,meta_dbattributes)
  out <- dplyr::filter(out, !is.na(AgroLabelDbAttribute))
  .data <- data.frame(Parameter = out$AgroLabelDbAttribute, 
                      Value     = out$Value, 
                      stringsAsFactors = FALSE)
}
 

# #Ejemplo 1
# .data <- ag_get_expdetails_studyId(studyDbId = 6,format = "data.frame",
#                                      serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#                                      version ="/0212/r")
# .data$Value[8] <- "omar benites alfaro"
# 
# .data <- clean_other_expdetails(.data, "experimentType", "experimentTypeOther")
# 
# meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
# .data <- .modify_labels_expdetails(.data, meta_dbattributes)

# 
# 
# #Ejemplo 2
# .data <- ag_get_expdetails_studyId(studyDbId = 6,format = "data.frame",
#                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#                                    version ="/0212/r")
# #.data$Value[8] <- "omar benites alfaro" 
# 
# .data <- clean_other_expdetails(.data, "experimentType", "experimentTypeOther")
#
#
#




