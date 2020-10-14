#' Replace attributes that has others values in experimenta-site metadata
#'
#' @param .data data to clean up \code{Other} values
#' @param attribute attribute of the database. Attribute name from AgroFIMS database where a user input is stored 
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @description Tipically, users type values that are not mapped in the agronomy ontology. For this reason, the API response retrieve additional information
#' that should ensemble in a data structure.
#' @examples \dontrun{
#' .data <- ag_get_personnel_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_personnel(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' }
#' @export
#'     
.replace_other_attribute_sitedesc <- function(.data, attribute, other_attribute){
  
  #TODO: check number of rows , lanzar error si hay 0 filas 
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


#' Get other row position in experiment-site data
#' 
#' @param .data data to clean up \code{Other} values
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @examples \dontrun{
#' .data <- ag_get_sitedesc_studyId(studyDbId = 21,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data. <- .get_other_position_sitedesc(.data, "experimentTypeOther")
#' }
#' @export
#' 
.get_other_rowpos_sitedesc <- function(.data, other_attribute){
  which(.data$DbAttribute==other_attribute)
}


#' Clean values in experiment-site information
#' 
#' @description Tipically values users fill forms or combos with standarized inputs in AgroFIMS. However, sometimes ontologized inputs are not enough.
#' For this reason, they select the \code{Other} value in order to fill options that have not been maped in the Agronomic Ontology in AgroFIMS
#' @param .data data to clean up \code{Other} values
#' @param attribute attribute of the database. Attribute name from AgroFIMS database where a user input is stored 
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @examples \dontrun{
#' .data <- ag_get_sitedesc_studyId(studyDbId = 21,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data. <- clean_sitedesc(.data, "experimentType", "experimentTypeOther")
#' }
#' @export  
#'  
clean_sitedesc <- function(.data){
  
  index <- c(.get_other_rowpos_sitedesc(.data, "inhighlevelother"),
             .get_other_rowpos_sitedesc(.data, "insitevegetationother"),
             .get_other_rowpos_sitedesc(.data, "soilclasssystemother")
            )
  .data <- .replace_other_attribute_sitedesc(.data, "inhighlevelid", "inhighlevelother")
  .data <- .replace_other_attribute_sitedesc(.data, "insitevegetation", "insitevegetationother")
  .data <- .replace_other_attribute_sitedesc(.data, "soilclasssystemid", "soilclasssystemother")
  
  .data <- .data[-index, ]
  
}





#' Convert API-data.frame response to AgroFIMS Excel logic table
#' 
#' @param .data data.frame table with experimental details metadata
#' @param meta_dbattributes metadata attributes and labels in AgroFIMS
#' @description Transform an API table to Excel table format
#' @author Omar Benites
#' @description set and modify databases attributes by AgroFIMS label in order to export the fieldbook file
#' @export
#' 
convert_to_xlsx_sitedesc <- function(.data, meta_dbattributes){
  
   out <- dplyr::left_join(.data, meta_dbattributes)
   out <- dplyr::filter(out, !is.na(AgroLabelDbAttribute))
  .data <- data.frame(Parameter = out$AgroLabelDbAttribute, 
                      Value     = out$Value, 
                      stringsAsFactors = FALSE)
}



#' Get metadata from experiment site information
#' 
#' @param expsiteId or experimente site ID
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @param meta_dbattributes data dictionary of metadata. It includes equivalences between excel and database names.
#' @description get metadata from experimental details
#' @export
#' 
get_sitedesc_metadata <- function(expsiteId = NULL, format= NULL, 
                                    serverURL=NULL,  version = NULL,
                                    meta_dbattributes=NULL){
  
  sitedesc_data <- ag_get_sitedesc_expsiteId(expsiteDbId = expsiteId,
                                        format = "data.frame",
                                        serverURL = serverURL, version = version)
  
  cond <- nrow(sitedesc_data)>0 #has_agronomic_metadata(sitedesc_data) 
  
  if(cond){
    sitedesc_data <- clean_sitedesc(sitedesc_data)
    sitedesc_data <- convert_to_xlsx_sitedesc(sitedesc_data, meta_dbattributes)
  } else{
    sitedesc_data <- data.frame()
  } 
  sitedesc_data  
}  
  





