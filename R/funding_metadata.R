#' Replace attributes that has others values in funding agency
#'
#' @param .data data to clean up \code{Other} values
#' @param attribute attribute of the database. Attribute name from AgroFIMS database where a user input is stored 
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @description Tipically, users type values that are not mapped in the agronomy ontology. For this reason, the API response retrieve additional information
#' that should ensemble in a data structure.
#' @examples \dontrun{
#' .data <- ag_get_fundagency_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_funding(.data, "fundagencytypeId", "fundagencytypeother")
#' }
#' @export
#'     
.replace_other_attribute_fundagency <- function(.data, attribute, other_attribute){
  
  #TODO: check number of rows , lanzar error si hay 0 filas 
  
  n <- as.integer(nrow(.data))
  for(i in seq.int(n)){
    
    #TODO: CHECK  .data[i, attribute] is different from NULL
    if(.data[i, attribute]=="Other"){ #Is equal to Other
      if(!is.na(.data[i,"fundagencytypeother"])){ #check if is NA
        .data[i, attribute] <- .data[i, other_attribute]  
      }else { #put 
        .data[i, attribute] <- ""
      }
    }
  }
  .data
}


#' Clean labels in order to export in MS Excel format
#' @param .data data.frame table with 
#' @description Functionality to clean and poolish the AGROFIMS API response.
#' @author Omar Benites
#' @description clean api response attributes by AgroFIMS label in order to export the fieldbook file
#' @examples \dontrun{
#' .data <- ag_get_fundagency_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_funding(.data, "fundagencytypeId", "fundagencytypeother")
#' .data <- clean_fundagency(.data)
#' }
#' @export
#' 
clean_fundagency <- function(.data){
  
  .data <- .replace_other_attribute_fundagency(.data, "fundagencytypeid", "fundagencytypeother")
  
  #2 casos: cgiar center y los demás
  fatype <- vector("character", as.integer(nrow(.data))) #fund agency type
  faname <- vector("character", as.integer(nrow(.data))) #fund agency name
  
  for(i in 1:nrow(.data)){
    fatype[i] <- .data[,"fundagencytypeid"][i]
    if(.data[,"fundagencytypeid"][i]=="CGIAR center"){
      faname[i] <- .data[,"fundagencytypecenterid"][i] #in case of having cgiar center 
    }else {
      faname[i] <- .data[,"fundagencytypename"][i] #other cases
    }
  }
  .data <- data.frame(fatype, faname, stringsAsFactors = FALSE)
  names(.data) <- c("fundagencytypeid", "fundagencytypename")
  .data
}


#' Convert API-data.frame response to AgroFIMS Excel logic table
#' 
#' @param .data data.frame table with 
#' @param meta_dbattributes metadata attributes and labels in AgroFIMS
#' @description Transform an API table to Excel table format
#' @author Omar Benites
#' @description set and modify databases attributes by AgroFIMS label in order to export the fieldbook file
#' @importFrom data.table rbindlist
#' @importFrom tibble rownames_to_column
#' @examples \dontrun{
#' .data <- ag_get_fundagency_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_funding(.data, "fundagencytypeId", "fundagencytypeother")
#' .data <- clean_fundagency(.data)
#' .data <- convert_to_xlsx_fundagency(.data)
#' }
#' @export
#' 
convert_to_xlsx_fundagency <- function(.data, meta_dbattributes){
  
  #out <- dplyr::left_join(.data, meta_dbattributes)
  names_data <- names(.data) 
  meta_dbattributes <- meta_dbattributes %>%  dplyr::filter(DbAttribute %in% names_data)
  
  lbl <- tolower(meta_dbattributes$AgroLabelDbAttribute)
  
  out <- vector(mode = "list",length = as.integer(nrow(.data)))
  for( i in 1:nrow(.data)){
    out[[i]] <- as.data.frame(t(.data[i,]),stringsAsFactors=FALSE) 
    rownames(out[[i]]) <- paste(lbl,colnames(out[[i]]))
    out[[i]] <- tibble::rownames_to_column(out[[i]])
  }
  .data <- data.table::rbindlist(out,use.names = FALSE) %>% as.data.frame(stringsAsFactors=FALSE)
  names(.data) <- c("Parameter", "Value")
  .data
  
}


#' Get metadata from funding agency
#' 
#' @param studyId study or experiment ID
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @param meta_dbattributes data dictionary of metadata. It includes equivalences between excel and database names.
#' @description get metadata from funding agency
#' @export
#' 
get_fundagency_metadata <- function(studyId = NULL, format= NULL, 
                                    serverURL=NULL,  version = NULL,
                                    meta_dbattributes=NULL){
  
  fundagency_data <- ag_get_fundagency_studyId(studyDbId = studyId,format = "data.frame",
                                               serverURL = serverURL, version = version)
  cond <- has_agronomic_metadata(fundagency_data)
  if(cond){
    fundagency_data[,is.na(fundagency_data)] <- "" #replace all na with ""
    fundagency_data <- clean_fundagency(fundagency_data)
    fundagency_data <- convert_to_xlsx_fundagency(fundagency_data, meta_dbattributes)
  } else{
    msg <- cond
    fundagency_data <- data.frame()
  } 
}


