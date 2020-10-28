#' Replace attributes that has others values in project entity
#'
#' @param .data data to clean up \code{Other} values
#' @param attribute attribute of the database. Attribute name from AgroFIMS database where a user input is stored 
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @description Tipically, users type values that are not mapped in the agronomy ontology. For this reason, the API response retrieve additional information
#' that should ensemble in a data structure.
#' @examples \dontrun{
#' .data <- ag_get_projentity_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_projentity(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' }
#' @export
#'     
.replace_other_attribute_projentity <- function(.data, attribute, other_attribute){
  
  #TODO: check number of rows , lanzar error si hay 0 filas 
  
  n <- as.integer(nrow(.data))
  for(i in seq.int(n)){
    
    #TODO: CHECK  .data[i, attribute] is different from NULL
    if(.data[i, attribute]=="Other"){ #Is equal to Other
      if(!is.na(.data[i,"projentityother"])){ #check if is NA
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
#' .data <- ag_get_projentity_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_projentity(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' .data <- clean_projentity(.data)
#' }
#' @export
#' 
clean_projentity <- function(.data){
  
  .data <- .replace_other_attribute_projentity(.data, "projentityorgid", "projentityother")
  
  #2 casos: cgiar center y los demás
  projentitype <- vector("character", as.integer(nrow(.data))) #proj entity type type
  projename <- vector("character", as.integer(nrow(.data))) #proj entity name
  projcrp <- vector("character", as.integer(nrow(.data))) #proj CGIAR progam name (in cgiar is selected)
  
  for(i in 1:nrow(.data)){
    projentitype[i] <- .data[,"projentityorgid"][i]
    if(.data[,"projentityorgid"][i]=="CGIAR center"){
      projename[i] <- .data[,"projentitycenterid"][i] #in case of having cgiar center 
      projcrp[i] <-  .data[,"projentitycrpid"][i]
    
    } else {
      projename[i] <- .data[,"projentityname"][i] #other cases
      projcrp[i] <- ""
    }
  }
  .data <- data.frame(projentitype, projename, projcrp, stringsAsFactors = FALSE)
  names(.data) <- c("projentityorgid", "projentityname", "projentitycrpid")
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
#' .data <- ag_get_projentity_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_projentity(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' .data <- clean_projentity(.data)
#' .data <- convert_to_xlsx_projentity(.data)
#' }
#' @export
#' 
convert_to_xlsx_projentity <- function(.data, meta_dbattributes=NULL){
  
  
  out <- vector(mode = "list",length = as.integer(nrow(.data)))
  
  for(i in 1:nrow(.data)){
     
    out[[i]] <- as.data.frame(t(.data[i,]),stringsAsFactors=FALSE) 

     if(.data[i,"projentityorgid"]=="CGIAR center"){
       
       #pl <- meta_dbattributes %>% dplyr::filter(module!="projentity", Dependency!="CGIAR")
       pl <- c("Project management entity","Project management center","Project management contributor")
       rownames(out[[i]]) <-  paste(pl,i) # paste(pl$AgroLabelDbAttribute, i)
       index <- seq.int(1L,3L)
       
     } else {
       
       #pl <- meta_dbattributes %>% dplyr::filter(module=="projentity", Dependency!="CGIAR")
       pl <- c("Project management entity","Project management entity name","Project management contributor")
       rownames(out[[i]]) <-  paste(pl,i)
       index <- seq.int(1L,2L)
     }
    
    out[[i]] <- tibble::rownames_to_column(out[[i]])
    out[[i]] <- out[[i]][index,]
    
  }
  
  .data <- data.table::rbindlist(out, use.names = FALSE) %>% as.data.frame(stringsAsFactors=FALSE)
  names(.data) <- c("Parameter", "Value")
  .data
  
}



#' Get metadata from project entity agency
#' 
#' @param studyId study or experiment ID
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @param meta_dbattributes data dictionary of metadata. It includes equivalences between excel and database names.
#' @description get metadata from project entity agency
#' @importFrom ragapi ag_get_projentity_studyId
#' @export
#'
get_projentity_metadata <- function(studyId = NULL, format= NULL, 
                                    serverURL=NULL,  version = NULL,
                                    meta_dbattributes=NULL){
  
  projentity_data <- ag_get_projentity_studyId(studyDbId = studyId, format = "data.frame",
                                           serverURL = serverURL, version = version)
  #check if there is agronomic data under certain conditions
  cond <- has_agronomic_metadata(projentity_data) 
  
  if(cond){
    #projentity_data[,is.na(projentity_data)] <- ""
    projlead_data <- projlead_data %>% replace(is.na(.), "")
    projentity_data <- clean_projentity(projentity_data)
    projentity_data <- convert_to_xlsx_projentity(projentity_data, meta_dbattributes)
  } else{
    projentity_data <- data.frame()
  } 
  
}


