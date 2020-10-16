#' Replace attributes that has others values in project entity
#'
#' @param .data data to clean up \code{Other} values
#' @param attribute attribute of the database. Attribute name from AgroFIMS database where a user input is stored 
#' @param other_attribute Other attribute name related to \code{attribute} parameter used to store \code{Other} values or non-standardized inputs. 
#' @author Omar Benites
#' @description Tipically, users type values that are not mapped in the agronomy ontology. For this reason, the API response retrieve additional information
#' that should ensemble in a data structure.
#' @examples \dontrun{
#' .data <- ag_get_projlead_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_projlead(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' }
#' @export
#'     
.replace_other_attribute_projlead <- function(.data, attribute, other_attribute){
  
  #TODO: check number of rows , lanzar error si hay 0 filas 
  
  n <- as.integer(nrow(.data))
  for(i in seq.int(n)){
    
    #TODO: CHECK  .data[i, attribute] is different from NULL
    if(.data[i, attribute]=="Other"){ #Is equal to Other
      if(!is.na(.data[i,"projleadother"])){ #check if is NA
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
#' .data <- ag_get_projlead_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_projlead(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' .data <- clean_projlead(.data)
#' }
#' @export
#' 
clean_projlead <- function(.data){
  
  .data <- .replace_other_attribute_projlead(.data, "projleadorgid", "projleadother")
  
  n <- which(names(.data)=="createdat")
  .data <- .data[, -n]
  
  #2 casos: cgiar center y los demás
  pleadtype <- vector("character", as.integer(nrow(.data))) #proj entity type type
  pleadname <- vector("character", as.integer(nrow(.data))) #proj entity name
  pleadcrp <- vector("character", as.integer(nrow(.data))) #proj CGIAR progam name (in cgiar is selected)
  pleadperson <- vector("character", as.integer(nrow(.data)))
  
  for(i in 1:nrow(.data)){
    pleadtype[i] <- .data[,"projleadorgid"][i]
    
    if(.data[,"projleadorgid"][i]=="CGIAR center"){
      pleadname[i] <- .data[,"projleadcenterid"][i] #in case of having cgiar center 
      pleadcrp[i] <-  .data[,"projleadcrpid"][i]
      
    } else {
      #pleadname[i] <- .data[,"projleadname"][i] #other cases
      pleadcrp[i] <- ""
    }
    pleadperson[i] <- .data[,"projleadperson"][i]
  }
  .data <- data.frame(pleadtype, pleadname, pleadcrp, pleadperson, stringsAsFactors = FALSE)
  names(.data) <- c("projleadorgid", "projleadcenterid", "projleadcrpid","projleadperson")
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
#' .data <- ag_get_projlead_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_projlead(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' .data <- clean_projlead(.data)
#' .data <- convert_to_xlsx_projlead(.data)
#' }
#' @export
#' 
convert_to_xlsx_projlead <- function(.data, meta_dbattributes=NULL){
  
  
  out <- vector(mode = "list",length = as.integer(nrow(.data)))
  
  for(i in seq.int(as.integer(nrow(.data))) ){
    
    out[[i]] <- as.data.frame(t(.data[i,]),stringsAsFactors=FALSE) 
    
    if(.data[i,"projleadorgid"]=="CGIAR center"){
      
      #pl <- meta_dbattributes %>% dplyr::filter(module!="projlead", Dependency!="CGIAR")
      pl <- c("Experiment lead organization","Experiment lead center","Experiment lead contributor","Experiment lead person")
      rownames(out[[i]]) <-  paste(pl,i) # paste(pl$AgroLabelDbAttribute, i)
      index <- seq.int(1L,4L)
      
    } else {
      
      #pl <- meta_dbattributes %>% dplyr::filter(module=="projlead", Dependency!="CGIAR")
      pl <- c("Experiment lead organization","Experiment lead center","Experiment lead contributor", "Experiment lead person")
      rownames(out[[i]]) <-  paste(pl,i)
      index <- c(1L,4L)#seq.int(2L,3L)
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
#' @importFrom ragapi ag_get_projlead_studyId
#' @examples \dontrun{
#' b2 <- get_projlead_metadata(studyId  = 3,format = "data.frame",
#' serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",version ="/0233/r") 
#' }
#' @export
#'
get_projlead_metadata <- function(studyId = NULL, format= NULL, 
                                    serverURL=NULL,  version = NULL,
                                    meta_dbattributes=NULL){
  
  projlead_data <- ag_get_projlead_studyId(studyDbId = studyId, format = "data.frame",
                                               serverURL = serverURL, version = version)
  #check if there is agronomic data under certain conditions
  cond <- has_agronomic_metadata(projlead_data) 
  
  if(cond){
    projlead_data[,is.na(projlead_data)] <- ""
    projlead_data <- clean_projlead(projlead_data)
    projlead_data <- convert_to_xlsx_projlead(projlead_data, meta_dbattributes)
  } else{
    projlead_data <- data.frame()
  } 
  
}

##ADD CHECKS
