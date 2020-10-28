#' Replace attributes that has others values in personnel metadata
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
.replace_other_attribute_personnel <- function(.data, attribute, other_attribute){
  
  #TODO: check number of rows , lanzar error si hay 0 filas 
  
  n <- as.integer(nrow(.data))
  for(i in seq.int(n)){
    
    #TODO: CHECK  .data[i, attribute] is different from NULL
    
    if(!is.na(.data[i, attribute]))
    {
        if(.data[i, attribute]=="Other"){ #Is equal to Other
      
          if(!is.na(.data[i,other_attribute])){ #check if is NA
            .data[i, attribute] <- .data[i, other_attribute]  
          
          } 
          else { #put 
            
            .data[i, attribute] <- ""
          
          } #end if
          
        }#end if
    } #end if
  }#end for loop
  
  .data
}


#' Clean labels in order to export in MS Excel format
#' @param .data data.frame table with 
#' @description Functionality to clean and poolish the AGROFIMS API response.
#' @author Omar Benites
#' @description clean api response attributes by AgroFIMS label in order to export the fieldbook file
#' @examples \dontrun{
#' .data <- ag_get_personnel_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_personnel(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' .data <- clean_personnel(.data)
#' }
#' @export
#' 
clean_personnel <- function(.data){
  
  .data <- .replace_other_attribute_personnel(.data, "persontypeid", "persontypeother")
  .data <- .replace_other_attribute_personnel(.data, "personaffiliationid", "personaffiliationnameother")
  
  .data <- .data[, -which(names(.data)=="createdat")]
  .data <- .data[,-c(which(names(.data)=="persontypeother"), which(names(.data)=="personaffiliationnameother"))]
  
  #2 casos: cgiar center y los demás
  persontypeid <- vector("character", as.integer(nrow(.data))) #proj entity type type
  personfirstname <- vector("character", as.integer(nrow(.data))) #proj entity name
  personlastname <- vector("character", as.integer(nrow(.data))) #proj CGIAR progam name (in cgiar is selected)
  personemailaddress <- vector("character", as.integer(nrow(.data)))
  
  personaffiliationid <- vector("character", as.integer(nrow(.data)))
  personaffiliationname <-  vector("character", as.integer(nrow(.data)))
  personaffiliationcenterid <-  vector("character", as.integer(nrow(.data)))
  personorcid <- vector("character", as.integer(nrow(.data)))
  
  print(names(.data))
  
  for(i in 1:nrow(.data)){
    persontypeid[i] <- .data[,"persontypeid"][i]
    personfirstname[i] <- .data[,"personfirstname"][i]
    personlastname[i] <- .data[,"personlastname"][i]
    personemailaddress[i] <- .data[,"personemailaddress"][i]
    personorcid[i] <- .data[,"personorcid"][i]
    personaffiliationid[i] <- .data[,"personaffiliationid"][i]
    
    if(!is.na(.data[,"personaffiliationid"][i])){
        
          if(.data[,"personaffiliationid"][i]=="CGIAR center"){
            
            personaffiliationname[i] <-  "" #.data[,"personaffiliationname"][i]
            personaffiliationcenterid[i] <-  .data[,"personaffiliationcenterid"][i]
            
          } else {
            
            personaffiliationname[i] <- .data[,"personaffiliationname"][i]
            personaffiliationcenterid[i] <- ""#.data[,"personaffiliationcenterid"][i]
            
          }
      } #end personaffiliationid          
        
    }
    
  .data <- data.frame(persontypeid, personfirstname, personlastname, personemailaddress, personaffiliationid,
                      personaffiliationname, personaffiliationcenterid, personorcid, stringsAsFactors = FALSE)
  names(.data) <- c("persontypeid" ,"personfirstname", "personlastname", "personemailaddress",
                    "personaffiliationid", "personaffiliationname","personaffiliationcenterid","personorcid" )
  
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
#' .data <- ag_get_personnel_studyId(studyDbId = 28,format = "data.frame",
#'                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                    version ="/0212/r")
#' .data <- .replace_other_attribute_personnel(.data, "fundAgencyTypeId", "fundAgencyTypeOther")
#' .data <- clean_personnel(.data)
#' .data <- convert_to_xlsx_personnel(.data)
#' }
#' @export
#' 
convert_to_xlsx_personnel <- function(.data, meta_dbattributes=NULL){
  
  #.data[,is.na(.data)] <- "" #replace all na with ""
  .data <- .data %>% replace(is.na(.), "")
  out <- vector(mode = "list",length = as.integer(nrow(.data)))
  
  for(i in seq.int(as.integer(nrow(.data))) ){
    
    out[[i]] <- as.data.frame(t(.data[i,]),stringsAsFactors=FALSE) 
    
        if(.data[i,"personaffiliationid"]=="CGIAR center"){
          
          pl <- c("Person type",	"Person, first name", "Person, last name"	,"Person email",	
                  "Person, affiliation",	"Person, affiliation name",	"Organization name",	"Person, ORCID") 
          rownames(out[[i]]) <-  paste(pl,i) # paste(pl$AgroLabelDbAttribute, i)
          index <- 6L #exclude this column of non-cgiar centers
          
        } else {
          
          pl <- c("Person type",	"Person, first name", "Person, last name"	,"Person email",	
                  "Person, affiliation",	"Person, affiliation name",	"Organization name",	"Person, ORCID") 
          
          rownames(out[[i]]) <-  paste(pl,i)
          index <- 7L   #exclude this column when users choose cgiar centers
      }
      out[[i]] <- tibble::rownames_to_column(out[[i]])
      out[[i]] <- out[[i]][-index,] #all the columns except the index values
   
    
    
    
    
    
  }
  
  .data <- data.table::rbindlist(out, use.names = FALSE) %>% as.data.frame(stringsAsFactors=FALSE)
  names(.data) <- c("Parameter", "Value")
  .data
  
}



#' Get metadata from personnel metadata agency
#' 
#' @param studyId study or experiment ID
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @param meta_dbattributes data dictionary of metadata. It includes equivalences between excel and database names.
#' @description get metadata from personnel metadata
#' @importFrom ragapi ag_get_personnel_studyId
#' @examples \dontrun{
#' b2 <- get_personnel_metadata(studyId  = 3,format = "data.frame",
#' serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",version ="/0233/r") 
#' }
#' @export
#'
get_personnel_metadata <- function(studyId = NULL, format= NULL, 
                                  serverURL=NULL,  version = NULL,
                                  meta_dbattributes=NULL){
  
  personnel_data <- ag_get_personnel_studyId(studyDbId = studyId, format = "data.frame",
                                           serverURL = serverURL, version = version)
  #check if there is agronomic data under certain conditions
  cond <- has_agronomic_metadata(personnel_data) 
  
  if(cond){
    personnel_data <- clean_personnel(personnel_data)
    personnel_data <- convert_to_xlsx_personnel(personnel_data, meta_dbattributes)
  } else{
    personnel_data <- data.frame()
  } 
  
}

##ADD CHECKS
