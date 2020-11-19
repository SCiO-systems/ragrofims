#' Get fertilizer information about factor in experimental designs
#' 
#' @param expsiteId experiment-site Id or expsiteId
#' @param format character type of data structure. By default \code{data.frame}.
#' @param serverURL database server URL
#' @param version api version
#' @examples \dontrun{
#' library(ragapi)
#' library(ragrofims)
#' out<- get_agrofims_designprod(25, "data.frame")
#' }
#' @importFrom ragapi ag_get_cropmea_expsiteId ag_get_phenomea_expsiteId ag_get_soil_expsiteId  ag_get_soil_expsiteId
#' @export
#' 
get_agrofims_designprod <- function(expsiteId= NULL,
                                      format = "data.frame",
                                      serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                      version = "/0291/r"
)
{
  
  fertproduct <- ag_get_edsfert_expsiteId(
                    expsiteDbId = expsiteId,
                    format = format,
                    serverURL = serverURL,
                    version = version
                  )
  
  if(nrow(fertproduct)==0){
    out <- data.frame()
    return(out)
  }
  
  fertproduct <- fertproduct %>% dplyr::filter(typefertilizer=="Product")
  if(nrow(fertproduct)>0){
    
    #Separate and mutate elementlist columns in multiple nutrient columns
    #fertproduct <- ragrofims::mutate_crop_names(fertproduct)
    #fertproduct <- mutate_fertprod_nutelement(fertproduct)
    #Transform to numeric
    fertproduct <- mutate_nutrient_split(fertproduct)
    names(fertproduct) <- stringr::str_replace_all(string = names(fertproduct), pattern = "split_",replacement = "")
    fertproduct[,"unitvalue"] <- as.numeric(fertproduct[,"unitvalue"])
    out <- fertproduct
    #fertilizer <- calc_nutamount(fertilizer)
    #out <- fertproduct %>% dplyr::filter(unitvalue!="")
    
  } else {
    out <- data.frame()
  }
  
  return(out)
  
} 



#' Get nutrient information about factor in experimental designs
#' 
#' @param expsiteId experiment-site Id or expsiteId
#' @param format character type of data structure. By default \code{data.frame}.
#' @param serverURL database server URL
#' @param version api version
#' @examples \dontrun{
#' library(ragapi)
#' library(ragrofims)
#' out <- get_agrofims_designnut(25, "data.frame")
#' }
#' @importFrom ragapi ag_get_edsfert_expsiteId
#' @export
#' 
get_agrofims_designnut <- function(expsiteId= NULL,
                                    format = "data.frame",
                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                    version = "/0291/r"
)
{
  
  nutrient <- ragapi::ag_get_edsfert_expsiteId(
    expsiteDbId = expsiteId,
    format = format,
    serverURL = serverURL,
    version = version
  )
  
  if(nrow(nutrient)==0){
    out <- data.frame()
    return(out)
  }
  
  nutrient <- nutrient %>% dplyr::filter(typefertilizer=="Nutrient")
  if(nrow(nutrient)>0){
    
    #Calculation will ommit mising values.
    #Separate and mutate elementlist columns in multiple nutrient columns
    nutrient <- mutate_nutrient_split(nutrient)
    names(nutrient) <- stringr::str_replace_all(string = names(nutrient), pattern = "split_",replacement = "")
    nutrient[,"unitvalue"] <- as.numeric(nutrient[,"unitvalue"])
    out <- nutrient
    #fertilizer <- calc_nutamount(fertilizer)
    #out <- nutrient %>% dplyr::filter(unitvalue!="")
    
  } else {
    out <- data.frame()
  }
  
  return(out)
  
} 

#' Calculation of nutrient and fertilizer amount in experimental design factors
#' 
#' @param fertilizer data.frame fertilizer table
#' @param factorId integer factor database id , internally coded by \code{groupId} in AGROFIMS Database.
#' @examples \dontrun{
#' library(ragrofims)
#' library(ragapi)
#' fertilizer <-get_agrofims_designprod(25)
#' out <- calc_fert_design(fertilizer,2)
#' }
#' @export
#' 
calc_fert_design <- function(fertilizer, factorId = NULL){
  
  
  if(nrow(fertilizer)==0 || is.null(factorId)){
    return(data.frame())
  }
  
  factorId <- as.character(factorId)
  fertilizer <- fertilizer %>% dplyr::filter(group==factorId)
  
  if(nrow(fertilizer)>0){
    
    meta_attributes <- c("productvalue", "group", "fertilizerorder", "levelnamesplit", "unitvalue")
                         
    nut_names <- c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl")
    fertilizer <- fertilizer %>% mutate(N = (unitvalue*N)/100 ) %>% 
      mutate(P = (unitvalue*P)/100) %>%
      mutate(K = (unitvalue*K)/100) %>% 
      mutate(Ca = (unitvalue*Ca)/100) %>% 
      mutate(Mg = (unitvalue*Mg)/100) %>% 
      mutate(S = (unitvalue*S)/100) %>% 
      mutate(Mb = (unitvalue*Mb)/100) %>% 
      mutate(Zn = (unitvalue*Zn)/100) %>% 
      mutate(B = (unitvalue*B)/100) %>% 
      mutate(Cu = (unitvalue*Cu)/100) %>% 
      mutate(Fe = (unitvalue*Fe)/100) %>% 
      mutate(Mn = (unitvalue*Mn)/100) %>% 
      mutate(Ni = (unitvalue*Ni)/100)
    
    fertilizer <- fertilizer[,c(meta_attributes, nut_names)]
    #fertilizer <- fertilizer[, c("productvalue", "levelnamesplit", "unitvalue", "unit")]
    #names(fertilizer) <- 
    
  } else {
    fertilizer <- data.frame()
  }
} 

