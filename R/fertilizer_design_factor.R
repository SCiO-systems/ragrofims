#' Get fertilizer information about factors in experimental designs
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
    fertproduct[,"productvalue"] <- as.character(fertproduct[,"productvalue"])
    out <- fertproduct %>% dplyr::filter(productvalue!="")
    #fertilizer <- calc_nutamount(fertilizer)
    #out <- fertproduct %>% dplyr::filter(unitvalue!="")
    
  } else {
    out <- data.frame()
  }
  
  return(out)
  
} 



#' Get nutrient information about factors in experimental designs
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
  print("INSIDE NUTRIENT")
  nutrient <- ragapi::ag_get_edsfert_expsiteId(
    expsiteDbId = expsiteId,
    format = format,
    serverURL = serverURL,
    version = version
  )
  print("AFTER RAGAPI")
  print(nutrient)
  
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
    nutrient[,"productvalue"] <- as.character(nutrient[,"productvalue"])
    nutrient[,"unitvalue"] <- as.numeric(nutrient[,"unitvalue"])
    #nutrient <- nutrient %>% dplyr::filter(productvalue!="")
    out <- nutrient

    #fertilizer <- calc_nutamount(fertilizer)
    #out <- nutrient %>% dplyr::filter(unitvalue!="")
    
  } else {
    out <- data.frame()
  }
  
  return(out)
  
} 

#' Calculation of nutrient amount in experimental design factors
#' 
#' @param fertilizer data.frame fertilizer table
#' @param factorId integer factor database id , internally coded by \code{groupId} in AGROFIMS Database.
#' @importFrom dplyr filter left_join mutate
#' @examples \dontrun{
#' library(ragrofims)
#' library(ragapi)
#'  factors_data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId = expsiteId,
#'                    format = "data.frame",
#'                    serverURL = serverURL, version = version
#'                    )
#' fertilizer <-get_agrofims_designprod(25)
#' out <- calc_fert_design(fertilizer, factors_data, 4)
#' }
#' @export
#' 
calc_nut_design <- function(fertilizer, factorId = NULL){
  
  
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
      mutate(S = (unitvalue*S)/100)   %>% 
      mutate(Mb = (unitvalue*Mb)/100) %>% 
      mutate(Zn = (unitvalue*Zn)/100) %>% 
      mutate(B = (unitvalue*B)/100)   %>% 
      mutate(Cu = (unitvalue*Cu)/100) %>% 
      mutate(Fe = (unitvalue*Fe)/100) %>% 
      mutate(Mn = (unitvalue*Mn)/100) %>% 
      mutate(Ni = (unitvalue*Ni)/100)
    
    fertilizer <- fertilizer[,c(meta_attributes, nut_names)]
    
  } else {
    fertilizer <- data.frame()
  }
} 


#' Calculation of fertilizer amount in experimental design factors
#' 
#' @param fertilizer data.frame fertilizer table
#' @param fert_factors fertilizer products in factors
#' @param factorId integer factor database id , internally coded by \code{groupId} in AGROFIMS Database.
#' @examples \dontrun{
#' library(ragrofims)
#' library(ragapi)
#' fertilizer <-get_agrofims_designprod(25)
#' out <- calc_fert_design(fertilizer,3)
#' }
#' @export
#' 
calc_fertprod_design <- function(fertilizer, fert_factors=NULL, factorId = NULL){
  
  
  if(nrow(fertilizer)==0 || is.null(factorId) ){
    return(data.frame())
  }
  
  if(nrow(fert_factors)==0){
    return(data.frame())
  }
  
  factorId <- as.character(factorId)
  fertilizer <- fertilizer %>% dplyr::filter(group==factorId)
  if(nrow(fertilizer)>0){
   
  fertilizer <- dplyr::left_join(fert_factors, fertilizer, by= "group")
        
    meta_attributes <- c("productvalue", "factortype", "factorunit", "unitvalue")
    nut_names <- c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl")
    fertilizer <- fertilizer %>% 
      mutate(N =  calc_prodnut_split(unitvalue, N)) %>% 
      mutate(P = calc_prodnut_split(unitvalue, P)) %>% 
      mutate(K = calc_prodnut_split(unitvalue, K)) %>% 
      mutate(Ca = calc_prodnut_split(unitvalue, Ca)) %>% 
      mutate(Mg = calc_prodnut_split(unitvalue, Mg)) %>% 
      mutate(S = calc_prodnut_split(unitvalue, S)) %>% 
      mutate(Mb = calc_prodnut_split(unitvalue, Mb)) %>% 
      mutate(Zn = calc_prodnut_split(unitvalue, Zn)) %>% 
      mutate(B = calc_prodnut_split(unitvalue, B)) %>% 
      mutate(Cu = calc_prodnut_split(unitvalue, Cu)) %>% 
      mutate(Fe = calc_prodnut_split(unitvalue, Fe)) %>% 
      mutate(Mn = calc_prodnut_split(unitvalue, Mn)) %>% 
      mutate(Ni = calc_prodnut_split(unitvalue, Ni))
    
    fertilizer <- fertilizer[,c(meta_attributes, nut_names)]
    
    #fertilizer <- fertilizer[, c("productvalue", "levelnamesplit", "unitvalue", "unit")]
    #names(fertilizer) <- 
    
  } else {
    fertilizer <- data.frame()
  }
} 


#' Filter factors by group
#' @description Every factor in the AgroFIMS interface has an numeric id that indicates a order or position. This id or position is used to filter and select certain factor that has additional information. 
#' Example: Fertilizer type and amount, or, nutrient content type and amount.
#' @param .data data about factor in experimental designs
#' @param factorId character factorId or group number 
#' @export 
#' 
filter_factors_factorId <- function(.data, factorId=NULL){
  
  if(nrow(.data)==0 || is.null(factorId)){
    return(data.frame())
  }
  factorId <- as.character(factorId)
  .data <- .data %>% dplyr::filter(group==factorId)     
  
  if(nrow(.data)>0){
    .data <- tibble::rowid_to_column(.data)
    .data <- .data %>% dplyr::select(rowid,group, factortype, factorunit, numberofsplits)
    return(.data)          
  } else {
    return(data.frame())  
  } 
}
