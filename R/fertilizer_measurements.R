#' Get fertilizer product data from AgroFIMS platform
#' 
#' @param expsiteId experiment-site Id or expsiteId
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @importFrom ragapi ag_get_cropmea_expsiteId ag_get_phenomea_expsiteId ag_get_soil_expsiteId  ag_get_soil_expsiteId
#' @export
#' 
get_agrofims_fertproducts <- function(expsiteId=NULL,
                                     format = "data.frame",
                                     serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                     version = "/0291/r"
                                    )
{
  
  fertproduct <- ag_get_fertmea_expsiteId(
                                  expsiteDbId = expsiteId,
                                   format = format,
                                   serverURL = serverURL,
                                   version = version)
  
  if(nrow(fertproduct)==0){
      out <- data.frame()
      return(out)
  }
  
  fertproduct <- fertproduct %>% dplyr::filter(typefertilizer=="Product")
  if(nrow(fertproduct)>0){
    
    #Calculation will ommit mising values.
    crop <- unique(fertproduct$cropcommonname)
    out <- vector("list",length = length(crop))
    vars <- vector("list",length = length(crop))
    #Separate and mutate elementlist columns in multiple nutrient columns
    fertproduct <- ragrofims::mutate_crop_names(fertproduct)
    fertproduct <- mutate_fertprod_nutelement(fertproduct)
    #Transform to numeric
    fertproduct[,"unitvalue"] <- as.numeric(fertproduct[,"unitvalue"])
    #fertilizer <- calc_nutamount(fertilizer)
    out <- fertproduct %>% dplyr::filter(unitvalue!="")
      
  } else {
    out <- data.frame()
  }
  
return(out)
  
} 

#' Mutate nutrient element list 
#' @description AgroFIMS retrieve in one column all the nutrient elements concatenated by pipes \code{|}. This function splits in multiple
#' columns where each columns is a nutrient element respectively.
#' @param fertilizer data.frame table of fertilizer
#' @importFrom purrr map_at
#' @importFrom tidyr separate
#' @export
#'

mutate_fertprod_nutelement <- function(fertilizer){
  
  fertilizer <- fertilizer %>% tidyr::separate(col = "elementlist",
                                               into = c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl"),
                                               sep = "\\|"
                                               )
  fertilizer <- fertilizer %>% 
                  purrr::map_at(.at = c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl"), .f = as.numeric) %>% 
                  as.data.frame(stringsAsFactors=FALSE)
  
}


#' Calculation of nutrient amounts based on fertilizer products ()
#' @param fertilizer data.frame fertilizer table which includes fertilizer products and nutrient elements
#' @importFrom dplyr mutate
#' @export 
#'
calc_nutamount <- function(fertilizer){

  if(nrow(fertilizer)>0){
    
    meta_attributes <- c("indexorder","productvalue", "unit")
    nut_names <- c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl")
    fertilizer <- fertilizer %>% mutate(N = unitvalue*N) %>% 
      mutate(P = unitvalue*P) %>%
      mutate(K = unitvalue*K) %>% 
      mutate(Ca = unitvalue*Ca) %>% 
      mutate(Mg = unitvalue*Mg) %>% 
      mutate(S = unitvalue*S) %>% 
      mutate(Mb = unitvalue*Mb) %>% 
      mutate(Zn = unitvalue*Zn) %>% 
      mutate(B = unitvalue*B) %>% 
      mutate(Cu = unitvalue*Cu) %>% 
      mutate(Fe = unitvalue*Fe) %>% 
      mutate(Mn = unitvalue*Mn) %>% 
      mutate(Ni = unitvalue*Ni)
    
    fertilizer <- fertilizer[,c(meta_attributes, nut_names)]

  } else {
    
    fertilizer <- data.frame()
  }
  
}

# test(names(calc_nutamount(out)), "calcnutamount")
# test(names(calc_nutamount(out)), c(12,23,NA,NA))
# test(names(calc_nutamount(out)), c(12,23,12,12))

#' Get fertilizer product table by crop
#' @param fertproducts data.frame fertilizer product table
#' @param crop character crop name
#' @export
#' 

get_fertproducts_crop <- function(fertproducts, crop){
  
  if(nrow(fertproducts)==0){
    
    out <- data.frame()
    
  } else if(nrow(fertproducts)>0) {
    
    fertproducts <- fertproducts %>% filter(cropcommonname == crop)
    
          #Check if the crop is in the table
          if(nrow(fertproducts)>0){
            out <- fertproducts
            
          }else { #do not find the crop in the table
            out <- data.frame()
            
          }
  }
  return(out)
}

