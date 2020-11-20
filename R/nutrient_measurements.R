#' Get fertilizer product data from AgroFIMS platform
#' 
#' @param expsiteId experiment-site Id or expsiteId
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @importFrom ragapi ag_get_cropmea_expsiteId ag_get_phenomea_expsiteId ag_get_soil_expsiteId  ag_get_soil_expsiteId
#' @export
#' 
get_agrofims_nutrients <- function(expsiteId=NULL,
                                      format = "data.frame",
                                      serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                      version = "/0291/r"
)
{
  
  nutrient <- ag_get_fertmea_expsiteId(
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
    crop <- unique(nutrient$cropcommonname)
    out <- vector("list",length = length(crop))
    vars <- vector("list",length = length(crop))
    #Separate and mutate elementlist columns in multiple nutrient columns
    nutrient <- ragrofims::mutate_crop_names(nutrient)
    nutrient <- mutate_fertprod_nutelement(nutrient)
    nutrient <- mutate_nutrient_split(nutrient)
    #Transform to numeric
    #fertilizer <- calc_nutamount(fertilizer)
    out <- nutrient %>% dplyr::filter(productvalue!="")
    
  } else {
    out <- data.frame()
  }
  
  return(out)
  
} 


#' Mutate nutrient split element list 
#' @description AgroFIMS retrieve in one column all the nutrient split elements concatenated by pipes \code{|}. This function splits in multiple
#' columns where each columns is a nutrient split element respectively.
#' @param nutrient data.frame table of nutrient
#' @importFrom purrr map_at
#' @importFrom tidyr separate
#' @export
#'

mutate_nutrient_split <- function(nutrient){
  
  nutrient <- nutrient %>% tidyr::separate(col = "elementlistgroup",
                                               into = paste0("split_", c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl")),
                                               sep = "\\|"
                                               )
  nutrient <- nutrient %>% 
    purrr::map_at(.at = paste0("split_",c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl")), .f = as.numeric) %>% 
    as.data.frame(stringsAsFactors=FALSE)
  
}




#' Calculation of product amounts based on nutrient concentration
#' @param nutrient data.frame nutrient table which includes fertilizer products and nutrient split elements
#' @importFrom dplyr mutate
#' @export 
#'
calc_prodamount <- function(nutrient){
  
  if(nrow(nutrient)>0){
    
    meta_attributes <- c("indexorder","productvalue", "unit")
    nut_names <- c("N","P", "K","Ca","Mg","S","Mb", "Zn", "B", "Cu", "Fe", "Mn" ,"Ni","Cl")
    nutrient <- nutrient %>% mutate(N =  calc_prodnut_split(split_N,N)) %>% 
      mutate(P = calc_prodnut_split(split_P,P)) %>% 
      mutate(K = calc_prodnut_split(split_K, K)) %>% 
      mutate(Ca = calc_prodnut_split(split_Ca,Ca)) %>% 
      mutate(Mg = calc_prodnut_split(split_Mg,Mg)) %>% 
      mutate(S = calc_prodnut_split(split_S,S)) %>% 
      mutate(Mb = calc_prodnut_split(split_Mb,Mb)) %>% 
      mutate(Zn = calc_prodnut_split(split_Zn,Zn)) %>% 
      mutate(B = calc_prodnut_split(split_B,B)) %>% 
      mutate(Cu = calc_prodnut_split(split_Cu,Cu)) %>% 
      mutate(Fe = calc_prodnut_split(split_Fe,Fe)) %>% 
      mutate(Mn = calc_prodnut_split(split_Mn,Mn)) %>% 
      mutate(Ni = calc_prodnut_split(split_Ni,Ni))
    
    nutrient <- nutrient[,c(meta_attributes, nut_names)]
    
  } else {
    
    nutrient <- data.frame()
  }
  
}

#' Calculation of nutrient concentration per split application
#' 
#' @param nut_split numeric nutrient element per split
#' @param prodnut numeric nutrient element in product
#' @description calculae nutrient concentration in fertilizer products
#' @export
#' 
calc_prodnut_split <- function(nut_split, prodnut){
    
    nut_split <- as.numeric(nut_split) 
    prodnut <- as.numeric(prodnut)
    
   #if( length(nut_split) >0 && lenght(prodnut>0 ){
     out <- (nut_split/prodnut)*100
   #}  else{
   #out <- 0
   #}
   out
  
}
