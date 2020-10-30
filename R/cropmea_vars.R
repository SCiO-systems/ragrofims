#' Crop measurement fieldbook
#' @param design design 
#' @param traitlist  traitlist
#' @export
#' 
cr_cmeasurement_fbook <- function(design, traitlist){
  
  
  traitlist <- traitlist %>% 
                 filter(singularity=="crop_measurement") %>% 
                 as.data.frame(stringsAsFactors=FALSE)  
  temp_design <- design
  crop <- unique(traitlist$cropcommonname)
  out <- vector("list",length = length(crop))
  vars <- vector("list",length = length(crop))
  
  for(i in seq.int(crop)){
    vars[[i]] <- traitlist %>%  filter(cropcommonname==crop[i]) %>% select(variableName) %>% nth(1)
    design[, vars[[i]] ] <- NA #add variable for crop measurements #columns
    out[[i]] <- design  #create design + variable data.frame
    design <- temp_design
  }
  if(length(out)==1){
    out <- out[[1]] #in case of monocrop
  }
  out
  
} 

#' Get crops
#' @param traitlist data.frame trait list table
#' @param singularity chr Types of variables. Ex. \code{crop_measurement}, \code{crop_phenology}, \code{management_practices} and \code{weather} 
#' @export
#' 
get_crops <- function(traitlist, singularity="crop_measurement"){
  
  if(singularity=="crop_measurement"){
    crops <- traitlist %>% 
      filter(singularity=="crop_measurement") %>% select(cropcommonname) %>% nth(1)
  } else if(singularity=="crop_phenology"){
    crops <- traitlist %>% 
      filter(singularity=="crop_phenology") %>% select(cropcommonname) %>% nth(1)
  } else if(singularity=="management_practices"){
    crops <- traitlist %>% 
      filter(singularity=="management_practices") %>% select(cropcommonname) %>% nth(1)
  }   
  crops <- unique(crops)
  return(crops)

}




