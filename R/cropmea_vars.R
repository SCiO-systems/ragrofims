#' Crop measurement fieldbook
#' @param design design 
#' @param traitlist  traitlist
#' @export
#' 
cr_cmeasurement_fbook <- function(design, traitlist){
  
  
  traitlist <- traitlist %>% 
                 filter(singularity=="crop_measurement") %>% 
                 as.data.frame(stringsAsFactors=FALSE)  
  
  crop <- unique(traitlist$cropcommonname)
  out <- vector("list",length = length(crop))
  for(i in seq.int(crop)){
    vars <- traitlist %>%  filter(cropcommonname==crop[i]) %>% select(variableName) %>% nth(1)
    design[, vars] <- NA #add variable for crop measurements #columns
    out[[i]] <- design  #create design + variable data.frame
  }
  if(length(out)==1){
    out <- out[[1]] #in case of monocrop
  }
  out
  
} 

#' Get crops
#' @param traitlist data.frame trait list table
#' @export
#' 
get_crops <- function(traitlist){
  
  crops <- traitlist %>% 
                filter(singularity=="crop_measurement") %>% select(cropcommonname) %>% nth(1)
  crops <- unique(crops)
  return(crops)
}




