#' Create Phenology measurement fieldbook
#' 
#' @description create phenology book(s) based on trait list tables
#' @param design design 
#' @param traitlist  traitlist
#' @export
#' 
cr_phenology_fbook <- function(design, traitlist){
  
  
  traitlist <- traitlist %>% 
    filter(singularity=="crop_phenology") %>% 
                        as.data.frame(stringsAsFactors=FALSE)  
  
  temp_design <- design
  crop <- unique(traitlist$cropcommonname)
  out <- vector("list",length = length(crop))
  vars <- vector("list",length = length(crop))
  
  for(i in seq.int(crop)){
    
    #vars <- traitlist %>%  filter(cropcommonname==crop[i]) %>% select(variableName) %>% nth(1)
    #if(length(vars)>0){
      #design[, vars] <- NA #add variable for crop measurements #columns
      #out[[i]] <- design  #create design + variable data.frame  
    
    vars[[i]] <- traitlist %>%  filter(cropcommonname==crop[i]) %>% select(variableName) %>% nth(1)
    design[, vars[[i]] ] <- NA #add variable for crop measurements #columns
    out[[i]] <- design  #create design + variable data.frame
    design <- temp_design
    # names(out[i]) <- crop[i]
    #} else {
    #  out[[i]] <- design
    
    #}
   
  }
  if(length(out)==1){
    out <- out[[1]] #in case of monocrop
  }
 
  out
  
} 
