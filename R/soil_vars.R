#' Create Phenology measurement fieldbook
#' 
#' @description create soil field book(s) based on trait list tables
#' @param design design 
#' @param traitlist  traitlist
#' @export
#' 
cr_soil_fbook <- function(design, traitlist){
  
  vars <- traitlist %>% 
                      filter(singularity=="soil") %>% 
                      select(variableName) %>% nth(1)
  design[, vars] <- NA #add variable for soil measurements #columns
  out <- design  #create design + variable data.frame
  return(out)
  
} 