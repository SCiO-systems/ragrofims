#' Create Phenology measurement fieldbook
#' 
#' @description create soil field book(s) based on trait list tables
#' @param design design 
#' @param traitlist  traitlist
#' @export
#' 
cr_soil_fbook <- function(design, traitlist){
  
  # vars <- traitlist %>% 
  #                     filter(singularity=="soil") %>% 
  #                     select(variableName) %>% nth(1)
  soil_vars <- traitlist %>% dplyr::filter(singularity=="soil")
  vars <- add_season_numplot_prefix(soil_vars)
  design[, vars] <- NA #add variable for soil measurements #columns
  out <- design  #create design + variable data.frame
  return(out)
  
} 