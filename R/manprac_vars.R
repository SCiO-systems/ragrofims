#' Create Management Pratices fieldbook
#' 
#' @description create a management practices field book based on trait list tables
#' @param design data.frame experimental design layout
#' @param traitlist data.frame trait list table which includes all management practices variables. Ex. units, evalutation per season/plot.
#' @importFrom dplyr filter select
#' @export
#' 
cr_mangprac_fbook <-function(design, traitlist){
  
  # manprac_vars <- traitlist %>% 
  #                 filter(singularity=="management_practices") %>% 
  #                 select(variableName) %>% nth(1)
  manprac_vars <- traitlist %>% 
                            dplyr::filter(singularity=="management_practices")
  vars <- add_season_numplot_prefix( manprac_vars)
  
  design[, vars] <- NA #add variable for soil measurements #columns
  #TODO: AGREGAR NUM PER SEASON AND PER PLOT 
  
  out <- design  #create design + variable data.frame
  return(out)
  
}
  
