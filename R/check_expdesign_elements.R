#' Check factor names
#' @param .data data.frame experimental data which includes factors and levels retrieved from AGROFIMS database via API.
#' @importFrom checkmate allMissing anyMissing
#' @export
#' 
ck_factor_names <- function(.data){
  
  #check 1
  cond1 <- checkmate::allMissing(.data[,"factorname"])
  cond2 <- checkmate::anyMissing(.data[,"factorname"])
   
  if(cond1 || cond2){
    out <- "There are missing factors in your experiment. Check the design tab."
    
  } else {
    out <- TRUE
  } 
  out
}



#' Check missing values in levels
#' 
#' @param .data data.frame experimental data which includes factors and levels retrieved from AGROFIMS database via API.
#' @importFrom checkmate allMissing anyMissing
#' @export
#' 
ck_level_values <- function(.data){
  
  #check 1
  cond1 <- checkmate::allMissing(.data[,"levelname"])
  cond2 <- checkmate::anyMissing(.data[,"levelname"])
  cond3 <- any_double_quotes(.data[,"levelname"])
  
  if((cond1 || cond2) || cond3){
    out <- "There are missing levels in your experimetal design. Check the design tab."
    
  } else {
    out <- TRUE
  } 
  out
}