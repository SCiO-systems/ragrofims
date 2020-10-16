#' Check factor names
#' @importFrom checkmate allMissing anyMissing
#' @export
#' 
ck_factor_names <- function(.data){
  
  #check 1
  cond1 <- checkmate::allMissing(.data[,"factorname"])
  cond2 <- checkmate::anyMissing(.data[,"factorname"])
   
  if(cond1 & cond2){
    out <- "There are missing factors in your experiment. Check the design tab."
    
  } else {
    out <- TRUE
  } 
  out
}



#' Check missing values in levels
#' @importFrom checkmate allMissing anyMissing
#' @export
#' 
ck_level_values <- function(.data){
  
  #check 1
  cond1 <- checkmate::allMissing(.data[,"levelname"])
  cond2 <- checkmate::anyMissing(.data[,"levelname"])
  
  if(cond1 & cond2){
    out <- "There are missing levels in your experimetal design. Check the design tab."
    
  } else {
    out <- TRUE
  } 
  out
}