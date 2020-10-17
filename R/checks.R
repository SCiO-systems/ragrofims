#' Internal function to detect if data 
#' @param .data data retrieved from AgAPI
#' @description check anomalies in agronomic data after retrieving information from AGROFIMS database
#' @details Minimal conditions are 1 row, 2 columns, not all missing data
#' @importFrom checkmate checkDataFrame allMissing
#' @export
#'     
has_agronomic_metadata <- function(.data){
  
  n <- ncol(.data)
  .data <- .data[,-n]
  
  cond1 <- checkDataFrame( 
    x = .data,
    #types = character(0L),
    min.rows = 1L, #1 row at minimun
    min.cols = 2L  #2 cols at minimun
  )
  cond1 <- isTRUE(cond1)
  #if(isFALSE(cond1)){ return (FALSE)}
  #second condition: #check if all is complete
  cond2 <-  !all(sapply(.data, allMissing)) #not all is missing
  #cond3 <- !allDoubleQuotes(.data[,1]) #check if all are double quotes
  return(isTRUE(cond1 && cond2))
  
}

allDoubleQuotes <-  function(.x){
  if(length(.x)>0){
    identical(.x, "")   
  } else {
    FALSE
  }
}

any_double_quotes <- function(.x){
  
  if(length(.x)>0){
    return(any(stringr::str_detect(.x, "^$")==TRUE,na.rm = TRUE))     
  } else {
    return(FALSE)
  }
  
}

replaceNaCharacter <- function(.data){
  .data[is.na(.data)] <- ""
}

#' Detect if levels are missing
#' @param .data 
is_levels_complete <- function(.data){
  #If TRUE, all levels are complete, otherwise some are missing
  sum(match(x = .data[,"levelname"],""),na.rm = TRUE)==0 
}



