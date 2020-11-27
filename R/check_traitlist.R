#' Check duplicates on trait list tables
#'
#' @description check whether exist duplicates variables or measurements in the trait list table
#' @param traitlist data.frame trait list table
#' @importFrom dplyr select
#' @export
#' 
check_duplicates_traitlist <- function(traitlist){
  
 traitlist <- traitlist %>% dplyr::select(cropcommonname, measurement, parametermeasured)
 n_dup <- nrow(traitlist[duplicated.data.frame(traitlist),])
 if(n_dup>0){
   return(TRUE)
 } else {
   return(FALSE)
 }
}
  
  
#' Remove duplicates in trait list tables
#' @description remove duplicates variables or measurements in the trait list table. This trait list is retrieve from AGROFIMS database via AgrAPI.
#' @param traitlist data.frame trait list table
#' @export
#' 
remove_duplicates_traitlist <- function(traitlist){
  traitlist <- traitlist %>% dplyr::distinct(cropcommonname, measurement, parametermeasured, .keep_all= TRUE)
}
