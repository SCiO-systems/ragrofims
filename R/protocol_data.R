#' Get protocol data from experimental data
#' 
#' @param expsiteId experiment-site Id or expsiteId
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @importFrom ragapi ag_get_cropmea_expsiteId ag_get_phenomea_expsiteId ag_get_soil_expsiteId  ag_get_soil_expsiteId
#' @export
#'

get_manprac_protocol <- function(expsiteId=NULL,
                                 format = "data.frame",
                                 serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                 version = "/0253/r"
                                ){
  
  protocol <- ragapi::ag_get_manprac_expsiteId(expsiteDbId = expsiteId,
                                           format = format,
                                           serverURL = serverURL,
                                           version =version)
  
  if(nrow(protocol)>0){
    #add values in variableName
    protocol <- mutate_variable_name(protocol)
    #count number of evaluations
    count_index <- protocol  %>% 
                    dplyr::count(cropcommonname, indexorder, variableName) %>% 
                    nth(4)
    #number of evaluation
    neval <- seq_protocol(count_index)
    protocol <-  protocol %>% mutate(variableName = paste0(variableName,"__",neval))
    protocol <-  protocol %>% dplyr::filter(!is.na(value))#filter empty values
    protocol$samplesperseason <- "1"
    protocol$samplesperplot <- "1"
    protocol$AgroFIMSId <- 1:nrow(protocol)
    protocol$VariableId <- 1:nrow(protocol)
    protocol <- protocol
  } else {
    protocol <- data.frame()
  }
  protocol
  
}

#' Vectorized creation of sequence of number of evaluation based on initial values.
#' @param .x 
#' @export
#' 
seq_protocol <- function(.x){
  out <- unlist(lapply(.x, function(.x)seq.int(.x)))
}

