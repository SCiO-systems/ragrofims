#' Mutate multiple timing values for trait list table
#' 
#' @description AgroFIMS timing values are divided in 5 attributes or columns: Days after planting, Growth stage, Frequency,
#' Date and Other. Therefore, this function assign in the column \code{meaTimeValue} all the different values according to each case or attribute.
#' @importFrom dplyr mutate as_tibble case_when
#' @export

mutate_timming_values <- function(traitlist){
  
  traitlist <- traitlist %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(meaTimeValue="") %>% 
    dplyr::mutate(meaTimeValue= dplyr::case_when(
                                        timing =="Days after planting" ~ as.character(timingdaysafterplanting),
                                        timing =="Growth stage" ~ as.character(timinggrowthstage),
                                        timing =="Frequency" ~ as.character(timingfrequency),
                                        timing =="Date" ~ as.character(timingdate),
                                        timing =="Other" ~ as.character(timingother),
                                        TRUE ~ NA_character_)
                  )
}

#test 1: to_timming_kdsmart(ncol(2),2)
#test 1: to_timming_kdsmart(ncol(2),2)

#' Mutate multiple timing values for trait list table
#' 
#' @description AgroFIMS timing values are divided in 5 attributes or columns: Days after planting, Growth stage, Frequency,
#' Date and Other. Therefore, this function assign in the column \code{meaTimeValue} all the different values according to each case or attribute.
#' @importFrom dplyr mutate as_tibble case_when select
#' @export

mutate_variable_name <- function(traitlist){
  
  #Variable Name
  traitlist <- traitlist%>% 
    as_tibble() %>% #as tibble data structure
    mutate(variableName2="") %>% #all as character
    mutate(variableName2= case_when(
      (singularity=="crop_measurement" & !is.na(unit)) ~ paste0(str_replace_all(measurement,"[:space:]","_"),"_",unit),
      (singularity=="crop_measurement" & is.na(unit)) ~ paste0(str_replace_all(measurement,"[:space:]","_")),
      (singularity!="crop_measurement" & !is.na(unit)) ~ paste0(variableName,"_",unit),
      (singularity!="crop_measurement" & is.na(unit)) ~ paste0(variableName)#,
      #TRUE ~ str_replace_all((measurement),"[:space:]","_")
    )#end case when
    )#end mutate  
  traitlist <- traitlist %>% mutate(variableName=variableName2) %>% select(-variableName2)
  
} 

#' Mutate variable validation attribute for KDSmart mobile application
#' @description Mutate and assign in variableValidation attribute all the kdsmart validations according to the type of variable (DECIMAL, INTEGER, NUMERIC,DATE,TEXT)
#' @export
#' 
mutate_variable_validation_kdsmart <- function(traitlist){
  
  traitlist <- traitlist%>% 
        as_tibble() %>% #as tibble data structure
        mutate(variableValidation="") %>% #all as character
        mutate(variableValidation=case_when(
                variableDataType=="DECIMAL" ~ paste0(variableLowerLimit ," <= x <= ", variableUpperLimit),
                variableDataType=="INTEGER" ~  paste0(variableLowerLimit ," <= x <= ", variableUpperLimit),
                variableDataType=="CATEGORICAL" ~ as.character(variableCategory),
                variableDataType=="TEXT" ~ "TEXT",
                variableDataType=="DATE" ~ "DATE"
                                            )#END CASE_WHEN
        )#END MUTATE
}

######3
#' Mutate number of measurement per season and per plot 
#' @param traitlist trait list table
#' @description set samplesperseason and samplesperplot to 1 evaluations by default
#' @export
#' 
mutate_nummeasurement_phenology <- function(traitlist){
  
  traitlist <- traitlist %>% 
      as_tibble() %>% #as tibble data structure
      #mutate(samplesperseason="") %>% #all as character
      mutate(samplesperseason = case_when(
                                        singularity=="crop_phenology" ~  "1",
                                        TRUE ~ samplesperseason
                                        )
      ) %>%   
      #mutate(samplesperplot="") %>%
      mutate(samplesperplot = case_when(
                                        singularity=="crop_phenology" ~  "1",
                                        TRUE ~ samplesperplot
                                        )
      )
  traitlist
} 


#####

#' Convert from AgroFIMS format to KDSmart trait list format
#' 
#' @param traitlist data.frame table of al the list of traits
#' @param dictionary data.frame dictionary of all the equivalences between AgroFIMS and mobile apps attributes
#' @importFrom dplyr select filter
#' @export
#'  
#'  
agro_to_kdsmart <- function(traitlist, dictionary){
  
  traitlist_names <- names(traitlist)
  dictionary <- dictionary %>% 
                         dplyr::filter(!is.na(kdsmart)) %>% 
                         dplyr::filter(DbAttributes %in%  traitlist_names) %>%  
                         dplyr::select(DbAttributes, kdsmart) %>% 
                         as.data.frame(stringsAsFactors=FALSE)   
      
  
  db_attributes <-  dictionary$DbAttributes 
  
  
  #print(db_attributes)
  
  #db_attributes <-  intersect(names(traitlist),dictionary$DbAttributes) #names(traitlist)
  
  traitlist <- traitlist[, db_attributes]
  names(traitlist) <-  dictionary$kdsmart
  traitlist
    
}



#' Convert from AgroFIMS format to Field Book App trait list format
#' 
#' @param traitlist data.frame table of al the list of traits
#' @param dictionary data.frame dictionary of all the equivalences between AgroFIMS and mobile apps attributes
#' @importFrom dplyr select filter
#' @export
#'  
agro_to_fbapp <- function(traitlist, dictionary){
  
  variables <- dictionary %>% 
                dplyr::filter(!is.na(fbapp)) %>% 
                dplyr::select(DbAttributes, fbapp)
  
  traitlist <- mutate_timming_values(traitlist)
  
}


