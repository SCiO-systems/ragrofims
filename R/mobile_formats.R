#' Mutate multiple timing values for trait list table
#' 
#' @description AgroFIMS timing values are divided in 5 attributes or columns: Days after planting, Growth stage, Frequency,
#' Date and Other. Therefore, this function assign in the column \code{meaTimeValue} all the different values according to each case or attribute.
#' @param traitlist data.frame trait list data
#' @importFrom dplyr mutate as_tibble case_when
#' @export

mutate_crop_names <- function(traitlist){
  
  traitlist <- traitlist %>% dplyr::mutate(
                              cropcommonname = case_when(
                                  cropcommonname=="Other"~ as.character(cropcommonnameother),
                                  cropcommonname=="Field-level/all plots"~ "Field",
                                  TRUE~cropcommonname)       
                              )
  traitlist
}


#' Mutate timing values in trait list tables
#' 
#' @description Traitlist tables includes timing values such as \code{Days after plating}, \code{Growth stage}, \code{Date} and \code{Frequency}. Those values are dispersing in multiple columns, so 
#' the idea is place this value in the \code{meaTimeValue} attribute that store timing values from different type of timing inputs.
#' @importFrom dplyr mutate case_when 
#' @param traitlist data.frame trait list data
#' @export

mutate_timming_values <- function(traitlist){
  
  #si solo hay filas de man prac
    
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
  
  traitlist
}


#' Mutate variable data type attribute for ODK mobile application
#' 
#' @description Mutate and assign in variableDataType attribute all the ODK validations according to the type of variable (DECIMAL, INTEGER, NUMERIC,DATE,TEXT)
#' @param traitlist data.frame trait list data
#' @export
#' 
mutate_variable_type_odk <- function(traitlist){
  
  traitlist <- traitlist %>% 
    as_tibble() %>% #as tibble data structure
    mutate(variableDataType2="") %>% #all as character
    mutate(variableDataType2=
             case_when(
                      variableDataType=="DECIMAL" ~ "decimal",
                      #paste0(variableLowerLimit ," <= x <= ", variableUpperLimit),
                      variableDataType=="INTEGER" ~  "integer",
                      variableDataType=="CATEGORICAL" ~ paste0("select_one ", tolower(variableName)),
                      variableDataType=="TEXT" ~ "text",
                      variableDataType=="DATE" ~ "date"
                      )#END CASE_WHEN
          ) #END MUTATE
  traitlist <- traitlist %>% dplyr::mutate(variableDataType=variableDataType2) %>% dplyr::select(-variableDataType2)
  
}

#' Mutate variable validation attribute for KDSmart mobile application
#' 
#' @description Mutate and assign in variableValidation attribute all the kdsmart validations according to the type of variable (DECIMAL, INTEGER, NUMERIC,DATE,TEXT)
#' @param traitlist data.frame trait list data
#' @export
#' 
mutate_variable_validation_odk <- function(traitlist){
  
  traitlist <- traitlist%>% 
    as_tibble() %>% #as tibble data structure
    mutate(variableValidation="") %>% #all as character
    mutate(variableValidation=case_when(
      variableDataType=="DECIMAL" ~ paste0("(. >= ", "'",variableLowerLimit,"')", " and ", "(. <= ", "'",  variableUpperLimit, "')"),
      variableDataType=="INTEGER" ~  paste0("(. >= ", "'",variableLowerLimit,"')", " and ", "(. <= ", "'",  variableUpperLimit, "')"),
      variableDataType=="CATEGORICAL" ~ "" ,
      variableDataType=="TEXT" ~ "",
      variableDataType=="DATE" ~ ""
    )#END CASE_WHEN
    )#END MUTATE
}


#' Mutate variable names for concatenation of variables and units
#' 
#' @description AgroFIMS timing values are divided in 5 attributes or columns: Days after planting, Growth stage, Frequency,
#' Date and Other. Therefore, this function assign in the column \code{meaTimeValue} all the different values according to each case or attribute.
#' @param traitlist data.frame trait list data
#' @importFrom dplyr mutate as_tibble case_when select
#' @export

mutate_variable_name <- function(traitlist){
  
  #Variable Name
  traitlist <- traitlist%>% 
    as_tibble() %>% #as tibble data structure
    mutate(variableName2="") %>% #all as character
    mutate(variableName2= case_when(
      
      ((singularity=="crop_measurement" & !is.na(unit)) & (!is.na(cropcommonname) & !is.na(parametermeasured)) ) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_",parametermeasured,"_", str_replace_all(measurement,"[:space:]","_"),"_",unit),
      ((singularity=="crop_measurement" & is.na(unit)) & (!is.na(cropcommonname) & !is.na(parametermeasured))) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_", parametermeasured, "_", str_replace_all(measurement,"[:space:]","_")),
      
      ((singularity=="crop_measurement" & !is.na(unit)) & (!is.na(cropcommonname) & is.na(parametermeasured))) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_", str_replace_all(measurement,"[:space:]","_")),
      
            
      
      ((singularity=="crop_measurement" & is.na(unit)) & (!is.na(cropcommonname) & is.na(parametermeasured))) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_", str_replace_all(measurement,"[:space:]","_")),
      
      ((singularity=="management_practices" & !is.na(unit)) & (!is.na(cropcommonname))) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_", str_replace_all(measurement,"[:space:]","_"),"_",unit),
      ((singularity=="management_practices" &  is.na(unit)) & (!is.na(cropcommonname))) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_",str_replace_all(measurement,"[:space:]","_")),
      
      ((singularity=="crop_phenology" & !is.na(unit)) & (!is.na(cropcommonname))) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_", str_replace_all(measurement,"[:space:]","_")),
      ((singularity=="crop_phenology" &  is.na(unit)) & (!is.na(cropcommonname))) ~ paste0(str_replace_all(cropcommonname,"[:space:]","_"),"_",str_replace_all(measurement,"[:space:]","_")),
      
      (singularity=="weather" & !is.na(unit)) ~ paste0(variableName,"_",unit),
      (singularity=="weather" & is.na(unit)) ~ paste0(variableName),
      
      (singularity=="soil" & !is.na(unit)) ~ paste0(cropcommonname,"_", str_replace_all(measurement,"[:space:]","_"), "_",unit),
      (singularity=="soil" & is.na(unit)) ~ paste0(cropcommonname,"_", str_replace_all(measurement,"[:space:]","_"))
      
      
      # TRUE ~ str_replace_all((measurement),"[:space:]","_")
    )#end case when
    )#end mutate  
  traitlist <- traitlist %>% mutate(variableName=variableName2) %>% select(-variableName2)
  
} 

#' Mutate variable validation attribute for KDSmart mobile application
#' 
#' @description Mutate and assign in variableValidation attribute all the kdsmart validations according to the type of variable (DECIMAL, INTEGER, NUMERIC,DATE,TEXT)
#' @param traitlist data.frame trait list data
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


#' Mutate variable validation attribute for KDSmart mobile application
#' 
#' @description mutate validation rules according different types of inputs in order to complaint with Field Book App.
#' @param traitlist data.frame trait list data
#' @export
#' 
mutate_variable_validation_fbapp <- function(traitlist){
  
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





#' Mutate number of measurement per season and per plot 
#' @param traitlist trait list table
#' @description set atributes samplesperseason and samplesperplot to 1 evaluations by default
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


#' Flatten management practices table to retrieve actual plan evaluations (in trait list)
#' 
#' @param traitlist_dt data.frame trait list table
#' @export
#' 
flatten_manprac_actualplan <- function(traitlist_dt){
  
  #extrat manprac_actualplan data
  manprac_dt <- traitlist_dt %>% dplyr::filter(singularity=="management_practices")
  dup_row_index <-which(duplicated (paste0(manprac_dt$cropcommonname, manprac_dt$measurement, manprac_dt$indexorder, 
         manprac_dt$measurement, manprac_dt$variableName)))
  #dup_row_index <- which(duplicated(paste0(manprac_dt$,manprac_dt$measurement)))
  if(nrow(manprac_dt)>0 && length(dup_row_index)>0){
    #Find duplicates values
    #dup_row_index <- which(duplicated(manprac_dt$variableName))
    manprac_dt <- manprac_dt[-dup_row_index,] #remove duplicates
    
    #counting of how many evaluation per variableName or measurement
    manprac_count_dt <- traitlist_dt %>% 
                        dplyr::count(cropcommonname, indexorder, variableName)
    
    manprac_dt <- dplyr::left_join(manprac_dt, manprac_count_dt)
    manprac_dt$samplesperseason <- manprac_dt$n #assign to samplesperseason
    manprac_dt$samplesperplot <- "1"
    manprac_dt$n <- NULL #remove temporal column "number of evaluation"
    
    #Reduce 
    traitlist_dt <- traitlist_dt %>% dplyr::filter(singularity!="management_practices")
    traitlist_dt <- rbind(traitlist_dt, manprac_dt)
  
  } else if(nrow(manprac_dt)>0 && length(dup_row_index)==0){
    manprac_dt$samplesperseason <- "1" #assign to samplesperseason
    manprac_dt$samplesperplot <- "1"
    traitlist_dt <- traitlist_dt %>% dplyr::filter(singularity!="management_practices")
    traitlist_dt <- rbind(traitlist_dt, manprac_dt)
  }  else { 
     traitlist_dt <- traitlist_dt
  } 
  traitlist_dt
} 


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
                         dplyr::arrange(kdsmart_order) %>% 
                         dplyr::select(DbAttributes, kdsmart) %>% 
                         as.data.frame(stringsAsFactors=FALSE)   
      
  
  db_attributes <-  dictionary$DbAttributes 
  
  traitlist <- traitlist[, db_attributes]
  names(traitlist) <-  dictionary$kdsmart
  traitlist
    
}


#' Set names for kdsmart protocol format
#' @param protocol_dt protocol data
#' @param dictionary dictionary to transform AgroFIMS attributes headers to KDSmart format
#' @export 
#' 
set_protocol_names_kdsmart <- function(protocol_dt, dictionary){
 
  traitlist_names <- names(protocol_dt)
  dictionary <- dictionary %>%
                dplyr::filter(!is.na(kdsmart_protocol)) %>%
                dplyr::filter(DbAttributes %in%  traitlist_names) %>%  
                dplyr::select(DbAttributes, kdsmart_protocol) %>% 
                as.data.frame(stringsAsFactors=FALSE)
  
  db_attributes <-  dictionary$DbAttributes 
  
  protocol_dt <- protocol_dt[, db_attributes]
  names(protocol_dt) <-  dictionary$kdsmart_protocol
  sel_kdsmart_headers <- c("Crop",	"TraitName",	"Value",	"TraitUnit")
  protocol_dt <- protocol_dt[, sel_kdsmart_headers]
  protocol_dt
  
}

#' Mutate \code{format} data type attribute for Field Book App compliant
#' @param traitlist data.frame trait list table with all the variables to be measured in the field
#' @export 
#' 
mutate_format_fbapp <- function(traitlist){
  

  traitlist <- traitlist %>% mutate(variableDataType = case_when(variableDataType == "DECIMAL" ~ "numeric",
                                                                 variableDataType == "CATEGORICAL" ~ "categorical",
                                                                 variableDataType == "TEXT" ~ "text",
                                                                 variableDataType == "DATE" ~ "date",
                                                                 variableDataType == "INTEGER" ~ "numeric",
                                                                 variableDataType == "OTHER" ~ "text")
                                    )
}

#' Convert from AgroFIMS format to Field Book App trait list format
#' 
#' @param traitlist data.frame table of al the list of traits
#' @param dictionary data.frame dictionary of all the equivalences between AgroFIMS and mobile apps attributes
#' @importFrom dplyr select filter
#' @export
#'  
agro_to_fbapp <- function(traitlist, dictionary){
  
  traitlist_names <- names(traitlist)
  dictionary <- dictionary %>% 
                          dplyr::filter(!is.na(fbapp)) %>% 
                          dplyr::filter(DbAttributes %in%  traitlist_names) %>%  
                          dplyr::select(DbAttributes, fbapp) %>% 
                          as.data.frame(stringsAsFactors=FALSE)   
  
  
  db_attributes <-  dictionary$DbAttributes 
  
  traitlist <- traitlist[, db_attributes]
  names(traitlist) <-  dictionary$fbapp
  traitlist
  
}

#' Convert from AgroFIMS format to ODK survey - trait list format
#' 
#' @param traitlist data.frame table of al the list of traits
#' @param dictionary data.frame dictionary of all the equivalences between AgroFIMS and mobile apps attributes
#' @importFrom dplyr select filter
#' @export
#'  
agro_to_odk_survey <- function(traitlist, dictionary){
  
  traitlist_names <- names(traitlist)
  dictionary <- dictionary %>% 
                          dplyr::filter(!is.na(odk)) %>% 
                          dplyr::filter(DbAttributes %in%  traitlist_names) %>%
                          #dplyr::arrange(odk) %>%
                          dplyr::select(DbAttributes, odk) %>% 
                          as.data.frame(stringsAsFactors=FALSE)   
  
  db_attributes <-  dictionary$DbAttributes 
  
  traitlist <- traitlist[, db_attributes]
  names(traitlist) <-  dictionary$odk
  #Aditional columns
  traitlist$name <- traitlist$`label::English`#"] #add name column
  #remove special characters and swap for introduce "_"
  traitlist$name <- stringr::str_replace_all(traitlist$name, pattern = "[[:punct:]]"  ,replacement = "_")
  traitlist$name <- stringr::str_replace_all(traitlist$name, pattern = "[[:space:]]"  ,replacement = "_")
  traitlist$name <- stringr::str_replace_all(traitlist$name, pattern = "°"  ,replacement = "_")
  
  traitlist$`label::English` <- stringr::str_replace_all(traitlist$`label::English`, pattern = "[[:punct:]]"  ,replacement = "_")
  traitlist$`label::English` <- stringr::str_replace_all(traitlist$`label::English`, pattern = "[[:space:]]"  ,replacement = "_")
  traitlist$`label::English` <- stringr::str_replace_all( traitlist$`label::English`, pattern = "°"  ,replacement = "_")
  
  #appeareance 
  traitlist$appearance <- ""
  return(traitlist)
}

#' Convert from AgroFIMS format to ODK choices description
#' 
#' @description convert AgroFIMS trait list data to ODK choices-sheet format
#' @param traitlist data.frame table of al the list of traits
#' @param dictionary data.frame dictionary of all the equivalences between AgroFIMS and mobile apps attributes
#' @importFrom dplyr select filter
#' @importFrom data.table rbindlist
#' @export
#' 
agro_to_odk_choices <- function(traitlist){
  
  traitlist <-  traitlist %>% dplyr::filter(variableDataType=="CATEGORICAL")  
  if(nrow(traitlist)>0){
    
    list_name <- categorical_levels <- vector(mode="character",nrow(traitlist))
    out <- vector(mode="list")
    for(i in 1:nrow(traitlist)){
      list_name <-  tolower(traitlist$variableName[i])
      categorical_levels <- traitlist$variableCategory[i] %>% strsplit("\\|")
      categorical_levels <- categorical_levels[[1]]
      out[[i]] <- data.frame(list_name =list_name, categorical_levels=categorical_levels)  
    }
    out <- data.table::rbindlist(out) %>% as.data.frame(stringsAsFactors=FALSE)
    out <- out %>% dplyr::mutate(label=categorical_levels )
    names(out) <- c("list name", "name", "label::English")
    return(out)
  } else{
    out <- data.frame()
  } 
  return(out)
}  

#' Create ODK settings structure
#' @param expsiteid character experiment site Id
#' @export
#' 
odk_settings_structure <- function(expsiteId="DS92390CMNHGTO"){
  
  out <- data.frame(
                    form_title ="AgroFIMS_group_looped",
                    form_id = paste0("build_AgroFIMS-group-looped_",expsiteId),
                    public_key	= "",
                    submission_url = "",
                    instance_name = "",
                    stringsAsFactors = FALSE
                   )
  return(out)
}




#' Create ODK survey structures for only one row rules
#' 
#' @param type character ODK type attribute
#' @param name character ODK name attribute
#' @param label character ODK label attribute
#' @param hint character ODK hint attribute
#' @param read_only character ODK read_only attribute
#' @param constraint character ODK constraint attribute
#' @param appeareance character ODK constraint appeareance
#' @export
#' 
odk_survey_structure <- function(type="", name="", label="", hint="", 
                                 read_only="", constraint="", appearance=""){
  
  odk_list <- list(
    type= type,
    name = name,
    label = label,
    hint = hint,
    read_only = read_only,
    constraint	= constraint,
    appearance =  appearance
  )
  out <- data.frame(odk_list)
  headers_odk <- c("type","name","label::English", "hint::English",	
                   "read_only","constraint", "appearance") 
  names(out) <- headers_odk
  return(out)
}

  

#' Create Field Book App template
#' 
#' @param design data.frame statistical design object including crop measurements
#' @importFrom janitor remove_empty
#' @importFrom stringr str_replace_all
#' @export
#'

cr_fbapp <- function(design){
  
  names(design) <- stringr::str_replace_all(string = names(design), pattern = "µ",replacement = "u")
  names(design) <- stringr::str_replace_all(string = names(design), pattern = "\\(",replacement = "_")
  names(design) <- stringr::str_replace_all(string = names(design), pattern = "\\)",replacement = "_")
  names(design) <- stringr::str_replace_all(string = names(design), pattern = "[[:space:]]",replacement = "_")
  
  design <- janitor::remove_empty(design, which = "cols")
  plotid <- 1:nrow(design)
  design <- cbind(plotid, design)
  
}


