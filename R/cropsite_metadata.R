#' Convert crop information response to AgroFIMS Excel R-table
#' 
#' @param .data data.frame table with crop information metadata
#' @param meta_dbattributes metadata attributes and labels in AgroFIMS
#' @description Transform an API table to Excel table format
#' @author Omar Benites
#' @description set and modify databases attributes by AgroFIMS label in order to export the fieldbook file
#' @export
#' 
convert_to_xlsx_cropdesc <- function(.data, meta_dbattributes){
  
  meta_dbattributes <- meta_dbattributes %>% dplyr::filter(module=="crop")
  
  crop <- unique(.data$cropcommonname)
  croppingtypeid <- unique(.data$croppingtypeid)
  out <- vector(mode = "list",length = length(crop))
  
  if(length(crop)==1){
    
    .data <- .data %>% dplyr::filter(cropcommonname==crop[1]) %>% dplyr::select(cropcommonname,varietyname,intercropValueRowCrop)
    .data <- as.data.frame(t(.data),stringsAsFactors=FALSE) %>% tibble::rownames_to_column()    
    .data <- .data %>% dplyr::mutate(V1 = case_when(rowname=="varietyname" ~ stringr::str_replace(V1,pattern = "\\|",replacement = ", "), 
                                                    TRUE~V1))
    names(.data) <- c("DbAttribute", "Value")
    .data <- dplyr::left_join(.data, meta_dbattributes)
    .data <- .data %>% dplyr::filter(DbAttribute!="intercropValueRowCrop")
    out <- .data[,c("AgroLabelDbAttribute", "Value")]
    
  } else {
    for(i in 1:length(crop)){
      out[[i]] <- .data %>% dplyr::filter(cropcommonname==crop[i]) %>% dplyr::select(cropcommonname,varietyname,intercropValueRowCrop)
      out[[i]] <- as.data.frame(t(out[[i]]),stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
      out[[i]]$id <- as.character(i)
    }
    .data <- data.table::rbindlist(out) %>% as.data.frame(stringsAsFactors=FALSE)  
    .data <- .data %>% dplyr::mutate(V1 = case_when(rowname=="varietyname" ~ stringr::str_replace(V1,pattern = "\\|",replacement = ", "), 
                                                    TRUE~V1))
    
    names(.data) <- c("DbAttribute", "Value","id")
    .data <- dplyr::left_join(.data, meta_dbattributes)
    
    if(croppingtypeid!="Intercrop"){
      .data <- .data %>% dplyr::filter(DbAttribute!="intercropValueRowCrop")  
    }
    .data$AgroLabelDbAttribute <- paste(.data$AgroLabelDbAttribute, .data$id)
    out <- .data[,c("AgroLabelDbAttribute", "Value")]
  }
  return(out)
  
}

#' Filter crop information according to croppping type.
#' 
#' @description every croppping type has specific information. For this reason, this function filter this information according to each cropping type.
#' @param .data crop information data
#' @export

filter_cropdesc_data <- function(.data){
      
  if(nrow(.data)>0){
    
    .data <- mutate_crop_names(.data)
    
    croppping_type <- unique(.data$croppingtypeid)
    
    if(croppping_type=="Monocrop"){
      
      .data <- .data %>% dplyr::filter(cropcommonname!="Field")
      
    } else if(croppping_type=="Intercrop"){
      
      .data <- .data %>% dplyr::filter(cropcommonname!="Field")
      
    } else if(croppping_type=="Relay crop"){
      
      .data <- .data %>% dplyr::filter(cropcommonname!="Field")
      
    }
    return(.data)
  } else {
    return(data.frame())
  }  
    
}



#' Get metadata of crop information from AGROFIMS experiments
#' 
#' @param expsiteId experimente site ID
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @param meta_dbattributes data dictionary of metadata. It includes equivalences between excel and database names.
#' @description get metadata from experimental details
#' @export
#' 
get_cropdesc_metadata <- function(expsiteId = NULL, format= NULL, 
                                  serverURL="https://research.cip.cgiar.org/agrofims/api/dev",  
                                  version = "/0291/r",
                                  meta_dbattributes=NULL){
  
  cropdesc_data <- ragapi::ag_get_cropsite_expsiteId(expsiteDbId = expsiteId,
                                                     format = "data.frame",
                                                     serverURL = serverURL, 
                                                     version = version
                                                    )
  cond <- has_agronomic_metadata(cropdesc_data)
  
  if(cond){
    
    cropdesc_data <- filter_cropdesc_data(cropdesc_data)
    out <- convert_to_xlsx_cropdesc(cropdesc_data, meta_dbattributes)      
    
  } else {
    return(data.frame())
  }
  
}  