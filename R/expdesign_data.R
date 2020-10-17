#' Get experimental design factors
#' @param .data experimental design data
#' @export
#'
get_expdesign_factors <- function(.data){
  
  #TODO: check if there are missing data
  .factor <- vector(mode="character", length = nrow(.data))
  
  for(i in seq.int(nrow(.data))){
    .factor[i] <- .data[i,"factorname"] 
    if(.data[i,"factorname"]=="Other") {
      .factor[i] <-  .data[i,"factorother"] 
    }
  }
  paste0(.factor,"_f",seq.int(.factor))
}

#' Get factorial levels from experimental designs
#' @param .data experimental design data
#' @importFrom purrr flatten map
#' @importFrom stringr str_split  
#' @export 

get_factorial_levels <- function(.data){
  
  #TODO: check if there are missing data
  .lvl <- vector(mode="character", length = nrow(.data))
  
  for(i in seq.int(nrow(.data))) {
    .lvl[i] <- .data[i,"levelname"] 
    if(grepl(pattern = "Other", .lvl[i])) {
        other <- .data[i,"levelnameother"] 
        .lvl[i] <- gsub(pattern = "\\|Other", replacement = "", x = .lvl[i])#remove Other
        .lvl[i] <- paste(.lvl[i],other,sep = "|") #add other value
        
    }
  }  
  .lvl <- as.list(.lvl)
  .lvl <- flatten(map(.lvl, function(x) str_split(x,pattern = "\\|")))
}

#' Get levels from non-full-factorial experiments. 
#' @param .data experimetal design data
#' @description Extract levels from non-full factorial experiments such as RCBD and CRD.
#' @export
#' 
get_nonfactorial_levels <- function(.data){
  
}

#' Get experimental design abbreviation
#' @param .data experimental design data
#' @param col_name column whic has experimental design abbreviation
#' @export
#' 
get_expdesign_abbr <- function(.data, col_name = "parametercode"){
  
  out <- unique(.data[,col_name])
  return(out)
  
}


#' Get experimental design parameters
#' @param .data experimental design data
#' @param abbr column whic has experimental design abbreviation
#' @export
#' 
get_expdesign_params <- function(.data, abbr = "frcbd"){
  
  nblock <- ifelse(unique(.data[,"nblock"])=="undefined", "2", unique(.data[,"nblock"]))
  nrep <- ifelse(unique(.data[,"nrep"])=="undefined", "2", unique(.data[,"nrep"]))
  ntrt <- ifelse(unique(.data[,"ntrt"])=="undefined", "2", unique(.data[,"ntrt"]))
  
  out <- list(nblock=nblock, nrep=nrep, ntrt=ntrt)
  
}


#' Get experimental design from AgroFIMS via Agronomic API
#' 
#' @param expsiteId character experiment site ID from AgroFIMS database
#' @param format data format: json, list and data.frame
#' @param serverURL URL of the agrofims server
#' @param version version of the call. By default version \code{0212}.
#' @importFrom ragapi ag_get_edsfactors_expsiteId
#' @examples \dontrun{
#' #Get experimental design
#' out <- get_design_data(expsiteDbId = 6, 
#'                        format=c("json","list","data.frame"),
#'                        serverURL="https://research.cip.cgiar.org/agrofims/api/dev", 
#'                        version = "/0233/r"
#'                        )
#' 
#' }
#' @export
#' 
get_experimental_design <- function(expsiteId = NULL, format=c("json","list","data.frame"),
                            serverURL="https://research.cip.cgiar.org/agrofims/api/dev", 
                            version = "/0233/r"
                             ){
 
  .factors_data <- ragapi::ag_get_edsfactors_expsiteId(
                              expsiteDbId=expsiteId, 
                              format=format,
                              serverURL =  serverURL,
                              version = version
                              )
    
  cond1 <- has_agronomic_metadata(.factors_data) 
  cond2 <- ck_factor_names(.factors_data)
  cond3 <- ck_level_values(.factors_data)
  
  if(checkmate::testLogical(c(cond1,cond2,cond3))){
   
    fnames <- get_expdesign_factors(.factors_data) #get factor names
    flevels <- get_factorial_levels(.factors_data) #get levels 
    design_abbr <- get_expdesign_abbr(.factors_data) #get design abbreviation
    design_params <- get_expdesign_params(.factors_data) #get parameters from design
    block <-  design_params$nblock #number of blocks 
    rep <-  design_params$nrep #number of replications
    ntrt <- design_params$ntrt #number of treatments
    
    
    out <- cr_experimental_design(design_abbr, rep=rep, block=block, trt=NULL, ntrt=ntrt, 
                                  fnames=fnames,flevels=flevels)
  } else {
    out <- paste0(c(cond2,cond3), collapse= " , ")    #data.frame()
  } 
  
  return(out)

}

#' Check if a experimental design
#' @export
ck_expdesign <- function(expdesign){
    
    #check if it is a experimental design
    # YES wheter is a  data.frame , otherwise FALSE
    checkmate::testDataFrame(expdesign,min.rows = 1,min.cols = 2) 
}





#' Get metadata from experimental desing information
#' 
#' @param expsiteId expsiteDbId or experimente site ID
#' @param format type of data structure format
#' @param serverURL database server URL
#' @param version api version
#' @param meta_dbattributes data dictionary of metadata. It includes equivalences between excel and database names.
#' @description get metadata from experimental details
#' @export
#' 
get_dsginfo_data <- function(expsiteId = NULL, format= NULL, 
                                  serverURL=NULL,  version = NULL,
                                  meta_dbattributes=NULL){
  
  .info_data <- ragapi::ag_get_edsinfo_expsiteId(expsiteDbId = expsiteId,
                                             format = "data.frame",
                                             serverURL = serverURL, version = version)
  .factors_data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId = expsiteId,
                                           format = "data.frame",
                                           serverURL = serverURL, version = version)
  
  cond1 <- has_agronomic_metadata(.info_data) 
  cond2 <- has_agronomic_metadata(.factors_data ) 
  
  if(cond){
    #sitedesc_data[,sitedesc_data] <- ""
    #sitedesc_data <- clean_sitedesc(sitedesc_data)
    #sitedesc_data <- convert_to_xlsx_sitedesc(sitedesc_data, meta_dbattributes)
  } else{
    sitedesc_data <- data.frame()
  } 
  
}


