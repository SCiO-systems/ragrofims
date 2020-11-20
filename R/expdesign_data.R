#' Get experimental design factors
#' 
#' @param .data experimental design data
#' @export
#'
get_expdesign_factors <- function(.data){
  
  #TODO: check if there are missing data
  .factors <- vector(mode="character", length = nrow(.data))
  
  for(i in seq.int(nrow(.data))){
    .factors[i] <- .data[i,"factorname"] 
    if(.data[i,"factorname"]=="Other") {
      .factors[i] <-  .data[i,"factorother"] 
    }
  }
  
  .factors <- unique(paste0(.factors, .data[,"group"]))#keep unique values
  .factors <- gsub(pattern = "_(\\d+)$", "", x = .factors) #remove unncesary chars
  .factors <- paste0(.factors,"_f", seq.int(.factors)) #factor names
   
}

#' Mutate levels and units
#' @description when there exist factors which unis of measurements, this function concatenate them in one level. Ex. 30kg.
#' @param .data experimental design data
#' @importFrom purrr flatten map
#' @importFrom stringr str_split  
#' @export 
#' 
mutate_level_unit <- function(.data){
  .data <- .data %>% 
    as_tibble() %>% #as tibble data structure
    #mutate(levelname="") %>% #all as character
    mutate(levelnameother= case_when(
      (levelnameother!="" && factorunit!="") ~ paste0(levelnameother, factorunit),
      (levelnameother=="" || factorunit=="") ~ paste0(levelnameother)
      )#end case when
    )#end mutate  
  .data <- .data %>% mutate(levelname = case_when(    
    (levelname!="" && factorunit!="") ~ paste0(levelname,factorunit),
    (levelname=="" || factorunit=="") ~ paste0(levelname),
    )#end case when
  )
  .data <- .data %>% as.data.frame(stringsAsFactors=FALSE)
}


#' Get factorial levels from experimental designs
#' @param .data experimental design data
#' @importFrom purrr flatten map discard
#' @importFrom stringr str_split
#' @importFrom utils stack
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
  
  for(i in seq.int(.lvl)){
    names(.lvl) <-  paste0("lvl", .data[,"group"])
    if(.data[i,"factorunit"]!=""){ #add unit to measurements
      .lvl[[i]] <- paste0(.lvl[[i]],.data[i,"factorunit"])
    } 
    if(.data[i,"factortype"]!=""){ #add unit to measurements
      .lvl[[i]] <- paste0(.data[i,"factortype"],.lvl[[i]])
    } 
    
  }
  
  #When there is only one factor, skip this next step
  if(length(.lvl)>1){ 
    n <- length(.lvl)-1
    for(i in 1:n){
      if(names(.lvl)[i]==names(.lvl)[i+1]){
        .lvl[[i]] <- c(.lvl[[i]], .lvl[[i+1]])
        .lvl[[i+1]] <- NA_character_
      } else {
        .lvl[[i]] <- .lvl[[i]]
      }
    }
    .lvl <- .lvl %>% purrr::discard( function(x) checkmate::testScalarNA(x) ) #discard NA elements   
    .lvl <- lapply(.lvl, function(x) x[!is.na(x)]) #remove NA values inside each factor-level
    .lvl <-  with(stack(.lvl), split(values, ind)) #combine list-elements with same name
  }
  
 
  
  return(.lvl)
  
}

#' Get levels from non-full-factorial experiments. 
#' @param .info_data experimetal design data
#' @description Extract levels from non-full factorial experiments such as RCBD and CRD.
#' @export
#' 
get_nonfactorial_levels <- function(.info_data){
  
 
  #Extract treatment column
  trt <- .info_data %>% dplyr::filter(DbAttribute=="treatment") %>% 
                        dplyr::select(Value) %>% 
                        dplyr::nth(1)
  trt <- stringr::str_split(trt , pattern = "\\|")[[1]] %>% 
         stringr::str_replace_all(pattern = "[:space:]", replacement = "_")
  
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
                            version = "/0291/r"
                             ){
 
  
  .factors_data <- ragapi::ag_get_edsfactors_expsiteId(
                              expsiteDbId = expsiteId, 
                              format = format,
                              serverURL =  serverURL,
                              version = version
                              )
  
  .info_data <- ragapi::ag_get_edsinfo_expsiteId(expsiteDbId = expsiteId,
                                                 format = format,
                                                 serverURL = serverURL, version = version)
  
  
  
  cond1 <- has_agronomic_metadata(.factors_data) 
  cond2 <- ck_factor_names(.factors_data)
  cond3 <- ck_level_values(.factors_data)
  
  if(checkmate::testLogical(c(cond1,cond2,cond3))){
   
    .factors_data <- replaceNaCharacter(.factors_data)    
    fnames <- get_expdesign_factors(.factors_data) #get factor names
    flevels <- get_factorial_levels(.factors_data) #get levels 
    design_abbr <- get_expdesign_abbr(.factors_data) #get design abbreviation
    design_params <- get_expdesign_params(.factors_data) #get parameters from design
    trt <- get_nonfactorial_levels(.info_data = .info_data)#get treatment
    block <-  design_params$nblock #number of blocks 
    rep <-  design_params$nrep #number of replications
    ntrt <- design_params$ntrt #number of treatments
    
    out <- cr_experimental_design(design_abbr, rep=rep, block=block, trt=trt, ntrt=ntrt, 
                                  fnames=fnames,flevels=flevels)
    
  } else {
    out <- paste0(c(cond2,cond3), collapse= " , ")    #data.frame()
  } 
  
  return(out)

}

#' Check if it is a experimental design
#' @description check if it fill the conditions to be a experimental design object.
#' @param expdesign data.frame experimental design data.frame
#' @export
#' 
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
#' @examples \dontrun{
#' meta_dbattributes <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/124fPX0f_J9Ws-f4ZgSX6AyXQVbCZY50nMsuaFfPaDWg/edit#gid=997278569")
#' get_dsginfo_data(expsiteId = 22, format="data.frame", serverURL = "https://research.cip.cgiar.org/agrofims/api/dev", version= "/0291/r", meta_dbattributes)
#' }
#' @export
#' 
get_dsginfo_data <- function(expsiteId = NULL, 
                             format=c("json","list","data.frame"),
                             serverURL="https://research.cip.cgiar.org/agrofims/api/dev", 
                             version = "/0291/r",
                             meta_dbattributes=NULL){
  
  format <- match.arg(format)
  
  .info_data <- ragapi::ag_get_edsinfo_expsiteId(expsiteDbId = expsiteId,
                                             format = "data.frame",
                                             serverURL = serverURL, version = version)
  
  .factors_data <- ragapi::ag_get_edsfactors_expsiteId(expsiteDbId = expsiteId,
                                           format = "data.frame",
                                           serverURL = serverURL, version = version)
  
  if(nrow(.info_data)==0 ||  nrow(.factors_data)==0){
    
    return(data.frame())  
    
  } 
  
  expsiteId <- .info_data[1,"Value"]#get expsiteId
  expunit <- .info_data[.info_data$DbAttribute=="variable","Value"]
  
  if(checkmate::test_false(expsiteId)){ #if there experiment-site does not exist
    
    out <- data.frame()
    
  } else {
    
    .factors_data <- .factors_data %>%  replace(is.na(.), "")
    
    meta_dbattributes <-  meta_dbattributes %>% dplyr::filter(module=="design")
    
    design <-  .info_data[.info_data$DbAttribute=="parametercode","Value"]
    
    .info_data <- dplyr::left_join(meta_dbattributes, .info_data, by="DbAttribute")
    
    .info_data <- .info_data %>% dplyr::mutate(Value = case_when(
                                                        Value=="undefined" ~"",
                                                        is.na(Value) ~ "",
                                                        TRUE~Value
                                                       )
                                               )
    
    out <- .info_data %>% dplyr::mutate(Dependency = case_when(
                                                    is.na(Dependency)~ "",
                                                    TRUE~Dependency
                                                    )
                                        )
    min_info <- filter_dsginfo_design(out, design,.factors_data = .factors_data)
    unit_info <- filter_expunitinfo_design(out, expunit, design)
    out <- rbind(min_info,unit_info) %>% dplyr::select(AgroLabelDbAttribute, Value)
    names(out) <- c("Parameter", "Value")
    
  } 
  return(out)
}

#' Filter experimental design information by design
#' @description According to different 
#' @param dsginfo experimetanl design information
#' @param design experimental design
#' @param .factors_data table of factors retrived by AgroFIMS API.
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#' @export

filter_dsginfo_design <- function(dsginfo, design,.factors_data){
  
  #pos <- grep(pattern = design , x = dsginfo$design_dependencies)
  .factors_data <- tidyr::replace_na(.factors_data)
  
  dsginfo <- dsginfo %>% dplyr::filter(Dependency=="no")
  design_pattern <- paste0("^", design, "$")
  pos <- which(purrr::map_lgl(.x = dsginfo$design_dependencies,
                       function(x) sum(stringr::str_detect(strsplit(x, "\\|")[[1]], 
                                                           design_pattern ))>0  )==TRUE)
  
  dsginfo[dsginfo$DbAttribute=="nfactors", "Value"] <- as.character(nrow(.factors_data))
  
  
  factors <- get_expdesign_factors(.factors_data)  
  factors_label <- paste0("Factor ", seq.int(factors))
  flevels <- get_factorial_levels(.factors_data) %>% purrr::map_chr(function(x)paste(x,collapse = ", ")) %>% as.list()
  flevels_label <- paste0("Factor ", seq.int(flevels), "-Levels")
  fvalues <- flabels <- NULL
  for(i in 1:nrow(.factors_data)){
      fvalues <- append(fvalues,c(factors[i],flevels[[i]]))
      flabels <- append(flabels,c(factors_label[i],flevels_label[[i]]))
  }
  ftable <- data.frame(flabels, fvalues)
  names(ftable) <- c("AgroLabelDbAttribute", "Value")
  
  dsginfo <- data.table::rbindlist(list(dsginfo, ftable),use.names = TRUE,fill = TRUE) %>% as.data.frame(stringsAsFactors=FALSE)

  
}


#' Filter experimental units
#' 
#' @description based on differente types of experimental units, this function filter values according to what users have selected in the interface.
#' @param dsginfo data.frame experimental design information
#' @param expunit character experimental unit. Example: \code{plot}, \code{field} and \code{pot}.
#' @param design character experimetal design abbreviation used in AGROFIMS database. Example: \code{crd} for completely randomized design. Check other such as \code{rcbd}, \code{frcbd}
#' , \code{fcrd}, , \code{spsp}, among others. 
#' @description Filter experimental unit values from AGROFIMS experiments
#' @export
#' 
filter_expunitinfo_design <- function(dsginfo , expunit, design){
  
  dsginfo <- assign_expunit(dsginfo, "length_p", "length_unit_p")  
  dsginfo <- assign_expunit(dsginfo, "width_p", "width_unit_p")
  dsginfo <- assign_expunit(dsginfo, "length_f", "length_unit_f")
  dsginfo <- assign_expunit(dsginfo, "width_f", "width_unit_f")
  dsginfo <- assign_expunit(dsginfo, "diameter", "diameter_unit")
  dsginfo <- assign_expunit(dsginfo, "depth", "depth_unit")
  
  dsginfo <- assign_expunit(dsginfo, "main_exp_plot_width", "main_exp_plot_width_unit")
  dsginfo <- assign_expunit(dsginfo, "main_exp_plot_width", "main_exp_plot_width_unit")
  dsginfo <- assign_expunit(dsginfo, "sub_exp_plot_length", "sub_exp_plot_length_unit")
  dsginfo <- assign_expunit(dsginfo, "sub_exp_plot_width", "sub_exp_plot_width_unit")
  dsginfo <- assign_expunit(dsginfo, "subsub_exp_plot_length", "subsub_exp_plot_length_unit")
  dsginfo <- assign_expunit(dsginfo, "subsub_exp_plot_width", "subsub_exp_plot_width_unit")
  
  #remove rows with unit expresions, they are not neccessary
  dsginfo <- dsginfo %>%  dplyr::filter(!grepl("unit",DbAttribute))
  ##################
  
  if((design=="crd" || design=="rcbd" || design=="fcrd" || design=="frcbd") && expunit!=""){
    
    dsginfo <- dsginfo %>% dplyr::filter(Dependency==expunit)
    
  } else if((design=="crd" || design=="rcbd" || design=="fcrd" || design=="frcbd") && expunit==""){
    
    expunit <- "plot"
    dsginfo <- dsginfo %>% dplyr::filter(Dependency==expunit)
    
  } else if(design=="sprcbd" || design=="strip"){
    
    dsginfo <- dsginfo %>% dplyr::filter(DbAttribute %in%  c("main_exp_plot_length","main_exp_plot_width","sub_exp_plot_length","sub_exp_plot_width"))
    
  } else if(design=="spsp"){
    
    dsginfo <- dsginfo %>% dplyr::filter(DbAttribute %in% c("main_exp_plot_length", "main_exp_plot_width","sub_exp_plot_length",
                                                           "sub_exp_plot_width","subsub_exp_plot_length" , "subsub_exp_plot_width"))
  }
  
  return(dsginfo)
  
}


#' Assign  experimental unit
#' 
#' @description Assign and paste dimension values and dimension units. Example: \code{15 kg} , \code{20 ton/hec}.
#' @param .info_data data.frame information  
#' @param dbattr_mea character internal code for input dimension values. Example \code{length_p} is for plot length
#' @param dbattr_expunit character internal code for input units dimension. Example \code{length_p_unit} is for plot length units
#' @export
#' 
assign_expunit <- function(.info_data, dbattr_mea = "length_p", dbattr_expunit="length_p_unit"){
  
  expmea <- .info_data %>% dplyr::filter(DbAttribute==dbattr_mea) %>% select(Value) %>% nth(1) 
  expunit <- .info_data %>% dplyr::filter(DbAttribute==dbattr_expunit) %>% select(Value) %>% nth(1) 
  
  
  .info_data <- .info_data %>% dplyr::mutate(Value=case_when(
                                                    DbAttribute == dbattr_mea ~ paste(expmea,expunit),
                                                    TRUE~Value
                                                    )
                                             )
}


