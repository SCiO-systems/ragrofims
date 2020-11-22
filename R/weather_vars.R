#' Weather Fieldbook  
#' 
#' @description Create weather field book from AgroFIMS database based on trailist
#' @param traitlist data.frame trait list data which includes weather data
#' @importFrom dplyr as_tibble mutate case_when
#' @export
#'
cr_weather_fbook <- function(traitlist){
  
  # weather_vars <- traitlist %>% 
  #                   filter(singularity=="weather") %>% 
  #                   select(variableName) %>% nth(1)
  weather_vars <- traitlist %>% dplyr::filter(singularity=="weather")
  vars <- add_season_numplot_prefix( weather_vars)
  out <- data.frame(matrix("", ncol = length(vars), nrow = 1))
  names(out) <- vars
  return(out)
}


