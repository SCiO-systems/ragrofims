#' Weather Fieldbook  
#' 
#' @description Create weather field book from AgroFIMS database based on trailist
#' @importFrom dplyr as_tibble mutate case_when
#' @export
#'
cr_weather_fbook <- function(traitlist){
  
  weather_vars <- traitlist %>% 
                    filter(singularity=="weather") %>% 
                    select(variableName) %>% nth(1)
 
  out <- data.frame(matrix("", ncol = length(weather_vars), nrow = 1))
  names(out) <- weather_vars
  return(out)
}


