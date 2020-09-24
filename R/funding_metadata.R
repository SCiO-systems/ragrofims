# .data <- ag_get_fundagency_studyId(studyDbId = 6,format = "data.frame",
#                                    serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#                                    version ="/0212/r")
# 
# 
# 
# replace_other_fundagency <- function(.data, 
#                                    attribute="fundAgencyTypeId", 
#                                    other_attribute="fundAgencyTypeOther"){
#   
#   #detect "other" values for lazy evaluation
#   if(any(grepl("^Other$",.data[,attribute])==TRUE)){
#       
#     for(i in 1:nrow(.data)){
#       
#       if(.data[, attribute][i]=="Other"){
#         .data[, attribute][i]  <-  .data[,other_attribute][i] 
#       }
#     } 
#   }
#    
#   .data 
# }
# 
# 
# 
# 
# 
# 
# #Example 1
# clean_other_fundagency(.data, 
#                        attribute="fundAgencyTypeId", 
#                        other_attribute="fundAgencyTypeOther")
#     
