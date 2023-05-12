pull_data<-function(formid,servername,username,password,key=NULL,newserver=T){
  isdf<- FALSE
  waittime<-0
  if (is.null(key)) {
    while(!isdf){
      Sys.sleep(waittime)
      if (newserver){
        request<-httr::GET(paste0("https://",servername,".surveycto.com/api/v2/forms/data/wide/json/",formid,"?date=0"),
                           httr::authenticate(username,password))
      } else {
        request<-httr::GET(paste0("https://",servername,".surveycto.com/api/v2/forms/data/wide/json/",formid,"?date=0"),
                           httr::authenticate(username,password,type = "digest"))
      }
      text<-httr::content(request,"text")
      import<-jsonlite::fromJSON(text,flatten = T)
      isdf<- is.data.frame(import)
      if(!isdf){
        # waittime<-readr::parse_number(import$error$message) +1
      } else (waittime<-0)
    }
  } else {
    while(!isdf){
      Sys.sleep(waittime)
      if (newserver){
        request<-httr::POST(paste0("https://",servername,".surveycto.com/api/v2/forms/data/wide/json/",
                                   formid,"?date=0"),
                            httr::authenticate(username,password),
                            body = list(private_key=httr::upload_file(key)),
                            encode = "multipart")
      } else {
        request<-httr::POST(paste0("https://",servername,".surveycto.com/api/v2/forms/data/wide/json/",
                                   formid,"?date=0"),
                            httr::authenticate(username,password,type = "digest"),
                            body = list(private_key=httr::upload_file(key)),
                            encode = "multipart")
      }
      text<-httr::content(request,"text")
      import<-jsonlite::fromJSON(text,flatten = T)
      isdf<- is.data.frame(import)
      if(!isdf){
        # waittime<-readr::parse_number(import$error$message) +1
      } else (waittime<-0)
    }
  }
  return(import)
}
# servername <- 'ipormw'
# formid <- 'malawirammps'
# username <- 'kelly.mccain@lshtm.ac.uk'
# password <- 'ashipISsafe16#'
# request<-httr::GET(paste0("https://",servername,".surveycto.com/api/v1/forms/data/wide/csv/",formid),
#                    httr::authenticate(username,password))
# testdf <- pull_data('malawirammps', 'ipormw', 'kelly.mccain@lshtm.ac.uk', "rammpS!CTOsurvey")
# testdrc <- pull_data('cati_lang_reassign', 'rammps', 'kelly.mccain@lshtm.ac.uk', 'rammpS!CTOsurvey')
# 
# 
# 
# # from surveycto online
# #installing and saving R packages needed to run the commands GET() and fromJSON()
# library(httr)
# library(jsonlite)
# 
# #API Request with digest authentication
# request <-
#   GET("https://rammps.surveycto.com/api/v2/forms/data/wide/json/cati_lang_reassign?date=0",
#       authenticate("kelly.mccain@lshtm.ac.uk", "rammpS!CTOsurvey"))
# 
# #retrieve the contents of a request as a character vector
# data_text <- content(request, "text")
# 
# #convert from JSON data to R object
# datadrctest <- fromJSON(data_text, flatten = TRUE)
