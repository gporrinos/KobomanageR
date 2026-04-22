



#' @export
kobo_survey <- function(url,
                        username,
                        password,
                        questionnaire_name){
  api      <- kobo_api(url, username, password) 
  assetid  <- api$asset[which(api$name == questionnaire_name)]
  asseturl <- paste0("https://", url, "/api/v2/assets/", assetid, "/")
  res           <- httr::GET(asseturl, httr::authenticate(username, password))
  httr::stop_for_status(res)
  survey        <- httr::content(res, as = "parsed")$content$survey
  return(survey)
}
