
#' @export
kobo_version_names <- function(url,
                              username,
                              password,
                              questionnaire_name,
                              select_uids = NULL){
  
  
  api      <- kobo_api(url, username, password) 
  assetid  <- api$asset[which(api$name == questionnaire_name)]
  asseturl <- paste0("https://", url, "/api/v2/assets/", assetid, "/versions/")
  
  # Obtain a list of all uids
  res      <- httr::GET(asseturl, httr::authenticate(username, password))
  httr::stop_for_status(res)
  uids     <- unlist(lapply(httr::content(res, as = "parsed")$results, 
                                 function(x) x$uid))
  
  # Create df of all versions
  if(!is.null(select_uids) ) uids <- uids[uids %in% select_uids]
  version.df    <- data.frame(version = "a", uid = "b")[-1,]
  
  # Obtain version name of each uid
  for(uid in uids) {
    asseturl <- paste0("https://", url, "/api/v2/assets/", assetid, "/versions/",uid)
    res           <- httr::GET(asseturl, 
                               httr::authenticate(username, password))
    httr::stop_for_status(res)
    version_name  <- httr::content(res, as = "parsed")$content$settings$version
    if(is.null(version_name)) version_name = as.character(NA)
    version.df = rbind(version.df, data.frame(version = version_name, uid = uid))
  }
  
  return(version.df)
}
