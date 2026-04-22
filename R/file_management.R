


#' @export
kobo_download <- function(url,
                         username,
                         password,
                         questionnaire_name,
                         language = "en") {
  api      <- kobo_api(url, username, password)
  assetid  <- api$asset[which(api$name == questionnaire_name)]
  asseturl <- paste0("https://", url, "/api/v2/assets/", assetid, "/data/")
  output   <- list()
  tryCatch(
    {
      repeat {
        res           <- httr::GET(asseturl, httr::authenticate(username, password))
        httr::stop_for_status(res)
        parsed        <- httr::content(res, as = "parsed")
        output        <- c(output, parsed$results)
        if (is.null(parsed$"next")) break
        asseturl <- parsed$"next"
      }
      cat(paste0("      ", MSSG(language)$downloaded_questionnaire, " '", questionnaire_name, "'"),"\n")
    }, error = function(e){
      if(httr::status_code(res) == 404)
        cat(paste0("      (!)  ",
                   MSSG(language)$questionnaire," '",
                   questionnaire_name, "' ",
                   MSSG(language)$does_not_exist),"\n")
      output = list()

    }
  )
  return(output)}


