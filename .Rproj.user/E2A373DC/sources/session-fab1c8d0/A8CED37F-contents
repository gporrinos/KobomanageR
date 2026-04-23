

#' Download submissions from KoboToolbox
#'
#' Downloads all submissions (data records) from a KoboToolbox form (asset)
#' using the KoboToolbox v2 API. The function handles pagination automatically
#' and returns a complete list of submissions.
#'
#' If `assetid` is not provided, it is retrieved using the form `name`.
#'
#' @param url Character. KoboToolbox server URL (e.g., `"eu.kobotoolbox.org"`).
#' @param username Character. KoboToolbox username for authentication.
#' @param password Character. KoboToolbox password for authentication.
#' @param assetid Character. Kobo asset UID. Optional if `name` is provided.
#' @param name Character. Form (questionnaire) name used to retrieve `assetid`
#'   if not supplied.
#' @param language Character. Language code used for messages. Default is `"en"`.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates required inputs (`assetid` or `name` must be provided).
#'   \item Retrieves `assetid` from the Kobo API if only `name` is given.
#'   \item Sends GET requests to the KoboToolbox `/api/v2/assets/{assetid}/data/` endpoint.
#'   \item Automatically follows pagination via the `next` field.
#'   \item Aggregates all submission records into a single list.
#' }
#'
#' On success, a message is printed indicating the name of submissions downloaded.
#'
#' Common HTTP error codes:
#' \itemize{
#'   \item \strong{401 / 403}: Unauthorized access (invalid credentials or permissions)
#'   \item \strong{404}: Asset (form) not found
#' }
#'
#' @return
#' A list containing all submission records retrieved from the KoboToolbox API.
#'
#' @section Dependencies:
#' This function relies on:
#' \itemize{
#'   \item `kobo_api()` to retrieve asset metadata when `name` is used
#'   \item `MSSG()` for multilingual messages
#'   \item `httr` for HTTP requests
#' }
#'
#' @examples
#' \dontrun{
#' # Download by asset ID
#' data <- kobo_download(
#'   url = "eu.kobotoolbox.org",
#'   username = "user",
#'   password = "pass",
#'   assetid = "aBc123"
#' )
#'
#' # Download by form name
#' data <- kobo_download(
#'   url = "eu.kobotoolbox.org",
#'   username = "user",
#'   password = "pass",
#'   name = "My Form"
#' )
#' }
#'
#' @export
kobo_download <- function(url,
                          username,
                          password,
                          assetid = NULL,
                          name    = NULL,
                          language = "en") {
  
  
  # --------- Multilingual messages -------- #
  
  msg = MSSG(language)
  
  
  
  
  # ----------- Input validation ----------- #
  
  if(is.null(assetid) & is.null(name)) {
    stop(msg$missing_assetid_and_name)
  }
  
  
  
  # --- Obtain 'assetid' if not provided --- #
  
  if(is.null(assetid)){
    api     = kobo_api(url, username, password)
    assetid = api$assetid[which(api$name == name)]
  }
  
  
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
      cat(paste0("      ", 
                 msg$submissions_downloaded, 
                 if(!is.null(name)) paste0(" ",msg$from, " '", name, "'")
                 ),"\n")
    }, error = function(e){
      
      message("  ", switch(
        as.character(res$status_code),
        "404" = paste0(msg$questionnaire, " '", name, "' ", msg$not_found),
        "401" = paste0(msg$unauthorised),
        "403" = paste0(msg$unauthorised),
        
        paste("Unexpected error (HTTP", res$status_code, ")")
      ))
    }
  )
  return(output)}







#' Delete KoboToolbox submissions
#'
#' Deletes one or more submissions from a KoboToolbox form (asset).
#' Submissions can be specified either by their internal submission IDs
#' (`_id`) or by their UUIDs (`_uuid`).
#'
#' If `assetid` is not provided, it will be retrieved using the form
#' `name`. If `submission_id` is not provided, it will be derived from
#' the provided `submission_uuid`.
#'
#' @param assetid Character. KoboToolbox asset UID. Optional if `name` is provided.
#' @param name Character. Name of the form (questionnaire). Used to retrieve
#'   `assetid` if not supplied.
#' @param url Character. KoboToolbox server URL. Defaults to
#'   `"eu.kobotoolbox.org"`.
#' @param submission_id Character vector. One or more submission `_id` values.
#' @param submission_uuid Character vector. One or more submission `_uuid` values.
#'   Used only if `submission_id` is not provided.
#' @param language Character. Language code used for status and error messages.
#'   Default is `"en"`.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates required inputs.
#'   \item Retrieves the asset ID if only the form name is provided.
#'   \item Converts submission UUIDs to submission IDs if needed.
#'   \item Sends a DELETE request for each submission via the KoboToolbox API.
#'   \item Displays status messages for each deletion attempt.
#' }
#'
#' Successful deletions typically return HTTP status `204 No Content`.
#'
#' @return
#' This function does not return a value. It prints status messages
#' indicating the result of each deletion.
#'
#' @section Dependencies:
#' This function relies on the following helper functions:
#' \itemize{
#'   \item `kobo_api()` to retrieve asset metadata
#'   \item `kobo_download()` to fetch submissions
#'   \item `MSSG()` for multilingual messages
#'   \item `coalesce_if_null()` for safe value extraction
#' }
#'
#' @export
kobo_delete_submission <- function(assetid = NULL,
                                   name    = NULL,
                                   url     = "eu.kobotoolbox.org",
                                   submission_id = NULL,
                                   submission_uuid = NULL,
                                   language = "en"){
  
  
  
  # --------- Multilingual messages -------- #
  
  msg = MSSG(language)
  
  
  
  # ----------- Input validation ----------- #
  if(is.null(assetid) & is.null(name)) {
    stop(msg$missing_assetid_and_name)
  }
  if(is.null(submission_id) & is.null(submission_uuid)) {
    stop(msg$missing_id_and_uuid)
  }
  
  
  
  # --- Obtain 'assetid' if not provided --- #
  if(is.null(assetid)){
    api     = kobo_api(url, username, password)
    assetid = api$assetid[which(api$name == name)]
    if (length(assetid) == 0) {
      stop(paste0(msg$questionnaire, " '", name, "' ", msg$not_found))
    }
  }
  
  
  
  #  Obtain ids from uuids if not provided   #
  if(is.null(submission_id)){
    dat <- kobo_download(url      = url, 
                         assetid  = assetid,
                         username = username,
                         password = password, 
                         language = language
    )
    ids   = unlist( lapply( dat, function(x) coalesce_if_null(x$'_id') ) )
    uuids = unlist( lapply( dat, function(x) coalesce_if_null(x$'_uuid') ) )
    submission_id = ids[which(uuids %in% submission_uuid)]
    
  }
  
  # -------- Delete submissions  -------- #
  
  for(subm in submission_id){
    submission_url = paste0("https://", url,
                            "/api/v2/assets/",assetid,
                            "/submissions/", subm)
    
    res = httr::DELETE(url = submission_url, 
                       httr::authenticate(username, password))
    
    # Status messages
    message("  ", switch(
      as.character(res$status_code),
      
      "204" = paste0(msg$Submission, " '", subm, "' ", msg$deleted_successfully),
      "200" = paste0(msg$Submission, " '", subm, "' ", msg$deleted_successfully),
      "404" = paste0(msg$Submission, " '", subm, "' ", msg$not_found),
      "401" = paste0(msg$deletion_of, " '", subm, "' ", msg$unauthorised),
      
      paste("Unexpected error (HTTP", res$status_code, ")")
    )
    )
  }
}


