
#' @export
kobo_projects <- function(url, username, password) {
  url <- paste0("https://", url, "/api/v2/assets.json")
  res     <- httr::GET(url,
                 httr::authenticate(user     = username,
                              password = password))
  httr::stop_for_status(res)
  temp = lapply(httr::content(res, as= "parsed")$results, function(res) {
    data.frame(name         = res$name,
               assetid      = res$uid,
               active       = res$deployment__active,
               submissions  = res$deployment__submission_count,
               owner        = res$owner__username,
               date_created = res$date_created,
               date_modified = res$date_modified,
               URL          = res$url)
  })
  api =   data.frame(name          = NA,
                     assetid       = NA,
                     active        = NA,
                     submissions   = NA,
                     owner         = NA,
                     date_created  = NA,
                     date_modified = NA,
                     URL           = NA)[-1,]
  for(x in temp) api = rbind(api, x)
  return(api)}

