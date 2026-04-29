




### ---------------------- GET BASE URL ---------------------- ###

#' @export
GET_BASE_URL <- function(instance) {
  switch(instance,
         sandbox = "https://sandbox.zenodo.org",
         zenodo  = "https://zenodo.org",
         stop("Invalid instance"))
}








### ---------------------- ALL DEPOSITION INFORMATION ---------------------- ###
#' @export
ALL_DEPOSITIONS <- function(token, instance) {

  url = GET_BASE_URL(instance)

  page <- 1
  all_depositions <- list()
  repeat {
    res <- httr::GET(paste0(url, "/api/deposit/depositions"),
               query = list(access_token = token,
                            page = page,
                            size = 100
               ))
    httr::stop_for_status(res)
    depositions <- httr::content(res, as = "parsed")

    if (length(depositions) == 0) break
    all_depositions <- c(all_depositions, depositions)
    page <- page + 1
  }
  return(all_depositions)}










### ----------------- RETRIEVE DEPOSITION ID BASED ON NAME ----------------- ###

#' @export
RETRIEVE_DEPOSITION_ID  <- function(title, token, instance) {

  all_dep       = ALL_DEPOSITIONS(token,instance)
  dep_names     = unlist(lapply(all_dep, function(x) x$title))
  relevant      = all_dep[which(dep_names == title)]
  id = unlist(lapply(relevant, function(x) x$id))
  return(id)}






### ------------------ GET DEPOSITION INFORMATION FROM ID ------------------ ###

#' @export
GET_DEPOSITION <- function(id, token, instance) {

  url = GET_BASE_URL(instance)

  res = httr::GET(
    paste0(url, "/api/deposit/depositions/", id),
    query = list(access_token = token)
  )
  httr::stop_for_status(res)
  return(httr::content(res))
}




