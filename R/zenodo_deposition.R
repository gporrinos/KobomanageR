

### ------------------------ CREATE NEW DEPOSITION ------------------------- ###

#' @export
CREATE_DEPOSITION <- function(title,
                              description,
                              creators,
                              token,
                              instance,
                              restricted = TRUE,
                              language   = "en") {

  msg = zenodo_messages(language)
  url = GET_BASE_URL(instance)

  metadata = list(
    title = title,
    upload_type = "dataset",
    description = description,
    creators = creators,
    publication_date = as.character(Sys.Date())
  )
  if(restricted){
    metadata = c(metadata,
                 list(access_right      = "restricted",
                      access_conditions = "Available upon request"))
  }

  # Create deposition
  res <- httr::POST(
    paste0(url, "/api/deposit/depositions"),
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "json",
    body = list(metadata = metadata)
  )

  # If error, stop
  httr::stop_for_status(res)

  # Wait until Zenodo has finished publishing deposition
  deposition <- WAIT_FOR_DEPOSITION(id       = httr::content(res)$id,
                                    token    = token,
                                    instance = instance)

  # Return completion message
  message(paste0(msg$created_repository, " '",title, "'", "\n"))

  # Return deposition information
  return(deposition)
}









### ------------------------ NEW DEPOSITION VERSION ------------------------ ###

#' @export
NEW_DEPOSITION_VERSION <- function(id,
                                   token,
                                   instance,
                                   restricted = TRUE,
                                   language = "en") {

  msg = zenodo_messages(language)
  url = GET_BASE_URL(instance)

  deposition  = GET_DEPOSITION(id, token, instance)
  title       = deposition$title
  creators    = deposition$metadata$creators
  description = deposition$metadata$description
  res <- httr::POST(
    paste0(paste0(url, "/api/deposit/depositions"), "/", id, "/actions/newversion"),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  httr::stop_for_status(res)
  deposition <- httr::content(res, as = "parsed")
  metadata = list(
    title = title,
    upload_type = "dataset",
    description = description,
    creators = creators,
    publication_date = as.character(Sys.Date())
  )
  if(restricted){
    metadata = c(metadata,
                 list(access_right      = "restricted",
                      access_conditions = "Available upon request"))
  }

  # Update metadata
  httr::PUT(
    paste0(url, "/api/deposit/depositions/", deposition$id),
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "json",
    body = list(metadata = metadata)
  )

  # Wait until Zenodo has finished publishing deposition
  deposition <- WAIT_FOR_DEPOSITION(id       = httr::content(res)$id,
                                    token    = token,
                                    instance = instance)

  # Return completion message
  message(paste0(msg$created_new_repository_version," '",
             deposition$title,
             "'", "\n"))
  return(deposition)
}







### ----------------------------- DELETE DRAFT ----------------------------- ###

#' @export
DELETE_DEPOSITION_DRAFT <- function(draft_id = NULL, draft_link, token, instance) {

  url = GET_BASE_URL(instance)

  if(is.null(draft_id)) {
    res <- httr::GET(draft_link, query = (list(access_token = token)))
    httr::stop_for_status(res)
    draft_id = httr::content(res, as = "parsed")$id
  }
  res <- httr::DELETE(
    paste0(url, "/api/deposit/depositions/", draft_id),
    query = list(access_token = token)
  )
  httr::stop_for_status(res)

  # Wait until draft deletion is confirmed
  Sys.sleep(3)
  repeat {
    tmp <- try(GET_DEPOSITION(draft_id, token, instance), silent = TRUE)
    if(inherits(tmp, "try-error") || is.null(tmp)) break
    Sys.sleep(1)
  }
}







### ---- CREATE NEW VERSION OR DEPOSITION AND DELETE UNPUBLISHED DRAFTS ---- ###



#' @export
CREATE_NEW_VERSION_OR_DEPOSITION <- function(title,
                                             description,
                                             creators,
                                             token,
                                             instance,
                                             restricted = TRUE,
                                             language = "en") {

  msg = zenodo_messages(language)

  #  1. ZENODO ACCOUNT INFORMATION
  zenodo_account_info = ALL_DEPOSITIONS(token,instance)
  zenodo_depo_names   = unlist(lapply(zenodo_account_info, function(x) x$title))
  deposition_exists   = length(which(zenodo_depo_names == title))
  deposition_exists   = if(deposition_exists == 0) FALSE else TRUE



  if(!deposition_exists){
    # 2. IF DEPOSITION DOES NOT EXIST, CREATE IT
    new_deposition = CREATE_DEPOSITION(title       = title,
                                       description = description,
                                       creators    = creators,
                                       token       = token,
                                       instance    = instance,
                                       restricted  = restricted,
                                       language    = language)
  } else {

    # 3. IF DEPOSITION(S) EXIST(S), RETRIEVE DEPOSITION ID
    id <- RETRIEVE_DEPOSITION_ID(title, token, instance)

    # 4. DELETE PREVIOUS UNFINISHED DRAFTS
    #    (This prevents duplication from aborted runs and will delete unpublished records)
    for(x in id){
      deposition_temp <- GET_DEPOSITION(x, token, instance)
      if(!deposition_temp$submitted){
        DELETE_DEPOSITION_DRAFT(draft_link = deposition_temp$links$latest_draft,
                                token = token, instance = instance)
      }
    }

    # 5. RETRIEVE DEPOSITION IDs AGAIN
    id <- RETRIEVE_DEPOSITION_ID(title, token, instance)

    # 6. CREATE NEW DEPOSITION OR SUBMISSION
    # If there were no published records, create new one
    if(length(id) == 0)
      new_deposition = CREATE_DEPOSITION(title       = title,
                                         description = description,
                                         creators    = creators,
                                         token       = token,
                                         instance    = instance,
                                         restricted  = restricted,
                                         language    = language)

    # If there was one published record, create new version
    if(length(id) == 1)
      new_deposition = NEW_DEPOSITION_VERSION(token       = token,
                                              id          = id,
                                              instance    = instance,
                                              restricted  = restricted,
                                              language    = language)

    # If there is more than one published record, generate an error
    if(length(id) > 1)
      stop(paste0(msg$more_than_one_deposition, " ", title,
                  msg$exists_en,".\n",
                  msg$check_server_for_duplicates))

  }
  return(new_deposition)
}






### ------------------- GET LATEST PUBLISHED VERSION ------------------- ###

#' @export
GET_LATEST_PUBLISHED_DEPOSITION <- function(title, token, instance,
                                            language = "en") {

  msg <- zenodo_messages(language)

  #  1. ZENODO ACCOUNT INFORMATION
  zenodo_account_info = ALL_DEPOSITIONS(token,instance)
  zenodo_depo_names   = unlist(lapply(zenodo_account_info, function(x) x$title))
  deposition_exists   = length(which(zenodo_depo_names == title))
  deposition_exists   = if(deposition_exists == 0) FALSE else TRUE


  if(!deposition_exists){
    # 2. IF DEPOSITION DOES NOT EXIST, RETURN NULL
    new_deposition = NULL
  } else {

    # 3. IF DEPOSITION(S) EXIST(S), RETRIEVE DEPOSITION ID
    id <- RETRIEVE_DEPOSITION_ID(title, token, instance)

    # 4. DELETE PREVIOUS UNFINISHED DRAFTS
    #    (This prevents duplication from aborted runs and will delete unpublished records)
    for(x in id){
      deposition_temp <- GET_DEPOSITION(x, token, instance)
      if(!deposition_temp$submitted){
        DELETE_DEPOSITION_DRAFT(draft_link = deposition_temp$links$latest_draft,
                                token = token, instance = instance)
      }
    }

    # 5. RETRIEVE DEPOSITION IDs AGAIN
    id <- RETRIEVE_DEPOSITION_ID(title, token, instance)

    # 6. GET DEPOSITION INFORMATION, IF RELEVANT
    # If there were no published records, return NULL
    if(length(id) == 0)
      new_deposition = NULL

    # If there was one published record, create new version
    if(length(id) == 1)
      new_deposition = GET_DEPOSITION(id, token, instance)

    # If there is more than one published record, generate an error
    if(length(id) > 1)
      stop(paste0(msg$more_than_one_deposition, " ", title, " ",
                  msg$exists_en,".\n",
                  msg$check_server_for_duplicates))

  }
  return(new_deposition)
}








### -------------------------- PUBLISH DEPOSITION -------------------------- ###



#' @export
PUBLISH_DEPOSITION <- function(id, token, instance) {

  url = GET_BASE_URL(instance)

  res <- httr::POST(
    paste0(url, "/api/deposit/depositions/", id,
           "/actions/publish"),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  httr::stop_for_status(res)
  WAIT_FOR_PUBLICATION(id, token, instance)
}

