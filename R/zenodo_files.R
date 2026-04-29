
#' @export
DOWNLOAD_DEPOSITION_FILE = function(
    title,
    token,
    instance,
    filename,
    filepath,
    stop_for_status = FALSE,
    language = "en"
){

  msg = zenodo_messages(language)

  # Get latest published deposition
  deposition = GET_LATEST_PUBLISHED_DEPOSITION(title = title,
                                                token = token,
                                                instance = instance)

  if(is.null(deposition)) {

    # Deposition does not exist --> FALSE or stop
    msg = paste0(msg$Deposition, " '", title, "' ", msg$does_not_exist)
    if(stop_for_status) stop(msg) else message(msg)
    return(FALSE)


  } else {

    filenames = unlist(lapply(deposition$files, function(x) x$filename))

    if(!filename %in% filenames) {

      # File does not exist --> return FALSE or stop
      msg = paste0("      ", msg$File, " '", filename, "' ",
                        msg$does_not_exist_in_deposition, " '", title, "'")
      if(stop_for_status) stop(msg) else message(msg)
      return(FALSE)

    } else  {

      # File and deposition exist --> run request
      res = httr::GET(
        paste0(deposition$links$bucket, "/", filename),
        httr::add_headers(Authorization = paste("Bearer", token)),
        httr::write_disk(file.path(filepath), overwrite = TRUE)
      )
      if(stop_for_status) httr::stop_for_status(res)
      message(paste0("      ", msg$File, " '", filename,
                     "' ", msg$downloaded_in ," \n'", filepath, "'"))
      return(TRUE)
    }
  }
}







### -------------------- DOWNLOAD ALL DEPOSITION FILES --------------------- ###



#' @export
DOWNLOAD_ALL_DEPOSITION_FILES <- function(id, path, token, instance, language = "en") {



  msg = zenodo_messages(language)
  url = GET_BASE_URL(instance)

  res     = httr::GET(paste0(url, "/api/deposit/depositions/", id),
                query = list(access_token = token)
  )
  httr::stop_for_status(res)
  deposition   = httr::content(res, as = "parsed")
  filenames    = unlist(lapply(deposition$files,
                               function(x) x$filename))
  cat("\n", msg$downloading_files_from, deposition$title, "\n")
  for (filename in filenames) {
    cat(paste0("      ", msg$downloading," ", filename, "\n"))
    res = httr::GET(
      paste0(deposition$links$bucket, "/", filename),
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::write_disk(file.path(path, filename), overwrite = TRUE)
    )
    httr::stop_for_status(res)
  }
}







### ------------------ FUNCTION TO UPLOAD DATA TO ZENODO ------------------- ###




#' @export
UPLOAD_TO_DEPOSITION <- function(filepath,  name, id, token, instance, language = "en") {

  msg = zenodo_messages(language)

  if(!file.exists(filepath)) stop(msg$file_does_not_exist)

  url = GET_BASE_URL(instance)

  deposition  = GET_DEPOSITION(id, token, instance)
  bucket_link = deposition$links$bucket
  message("      ", msg$uploading_file,
          paste0("'",name,"'"),
          msg$to, " ",
          deposition$title)
  res = httr::PUT(
    paste0(bucket_link, "/", basename(name)),
    httr::add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/octet-stream"
    ),
    body = httr::upload_file(filepath)
  )

  httr::stop_for_status(res)
  deposition <- GET_DEPOSITION(id, token, instance)
  return(paste0(url, "/record/", id, "/files/", name))
}






### ----------------------------- DELETE FILE ------------------------------ ###




#' @export
DELETE_FILE <- function(filename, id, token, instance, language = "en"){

  msg = zenodo_messages(language)

  deposition <- GET_DEPOSITION(id, token, instance)
  filenames = unlist(lapply(deposition$files, function(x) x$filename))
  link      = unlist(lapply(deposition$files, function(x) x$links$self))
  for(i in which(filenames == filename)){

    cat("      ", msg$deleted_file,
        paste0("'",filename,"'"),
        msg$from,
        deposition$title, "\n")
    res <- httr::DELETE(link[i],
                  httr::add_headers(Authorization = paste("Bearer", token)))
    httr::stop_for_status(res)
  }
}










#' @export
LOAD_DEPOSITION_FILE <- function(
    title,
    token,
    instance,
    filename,
    read_function,
    stop_if_failure = FALSE,
    language = "en"){

  # Load multilingual messages
  msg <- zenodo_messages(language)


  # Create temporary directory
  now <- gsub(" ", "_", gsub("-", "", gsub(":", "", substr(Sys.time(),3,19))))
  dir.create(file.path(tempdir(), now))
  filepath <- file.path(tempdir(), now, filename)
  suppressMessages(
    success <- DOWNLOAD_DEPOSITION_FILE(title    = title,
                                        token    = token,
                                        instance = instance,
                                        filename = filename,
                                        filepath = filepath
    )
  )
  if(success) {
    return(read_function(filepath))
  } else {
    if(stop_if_failure) stop()
    return(NULL)
  }
}

