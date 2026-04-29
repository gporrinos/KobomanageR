

#' @export
append_and_update <- function(
    local_data_directory,
    variables,
    databases,
    config = list(method= "FULL",
                  grouping_variable = NULL,
                  fileformat = "csv",
                  csv_separator = NULL),
    last_session = list(method= NULL,
                        grouping_variable = NULL,
                        fileformat = NULL,
                        csv_separator = NULL),
    zenodo = NULL,
    token,
    repository_name,
    cache,
    update_server_data = FALSE,
    language = "en") {




  msg <- KobomanageR_msg(language)

  # If 'last_session' is empty, substitute by current config parameters
  for(x in c("method",  "fileformat",  "csv_separator",  "grouping_variable"))
    if(is.null(last_session)) last_session = config[[x]]

  # Have configuration parameters changed?
  is_conf_same <- TRUE
  for(x in c("method",  "fileformat",  "csv_separator",  "grouping_variable"))
    is_conf_same = is_conf_same & !coalesce_to(config[[x]])  == coalesce_to(last_session[[x]])




  ### ----------- 1. GET DATA FROM SERVER  ----------- #

  # Download "data.sqlite" file
  now   <- gsub(" ", "_", gsub("-", "", gsub(":", "", substr(Sys.time(), 3, 19) )))
  dir.create(file.path(tempdir(), now))
  all_data_filepath <- file.path(tempdir(), now, "data.sqlite")
  DOWNLOAD_DEPOSITION_FILE(title = repository_name,
                           token    = token,
                           instance = zenodo[["instance"]],
                           filename = "data.sqlite",
                           filepath = all_data_filepath)

  # Read data in "data.sqlite" (creates file if it does not exist)
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        all_data_filepath)
  all_data <- lapply(DBI::dbListTables(con),
                     function(datname)  DBI::dbReadTable(conn = con,
                                                         name = datname))
  names(all_data) <- DBI::dbListTables(con)

  # Remove RAW data
  all_data <- all_data[!grepl("_RAW",names(all_data))]

  # Disconnect file
  DBI::dbDisconnect(con)



  ### ----------- 2. GET LOCAL DATA  ----------- #


  datnames <- levels(as.factor(c(names(all_data), names(databases))))

  all_data_local <- lapply(datnames, function(datname)
    load_local_data(datname               = datname,
                    local_data_directory  = config[["local_data_directory"]],
                    method                = last_session[["method"]],
                    fileformat            = last_session[["fileformat"]],
                    csv_separator         = last_session[["csv_separator"]],
                    class_list            = lapply(variables[[datname]], function(x) x$class)
    )
  )
  names(all_data_local) = datnames
  local_data_filepaths <- get_local_file_paths(datnames = datnames,
                                                   local_data_directory = config[["local_data_directory"]],
                                                   method = last_session[["method"]],
                                                   fileformat = last_session[["fileformat"]])


  # Remove empty dfs from both df lists
  for(datname in names(all_data_local))
    if(is.null(all_data_local[[datname]]))
      all_data_local = all_data_local[!names(all_data_local) == datname] else
        if(nrow(all_data_local[[datname]]) == 0)
          all_data_local = all_data_local[!names(all_data_local) == datname]


  for(datname in names(all_data))
    if(is.null(all_data[[datname]]))
      all_data = all_data[!names(all_data) == datname] else
        if(nrow(all_data[[datname]]) == 0)
          all_data = all_data[!names(all_data) == datname]




  ### ----------- 3. ARE THERE NEW SUBMISSIONS  ----------- #

  newdat <- datnames[
    unlist(lapply(
      datnames,
      function(datname)
        if(is.null(all_data_local[[datname]]) &
           !is.null(all_data[[datname]])) TRUE else
             length(
               which(
                 !all_data_local[[datname]]$uuid %in%
                   all_data[[datname]]$uuid)
             ) > 0
    ))
  ]




  ### ----------- 2. GET DATA WITH NEW SUBMISSIONS  ----------- #



  if(length(newdat) == 0  &
     !update_server_data &
     !is_conf_same) {
    message("There are no new submissions")
    return(all_data)
  } else {


    ### COLLATE NEW DATA
    all_data <- append_dflist(append_from = all_data,
                              append_to   = all_data_local,
                              variables   = variables,
                              enforce_class_of = "origin")



    ### UPDATE "all_data" TO SERVER, IF RELEVANT

    if(update_server_data){

      # Create new "data.sqlite" connection
      con <- DBI::dbConnect(RSQLite::SQLite(), all_data_filepath)

      # Write collated data in "data.sqlite"
      for (datname in names(all_data)) {
        if(!is.null(all_data[[datname]])){
          DBI::dbWriteTable(conn = con,
                            name = datname,
                            value = all_data[[datname]],
                            overwrite = TRUE,
                            row.names = FALSE)
        }}

      # Disconnect file
      DBI::dbDisconnect(con)

      # Create new deposition
      deposition <-
        CREATE_NEW_VERSION_OR_DEPOSITION(title       = repository_name,
                                         description = zenodo[["description"]],
                                         creators    = zenodo[["creators"]],
                                         token       = token,
                                         instance    = zenodo[["instance"]],
                                         language    = language)

      # Upload file "data.sqlite" to deposition
      UPLOAD_TO_DEPOSITION(filepath = all_data_filepath,
                           name     = "data.sqlite",
                           id       = deposition$id,
                           token    = token,
                           instance = zenodo[["instance"]],
                           language = language)
    }

    ### CREATE EMPTY DF TO APPEND FILEPATHS (DELETES UNUSED DIRECTORIES LATER)
    all_filepaths <- data.frame(path = as.character(),
                                filepath = as.character())
i= 1
    ### WRITE NEW DATA, OBTAIN FILE PATHS OF NEW DATA FILES AND UPLOAD TO SERVER
    for(i in 1:length(all_data)){

      # Get datname and dat
      datname = names(all_data)[i]
      dat     = all_data[[i]]


      # Get parent
      positions = which(
        lapply(databases, function(dat) dat$parent) == databases[[datname]]$parent &
          lapply(databases, function(dat) dat$dat)   %in% names(all_data) &
          lapply(databases, function(dat) dat$location) == "parent"
      )
      if(length(positions) == 0)
        parent = NULL else
          parent = all_data[[
            unlist(lapply(databases, function(dat) dat$dat)[positions])[1]
          ]]

      # Remove local files of datname
      file.remove(
        local_data_filepaths$filepath[local_data_filepaths$dat == datname]
      )


      # Create files of new data and obtain filepath of local file
      filepaths <- write_newdata(
        data                 = dat,
        name                 = datname,
        fileformat           = config[["fileformat"]],
        csv_separator        = config[["csv_separator"]],
        local_data_directory = config[["local_data_directory"]],
        method               = config[["method"]],
        nested               = databases[[datname]]$location == "nested",
        grouping_variable    = config[["grouping_variable"]],
        parent               = parent,
        return.filepaths     = TRUE)
      all_filepaths <- rbind(all_filepaths, filepaths)


      # Update to server, if relevant
      if(update_server_data){
        for(filepath in filepaths$filepath){
          if(config[["method"]] == "GROUPED") {
            filename = paste(basename(dirname(filepath)),
                             basename(filepath),
                             sep = "_")
          } else {
            filename = basename(filepath)
          }
          UPLOAD_TO_DEPOSITION(filepath = filepath,
                               name     = filename,
                               id       = deposition$id,
                               token    = token,
                               instance = zenodo[["instance"]],
                               language = language
                               )
          }
      }

    }


    # Publish deposition
    if(update_server_data){
      message("\n", msg$publishing_data_repo)
      deposition <- PUBLISH_DEPOSITION(id       = deposition$id,
                                       token    = token,
                                       instance = zenodo[["instance"]])
    }
  }



  ### ----------- 5. REMOVE UNUSED DIRECTORIES  ----------- #



  rm_dirs <- levels(as.factor(local_data_filepaths$path[!local_data_filepaths$path %in%
                                              all_filepaths$path]))
  remove_files(path = rm_dirs,
               recursive = TRUE)




  ### ----------- 6. SAVE CREDENTIALS IN CACHE  ----------- #


  saveRDS(list(fileformat        = config[["fileformat"]],
               method            = config[["method"]],
               grouping_variable = config[["grouping_variable"]],
               csv_separator     = config[["csv_separator"]]),
          file = file.path(cache, "settings.rds"))


  return(all_data)

}













































