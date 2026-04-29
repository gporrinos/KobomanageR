
################################################################################
####                          GET LATEST DATABASES                          ####
################################################################################




#' @export
append_dflist <- function(append_to,
                   append_from,
                   variables = NULL,
                   enforce_class_of = "destination"){

  if(!enforce_class_of %in% c("destination", "origin"))
    stop("\n'enforce_class_of' must be 'destionation' or 'origin'")

  all_datnames <- levels(as.factor(c(names(append_to),
                                     names(append_from))))


  all_data  <- lapply(
    all_datnames,
    function(datname){

      # If df named 'datname' does not exist in append_to, return "append_from"
      if(!datname %in% names(append_to)){
        output = append_from[[datname]]
        if(!is.null(variables))
          if(datname %in% names(variables)){
            output =enforce_class(output,
                                  lapply(variables[[datname]], function(x) x$class)
            )
          }
      } else {

        # If df named 'datname' does not exist in append_from, return append_to
        if(!datname %in% names(append_from)){
          output = append_to[[datname]]
          if(!is.null(variables))
            if(datname %in% names(variables)){
              output =enforce_class(data = output,
                                    class_list = lapply(variables[[datname]], function(x) x$class)
              )
            }
        } else {

          # If both exist, standardize column class
          to   = append_to[[datname]]
          from = append_from[[datname]]
          if(enforce_class_of == "destination")
            from   = standardise_class(
              template = to,
              data     = from)   else
                if(enforce_class_of == "origin")
                  to   = standardise_class(template = from,
                                           data     = to)

          # Exclude instances from "from" already in "to" and bind them
          from   = from[!from$uuid %in% to$uuid, ]
          output = dplyr::bind_rows(to, from)
        }
      }
      return(output)})

  names(all_data) = all_datnames
  return(all_data)

}








#' @export
load_local_data <-
  function(
    datname,
    local_data_directory,
    method,
    fileformat,
    csv_separator = NULL,
    class_list = NULL
  ){
    # VALIDATION
    if(length(datname) > 1)
      stop("datname length > 1")
    if(!method %in% c("GROUPED", "FULL"))
      stop("'method' must be 'GROUPED' or 'FULL'")
    # Write validation for df structure



    # --------------------- PART 1: METHOD == 'FULL'  --------------------- #

    if(method == "FULL"){
      # File path of local data
      filepath      = file.path(local_data_directory, paste0(datname, ".", fileformat))

      if(file.exists(filepath)){
        dat  = readDAT(sheetname     = datname,
                       filepath      = filepath,
                       fileformat    = fileformat,
                       csv_separator = csv_separator)
        dat <- enforce_class(data       = dat,
                                     class_list = class_list)
      } else {
        cat("file does not exist")
      }
    }


    # -------------------- PART 2: METHOD == 'GROUPED'  -------------------- #


    if(method == "GROUPED"){
      ### 1. GET DIRECTORY NAMES
      dirs <- list.dirs(local_data_directory,  full.names = FALSE)
      dirs <- dirs[!dirs == local_data_directory &  !dirs == ""]

      ### 2. LOAD AND COLLATE LOCAL FILES
      for(directory in dirs){
        path      = file.path(local_data_directory, directory)
        filepath  = file.path(local_data_directory, directory,
                              paste0(datname, ".", fileformat))
        if(file.exists(filepath)){

          # Load local file (partial)
          partial_dat <- readDAT(sheetname  = datname,
                                 filepath   = filepath,
                                 fileformat = fileformat,
                                 csv_separator = csv_separator)

          # Standardize class
          partial_dat <- enforce_class(data       = partial_dat,
                                       class_list = class_list)

          # Collate data bases
          if(!exists("dat", envir = environment(), inherits = FALSE)) {
            dat = partial_dat
          } else {
            partial_dat = standardise_class(
              template = dat,
              data     = partial_dat)
            dat = dplyr::bind_rows(dat, partial_dat)
          }
        }
      }
    }
    if(exists("dat", envir = environment(), inherits = FALSE))
      return(dat)  else    return(NULL)
  }













#' @export
get_local_file_paths  <- function(datnames,
                                  local_data_directory,
                                  method,
                                  fileformat) {


  # ---------------------------- VALIDATION ---------------------------- #


  if(!method %in% c("GROUPED", "FULL"))
    stop("'method' must be 'GROUPED' or 'FULL'")





  # ---------------------- CREATE EMPTY OUTPUT DF ---------------------- #


  all.filepaths = data.frame(path = as.character(),
                             filepath = as.character(),
                             dat = as.character(),
                             stringsAsFactors = FALSE)




  # --------------------- PART 1: METHOD == 'FULL'  --------------------- #

  if(method == "FULL"){
    for(datname in datnames){
      filepath      = file.path(local_data_directory, paste0(datname, ".", fileformat))
      if(file.exists(filepath))
        all.filepaths <- rbind(all.filepaths,
                               data.frame(path     = local_data_directory,
                                          filepath = filepath,
                                          dat      = datname)
        )
    }
  }


  # -------------------- PART 2: METHOD == 'GROUPED'  -------------------- #


  if(method == "GROUPED"){
    dirs <- list.dirs(local_data_directory,  full.names = FALSE)
    dirs <- dirs[!dirs == local_data_directory &  !dirs == ""]

    for(datname in datnames){

      for(directory in dirs){
        path      = file.path(local_data_directory, directory)
        filepath  = file.path(local_data_directory, directory,
                              paste0(datname, ".", fileformat))
        if(file.exists(filepath)){
          all.filepaths <- rbind(all.filepaths,
                                 data.frame(path     = path,
                                            filepath = filepath,
                                            dat      = datname)
          )
        }
      }
    }
  }
  return(all.filepaths)
}








#' @export
get_grouping_vector <- function(data,
                                grouping_variable,
                                nested = FALSE,
                                parent = NULL){
  if(nested && !grouping_variable %in% colnames(data)) {
    grouping_vector <- unlist(lapply(data$uuid, function(uuid) {
      temp = parent[which(parent$uuid == uuid),
                    which(colnames(parent) == grouping_variable)]
      if(length(temp) == 1) temp else NA
    }))
  } else {
    grouping_vector <- data[,which(colnames(data) == grouping_variable)]
  }
  return(grouping_vector)
}















#' @export
write_newdata <- function(data,
                          name,
                          fileformat,
                          csv_separator = NULL,
                          local_data_directory,
                          method,
                          nested = FALSE,
                          grouping_variable = NULL,
                          parent = NULL,
                          return.filepaths = TRUE){

  ##-------- VALIDATION ----------#

  if(!method %in% c("GROUPED", "FULL"))
    stop("'method' must be 'GROUPED' or 'FULL'")

  if(!file.exists(local_data_directory)) dir.create(local_data_directory)

  all.filepaths = data.frame(path = as.character(),
                             filepath = as.character(),
                             stringsAsFactors = FALSE)



  ##-------- 1. METHOD = FULL ----------#


  if(method == "FULL"){
    writeDAT(data = data,
             fileformat = fileformat,
             filepath   = file.path(local_data_directory,
                                    paste0(name, ".", fileformat)),
             csv_separator = csv_separator,
             sheetname     = name)
    all.filepaths = rbind(all.filepaths,
                          data.frame(path = local_data_directory,
                                     filepath = file.path(local_data_directory,
                                                          paste0(name, ".", fileformat))
                                     )
    )
  }



  ##-------- 2. METHOD = GROUPED ----------#

  if(method == "GROUPED"){
    grouping_vector <- get_grouping_vector(data = data,
                                           grouping_variable   = grouping_variable,
                                           nested = nested,
                                           parent = parent)

    out_dirs <- levels(as.factor(grouping_vector))
    if(length(which(is.na(grouping_vector))) > 0) {
      out_dirs <- c(out_dirs, "NA")
      grouping_vector[is.na(grouping_vector)] <- "NA"
    }

    for(directory in out_dirs){
      filename = paste0(name, ".", fileformat)
      path = file.path(local_data_directory, directory)
      if(!file.exists(path)) dir.create(path = path)
      writeDAT(data       = data,
               fileformat = fileformat,
               filepath   = file.path(path,
                                      paste0(name, ".", fileformat)),
               csv_separator = csv_separator,
               sheetname     = name)
      all.filepaths = rbind(all.filepaths,
                            data.frame(path = path,
                                       filepath = file.path(path,
                                                            paste0(name, ".", fileformat))
                            )
      )
    }
  }
  if(return.filepaths) return(all.filepaths)
}








#' @export
upload_dfs_to_deposition <-
  function(
    data,
    name,
    fileformat,
    csv_separator = NULL,
    method,
    nested = FALSE,
    grouping_variable = NULL,
    parent = NULL,
    append_group_name = FALSE,
    deposition_id,
    token,
    instance,
    language = "en"
  ){

  ##-------- VALIDATION ----------#

  if(!method %in% c("GROUPED", "FULL"))
    stop("'method' must be 'GROUPED' or 'FULL'")

  ## CHECK WHETHER DEPOSITION IS PUBLISHED, IF IT IS, RETURN AN ERROR

  ##-------- 1. METHOD = FULL ----------#


  if(method == "FULL"){
    filepath = file.path(tempdir(),
                         paste0(name, ".", fileformat))
    writeDAT(data = data,
             fileformat = fileformat,
             filepath   = filepath,
             csv_separator = csv_separator,
             sheetname     = name)
    UPLOAD_TO_DEPOSITION(filepath = filepath,
                         name     = paste0(name, ".", fileformat),
                         id       = deposition_id,
                         token    = token,
                         instance = instance,
                         language = language)
  }



  ##-------- 2. METHOD = GROUPED ----------#

  if(method == "GROUPED"){
    grouping_vector <- get_grouping_vector(data = data,
                                           grouping_variable   = grouping_variable,
                                           nested = nested,
                                           parent = parent)

    out_dirs <- levels(as.factor(grouping_vector))
    if(length(which(is.na(grouping_vector))) > 0) {
      out_dirs <- c(out_dirs, "NA")
      grouping_vector[is.na(grouping_vector)] <- "NA"
    }

    for(directory in out_dirs){
      filename = paste0(name, ".", fileformat)
      path = file.path(tempdir(), directory)
      if(!file.exists(path)) dir.create(path = path)
      filepath = file.path(path,
                paste0(name, ".", fileformat))
      writeDAT(data       = data,
               fileformat = fileformat,
               filepath   = filepath,
               csv_separator = csv_separator,
               sheetname     = name)
      nam = paste0(name, if(append_group_name) paste0("_", directory), ".", fileformat)
      UPLOAD_TO_DEPOSITION(filepath = filepath,
                           name     = nam,
                           id       = deposition_id,
                           token    = token,
                           instance = instance,
                           language = language)
    }
  }
}
