

#' Download and Process New KoboToolbox Submissions
#'
#' Downloads new submissions from KoboToolbox forms, processes them into
#' structured data frames, handles attachments, appends to existing data,
#' and uploads everything to a Zenodo deposition. Returns all submissions
#' and processed data.
#'
#' @param kobo A named list with KoboToolbox connection details:
#'   \describe{
#'     \item{url}{KoboToolbox server URL (without \code{https://})}
#'     \item{username}{KoboToolbox username}
#'     \item{password}{KoboToolbox password}
#'   }
#' @param repository_name Character. Name of the Zenodo deposition repository.
#' @param repository_location Character. Storage backend. Currently supports
#'   \code{"zenodo"}.
#' @param forms A named list of form definitions. Each element must contain:
#'   \describe{
#'     \item{name}{KoboToolbox form name}
#'     \item{parent}{Parent identifier used to group submissions}
#'   }
#' @param databases A named list of database definitions. Each element must contain:
#'   \describe{
#'     \item{parent}{Name of the parent form}
#'     \item{dat}{Name of the output data frame}
#'     \item{location}{Either \code{"parent"} or \code{"nested"}}
#'     \item{nestedname}{Name of the nested field (only used if \code{location = "nested"})}
#'     \item{version}{Form version to include. Use \code{NA} to include all.}
#'     \item{exclude_version}{Form version to exclude. Use \code{NA} to exclude none.}
#'   }
#' @param variables A named list of variable definitions, where top-level names
#'   correspond to database names (matching those defined in \code{databases}).
#'   Each element is itself a named list of variables, where names are variable
#'   names and each variable is a named list with the following fields:
#'   \describe{
#'     \item{class}{R class to enforce on the variable (e.g. \code{"character"},
#'       \code{"numeric"}, \code{"Date"}).}
#'     \item{char_replacement}{Logical. Whether to apply character replacement
#'       on this variable.}
#'     \item{pattern}{Semicolon-separated string of pattern names to replace.
#'       Each name must refer to a character vector defined in the function
#'       (e.g. \code{"dash"}, \code{"dot"}, \code{"space"}, \code{"linebreak"}).}
#'     \item{replacement}{Semicolon-separated string of replacement names.
#'       If a single value is provided it is recycled across all patterns.}
#'     \item{is_attachment}{Logical. Whether this variable contains a media
#'       attachment filename that should be linked to its Zenodo URL.}
#'   }
#'   Example:
#'   \preformatted{
#'   variables <- list(
#'     sample = list(
#'       species = list(
#'         class            = "character",
#'         char_replacement = TRUE,
#'         pattern          = "dash;space",
#'         replacement      = "dot",
#'         is_attachment    = FALSE
#'       ),
#'       photo = list(
#'         class            = "character",
#'         char_replacement = FALSE,
#'         pattern          = NA,
#'         replacement      = NA,
#'         is_attachment    = TRUE
#'       )
#'     )
#'   )}#' @param config A named list of output file configuration options. Defaults to
#'   \code{list(fileformat = "csv", csv_separator = ",")}. Supported fields:
#'   \describe{
#'     \item{fileformat}{Currently supports: \code{"csv"}, \code{"xlsx"}}
#'     \item{csv_separator}{Column separator for CSV output: \code{","}, \code{";"}}
#'   }
#' @param cleaning_function Optional function applied to the processed data before
#'   uploading. Must accept and return a named list of data frames. Defaults to
#'   \code{NULL}.
#' @param zenodo A named list with Zenodo configuration:
#'   \describe{
#'     \item{instance}{Either \code{"zenodo"} or \code{"sandbox"}}
#'     \item{description}{Deposition description}
#'     \item{creators}{List of creator metadata}
#'   }
#' @param token Character. Zenodo API token for authentication.
#' @param language Character. Language code for interface messages. One of
#'   \code{"en"}, \code{"pt"}, \code{"es"}. Defaults to \code{"en"}.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{submissions}{All submissions (old + new) as a named list of raw JSON lists.}
#'     \item{data}{All processed data (old + new) as a named list of data frames.}
#'   }
#'
#' @details
#' The function proceeds through the following steps:
#' \enumerate{
#'   \item Downloads previous submissions from Zenodo (\code{submissions.json}).
#'   \item Downloads new submissions from KoboToolbox, excluding previously
#'     processed UUIDs.
#'   \item Processes new submissions into data frames, handling nested data,
#'     form versions, variable selection, class enforcement, and character
#'     replacement.
#'   \item Downloads and uploads media attachments to a separate Zenodo
#'     deposition and stores their links in the data.
#'   \item Applies \code{cleaning_function} if provided.
#'   \item Downloads the existing \code{data.sqlite} from Zenodo, appends new
#'     data, and re-uploads it along with \code{submissions.json} and
#'     per-database files in the configured format.
#'   \item Publishes both the data and attachments depositions on Zenodo.
#' }
#'
#' If no new submissions are found, the function returns the existing data
#' from the previous Zenodo deposition without uploading anything.
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr GET DELETE authenticate add_headers stop_for_status write_disk
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbReadTable dbWriteTable
#' @importFrom RSQLite SQLite
#' @importFrom stats setNames
#'
#' @seealso
#' Internal functions used:
#' \code{\link{KobomanageR_msg}},
#' \code{\link{kobo_download}},
#' \code{\link{kobo_version_names}},
#' \code{\link{kobo_projects}},
#' \code{\link{kobo_survey}},
#' \code{\link{coalesce_to}},
#' \code{\link{enforce_class}},
#' \code{\link{create_new_var_after}},
#' \code{\link{append_dflist}},
#' \code{\link{writeDAT}},
#' \code{\link{LOAD_DEPOSITION_FILE}},
#' \code{\link{DOWNLOAD_DEPOSITION_FILE}},
#' \code{\link{UPLOAD_TO_DEPOSITION}},
#' \code{\link{CREATE_NEW_VERSION_OR_DEPOSITION}},
#' \code{\link{PUBLISH_DEPOSITION}}
#'
#'
#'
#' @export
download_new_submissions <- function(kobo,
                                     repository_name,
                                     repository_location,
                                     forms,
                                     databases,
                                     variables,
                                     config = list(fileformat = "csv",
                                                   csv_separator = ","),
                                     cleaning_function = NULL,
                                     zenodo = NULL,
                                     token  = NULL,
                                     language = "en"){






  msg <- KobomanageR_msg(language)
  datetime_lab =  gsub(" ", "_", gsub("-", "", gsub(":", "", substr(Sys.time(),3,19))))




  #### -------------------- DOWNLOAD PREVIOUS DATA -------------------- ####


  if(repository_location == "zenodo"){
    old_submissions <-  LOAD_DEPOSITION_FILE(title = repository_name,
                                             token = token,
                                             instance = zenodo[["instance"]],
                                             filename = "submissions.json",
                                             read_function = function(x) {
                                               jsonlite::fromJSON(x, simplifyVector = FALSE)
                                             }
    )
  }




  ################################################################################
  ####                     DOWNLOAD AND PROCESS NEW DATA                      ####
  ################################################################################








  ### ----------------- STEP 2: DOWNLOAD QUESTIONNAIRE DATA ------------------ ###




  # 1. DOWNLOAD ALL QUESTIONNAIRE DATA AND EXCLUDE PREVIOUS INSTANCES, IF ANY

  message("\n", msg$downloading_new_submissions,"\n")
  new_submissions <- lapply(forms, function(form){
    # Download new data
    new_sub = kobo_download(url      = kobo[["url"]],
                            username = kobo[["username"]],
                            password = kobo[["password"]],
                            name     = form$name,
                            language = language)

    # Exclude previous submissions
    old_sub      = old_submissions[[form$parent]]
    old_sub_uuid = if(is.null(old_sub))
      "una pachanguita mu rica" else
        unlist(lapply(old_sub, function(x) x$"_uuid"))
    new_sub_uuid = unlist(lapply(new_sub, function(x) x$"_uuid"))
    new_sub      = new_sub[!new_sub_uuid %in% old_sub_uuid]
    return(new_sub)
  })

  # Rename forms in "new submissions" list
  new_submissions_nam <- unlist(lapply(forms, function(x) {
    output        = unname(x$parent)
    names(output) = NULL
    return(output)
  }))
  names(new_submissions) <- unname(new_submissions_nam)
  rm(new_submissions_nam)




  ### ------------------ STEP 3: PROCESS QUESTIONNAIRE DATA ------------------ ###

  message("\n",msg$Processing_new_submissions,"\n")
  new_data <- lapply(databases, function(dat_info){


    ### 1. FETCH PARENT DATA
    parentname      = dat_info[["parent"]]
    parent          = new_submissions[[parentname]]

    if(length(parent) == 0) {
      message(paste0("      ", msg$no_new_submissions," ", msg$of, " '", dat_info[["dat"]], "'"))

    } else {


      ### 2. FETCH DATA SPECIFICATIONS
      location        = dat_info[["location"]]
      nestedname      = dat_info[["nestedname"]]
      datname         = dat_info[["dat"]]
      version         = dat_info[["version"]]
      exclude_version = dat_info[["exclude_version"]]
      form_title      = forms[[parentname]]$name



      ### 3. HANDLE VERSIONS, if relevant
      if(!is.na(version) | !is.na(exclude_version)) {

        # 3.1 Get existing versions in new submissions
        uids_in_form = unlist(lapply(parent, function(x) x$"__version__"))

        # 3.1 Obtain version names of form versions (relevant versions only)
        v  = kobo_version_names(
          url         = kobo[["url"]],
          username    = kobo[["username"]],
          password    = kobo[["password"]],
          name        = form_title,
          uids        = uids_in_form)
        v = v[v$uid %in% uids_in_form, ]


        # 3.2 Select relevant iuds
        if(is.na(version)) {
          uids = v$uid[!v$version == exclude_version]
        } else {
          if(is.na(exclude_version)) {
            uids = v$uid[v$version == version]
          } else {
            uids = v$uid[v$version == version &
                           !v$version == exclude_version]
          }
        }


        # 3.3 Select only relevant submissions
        parent      <- parent[uids_in_form %in% uids]


      }




      # 4. CHECK WHETHER SUBMISSIONS EXISTS AFTER SELECTING ONLY RELEVANT VERSIONS
      if(length(parent) == 0){
        message(paste0("      ", msg$no_new_submissions," ", msg$of, " '", datname, "'"))

      } else {


        # 4. PROCESS DATABASE
        message(paste0("      ", msg$Processing, " '", datname, "'"))

        # If nested, create a separate list, fetching uuid and id from parent
        if(location == "nested"){
          nested <- lapply(parent, function(instance) {
            if(nestedname %in% names(instance)){
              output       = instance[which(names(instance) == nestedname)][[1]]
              for(i in 1:length(output)) output[[i]]$"_uuid" = instance$"_uuid"
              for(i in 1:length(output)) output[[i]]$"_id" = instance$"_id"
              return(output)}
          })
          nested <- unlist(nested, recursive = FALSE) # Flatten list
        }


        # Select data from nested or parent data based on 'location' (i.e., nested or parent)
        dat <- get(location)

        if(length(dat) > 0){
          # Nested list to data frame via "jsonlite" package
          dat <- jsonlite::fromJSON(jsonlite::toJSON(dat,
                                                     auto_unbox = TRUE,
                                                     pretty = TRUE,
                                                     encoding = "UTF-8"),
                                    flatten = TRUE)


          # Remove group names from variables
          pos           = unlist(lapply(colnames(dat),  function(nam)  max(gregexpr("/",nam)[[1]])))
          colnames(dat) = substr(colnames(dat),  pos+1, nchar(colnames(dat)))


          # Create list of all variable names (including uuid and id)
          varnames      = names(variables[[datname]])
          varnames      = c("_uuid","_id", varnames)


          # Add questionnaire variables not in the data frame
          for(varname in varnames){
            dat$temp = NA
            colnames(dat)[which(colnames(dat) == "temp")] = varname
          }


          # Select only relevant variables
          dat           = dat[,varnames]


          # Add date_time tag
          dat$data_descarga = datetime_lab


          # Handle uuid and id
          colnames(dat)[colnames(dat) == "_uuid"] = "uuid"
          colnames(dat)[colnames(dat) == "_id"]   = "id"
          dat$uuid  = as.character(dat$uuid)
          dat$id    = as.character(dat$id)


          # Standardize class of data frame columns
          dat = enforce_class(data = dat,
                              class_list = lapply(variables[[datname]],
                                                  function(x) x$class))


          # Character replacement
          dash  = c("-")
          dot   = c(".")
          space = c(" ")
          linebreak = c("\n", "\r")
          for(varname in colnames(dat)){
            var_info         <- variables[[datname]][[varname]]
            if(!is.null(var_info[["char_replacement"]]))
              if(var_info[["char_replacement"]]) {

                patterns       = strsplit(var_info[["pattern"]],";")[[1]]
                replacements   = strsplit(var_info[["replacement"]],";")[[1]]
                if(length(replacements) == 1 & length(patterns) > 1)
                  replacements = rep(replacements, length(patterns))
                for(i in 1:length(patterns))
                  for(pattern in get(patterns[i]))
                    dat[,varname] = gsub(pattern     = pattern,
                                         replacement = get(replacements[i])[1],
                                         x           = dat[,varname],
                                         fixed       = TRUE)
              }
          }
          return(dat)
        }
      }
    }
  })


  # Rename forms in "new submissions" list
  new_data_nam <- unlist(lapply(databases, function(x) {
    output        = unname(x$dat)
    names(output) = NULL
    return(output)
  }))
  names(new_data) <- unname(new_data_nam)
  rm(new_data_nam)







  ################################################################################
  ####                      PROCESS ATTACHMENTS                     ####
  ################################################################################





  #### --------------- 0. CALCULATE NUMBER OF NEW INSTANCES --------------- ####


  total_instances <- sum(
    unlist(lapply(lapply(forms, function(y) y$parent),
                  function(x) if(is.null(new_submissions[[x]]))
                    0 else length(new_submissions[[x]])
    )))



  ### IF THERE IS NEW DATA --> PROCESS ATTACHEMENTS
  if(total_instances > 0) {






    ### - STEP 1: CREATE DATA FRAME WITH VARIABLE NAMES OF ATTACHMENTS - ###

    all_projects = kobo_projects(url      = kobo[["url"]],
                                 username = kobo[["username"]],
                                 password = kobo[["password"]])


    att_vars <- lapply(forms,
                       function(form) {
                         if(form$name %in% all_projects$name) {

                           ## Create empty attachment database
                           att_vars = data.frame(attachment = as.character(),
                                                 parent     = as.character(),
                                                 dat        = as.character())

                           # Get survey
                           survey <- kobo_survey(url      = kobo[["url"]],
                                                 username = kobo[["username"]],
                                                 password = kobo[["password"]],
                                                 name     = form$name)

                           ## Obtain names of variables that are attach
                           var_types = unlist(lapply(survey, function(x) coalesce_to(x$type)))
                           var_names = unlist(lapply(survey, function(x) coalesce_to(x$name)))
                           attachments = var_names[var_types %in%
                                                     c("image",  "audio",  "video",  "file",  "signature")]

                           ## Get database names contained in form "parent"
                           datinfo <- databases[
                             unlist(lapply(databases,
                                           function (x)
                                             x$parent == form[["parent"]]
                             ))]
                           datnames <- unlist(lapply(datinfo, function(x) x$dat))

                           ## Find in which databases is located the variable
                           for(datname in datnames) for(attachment in attachments)
                             if(!is.null(variables[[datname]][[attachment]]))
                               rbind(att_vars <- rbind(att_vars,
                                                       data.frame(attachment = attachment,
                                                                  parent     = form[["parent"]],
                                                                  dat        = datname)))

                           return(att_vars)
                         }
                       })

    att_vars <- do.call(rbind, Filter(Negate(is.null),  att_vars))



    ## Add attachments manually specified by the user in "is_attachment"
    for(dat in variables)
      for(var in dat)
        if(as.logical(var$is_attachment) &
           !var$variablename %in% att_vars$attachment   &
           !var$dat    %in% att_vars$dat)
          rbind(att_vars,
                data.frame(attachment = var$variablename,
                           parent     = databases[[var$dat]]$parent,
                           dat        = var$dat))




    ### ----- STEP 2: CREATE VARIABLE FOR STORING ATTACHMENT LINKS ----- ###



    for(i in 1:nrow(att_vars)){
      datname = att_vars$dat[i]
      if(!is.null(    new_data[[datname]]   )){
        varname = att_vars$attachment[i]
        if(varname %in% colnames(       new_data[[datname]]      )){
          new_data[[datname]]  <-
            create_new_var_after(new_var = paste0(varname, "_link"),
                                 ref_var = varname,
                                 data    = new_data[[datname]],
                                 class   = "character")
        }
      }
    }
    suppressWarnings( rm(varname, i, datname) )







    ### ----------- STEP 3: CREATE DATA FRAME OF ATTACHMENTS ----------- ###


    ### 1. CREATE EMPTY "ATTACHMENTS" DATA FRAME
    attachments <- data.frame(url        = as.character(),
                              filename   = as.character(),
                              mimetype   = as.character(),
                              filepath   = as.character(),
                              parentname = as.character())



    ### 2. CREATE EMPTY VECTOR TO TRACK NUMBER OF FORMS WITH ATTACHMENTS
    forms_with_atts <- c()



    ### 3. CREATE DATA FRAME OF ATTACHMENTS
    for(parentname in unlist(lapply(forms, function(x) x$parent))){

      ### 2.1. GET PARENT
      parent = new_submissions[[parentname]]


      ### 2.2. IF PARENT = NULL --> 0
      if(is.null(parent))  forms_with_atts <- c(forms_with_atts, 0)


      ### 2.3. IF PARENT != NULL --> PROCESS ATTACHMENTS
      if(!is.null(parent)) {


        # Obtain attachment information
        attch_info = lapply(parent, function(x){
          x = x$'_attachments'
          if(length(x) == 0 | is.null(x)) {output = NULL} else {
            temp = lapply(x, function(y)
              data.frame(url      = y$download_large_url,
                         filename = y$media_file_basename,
                         mimetype = y$mimetype,
                         filepath = file.path(tempdir(), y$media_file_basename),
                         parentname = parentname
              ))
            output = as.data.frame(do.call(rbind, Filter(Negate(is.null), temp)))
          }
          return(output)})


        # Bind all rows into a single data frame
        attch_info = as.data.frame(do.call(rbind, Filter(Negate(is.null), attch_info)))


        # If not att --> 0; else --> 1 and bind to "attachments"
        if(nrow(attch_info) == 0) {
          forms_with_atts <- c(forms_with_atts, 0)
        }  else   {
          forms_with_atts <- c(forms_with_atts, 1)
          attachments     <- rbind(attachments,  attch_info)
        }
      }
    }
    rm(attch_info)




    ### ---------------- STEP 5: UPLOAD NEW ATTACHMENTS ---------------- ###




    if(sum(forms_with_atts) > 0){
      message(paste0("\n", msg$Processing_new_attachments))
    } else {
      message(paste0("\n", msg$No_new_attachments))
    }


    if(sum(forms_with_atts) > 0){

      if(repository_location == "zenodo"){

        # 1. CREATE NEW VERSION OR DEPOSITION
        attch_deposition <- CREATE_NEW_VERSION_OR_DEPOSITION(title       = paste(repository_name, "ATTACHMENTS"),
                                                             description = zenodo[["description"]],
                                                             creators    = zenodo[["creators"]],
                                                             token       = token,
                                                             instance    = zenodo[["instance"]],
                                                             restricted  = FALSE,
                                                             language    = language)




        # 2. DELETE FILES FROM PREVIOUS VERSIONS TO PREVENT LARGE REP. SIZE
        # (links to files of previous versions still exist and are active)
        for(x in attch_deposition$files)
          httr::DELETE(x$links$self,
                       httr::add_headers(Authorization = paste("Bearer", token)))



        # 3. FOR EVERY ATTACHMENT: DOWNLOAD, UPLOAD TO ZENODO, AND ALLOCATE LINK
        message(paste0("", msg$Processing, " " ,nrow(attachments)," ", msg$new_attachments_process_may_take_a_while,"\n"))

        for(i in 1:nrow(attachments)){


          # 3.1 Download local copy
          message(paste0("      ", msg$downloading_attachment, " ",i," ", msg$of, " ",nrow(attachments)))
          res <- httr::GET(
            attachments$url[i],
            httr::authenticate(kobo[["username"]], kobo[["password"]]),
            httr::write_disk(attachments$filepath[i], overwrite = TRUE)
          )
          httr::stop_for_status(res)


          # 3.2 Upload to attachment deposition
          filename          = attachments$filename[i]
          attachment_link   =
            UPLOAD_TO_DEPOSITION(filepath = attachments$filepath[i],
                                 id       = attch_deposition$id,
                                 name     = attachments$filename[i],
                                 token    = token,
                                 instance = zenodo[["instance"]],
                                 language = language)


          # 3.3 Find containing variables and allocate link
          parentname = attachments$parentname[i]
          att        = att_vars[which(att_vars$parent == parentname),]
          for(j in 1:nrow(att)){
            datname = att$dat[j]
            dat     = new_data[[datname]]
            if(!is.null(dat)){
              varname = att$attachment[j]
              rw      = which(!is.na(dat[,varname]) &
                                dat[,varname] == filename)
              cl      = which(colnames(dat) == paste0(varname, "_link"))
              if(length(c(cl, rw)) > 0){
                dat[rw, cl]         = attachment_link
                new_data[[datname]] = dat
              }
            }
          }
        }
      }
    }
    suppressWarnings(rm(   rw, cl, dat, att, attachment_link,
                           parentname, res, filename, varname,
                           x
    ))








  }






  ################################################################################
  ####                   UPLOAD TO ZENODO AND SAVE NEW DATA                   ####
  ################################################################################






  #### --------------------- 1. CREATE COPY OF RAW DATA --------------------- ####



  RAW <- new_data
  names(new_data) = paste0(names(new_data), "_RAW")
  new_data = c(new_data, RAW)


  if(!is.null(cleaning_function)) new_data = cleaning_function(new_data)










  #### ---------- 2. NO DATA --> RETURN PREVIOUS DATA, IF EXISTS ---------- ####




  if(total_instances == 0){
    message(paste0("\n",msg$no_new_submissions))


    # 1. DOWNLOAD PREVIOUS DATA FILE AND OPEN CONNECTION
    #    (creates the file if it doesn't exist)
    con <- LOAD_DEPOSITION_FILE(title = repository_name,
                         token    = token,
                         instance = zenodo[["instance"]],
                         filename = "data.sqlite",
                         read_function = function(x){
                           DBI::dbConnect(RSQLite::SQLite(), x)
                         })

    # 2. READ TABLES WITHIN CONNECTION
    if(!is.null(con)){
      all_data        <- lapply(DBI::dbListTables(con), function(tbl) DBI::dbReadTable(con, tbl))
      names(all_data) <- DBI::dbListTables(con)
      DBI::dbDisconnect(con)
    } else
      all_data = NULL


    return(list(submissions = if(exists("old_submissions")) old_submissions else NULL,
                data        = if(length(all_data) == 0) NULL else all_data))

  }



  #### --------------- 5. IF DATA EXISTS, UPLOAD AND PUBLISH -------------- ####



  if(total_instances > 0){



    # 1. DOWNLOAD PREVIOUS DATA FILE AND OPEN CONNECTION
    now <- gsub(" ", "_", gsub("-", "", gsub(":", "", substr(Sys.time(), 3, 19) )))
    dir.create(file.path(tempdir(), now))
    filepath <- file.path(tempdir(), now, "data.sqlite")
    DOWNLOAD_DEPOSITION_FILE(title = repository_name,
                                token    = token,
                                instance = zenodo[["instance"]],
                                filename = "data.sqlite",
                                filepath = filepath)

    # 2. READ DATA IN "data.sqlite" (creates file if it does not exist)
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          filepath)
    old_data <- lapply(DBI::dbListTables(con),
                         function(datname)  DBI::dbReadTable(con, datname))
    names(old_data) <- DBI::dbListTables(con)

    # 3. APPEND NEW DATA
    all_data <- append_dflist(append_to   = old_data,
                              append_from = new_data,
                              variables   = variables,
                              enforce_class_of = "origin")

    ### 4. WRITE COLLATED DATABASES IN "data.sqlite"
    for (datname in names(all_data)) {
      if(!is.null(all_data[[datname]])){
        DBI::dbWriteTable(conn = con,
                          name = datname,
                          value = all_data[[datname]],
                          overwrite = TRUE,
                          row.names = FALSE)
      }
    }

    ### 5. DISCONNECT FILE
    DBI::dbDisconnect(con)


    ### 6. CREATE NEW VERSION OR DEPOSITION
    deposition <- CREATE_NEW_VERSION_OR_DEPOSITION(title       = repository_name,
                                                   description = zenodo[["description"]],
                                                   creators    = zenodo[["creators"]],
                                                   token       = token,
                                                   instance    = zenodo[["instance"]],
                                                   language    = language)

    ### 7. UPLOAD "data.sqlite" TO DEPOSITION
    UPLOAD_TO_DEPOSITION(filepath = filepath,
                         name     = "data.sqlite",
                         id       = deposition$id,
                         token    = token,
                         instance = zenodo[["instance"]],
                         language = language)


    ### 8. CREATE COLLATED SUBMISSIONS FILE
    all_submission_names <- levels(as.factor(c(names(old_submissions),
                                               names(new_submissions))))

    all_submissions <- stats::setNames(
      nm     = all_submission_names,
      object = lapply(all_submission_names, function(parentname) {
        c(old_submissions[[parentname]],
          new_submissions[[parentname]])
      })
    )


    ### 9. UPLOAD "submissions.json" TO DEPOSITION
    write(jsonlite::toJSON(all_submissions, auto_unbox = TRUE),
          file.path(tempdir(), now, "submissions.json"))
    UPLOAD_TO_DEPOSITION(filepath = file.path(tempdir(), now, "submissions.json"),
                         name     = "submissions.json",
                         id       = deposition$id,
                         token    = token,
                         instance = zenodo[["instance"]],
                         language = language)


    ### 10. UPLOAD ALL DATABASES IN SPECIFIED FORMAT
    for(i in 1:length(all_data)){
      datname = names(all_data)[i]
      dat     = all_data[[i]]
      writeDAT(data          = dat,
               fileformat    = config[["fileformat"]],
               filepath      = file.path(tempdir(), now,
                                         paste0(datname, ".", config[["fileformat"]])),
               csv_separator = config[["csv_separator"]],
               sheetname     = datname)
      UPLOAD_TO_DEPOSITION(filepath = file.path(tempdir(),
                                                now,
                                                paste0(datname, ".", config[["fileformat"]])),
                           name     = paste0(datname, ".", config[["fileformat"]]),
                           id       = deposition$id,
                           token    = token,
                           instance = zenodo[["instance"]],
                           language = language)
    }

    ### 11. PUBLISH ATTACHMENT DEPOSITION

    if(sum(forms_with_atts) > 0){

      message("\n", msg$publishing_attachment_repo)

      attch_deposition <- PUBLISH_DEPOSITION(id       = attch_deposition$id,
                                             token    = token,
                                             instance = zenodo[["instance"]])
    }






    ### 12. PUBLISH DATA DEPOSITION

    message("\n", msg$publishing_data_repo)
    deposition <- PUBLISH_DEPOSITION(id       = deposition$id,
                                     token    = token,
                                     instance = zenodo[["instance"]])

    return(list(submissions = all_submissions,
                data        = all_data))
  }

  message("","\n")
  message("","\n")
}
