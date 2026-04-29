
#' @export
get_config_from_local = function(config      = file.path("inst", "config"),
                                 cache       = file.path("inst", "cache")) {

  variables = readxl::read_excel(file.path(config, "forms.xlsx"), sheet = "vars")
  nams      = levels(as.factor(variables$dat))
  variables = lapply(levels(as.factor(variables$dat)), function(datname)
    table_to_list(variables[which(variables$dat == datname),],
                  label = "variablename"))
  names(variables) = nams


  list(
    cleaning_function =
      local({
        filepath = file.path(config, "clean_new_data.R")
        if(file.exists(filepath)){
          source(filepath, local = TRUE)
          if(exists("cleaning_function")) get("cleaning_function") else NULL
        } else NULL
      }),

    repository_name  =
      readxl::read_excel(file.path(config, "repo.xlsx"), sheet = "repo")$repository_title,


    repository_location =
      readxl::read_excel(file.path(config, "repo.xlsx"), sheet = "repo")$repository_location,


    zenodo = list(instance     = readxl::read_excel(file.path(config, "repo.xlsx"), sheet = "repo")$zenodo_instance,
                  description  = readxl::read_excel(file.path(config, "repo.xlsx"), sheet = "repo")$zenodo_description,
                  creators     = table_to_list(readxl::read_excel(file.path(config, "repo.xlsx"), sheet = "zenodo_creators"))
                  ),




    forms =
      table_to_list(readxl::read_excel(file.path(config, "forms.xlsx"), sheet = "forms"),
                    label = "parent"),


    databases =
      table_to_list(readxl::read_excel(file.path(config, "forms.xlsx"), sheet = "dat"),
                    label = "dat"),

    variables = variables,


    cache  = cache,


    last_session = if(file.exists(file.path(cache, "config.RDS")))
      readRDS(file.path(cache, "config.RDS")) else NULL,


    config = list(
      local_data_directory = readxl::read_excel(file.path(config, "files.xlsx"), sheet = "conf")$local_data_directory,
      method               = readxl::read_excel(file.path(config, "files.xlsx"), sheet = "conf")$method,
      fileformat           = readxl::read_excel(file.path(config, "files.xlsx"), sheet = "conf")$fileformat,
      grouping_variable    = readxl::read_excel(file.path(config, "files.xlsx"), sheet = "conf")$group,
      csv_separator        = readxl::read_excel(file.path(config, "files.xlsx"), sheet = "conf")$csv_separator
    )

  )

}

config = "inst/config"
