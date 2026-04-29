



#' Insert a new column after a reference column in a data frame
#'
#' This function creates a new column (`new_var`) filled with missing values
#' and inserts it into a data frame immediately after a specified reference
#' column (`ref_var`).
#'
#' If `new_var` already exists, the data frame is returned unchanged.
#' If `ref_var` is not found, a warning is issued.
#'
#' @param new_var A character string specifying the name of the new column.
#' @param ref_var A character string specifying the name of the reference column.
#' @param data A data frame in which to insert the new column.
#' @param class A character string indicating the class of the new column
#'   (e.g., `"character"`, `"numeric"`, `"integer"`).
#'
#' @return A data frame with the new column inserted after `ref_var`.
#'
#' @export
create_new_var_after <- function(new_var, ref_var, data, class){
  as.class = get(paste0("as.", class))
  if(!is.null(data)){
    if(ref_var %in% names(data)){
      if(!new_var %in% colnames(data)){
        firsthalf   = colnames(data)[1:which(colnames(data) == ref_var)]
        secondhalf  = colnames(data)[(which(colnames(data) == ref_var) + 1) : ncol(data)]
        data <- data.frame(data[,firsthalf],
                           temp = as.class(NA),
                           data[,secondhalf])
        colnames(data) <- c(firsthalf, new_var, secondhalf)
      }
      return(data)
    }
    warning(paste(ref_var, "does not exist"))
  }
  warning("'data' is empty (NULL)")
}




#' @export
table_to_list <- function(dat, label = NULL, varnames = NULL) {
  if(is.null(varnames)) varnames = colnames(dat)
  dat = as.data.frame(dat)
  output = lapply(1:nrow(dat),
                  function(i) {
                    temp = lapply(varnames,
                                  function(varname){
                                    dat[i, colnames(dat) == varname]
                                  })
                    names(temp) = varnames
                    return(temp)
                  })
  if(!is.null(label)) names(output) = dat[, which(colnames(dat) == label)]
  return(output)
}





#' Standardize column classes based on a template data frame
#'
#' This function sets the classes of columns in one data frame (`output`)
#' to match the classes of columns with the same names in another data frame
#' (`template`).
#'
#' Only columns present in both data frames are modified. Columns in `output`
#' that are not found in `template` are left unchanged.
#'
#' @param template A data frame providing the reference column classes.
#' @param data A data frame whose column classes will be modified.
#'
#' @return A data frame with column classes standardized to match `template`.
#'
#' @export
standardise_class <- function(template, data){
  cl = lapply(template, function(x) class(x))
  for(i in 1:length(cl)){
    class(data[,names(cl)[i] == colnames(data)]) = cl[[i]]
  }
  return(data)
}

#' @export
enforce_class <- function(data, class_list){
  data = as.data.frame(data)
  for(var in colnames(data)){
    if(!is.null(class_list[[var]])) {
      as.class = get(paste0("as.", class_list[[var]]))
      data[,var] <- as.class(data[,var])
    }
  }
  return(data)
}

