#' Remove files or directories with retry and timeout
#'
#' This function attempts to remove one or more files or directories,
#' retrying until successful or until a timeout is reached. It is useful
#' in situations where files may be temporarily locked (e.g., open
#' connections), as it repeatedly tries deletion while closing open
#' connections between attempts.
#'
#' For directories, files and subdirectories are removed recursively
#' (depending on the `recursive` argument).
#'
#' @param path A character vector of file or directory paths to remove.
#' @param recursive Logical; whether to delete directories recursively.
#'   Defaults to `TRUE`.
#' @param timeout Numeric; maximum number of seconds to keep retrying
#'   deletion before giving up. Defaults to `60`.
#' @param wait Numeric; number of seconds to wait between retry attempts.
#'   Defaults to `0.5`.
#'
#' @return Invisibly returns `TRUE` if all paths are successfully removed.
#'   Returns `FALSE` if the operation times out before completion.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Repeatedly attempts to delete each path until successful.
#'   \item Calls `closeAllConnections()` between attempts to release file locks.
#'   \item Prints progress to the console using `"."` for each retry cycle.
#'   \item Stops when all paths are removed or when `timeout` is exceeded.
#' }
#'
#' Non-existent paths are silently ignored.
#'
#' @examples
#' \dontrun{
#' # Remove a file
#' remove_files("temp.txt")
#'
#' # Remove multiple paths
#' remove_files(c("file1.txt", "dir1"))
#' }
#'
#' @export
#' @export
remove_files <- function(path,
                         recursive = TRUE,
                         timeout = 60,
                         wait = 0.5) {
  if(length(path) > 0){

    start_time <- Sys.time()

    repeat {
      # Safety: stop if timeout reached
      if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > timeout) {
        warning("Cleanup timed out before completion")
        return(invisible(FALSE))
      }


      # Close all connections
      suppressWarnings(closeAllConnections())
      success <- unlist(lapply(path, function(x) {
        if(file.exists(x)){
          is.file <- !dir.exists(x)
          is.dir  <- dir.exists(x)
          if(is.file) {
            return(file.remove(x))
          }
          if(is.dir){
            # Identify files and folders
            deletefiles <- list.files(x,full.names = TRUE, recursive = recursive)
            deletedirs  <- list.dirs(x,full.names = TRUE, recursive = recursive)
            delete      <- levels(as.factor(c(deletefiles, deletedirs)))

            # Delete directories
            if (length(delete) > 0) {
              res = unlink(delete, recursive = TRUE, force = TRUE)
              return(res == 0)
            } else {return(TRUE)}

          } else {
            return(TRUE)
          }
        }
      }))
      if(length(which(!success)) == 0) {
        cat("\n")
        break}

      Sys.sleep(wait)
    }
  }
}
