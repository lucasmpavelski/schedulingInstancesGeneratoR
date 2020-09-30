
write_txt <- function(object, ...) {
  0
}

#' Write instance to a txt file with format:
#' NumJobs NumMachines
#' ProcTime11 ProcTime12 ...
#' ProcTime21 ProcTime22 ...
#' ...
#'
#' @param object instance to written to a file
#' @param file file connection or path
#' @param append append instance data to existing file
#' @param sep separator used to write dimentions and processing times
#'
#' @exportMethod write_txt
#'
#' @examples
#' write_txt(generate_fsp_instance(20, 10))
setMethod("write_txt", "Instance", function(object,
                                            file = "data",
                                            append = FALSE,
                                            sep = " ") {
  write(c(nrow(object), ncol(object)),
    file = file,
    append = append,
    sep = sep
  )
  write(
    t(object),
    ncolumns = ncol(object),
    file = file,
    append = TRUE,
    sep = sep
  )
})
