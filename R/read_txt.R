#' Reads an instance from a txt file with format:
#' NumJobs NumMachines
#' ProcTime11 ProcTime12 ...
#' ProcTime21 ProcTime22 ...
#' ...
#'
#' @param file file connection or path
#' @param sep separator used to write dimensions and processing times
#' @param ... parameters passed to Instance
#'
#' @return Instance object
#'
#' @export read_txt
#' @importFrom utils read.delim2
#'
#' @examples
#' write_txt(generate_fsp_instance(10, 5), "example")
#' read_txt("example")
read_txt <- function(file = "data",
                     sep = "",
                     ...) {
  dt <- as.matrix(read.delim2(file, sep = sep, header = F, skip = 1))
  Instance(dt, nrow(dt), ncol(dt), ...)
}
