DISTRIBUTION_TYPES <- list(
  UNIFORM = "uniform",
  EXPONENTIAL = "exponential",
  BINOMIAL = "binomial",
  ERLANG = "erlang"
)

CORRELATION_TYPES <- list(
  RANDOM = "random",
  JOB_CORRELATED = "job-correlated",
  MACHINE_CORRELATED = "machine-correlated"
)

check_instance <- function(object) {
  errors <- character()
  if (!is.na(object@distribution_type) && !(object@distribution_type %in% DISTRIBUTION_TYPES)) {
    errors <-
      c(
        errors,
        paste0(
          "Unknown distribution type ",
          object@distribution_type,
          ", should be one of ",
          paste0(DISTRIBUTION_TYPES, collapse = ", ")
        )
      )
  }
  if (!is.na(object@correlation_type) && !(object@correlation_type %in% CORRELATION_TYPES)) {
    errors <-
      c(
        errors,
        paste0(
          "Unknown correlation type ",
          object@correlation_type,
          ", should be one of ",
          paste0(CORRELATION_TYPES, collapse = ", ")
        )
      )
  }
  if (!(object@correlation >= 0 && object@correlation <= 1)) {
    errors <-
      c(errors, paste0("Correlation value should be between 0 and 1"))
  }
  if (length(errors) == 0) {
    TRUE
  } else {
    errors
  }
}


#' Scheduling instance class
#'
#' @slot .Data unrelated, from matrix class
#' @slot distribution_type processing times distribution type (uniform, exponential, binomial or erlang)
#' @slot correlation_type processing times correlation type (random, machine-correlated, job-correlated)
#' @slot correlation correlation value (between 0 and 1)
#'
#' @return
#' @exportClass Instance
setClass(
  "Instance",
  contains = "matrix",
  slots = c(
    distribution_type = "character",
    correlation_type = "character",
    correlation = "numeric",
    seed = "integer"
  ),
  validity = check_instance
)

#' Scheduling instance constructor
#'
#' @param data processing time data
#' @param nrow number of jobs
#' @param ncol number of machines
#' @param byrow processing times order by machine (default) or by job
#' @param dimnames name of the processing times matrix dimentions
#' @param distribution_type processing times distribution type (uniform, exponential, binomial or erlang)
#' @param correlation_type processing times correlation type (random, machine-correlated, job-correlated)
#' @param correlation correlation value (between 0 and 1)
#' @param seed RNG seed used to generate the processing times
#'
#' @return a instance with given processing times
#' @export Instance
#' @importFrom methods new
#' @examples
#' Instance(1:10, 5, 2, distribution_type = "uniform")
Instance <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL,
                     distribution_type = NA_character_,
                     correlation_type = NA_character_,
                     correlation = 0.0,
                     seed = NA_integer_) {
  new(
    "Instance",
    data, nrow, ncol, byrow, dimnames,
    distribution_type = distribution_type,
    correlation_type = correlation_type,
    correlation = correlation,
    seed = seed
  )
}
