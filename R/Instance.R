DISTRIBUTION_TYPES = list(
  UNIFORM = 'uniform',
  EXPONENTIAL = 'exponential',
  BINOMIAL = 'binomial',
  ERLANG = 'erlang'
)

CORRELATION_TYPES = list(
  RANDOM = 'random',
  JOB_CORRELATED = 'job-correlated',
  MACHINE_CORRELATED = 'machine-correlated'
)

check_instance <- function(object) {
  errors <- character()
  if (!(object@distribution_type %in% DISTRIBUTION_TYPES)) {
    errors <-
      c(
        errors,
        paste0(
          'Unknown distribution type ',
          object@distribution_type,
          ', should be one of ',
          paste0(DISTRIBUTION_TYPES, collapse = ', ')
        )
      )
  }
  if (!(object@correlation_type %in% CORRELATION_TYPES)) {
    errors <-
      c(
        errors,
        paste0(
          'Unknown correlation type ',
          object@correlation_type,
          ', should be one of ',
          paste0(CORRELATION_TYPES, collapse = ', ')
        )
      )
  }
  if (!(object@correlation >= 0 && object@correlation <= 1)) {
    errors <-
      c(errors, paste0('Correlation value should be between 0 and 1'))
  }
  if (length(errors) == 0)
    TRUE
  else
    errors
}


#' Scheduling instance class
#'
#' @slot distribution_type integer.
#' @slot correlation_type integer.
#' @slot correlation numeric.
#' @slot seed integer.
#'
#' @return
#' @export Instance
#' @exportClass Instance
#'
#' @examples
setClass(
  'Instance',
  contains = 'matrix',
  slots = c(
    distribution_type = 'character',
    correlation_type = 'character',
    correlation = 'numeric'
  ),
  validity = check_instance
)

#' Scheduling instance constructor
#'
#' @slot distribution_type integer.
#' @slot correlation_type integer.
#' @slot correlation numeric.
#' @slot seed integer.
#'
#' @return
#' @export Instance
#'
#' @examples
Instance <- function(distribution_type = DISTRIBUTION_TYPES$UNIFORM,
                     correlation_type = CORRELATION_TYPES$JOB_CORRELATED,
                     correlation = NA_real_,
                     ...) {
  new(
    'Instance',
    ...,
    distribution_type = distribution_type,
    correlation_type = correlation_type,
    correlation = correlation
  )
}

#' Plot instance as pairs with correlation.
#'
#' @param Instance
#'
#' @return
#' @export
#'
#' @examples
setMethod('plot', 'Instance', function(x, y, ...) {
  panel.cor <- function(x,
                        y,
                        digits = 2,
                        prefix = '',
                        cex.cor,
                        ...) {
    usr <- par('usr')
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = '')
    if (missing(cex.cor))
      cex.cor <- 0.8 / strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
  }
  pairs(x,
        upper.panel = panel.cor,
        pch = 20, ...)
})