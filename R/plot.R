#' Plot instance as pairs with correlation.
#'
#' @param Instance instance to be plotted.
#'
#' @export
#' @importFrom graphics pairs par strwidth text
#'
#' @examples
#' plot(generate_fsp_instance(100, 3))
setMethod("plot", "Instance", function(x, y, ...) {
  panel.cor <- function(x,
                        y,
                        digits = 2,
                        prefix = "",
                        cex.cor,
                        ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) {
      cex.cor <- 0.8 / strwidth(txt)
    }
    text(0.5, 0.5, txt, cex = cex.cor * r)
  }
  pairs(x,
    upper.panel = panel.cor,
    pch = 20, ...
  )
})
