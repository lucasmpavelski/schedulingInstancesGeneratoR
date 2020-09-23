
rmvnorm <- function(n, mean, cov) {
  E = eigen(cov, symmetric=TRUE)
  mean.term = mean
  covariance.term = E$vec %*% (t(E$vec) * sqrt(E$val))
  independent.term = matrix(rnorm(n* length(mean)), nrow=length(mean))
  drop(t(mean.term + (covariance.term %*% independent.term)))
}

rmvexp <- function(n, rate=1, corr=diag(length(rate))) {
  rate <- rep(rate, length.out = ncol(corr))
  if(ncol(corr) == 1) rexp(n, rate)
  Z = rmvnorm(n, rep(0, ncol(corr)), cov=corr)
  cdf = pnorm(Z)
  sapply(1:ncol(corr), function(d) qexp(cdf[,d], rate[d]))
}

rmvunif <- function(n, min, max, corr) {
  Z = rmvnorm(n, rep(0, ncol(corr)), cov=corr)
  cdf = pnorm(Z)
  sapply(1:ncol(corr), function(d) qunif(cdf[,d], min, max))
}

rmverlang <- function(n, shape, rate, corr) {
  Z = rmvnorm(n, rep(0, ncol(corr)), cov=corr)
  cdf = pnorm(Z)
  sapply(1:ncol(corr), function(d) qgamma(cdf[,d], shape, rate))
}

generateBinomPt <- function(no_jobs, no_machines, correlation) {
  corMat <- diag(no_machines)
  corMat[corMat == 0] <- correlation
  corMat <- 2 * sin(corMat * pi / 6)
  X <- rnorm(no_machines * no_jobs)
  dim(X) <- c(no_jobs, no_machines)
  X <- X %*% chol(corMat)
  U <- pnorm(X)
  qnbinom(U, size = 50, prob = 0.5)
}

generateExpPt <- function(no_jobs, no_machines, correlation) {
  corMat <- diag(no_machines)
  corMat[corMat == 0] <- correlation
  corMat <- 2 * sin(corMat * pi / 6)
  ceiling(rmvexp(no_jobs, 1.0 / 50, corMat))
}

generateUnifPt <- function(no_jobs, no_machines, correlation) {
  corMat <- diag(no_machines)
  corMat[corMat == 0] <- correlation
  corMat <- 2 * sin(corMat * pi / 6)
  ceiling(rmvunif(no_jobs, 1, 99, corMat))
}

generateErlangPt <- function(no_jobs, no_machines, correlation) {
  corMat <- diag(no_machines)
  corMat[corMat == 0] <- correlation
  corMat <- 2 * sin(corMat * pi / 6)
  ceiling(rmverlang(no_jobs, 4, 4 / 50, corMat))
}

generatePt <- function(no_jobs, no_machines, distribution_type, correlation_type, correlation) {
  if (correlation_type == CORRELATION_TYPES$MACHINE_CORRELATED) {
    tmp <- no_jobs
    no_jobs <- no_machines
    no_machines <- tmp
  }
  dts <- DISTRIBUTION_TYPES
  generators <- setNames(
    c(generateUnifPt, generateBinomPt, generateExpPt, generateErlangPt),
    c(dts$UNIFORM, dts$BINOMIAL, dts$EXPONENTIAL, dts$ERLANG)
  )
  pts <- generators[[distribution_type]](no_jobs, no_machines, correlation)
  if (correlation_type == CORRELATION_TYPES$MACHINE_CORRELATED) {
    pts <- t(pts)
  }
  pts
}

#' Generate flowshop instances with given attributes
#'
#' @param no_jobs Number of jobs
#' @param no_machines Number of machines
#' @param ... Parameters passed to Instance(...)
#' @param seed Optional random number generator seed
#'
#' @return
#' @export generateFSPInstance
#'
#' @examples
generateFSPInstance <- function(no_jobs, no_machines, ..., seed = NA) {
  if (is.na(seed)) {
    seed <- runif(1, 0, .Machine$integer.max)
  }
  set.seed(seed)
  inst <- Instance(0, no_jobs, no_machines, ...)
  inst[,] <- generatePt(no_jobs, no_machines, ...)
  dimnames(inst) <- list(paste('job', 1:no_jobs), paste('mach', 1:no_machines))
  inst
}



