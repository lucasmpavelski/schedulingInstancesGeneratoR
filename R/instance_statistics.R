ratio <- function(inst) {
  nrow(inst) / ncol(inst)
}

pt_sd <- function(inst) {
  sd(inst)
}

mean_sd_per_machine <- function(inst) {
  mean(apply(inst, 2, sd))
}

mean_sd_per_job <- function(inst) {
  mean_sd_per_machine(t(inst))
}

mean_skew_per_machine <- function(inst) {
  mean_pts <- matrix(rep(apply(inst, 2, mean), nrow(inst)), nrow(inst), ncol(inst), byrow = T)
  m3 <- apply((inst - mean_pts)^3, 2, mean)
  s3 <- apply(inst, 2, sd)^3 + 1e-6
  mean(m3 / s3)
}

mean_skew_per_job <- function(inst) {
  mean_skew_per_machine(t(inst))
}

mean_kurt_per_machine <- function(inst) {
  inst <- generate_fsp_instance(4, 5)
  mean_pts <- matrix(rep(apply(inst, 2, mean), nrow(inst)), nrow(inst), ncol(inst), byrow = T)
  sd_pts <- matrix(rep(apply(inst, 2, sd), nrow(inst)), nrow(inst), ncol(inst), byrow = T) + 1e-6
  z <- (inst - mean_pts) / sd_pts
  z4 <- apply(z^4, 2, sum)
  n <- nrow(inst)
  scale <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))
  trans <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  mean(scale * z4 - trans)
}

mean_kurt_per_job <- function(inst) {
  mean_kurt_per_machine(t(inst))
}

#' Instance processing times statistics
#'
#' @param inst an instance
#'
#' @return A list with different statistics of processing times:
#' - standard deviation (pt_sd)
#' - mean standard deviation per machine (mean_sd_per_machine)
#' - mean standard deviation per job (mean_sd_per_job)
#' - mean skewness per machine (mean_skew_per_machine)
#' - mean skewness per job (mean_skew_per_job)
#' - mean kurtosis per machine (mean_kurt_per_machine)
#' - mean kurtosis per job (mean_kurt_per_job)
#'
#' @export
#'
#' @importFrom stats sd
#'
#' @examples
#' instance_statistics(generate_fsp_instance(20, 5, "erlang", "machine-correlated", 0.95))
instance_statistics <- function(inst) {
  list(
    no_jobs = nrow(inst),
    no_machines = ncol(inst),
    ratio = ratio(inst),
    pt_sd = pt_sd(inst),
    mean_sd_per_machine = mean_sd_per_machine(inst),
    mean_sd_per_job = mean_sd_per_job(inst),
    mean_skew_per_machine = mean_skew_per_machine(inst),
    mean_skew_per_job = mean_skew_per_job(inst),
    mean_kurt_per_machine = mean_kurt_per_machine(inst),
    mean_kurt_per_job = mean_kurt_per_job(inst)
  )
}
