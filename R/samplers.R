# Available objective functions
log_exp_target <- function(x) dexp(x, rate = 1, log = TRUE)
log_normal_target <- function(x) dnorm(x, mean = 0, sd = 1, log = TRUE)

metropolis_sampler <- function(log_target, n_iter, startval = 1, proposalsd = 1) {
  x <- numeric(n_iter)
  x[1] <- startval
  for(i in 2:n_iter) {
    currentx <- x[i - 1]
    proposedx <- rnorm(1, mean = currentx, sd = proposalsd)
    A <- exp(log_target(proposedx) - log_target(currentx))
    if(runif(1) < A) {
      x[i] <- proposedx
    } else {
      x[i] <- currentx
    }
  }
  x
}
