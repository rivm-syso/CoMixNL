# Calculates upper and lower limit for binomial probability given n_obs successes 
# in n_tot draws

calculate_binomialprob_lower <- function(n_tot, n_obs) {
  if(n_obs == 0) return(0)
  uniroot(function(x) pbinom(n_obs, size = n_tot, prob = x) - 0.975, c(0,1))$root
}

calculate_binomialprob_upper <- function(n_tot, n_obs) {
  if(n_obs == n_tot) return(1)
  uniroot(function(x) pbinom(n_obs, size = n_tot, prob = x) - 0.025, c(0,1))$root
}




