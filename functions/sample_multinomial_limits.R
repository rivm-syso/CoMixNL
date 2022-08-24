# Samples from multinomial distribution given vector n containing the number of
# observations per category (this is the same as bootstrapping)
# Function returns upper and lower limit of cumulative distribution

sample_multinomial_limits <- function(n) {
  tmp <- rmultinom(n = 1000, size = sum(n), prob = n) 
  
  tmp <- apply(tmp, MARGIN = 2, cumsum)/colSums(tmp)
  
  apply(tmp, MARGIN = 1, quantile, probs = c(0.025, 0.975))
}
