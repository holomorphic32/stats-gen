# The functions below can be used to calculate 95% error bars for
# large-sample sampling from a finite population.
#
# Large-sample sampling from a finite population can be represented
# by the hypergeometric distribution: here, k is the number of 
# white balls in the sample; n is the size of the sample; and N is the 
# number of all balls in the urn (i.e., the overall population).  
#
# Specifically, the functions below find the largest number of white 
# balls K that can be in the urn such that the probability of drawing 
# k white balls or fewer in a sample of size n is >= 0.025.
# 
# The function uQ is included for vectorized operations.

upperQuantileHypergeom <- function(k, n, N) {
  if (k == n) {
    return(N)
  }
  else {
    N <- ceiling(N)
    K <- seq(0, N)
    index <- which.min( abs(phyper(k, K, N - K, n, lower.tail = TRUE) 
                            - 0.025))
    return( min( K[index], N - (n-k) ) )
  }
}

uQ <- Vectorize(upperQuantileHypergeom)

# In a manner analogous to the functions above, the functions below 
# calculate the smallest number of white balls K in the urn such that the 
# probability of drawing k white balls or more in a sample of size n is 
# <= 0.975.

lowerQuantileHypergeom <- function(k, n, N) {
  
  N <- floor(N)
  K <- seq(0, N)
  index <- 
    which.min( abs(phyper(k-1, K, N - K, n, lower.tail = FALSE)
                   - 0.025) ) 
  return( max(K[index], k) )
}

lQ <- Vectorize(lowerQuantileHypergeom)