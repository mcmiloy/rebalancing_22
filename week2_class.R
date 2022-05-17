sim_t_test = function(n, dist, nsim, alpha = 0.05){
  # Data Generation
  if(dist == "normal"){
    x = matrix(rnorm(n * nsim, mean = 0, sd = 1), ncol = nsim)
  } else if(dist == "exp"){
    x = matrix((rexp(n * nsim, rate = 1) - 1), ncol = nsim)
  }
  
  # Alternative:
  #x = matrix(, ncol = nsim, nrow = n)
  #for(i in 1:nsim){
  #  matrix[,i] = rnorm(n)
  #}
  
  x_bar = colMeans(x) # All empirical means for each x-vector
  var_x = (colSums(x^2) - n * x_bar^2) / (n-1) # empirical variance for all x-vectors
  stat  = sqrt(n) * x_bar / sqrt(var_x) # (x_bar - mu), but mu = 0
  
  crit = qt(1-alpha/2, n-1) # Critical value from the t-distribution
  
  dec_vector = abs(stat) > crit # vector of decisions: TRUE := Reject H_0
  return(mean(dec_vector)) # empirical type-I error rate
}

res_t_test = function(x, nboot, alpha){ # This function conducts the resampling-based t-test
  n = length(x)
  x_bar = mean(x)
  vx = (sum(x^2) - n * x_bar^2) / (n-1)
  stat = sqrt(n) * x_bar / sqrt(vx) # mu = 0, thus (x_bar - mu) = x_bar; Test statistic based on X
  
  # warning: inefficient!
  stat_star = numeric(nboot) # empty vector for bootstrap statistics
  # for(j in 1:nboot){ # Within this for loop, we treat x as the population
  #   x_star = sample(x, size = n, replace = T) # Draw with replacement from x <=> Nonparametric bootstrap
  #   x_bar_star = mean(x_star)
  #   vx_star = (sum(x_star^2) - n * x_bar_star^2) / (n-1) # Empirical Variance of X*
  #   stat_star[j] = sqrt(n) * (x_bar_star - x_bar) / sqrt(vx_star) # E[X*] = x_bar, because X is the population of X*; Test statistic based on X*
  # }
  
  x_star = matrix(sample(x, size = n * nboot, replace = T), ncol = nboot)
  x_bar_star = colMeans(x_star)
  vx_star = (colSums(x_star^2) - n * x_bar_star^2) / (n-1)
  stat_star = sqrt(n) * (x_bar_star - x_bar) / sqrt(vx_star)
  
  crit_low = quantile(stat_star, alpha/2)
  crit_up  = quantile(stat_star, 1-alpha/2)
  
  dec = stat < crit_low || stat > crit_up # Results TRUE, if stat falls outside of critical values => Reject H_0
  
  return(dec)
}


sim_r_resampling = function(n, dist, nsim, alpha = 0.05, nboot){
  
  if(dist == "normal"){
    x = matrix(rnorm(n * nsim, mean = 0, sd = 1), ncol = nsim)
  } else if(dist == "exp"){
    x = matrix((rexp(n * nsim, rate = 1) - 1), ncol = nsim)
  } # Each X-Column is a population
  
  dec_vector = numeric(nsim) # creates empty vector of size nsim
  
  for(i in 1:nsim){ # i is a single simulation iteration
    dec_vector[i] = res_t_test(x[,i], nboot, alpha) # This function conducts the resampling-based test
    
  }
  return(mean(dec_vector))
}


sim_t_test(10, "exp", 1e4)
sim_r_resampling(10, "exp", 1e4, nboot=1e4)
