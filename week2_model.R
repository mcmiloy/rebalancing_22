# Function for simulation
my_sim = function(n, dist, nsim, nboot, alpha = 0.05, mu = 0, var_x = 1){
   if(var_x <= 0) stop("Variance must be positive!")
   set.seed(1)
   # Critical value
   crit = qt(1-alpha/2, n-1)
   
   # Helper Matrix for Resampling (see later) [These are columnwise indexes for resampling from X]
   #B = matrix(sample(c(1:n), replace = T, size = n * nboot), ncol = nboot, nrow = n)
   B = apply(matrix(1:n, ncol = nboot, nrow = n), 2, sample, replace = T)
   
   # Generate the data
   switch(dist,
   norm = {
      x = matrix(rnorm(n * nsim, mean = mu, sd = sqrt(var_x)), ncol = nsim)
   },
   exp = {
      x = (matrix(rexp(n * nsim), ncol = nsim) - 1) * sqrt(var_x) + mu
   })
   
   # Empirical mean
   x_bar = colMeans(x)
   
   # Empirical Variance
   vx    = (colSums(x^2) - n * x_bar^2) / (n - 1)
   
   # Test statistic
   stat  = sqrt(n) * (x_bar - mu) / sqrt(vx)
   
   # Spaceholder for critical values of bootstrap
   crit_boot = matrix(NA, nrow = 2, ncol = nboot)
   
   # Bootstrap
   for(i in 1:nsim){
      x_sim  = x[,i]
      #x_star = matrix(sample(x_sim, replace = T, size = n * nboot), ncol = nboot)
      x_star = matrix(x_sim[B], ncol = nboot) # This creates a matrix of resampled X-Vectors (X*), where each column is a bootstrap sample
      
      # Bootstrap Means
      x_bar_star = colMeans(x_star)
      
      # Bootstrap Variances
      vx_bar_star = (colSums(x_star^2) - n * x_bar_star^2) / (n-1)
      
      # Bootstrap statistic
      stat_star  = sqrt(n) * (x_bar_star - x_bar[i]) / sqrt(vx_bar_star) # x_bar[i] is the conditional mean: Considering that x is the conditional population for the bootstrap
      
      # Critical values of the bootstrap distribution
      crit_boot[,i] = quantile(stat_star, probs = c(alpha/2, 1-alpha/2))
   }
   
   # Test decisions:
      # Standard t-Test:
      dec_standard = mean(abs(stat) > crit) # How often is the absolute value of the standard t statistic larger than the critical value?
      
      # Bootstrap test:
      dec_boot     = mean(stat < crit_boot[1,] | stat > crit_boot[2,])
      
   # Results:
      
      pi = c(alpha - qnorm(1-alpha/2) / nsim * sqrt(alpha * (1-alpha)), alpha + qnorm(1-alpha/2) / nsim * sqrt(alpha * (1-alpha)))
      pi_t = dec_standard < pi[2] && dec_standard > pi[1]
      pi_b = dec_boot < pi[2] && dec_boot > pi[1]
      
      result = data.frame("n" = n, "Distribution" = dist, "nsim" = nsim, "nboot" = nboot, "type_1_t" = dec_standard, "type_1_boot" = dec_boot,
                          "pi_low" = pi[1], "pi_up" = pi[2], "in_pi_standard" = pi_t, "in_pi_boot" = pi_b)
      return(result)
}


n = c(10, 15, 20, 30, 50)
results = rbind(Reduce(rbind, lapply(n, my_sim, nsim = 1e4, nboot=1e4, dist = "norm")), Reduce(rbind, lapply(n, my_sim, nsim = 1e4, nboot=1e4, dist = "exp")))

if(!("ggplot2" %in% installed.packages())) install.packages("ggplot2", dependencies = T)
library(ggplot2)
long_results = rbind(results[,1:4], results[,1:4])
long_results$type1 = c(results$type_1_t, results$type_1_boot)
long_results$method = rep(c("standard", "bootstrap"), each = 10)

save(file = "ex2_res.RData", results, long_results)

ggplot(data = long_results, aes(x = n, y=type1, linetype = method, color = Distribution)) + 
   geom_line(size = 1) +
   geom_hline(yintercept = 0.05, size = 1)

ggsave("t1_convergence.pdf", scale = 2)
