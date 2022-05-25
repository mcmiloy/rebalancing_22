n <- 15
n_boot <- 10000
n_sim <- 1000
my_data <- c(1.5, 1.3, 1.8, 2.1, 0.9, 1.3, 1.9, 1.5, 2.0, 2.4, 0.9, 1.1)
s2 <- 1
tau_hat_sq_emp <- s2/n


boot_mean_var <- function(in_data, n_boot){
  boot_matrix <- matrix(sample(in_data, n*nboot, replace=TRUE), ncol=n_boot)
  boot_means <- colMeans(boot_matrix)
  theta_bar <- mean(boot_means)
  tau_hat_sq <- (1/(n_boot-1))*sum((boot_means - theta_bar)^2)
  return(tau_hat_sq)
}

# res <- boot_mean_var(my_data, n_boot)
tau_hats_sq <- c()
for(i in 1:n_sim){
  sample_data <- rnorm(n, sd=sqrt(s2))
  tau_hats_sq[i] <- boot_mean_var(sample_data, n_boot)
}

bias <- mean(tau_hats_sq - tau_hat_sq_emp)
mse <- mean((tau_hats_sq - tau_hat_sq_emp)^2)

print("bias: ", bias, "mse: ", mse)