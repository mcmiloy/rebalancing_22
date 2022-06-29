
reject_or_not_colwise <- function(data_mat, nboot, wild=FALSE){
  n <- nrow(data_mat)
  data_means <- colMeans(data_mat)
  data_vars <- (colSums(data_mat^2) - n*data_means^2)/(n-1)
  data_ts <- sqrt(n)*data_means/sqrt(data_vars)
  decisions <- c()
  for(i in 1:ncol(data_mat)){
    x <- data_mat[, i]
    boot_mat <- matrix(sample(x, n*nboot, replace=TRUE), ncol=nboot)
    if(wild){
      wild_mat <- matrix(sample(c(1,-1), n*nboot, replace=TRUE),
                         ncol=nboot)
      boot_mat <- (boot_mat - data_means[i]) * wild_mat
    }
    boot_means <- colMeans(boot_mat)
    boot_vars <- (colSums(boot_mat^2) - n*boot_means^2)/(n-1)
    t_stars <- sqrt(n)*(boot_means - data_means[i])/sqrt(boot_vars)
    p1 <- mean(t_stars >= data_ts[i])
    p2 <- mean(t_stars <= data_ts[i])
    decisions[i] <- (2*min(p1, p2) < 0.05)
  }
  return(decisions)
  
}

n <- 10
nsim <- 1000
nboot <- 3000

dat <- matrix(rnorm(n*nsim), ncol=nsim)
# mean(reject_or_not_colwise(dat, nboot, wild=TRUE))


reject_or_not_ttest <- function(data_mat){
  n <- nrow(data_mat)
  data_means <- colMeans(data_mat)
  data_vars <- (colSums(data_mat^2) - n*data_means^2)/(n-1)
  data_ts <- sqrt(n)*(data_means)/sqrt(data_vars)
  crit <- qt(0.975, n)
  decisions <- (abs(data_ts) >= crit)
  return(mean(decisions))
}

mean(reject_or_not_ttest(dat))


n <- 12
nboot <- 1000
nsim <- 100
s2 <- 3
x <- rnorm(n, sd = sqrt(s2))


tau_hat_sq_boot <- function(x, nboot){
  n <- length(x)
  boot_mat <- matrix(sample(x, n*nboot, replace=TRUE), ncol=nboot)
  boot_means <- colMeans(boot_mat)
  boot_means_mean <- mean(boot_means)
  tau_hat_sq <- sum((boot_means - boot_means_mean)^2)/(nboot - 1)
  return(tau_hat_sq/n)
}

simu <- function(n=12, nboot=1000, nsim=1000, s2=1){
  d_mat <- matrix(rnorm(n*nsim, sd=sqrt(s2)), ncol=nsim)
  tau_hat_sqs <- c()
  for(i in 1:nsim){
    x <- d_mat[, i]
    tau_hat_sq <- tau_hat_sq_boot()
  }
}
x <- c(
  .59, 1.23, 1.0, .84, .88, 1.71,
  1.81, 1.84, 2.03, 1.39, 1.3, 1.31,
  1.96, 1.33, 2.57, 1.19, 1.01, 2.06,
  1.32, 1.55, 1.28, 0.93, 1.63, 1.24,
  1.83, 1.81, 0.94, 1.46, 1.25, 1.56,
  .61, .83, 1.17, 2.24, 1.68, 1.51
)

tau_hat_sq_boot(x, 10000)

n <- 12
nboot <- 13
B <- apply(matrix(1:n, ncol=nboot, nrow=n), 2, sample, replace=TRUE)

matrix(x[B], ncol=nboot, nrow=n)




