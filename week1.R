# Exercise 2

mysimulation <-function(n,s2,alpha,distribution,nsim){
  
  if(distribution=="normal"){
    x <- matrix(rnorm(n=n*nsim)*sqrt(s2),ncol=nsim)
  }
  if(distribution=="exp"){
    x <- matrix((rexp(nsim*n)-1)*sqrt(s2),ncol=nsim)
  } #(Parameter is known)
  if(distribution=="log-normal"){
    x <- matrix((rlnorm(nsim*n)-1)*sqrt(s2),ncol=nsim)
  }
  if(distribution=="chi-sq"){
    x <- matrix((rchisq(nsim*n, 3)-1)*sqrt(s2),ncol=nsim)
  }
  if(distribution=="uniform"){
    x <- matrix((runif(nsim*n)-0.5)*sqrt(s2),ncol=nsim)
  }
  
  mx <-colMeans(x)
  vx <- (colSums(x^2)-n*mx^2)/(n-1)
  t_statistic <- sqrt(n)*mx/sqrt(vx)
  ttest <- mean((abs(t_statistic >= alpha)))
  

  result <- c(n=n, distribution=distribution, alpha=alpha ,sigma2=s2, ttest=ttest,
                nsim=nsim)
  result
}

distributions = c("normal", "exp", "log-normal", "chi-sq", "uniform")

result <- data.frame(matrix(ncol = 6, nrow = 0))

colnames(result) <- c("n", "distribution", "alpha", "sigma2", "ttest", "nsim")



for (n in c(5, 10, 20, 30, 50, 100)){
  for (distr in distributions){
    new_row <- mysimulation(n, 2, 0.95, distr, 10000)
    result[nrow(result)+1, ] = new_row
  }
}

# Exercise 3

ci_sim <- function(n, s2, nsim){
  x <- matrix(rnorm(n=n*nsim)*sqrt(s2),ncol=nsim)
  mx <- colMeans(x)
  vx <- (colSums(x^2)-n*mx^2)/(n-1)
  
  crit <- qt(0.975, n-1)
  cis_left <-  mx - crit/sqrt(n-1)*sqrt(vx)
  cis_right <- mx + crit/sqrt(n-1)*sqrt(vx)
  cis <- data.frame(left = cis_left, right = cis_right)
  quality <- 1 - mean(result_ci$right < 0 | result_ci$left >0)
}

# interpret quality as percentage of times the mean is in the CI
quality <- ci_sim(10, 2, 100) 

sum_helper <- function(x){
  n <- length(x)
  res <- 0
  for(i in 1:n){
    for(j in 1:n){
        if(i != j){ res <- res + x[i]*x[j]
      }
    }
  }
  res
}

# 3b
mu_sq_estimator <- function(n, mu, nsim){
  x <- matrix(rnorm(n=n*nsim) + mu,ncol=nsim)
  mx <-colMeans(x)
  v1 <- mx^2
  v2 <- c()
  for (i in 1:nsim){
    v2 <- append(v2, sum_helper(x[, i]))
  }
  v2 <- (1/((n-1)*n))*v2
  result_sq_est <- data.frame(MSE.v1=mean((v1-mu^2)^2), MSE.v2=mean((v2-mu^2)^2))
  result_sq_est
}

result_sq_est <- mu_sq_estimator(10, 2, 10000)



