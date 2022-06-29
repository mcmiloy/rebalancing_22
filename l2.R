set.seed(1)
x<-rnorm(10)
boxplot(x)

set.seed(1)
x<-rexp(10)
boxplot(x)

set.seed(1)
x<-rchisq(10, 7)
boxplot(x)

reject_or_not <- function(mat, crit){
  n <- nrow(mat)
  mx <- colMeans(mat)
  vx <- (colSums(mat^2) - n*mx^2)/(n-1)
  t_stats <- sqrt(n)*mx/sqrt(vx)
  return(abs(t_stats)>=crit)
}


simu <- function(n, s2, nsim, distr){
  crit <- qt(0.975, n-1)
  if(distr=="normal"){
    x <- matrix(rnorm(n*nsim), ncol=nsim)*sqrt(s2)
  }
  if(distr=="exp"){
    x <- (matrix(rexp(n*nsim),ncol=nsim)-1)*sqrt(s2)
  }
  ttest <- reject_or_not(x, crit)
  result <- data.frame(n=n, alpha=0.05, sigma2=s2,
                       tTest=mean(ttest))
  return(result)
}


estim <- function(mat){
  num_cols <- ncol(mat)
  num_rows <- nrow(mat)
  v1 <- colMeans(mat)^2
  v2 <- c()
  for(i in 1:num_cols){
    x <- mat[,i]
    temp <- outer(x, x)
    diag(temp) <- 0
    v2[i] <- sum(temp)/(num_rows*(num_rows-1))
  }
  return(list(v1, v2))
}
x <- matrix(rnorm(100*5), ncol=5)

simu_estim <- function(n, mu, s2, nsim){
  x <- matrix(rnorm(n*nsim)*sqrt(s2)+mu, ncol=nsim)
  temp <- estim(x)
  v1 <- unlist(temp[1]); v2 <- unlist(temp[2])
  res <- data.frame(n=n, mu=mu, s2=s2,
                    bias.v1=mean(v1-mu^2),
                    MSE.v1=mean((v1-mu^2)^2),
                    bias.v2=mean(v2-mu^2),
                    MSE.v2=mean((v2-mu^2)^2))
  return(res)
}

res <- simu_estim(10, 7, 20, 1000); print(res)
