n <- 20
nsim <- 1000
nboot <- 1000


my_wild_boot <-function(n, nboot, nsim, weight_fn){
  T = c(); Tboot = c()
  x_all <- matrix(rnorm(n*nsim),ncol=nsim)
  mx <- colMeans(x_all)
  vx <- (colSums(x_all^2)-n*mx^2)/(n-1)
  T=sqrt(n)*(mx)/sqrt(vx)
  
  for(i in 1:nsim){
    X <- x_all[,i]
    B<- apply(matrix(1:n, ncol=nboot, nrow=n),
              2,sample,replace=TRUE)
    xstar <- matrix(X[B], ncol=nboot,nrow=n) *      # ELEMENTWISE
      matrix(weight_fn(n*nboot), ncol=nboot,nrow=n)
    mxstar <- colMeans(xstar)
    vxstar = (colSums(xstar^2)-n*mxstar^2)/(n-1)
    Tstar = sqrt(n)*(mxstar - mx[i])/sqrt(vxb)
    p1 = mean(Tstar >= T[i])
    p2 = mean(Tstar <= T[i])
    Tboot[i] = (2*min(p1,p2)<0.05)
    
  }
  return(mean(Tboot))
}

T_sim <- my_wild_boot(n, nboot, nsim, rnorm)
print(T_sim)