n <- 30
nsim <- 1000
nboot <- 1000
Distribution <- "Normal"

myboot <- function(n,Distribution,nboot,nsim){
  T = Tboot = Tboot_norm = Tboot_rademacher = Tboot_unif = c()
  #------------Test Statistic of the Sample------------#
  if(Distribution == "Normal"){
    x <- matrix(rnorm(n*nsim),ncol=nsim)}
  if(Distribution == "Exp"){
    x <- matrix(rexp(n*nsim)-1,ncol=nsim)}
  mx <- colMeans(x)
  vx <- (colSums(x^2)-n*mx^2)/(n-1)
  T=sqrt(n)*(mx)/sqrt(vx)
  
  #------------Simulate the Nonparametric Bootstrap-----#
  B <- apply(matrix(1:n,ncol=nboot,nrow=n),2,sample,replace=TRUE)
  for(i in 1:nsim){
    xstar = matrix(x[,i][B],ncol=nboot,nrow=n)
    mxstar = colMeans(xstar)
    vxb = (colSums(xstar^2)-n*mxstar^2)/(n-1)
    Tstar = sqrt(n)*(mxstar - mx[i])/sqrt(vxb)
    p1 = mean(Tstar >= T[i])
    p2 = mean(Tstar <= T[i])
    Tboot[i] = (2*min(p1,p2)<0.05)
  }
  
  #------------Simulate the Wild Bootstrap-------------#
  for(i in 1:nsim){
    xstar <- x[,i]
    
    norm_weights <- matrix(rnorm(n*nboot),
                           ncol=nboot)
    rademacher_weights <- matrix(sample(c(1, -1), size=n*nboot, replace = TRUE),
                                 ncol=nboot)
    unif_weights <- matrix(runif(n*nboot, min=-sqrt(12)/2, max=sqrt(12)/2),
                              ncol=nboot)
    
    xs_norm <- std_weights * xstar
    xs_rademacher <- rademacher_weights * xstar
    xs_unif <- unif_weights * xstar
    
    mx_norm = colMeans(xs_norm)
    vx_norm = (colSums(xs_norm^2)-n*mx_norm^2)/(n-1)
    T_norm = sqrt(n)*(mx_norm - mx[i])/sqrt(vx_norm)
    p1_norm = mean(T_norm >= T[i])
    p2_norm =  mean(T_norm <= T[i])
    Tboot_norm[i] = (2*min(p1_norm,p2_norm)<0.05)
    
    
    mx_rademacher = colMeans(xs_rademacher)
    vx_rademacher = (colSums(xs_rademacher^2)-n*mx_rademacher^2)/(n-1)
    T_rademacher = sqrt(n)*(mx_rademacher - mx[i])/sqrt(vx_rademacher)
    p1_rademacher = mean(T_rademacher >= T[i])
    p2_rademacher =  mean(T_rademacher <= T[i])
    Tboot_rademacher[i] = (2*min(p1_rademacher,p2_rademacher)<0.05)
 
    
    mx_unif = colMeans(xs_unif)
    vx_unif = (colSums(xs_unif^2)-n*mx_unif^2)/(n-1)
    T_unif = sqrt(n)*(mx_unif - mx[i])/sqrt(vx_unif)
    p1_unif = mean(T_unif >= T[i])
    p2_unif =  mean(T_unif <= T[i])
    Tboot_unif[i] = (2*min(p1_unif,p2_unif)<0.05)

    
    
  }
  
  
  # # NOTE: "*" is ELEMENTWISE multiplication!!
  # for(i in 1:nsim){
  #   xstar <- x[,i]
  #   mxstar = colMeans(xstar)
  #   vxb = (colSums(xstar^2)-n*mxstar^2)/(n-1)
  #   
  #   xs_norm <- std_weights * xstar
  #   xs_rademacher <- rademacher_weights * xstar
  #   xs_unif <- unif_weights * xstar
  #   
  #   Tstar = sqrt(n)*(mxstar - mx[i])/sqrt(vxb)
  #   p1 = mean(Tstar >= T[i])
  #   p2 = mean(Tstar <= T[i])
  #   Tboot[i] = (2*min(p1,p2)<0.05)
  #   
  #   Tstar = sqrt(n)*(mxstar - mx[i])/sqrt(vxb)
  #   p1 = mean(Tstar >= T[i])
  #   p2 = mean(Tstar <= T[i])
  #   Tboot[i] = (2*min(p1,p2)<0.05)
  #   
  #   Tstar = sqrt(n)*(mxstar - mx[i])/sqrt(vxb)
  #   p1 = mean(Tstar >= T[i])
  #   p2 = mean(Tstar <= T[i])
  #   Tboot[i] = (2*min(p1,p2)<0.05)
  

}
myboot(10,"Normal",1000,1000)

result = data.frame(n=n,nsim=nsim,nboot=nboot,Dist=Distribution,
                    T=mean(abs(T)>qt(0.975,n-1)),
                    Tboot = mean(Tboot),
                    Tboot_norm = mean(Tboot_norm),
                    Tboot_rademacher = mean(Tboot_rademacher),
                    Tboot_unif = mean(Tboot_unif)
                    )


