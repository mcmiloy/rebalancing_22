D <- c(1,2,3,4,5,6)
nsim <- 10^5
n <- 10
mu <- 3.5 # NULL hypothesis

xsamples <- matrix(sample(D, n*nsim, TRUE), ncol=nsim)
xmeans <- colMeans(xsamples)
xsds <- sqrt((colSums(xsamples^2)-n*xmeans^2)/(n-1))
t_stats <- sqrt(n)*(xmeans-mu)/xsds 

c1 <- quantile(t_stats, 0.025)
c2 <- quantile(t_stats, 0.975)
hist(t_stats, freq=FALSE) 

mean(abs(t_stats)>=c2)
mean(abs(t_stats) >= qt(0.975, n-1)) #ttest



mu <- 1 # NULL hypothesis: avg waiting time = 1 minute
nsamples <- 10^5
xsamples <- matrix(rexp(n*nsamples), ncol=nsamples)
xmeans <- colMeans(xsamples)
xsds <- sqrt((colSums(xsamples^2) - n*xmeans^2)/(n-1))
t_stats <- sqrt(n)*(xmeans - mu)/xsds


c1 <- quantile(t_stats, 0.025)
c2 <- quantile(t_stats, 0.975)

hist(t_stats, freq=FALSE)

mean(t_stats<c1 | t_stats>c2)
mean(abs(t_stats) >= qt(.975, n-1))


#Assume x is the entire population!! (bottle corks)
x = c(
  .59, 1.23, 1.0, .84, .88, 1.71,
  1.81, 1.84, 2.03, 1.39, 1.3, 1.31,
  1.96, 1.33, 2.57, 1.19, 1.01, 2.06,
  1.32, 1.55, 1.28, 0.93, 1.63, 1.24,
  1.83, 1.81, 0.94, 1.46, 1.25, 1.56,
  .61, .83, 1.17, 2.24, 1.68, 1.51
)

mu <- 1.5 # NULL hypothesis
n <- length(x)
t_orig <- sqrt(n)*(mean(x)-mu)/sd(x)

pop_mean <- mean(x)
nsamples <- 10^6
xsamples <- matrix(sample(x, n*nsamples, replace=TRUE),
                   ncol=nsamples)
xmeans <- colMeans(xsamples)
xsds <- sqrt((colSums(xsamples^2) - n*xmeans^2)/(n-1))
t_stats <- sqrt(n)*(xmeans - pop_mean)/xsds

c1 <- quantile(t_stats, .025); c2 <- quantile(t_stats, .975)
# hist(t_stats, freq=FALSE)

pvalue <- 2*min(mean(t_stats <= t_orig), mean(t_stats>=t_orig))

pvt <- 2*min(pt(t_orig, n-1), 1-pt(t_orig, n-1))
