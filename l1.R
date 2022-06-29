library(magrittr)

x = c(
  .59, 1.23, 1.0, .84, .88, 1.71,
  1.81, 1.84, 2.03, 1.39, 1.3, 1.31,
  1.96, 1.33, 2.57, 1.19, 1.01, 2.06,
  1.32, 1.55, 1.28, 0.93, 1.63, 1.24,
  1.83, 1.81, 0.94, 1.46, 1.25, 1.56,
  .61, .83, 1.17, 2.24, 1.68, 1.51
)
n <- length(x)
mx <- mean(x)
vx <-  var(x)
t_stat <- sqrt(n)*(mx-1.5)/sqrt(vx)
p <- 2*min(pt(t_stat, n-1), 1-pt(t_stat, n-1))

m_reac <- c(2.4, 3.0, 3.0, 2.2, 2.2, 2.2, 2.2, 2.8, 2.0, 3.0)
n_mice <- m_reac %>% length()
mx <- mean(m_reac)
vx <- var(m_reac)
crit <- qt(0.975, n_mice)
lower <-mx - (crit*sqrt(vx))/sqrt(n_mice)
upper <-mx + (crit*sqrt(vx))/sqrt(n_mice)

# Exercise 2

reject_or_not <- function(x, mu=0){
  n <- length(x)
  mx <- mean(x)
  vx <-  var(x)
  t_stat <- sqrt(n)*(mx-mu)/sqrt(vx)
  p <- 2*min(pt(t_stat, n-1), 1-pt(t_stat, n-1))
  return(p<=0.05)
}

simu <- function(n=15){
  t1_errors <- c()
  for(del in seq(0, 1, 0.1)){
    results <- c()
    for(i in 1:1000){
      results[i] <- reject_or_not(rnorm(n)+del)
    }
    t1_errors[length(t1_errors)+1] <- mean(results)
  }
  return(t1_errors)
}

