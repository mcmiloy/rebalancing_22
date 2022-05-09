library(tidyverse)
n_sim <- 10000
n_res <- 10000
n <- 10; s2 <- 1
alpha <- 0.05

stat_values <- tibble(
  `pvalue` = numeric(),
  `pvalue_ttest` = numeric(),
  `mean_original` = numeric()
)


for(i in 1:n_sim){
  x <- rnorm(n, mean = 0, sd = sqrt(s2))
  
  mx <- mean(x)
  s_hat <- sqrt((sum(x^2)-n*mx^2)/(n-1))
  t_stat_orig <- sqrt(n)*mx/s_hat
  
  
  
  x_res <- matrix(sample(x, n*n_res, replace = TRUE), ncol=n_res)
  mx_res <-colMeans(x_res)
  sdx_res <- sqrt((colSums(x_res^2)-n*mx_res^2)/(n-1))
  t_stat_res <- sqrt(n)*((mx_res-mx)/sdx_res)
  
  pvalue <- 2*min(mean(t_stat_res <= t_stat_orig), 
                  mean(t_stat_res >= t_stat_orig))
  
  pvalue_ttest <- 2*min(pt(t_stat_orig, n-1),
                        1-pt(t_stat_orig, n-1)) # assumes std. normal distribution
  
  stat_values <- stat_values %>% 
    add_row(pvalue = pvalue,
            pvalue_ttest = pvalue_ttest,
            mean_original = mx)
}


print(stat_values)

t1_error_sim <- mean(stat_values$pvalue <= alpha)
print(t1_error_sim)


err <- (1.96/n_sim)*sqrt(alpha*(1-alpha))
prec_ival <- tribble(
  ~lower_b,    ~upper_b,
   alpha - err, alpha + err
)

if(t1_error_sim >= prec_ival$upper_b){
  print("test is liberal")
}
