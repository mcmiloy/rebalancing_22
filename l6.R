library(tibble)
library(magrittr)
library(ggplot2)
library(dplyr)

df <- tribble(
~ID, ~Before, ~After,
1, 8, 6,
2, 7, 6,
3, 7, 6,
4, 4, 6,
5, 6, 3,
6, 3, 6,
7, 6, 3,
8, 6, 5,
9, 3, 2,
10, 9, 2,
11, 4, 1,
12, 7, 1,
13, 5, 4,
14, 3, 6,
15, 8, 1
)

x <- df$Before; y <- df$After
boxplot(x, y, names=c("Before", "After"))


grid1=expand.grid(x,y)
prop1 = (grid1[,1]<grid1[,2]) +
  1/2*(grid1[,1]==grid1[,2])
p=mean(prop1)
p
#Y<X+1/2(X=Y)
prop2 = (grid1[,2]<grid1[,1]) +
  1/2*(grid1[,2]==grid1[,1])

# q=mean(prop2)
# q
# p+q

n<-length(x)
xy<-c(x,y)
rxy <- rank(xy)
mRx <-mean(rxy[1:n])
mRy <-mean(rxy[(n+1):(2*n)])
phat<-1/(2*n)*(mRy-mRx)+1/2


xy<-c(x,y)
rxy <- rank(xy)
rx <-rank(x)
ry <-rank(y)
Z1k <- 1/n*(rxy[1:n]-rx)
Z2k <- 1/n*(rxy[(n+1):(2*n)]-ry)
Dk <- Z1k-Z2k
sigmahat <-var(Dk)


T=sqrt(n)*(phat-1/2)/sqrt(sigmahat)
pvalue=2*min(pt(T,n-1),1-pt(T,n-1))
crit <-qt(0.975,n-1)
SE <- sqrt(sigmahat)/sqrt(n)
Lower = phat -crit*SE
Upper = phat +crit*SE

library(lawstat)
brunner.munzel.test(x, y)

