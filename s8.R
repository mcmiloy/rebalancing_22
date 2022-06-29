library(tibble)
library(ggplot2)
library(dplyr)
library(magrittr)

df <- tribble(
~before, ~after,
79.1, 73.3,
82.0, 74.7,
80.2, 75.1,
82.8, 77.4,
82.0, 76.5,
79.6, 75.3,
80.2, 74.3,
77.1, 69.1,
79.3, 75.4,
79.9, 75.9,
81.8, 76.6,
79.5, 74.0,
80.3, 75.1,
78.3, 72.7,
81.8, 74.0,
)

ggplot(df, aes(x=before, y=after)) + 
  geom_point()

df_centered <- tibble(
  before = df$before - mean(df$before),
  after = df$after - mean(df$after)
)

ggplot(df_centered, aes(x=before, y=after)) + 
  geom_point()

cs_split <- tibble(
  pp = df_centered %>%
    filter(before >= 0, after >= 0) %>%
    nrow(),
  np = df_centered %>%
    filter(before < 0, after >= 0) %>%
    nrow(),
  pn = df_centered %>%
    filter(before >= 0, after < 0) %>%
    nrow(),
  nn = df_centered %>%
    filter(before < 0, after < 0) %>%
    nrow(),
)

# sum(cs_split) # sanity check
cov(df_centered)

# t.test(df_centered$before, df_centered$after)

t.test(df$before, df$after, paired = TRUE)


n <- nrow(df)

diff <- df$before - df$after
mdiff <- mean(diff)
sddiff <- sd(diff)
tval <- sqrt(n)*(mdiff/sddiff)
p <- 2*min(pt(tval, n-1), 1-pt(tval, n-1))



