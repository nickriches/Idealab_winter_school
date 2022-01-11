

setwd("~/Dropbox/Desktop/Newcastle/research/hartshorne")

df <- read.csv("data.csv")

library(tidyverse)

df2 <- count(df, df$primelangs)
df3 <- arrange(df2, desc(df2$n))

df$one <- 1

df %>%
  group_by(primelangs) %>%
  mutate(one = 1) %>%
  summarise(n = sum(one)) ->
  df.L


df %>%
  gather(key = "q", value = "answer", starts_with("q"))


