

library(tidyverse)

setwd("~/Dropbox/Desktop/Newcastle/research/hartshorne")


df <- read.csv("data.csv")


df %>%
  count(primelangs) %>%
  arrange(desc(n)) ->
  df.L

df %>%
  mutate(one = 1) %>%
  group_by(primelangs) %>%
  summarise(n = sum(one)) ->
  df.L

df %>%
  filter(primelangs == "Spanish" |
         primelangs == "German" |
         primelangs == "French") %>%
  gather(key = "q", value = "answer", starts_with("q")) -> df

questions <- read.csv("data_munging_workshop/data-munging-workshop/questionnaire.csv")

df %>%
  merge(questions, by = "q") ->
  df

df$primelangs <- relevel(df$primelangs, ref = "Spanish")

df %>%
  filter(q == "q21_1") %>%
  glm(answer.x ~ primelangs, family=binomial(link='logit'), data = .) ->
  model
