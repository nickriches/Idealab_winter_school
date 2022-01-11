
# This sets the working directory to the location of the source R file.
# Alternatively, using the menu system go to Session > Set Working Directory
# > To Source File Location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load tidyverse (needs to be installed)

library(tidyverse)

# Read in dataset

df <- read.csv("data.csv")

df.L <- df %>% count(primelangs)

df.L <- df.L %>% arrange(desc(n))

# Now we need to differentiate speakers of single languages from speakers
# of multiple languages. To do this we're going to create a function to 

df %>% filter(primelangs == "Spanish" | 
              primelangs == "French" |
              primelangs == "German") -> df

# number_of_commas <- function(x){
#   result <- str_count(x, ",")
#   return(result)
# }
# 
# df$num_commas <- sapply(df$primelangs, number_of_commas)
# 
# df$num_langs <- df$num_commas + 1
# 
# table(df$num_langs)
# 
# df %>% filter(id < 50000) -> df

# Now we're going to turn wide data into long data

df <- df %>%
      gather(key = question, value = answer, starts_with("q")) %>%
      arrange(id)




# https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached

df.qs <- read.csv("questionnaire.csv")

df %>% merge(df.qs, by.x = "question", by.y = "q")

df %>% filter(question == "q21_1") -> df.21

model <- glm(answer ~ primelangs,family=binomial(link='logit'),data=df.21)
