source('./installation-instructions.R')
library(tibble)
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
losses <- read.table('./Data/NonFleetCo507.txt',
                        header = TRUE)
losses <- as_tibble(losses)

#overview of the tibble

str(losses)
head(losses)

#empirical distribution

empirical <- losses$Clm_Count %>%
  table %>% prop.table %>% as.numeric
k <- 1:(length(empirical) - 1)
ab0_relation <- empirical[k+1] / empirical[k] * k
ab0_data <- tibble(k = k, ab0_rel = ab0_relation)
ab0_data %>% lm(ab0_rel ~ k, data = .)

#empirical claim frequency 
mean(losses$Clm_Count)
sum(losses$Clm_Count)/sum(losses$TLength)
weighted.mean(losses$Clm_Count/losses$TLength, losses$TLength)
losses %>%
  summarize(emp_freq = sum(Clm_Count) / sum(TLength))

#variance
m <- sum(losses$Clm_Count)/sum(losses$TLength)
m
## [1] 0.1393352
var <- sum((losses$Clm_Count - m * losses$TLength)^2)/
  sum(losses$TLength)
var

