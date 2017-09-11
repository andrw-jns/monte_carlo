####################

####

library(tidyverse)
# values between 650 and 800
set.seed(101)
# round(rnorm(15, 0, 10))

# For argument's sake, experts' guesses are normally distributed.
# expert <- tibble(w = "experts", x = round(rnorm(15, 750, 20)))

expert <- round(rnorm(15, 750, 20))

# example from  /the-bootstrap-method-for-standard-errors-and-confidence-intervals/
iq <- c(61, 88, 89, 89, 90, 92, 93, 94, 98, 98, 101, 102, 105, 108, 109, 113, 114, 115, 120, 138)

mean_iq <- mean(iq)

mean_simple <- mean(iq)
sd(iq)/sqrt(20)

for(i in 1:99999){
  tmp <- sample(iq, length(iq), replace = T)
  mean_iq[1 + i] <- mean(tmp)
  rm(tmp)
}

mean(mean_iq)
sd(mean_iq)

quantile(mean_iq, probs = c(0.025, 0.5, 0.975))

# report the original mean, along with bootstrapped confindence intervals.

# ie. Resampling is used if one can't make assumptions about the distribution (normal 
# or otherwise, of the expert guesses?)

# expert <- for(i in 1:10){
#   expert <- expert %>% 
#     mutate(dollar = "1")
# }
# 
# add_sample <- function(df){
#   df %>% 
#     mutate(sample = "1")
# }

# for(i in 1:10){
#   expert[2+i] <- sample(expert$x, length(expert$x), replace = T)
#   colnames(expert)[2+i] <- paste0("sample_", i)
# }

means <- mean(expert)

for(i in 1:100){
  tmp <- sample(expert, length(expert), replace = T)
  #colnames(expert)[2+i] <- paste0("sample_", i)
  means[1 + i] <- mean(tmp)
  rm(tmp)
}

standar
sd(means)

expert_long <- gather(expert, sample, estimation, 2:12)

# ggplot(experts3, aes(w, x))+
#   geom_point()+
#   geom_point(data= experts3, aes(y,z))

ggplot(expert_long, aes(sample, estimation))+
  geom_boxplot()+
  facet_wrap(~sample)

