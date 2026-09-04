library(tidyverse)
library(gridExtra)

fnc <- function(n){
  vec <- numeric(n)
  
  for(i in 1:n){
    vec[i] <- sum(sample(1:6, 2, replace = TRUE))
  }
  return(as.data.frame(vec))
}


data <- fnc(10)


data %>% 
  ggplot(aes(x = vec)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", binwidth = .5) +
  geom_density(alpha = .2, fill = "orange") +
  scale_x_continuous(breaks = seq(min(data$vec), max(data$vec), by = 1)) +
  theme_minimal() +
  labs(title = paste("6d6 Roll Frequency With", length(data$vec), "Rolls")) +
  theme(plot.title = element_text(hjust = .5))



