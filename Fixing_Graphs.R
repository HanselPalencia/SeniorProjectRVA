x <- -1
my_sample <- c()

for (i in 1:201) {
  
  my_sample[i] <- x
  x = x + .01
  
  
}

library(tidyverse)
library(readxl)
library(rsm)

data_w <- read_xlsx("wheat_results_2018-12-11.xlsx")


testlm <- lm(breakdown ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data_w)



my_predict <- predict(testlm, newdata = data.frame(cooltime = my_sample, heattime = heat, holdtime = hold, stirrpm = stir, starchg = starch))


test <- cbind(my_sample, my_predict)

ggplot(data = data.frame(test)) +
  scale_y_continuous(limits = c(130, 230), breaks = seq(130, 230, 20)) +
  geom_hline(aes(yintercept = predict(testlm, newdata = data.frame(cooltime = -.562, heattime = 0, holdtime = 0, stirrpm = 0, starchg = 0)))) +
  geom_line(aes(x = my_sample, y = my_predict)) +
  labs(y = "Breakdown")
  


predict(testlm, newdata = data.frame(cooltime = 0,holdtime = 0,heattime = 0,stirrpm =0,starchg = 0))





































