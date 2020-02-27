library(tidyverse)
library(rsm)


dat <- read.csv("potato_starch_results_plus10_2018-11-27.csv")

df2_coded <- coded.data(dat,cooltime~(x1 - 228)/90, heattime~(x2 - 222)/90, holdtime~(x3 - 150)/90, 
                        stirrpm~(x4-160)/40, starchg~(x5-2)/.1 )


mylm  <- lm(peak ~ cooltime + heattime + holdtime + stirrpm + starchg, data = df2_coded)

summary(mylm)

myrsm  <- rsm(peak ~ FO(cooltime , heattime , holdtime , stirrpm, starchg), data = df2_coded)

summary(myrsm)

myrsm2  <- rsm(peak ~ SO(cooltime , heattime , holdtime , stirrpm, starchg), data = df2_coded)

summary(myrsm2)
# Look for model with possible better interactions

pairs(dat, panel = panel.smooth)


# Trough and Final are strongly related to one another
summary(lm(final ~ trough, data = dat))



myrsm_t  <- rsm(trough ~ FO(cooltime , heattime , holdtime , stirrpm, starchg), data = df2_coded)

summary(myrsm_t)

myrsm_b <- rsm(breakdown ~ FO(cooltime , heattime , holdtime , stirrpm, starchg), data = df2_coded)

summary(myrsm_b)


mylm  <- lm(peak ~ cooltime + heattime + holdtime + stirrpm + starchg + stirrpm*starchg, data = df2_coded)

summary(mylm)


# Testing validation against my first order model plus interaction and the SO model that was made by Bro. Palmer... Done with the LOOV Method

library(boot)
glm.fit <- glm(peak ~ cooltime + heattime + holdtime + stirrpm + starchg + stirrpm*starchg, data = df2_coded)

cv.glm(df2_coded, glm.fit)$delta 


glm.fit2 <- glm(peak ~ cooltime + heattime + holdtime + stirrpm + starchg + I(cooltime^2) + I(heattime^2) + I(holdtime^2) + I(stirrpm^2) + I(starchg^2) + cooltime:heattime + cooltime:holdtime + cooltime:stirrpm + cooltime:starchg + heattime:holdtime + heattime:stirrpm + heattime:starchg + holdtime:stirrpm + holdtime:starchg+ stirrpm:starchg, data = df2_coded)

cv.glm(df2_coded, glm.fit2)$delta

# It seems as though the model performs 1/3 better than the other The residual squared error for the first model is around 68 while the second is around 43. I am going to continue using this model because it seems as though it is the best both in prediction power and accuracy

# It will be interesting to see if this model is still the best for the other starches as well

