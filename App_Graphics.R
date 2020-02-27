# Acceptable Values for each of the four y variables

library(rsm)
library(tidyverse)

# Peak - Lower = 6950; Upper = 7050; Expected =  7000
# Trough - Lower = 2425; Upper = 2475; Expected = 2450
# Breakdown - Lower = 4500; Upper = 4600; Expected = 4550
# Final - Lower = 2975; Upper = 3025; Expected = 3000


dat <- read.csv("potato_starch_results_plus10_2018-11-27.csv")

df2_coded <- coded.data(dat,cooltime~(x1 - 228)/90, heattime~(x2 - 222)/90, holdtime~(x3 - 150)/90, 
                        stirrpm~(x4-160)/40, starchg~(x5-2)/.1 )

# Using coded data will make it easier to interpret everything as well as place inputs in the dashboard

# Peak Model
peaklm_coded <- lm(peak ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = df2_coded)
cpeak_C <- coef(peaklm_coded)
summary(peaklm_coded)

troughlm_coded <- lm(trough ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = df2_coded)
ctrough_C <- coef(troughlm_coded)
summary(troughlm_coded)

breaklm_coded <- lm(breakdown ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = df2_coded)
cbreak_C <- coef(breaklm_coded)
summary(breaklm_coded)

finallm_coded <- lm(final ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = df2_coded)
cfinal_C <- coef(finallm_coded)
summary(finallm_coded)


# As we get more information from exploring the relationships between the x variables we will be able to make better claims about how each of the x variables affect each y variable



# Univeral theme that will be on all plots

  
  


th <- theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

th_bottom <- theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())


# All Example X1 example plots. These are a bit off because they don't use the same values all the way through

# Peak
# p1 <- df2_coded %>% 
# ggplot(aes(x = as.factor(cooltime), y = peak)) +
#   #geom_line() +
#   stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*x + cpeak_C[3]*0 + cpeak_C[4]*1 + cpeak_C[5]*0 + cpeak_C[6]*1 + cpeak_C[7]*x*1 + cpeak_C[8]*x*1 + cpeak_C[9]*x*1 + cpeak_C[10]*x*-1+cpeak_C[11]*1*1 + cpeak_C[12]*1*1 + cpeak_C[13]*-1*0 + cpeak_C[14]*1*-1 + cpeak_C[15]*0*1 + cpeak_C[16]*-1*0 + cpeak_C[17]*x^2 + cpeak_C[18]*0^2 + cpeak_C[19]*1^2 + cpeak_C[20]*1^2 + cpeak_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
# th_left

 # Peak
 # p1 <- df2_coded %>% 
 # ggplot(aes(x = as.factor(cooltime), y = peak)) +
 #   #geom_line() +
 #   stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*x + cpeak_C[3]*0 + cpeak_C[4]*1 + cpeak_C[5]*0 + cpeak_C[6]*1 + cpeak_C[7]*x*1 + cpeak_C[8]*x*1 + cpeak_C[9]*x*1 + cpeak_C[10]*x*-1+cpeak_C[11]*1*1 + cpeak_C[12]*1*1 + cpeak_C[13]*-1*0 + cpeak_C[14]*1*-1 + cpeak_C[15]*0*1 + cpeak_C[16]*-1*0 + cpeak_C[17]*x^2 + cpeak_C[18]*0^2 + cpeak_C[19]*1^2 + cpeak_C[20]*1^2 + cpeak_C[21]*-1^2) +
 #   scale_y_continuous(limits = c(6950, 7050), breaks = seq(6950, 7050, 50))
 #   geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
 #   geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
 #   geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
 # th_left


# 
# # Trough
# t1 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(cooltime), y = trough)) +
#   #geom_line() +
#   stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*x + ctrough_C[3]*0 + ctrough_C[4]*1 + ctrough_C[5]*0 + ctrough_C[6]*1 + ctrough_C[7]*x*1 + ctrough_C[8]*x*1 + ctrough_C[9]*x*1 + ctrough_C[10]*x*-1+ctrough_C[11]*1*1 + ctrough_C[12]*1*1 + ctrough_C[13]*-1*0 + ctrough_C[14]*1*-1 + ctrough_C[15]*0*1 + ctrough_C[16]*-1*0 + ctrough_C[17]*x^2 + ctrough_C[18]*0^2 + ctrough_C[19]*1^2 + ctrough_C[20]*1^2 + ctrough_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#   th_left
#   
# 
# # Breakdown
# b1 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(cooltime), y = breakdown)) +
#   #geom_line() +
#   stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*x + cbreak_C[3]*0 + cbreak_C[4]*1 + cbreak_C[5]*0 + cbreak_C[6]*1 + cbreak_C[7]*x*1 + cbreak_C[8]*x*1 + cbreak_C[9]*x*1 + cbreak_C[10]*x*-1+cbreak_C[11]*1*1 + cbreak_C[12]*1*1 + cbreak_C[13]*-1*0 + cbreak_C[14]*1*-1 + cbreak_C[15]*0*1 + cbreak_C[16]*-1*0 + cbreak_C[17]*x^2 + cbreak_C[18]*0^2 + cbreak_C[19]*1^2 + cbreak_C[20]*1^2 + cbreak_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#   th_left
# 
# 
# # Final 
# f1 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(cooltime), y = final)) +
#   #geom_line() +
#   stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*x + cfinal_C[3]*0 + cfinal_C[4]*1 + cfinal_C[5]*0 + cfinal_C[6]*1 + cfinal_C[7]*x*0 + cfinal_C[8]*x*1 + cfinal_C[9]*x*1 + cfinal_C[10]*x*-1+cfinal_C[11]*0*1 + cfinal_C[12]*0*1 + cfinal_C[13]*-0*0 + cfinal_C[14]*1*-1 + cfinal_C[15]*0*1 + cfinal_C[16]*-1*0 + cfinal_C[17]*x^2 + cfinal_C[18]*0^2 + cfinal_C[19]*1^2 + cfinal_C[20]*1^2 + cfinal_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#   th_left
# 
# 
# # All plots with Heat Time as x variable 
# 
# # Peak
# # p2 <- df2_coded %>% 
# #   ggplot(aes(x = as.factor(heattime), y = peak)) +
# #   stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*1 + cpeak_C[3]*x + cpeak_C[4]*1 + cpeak_C[5]*0 + cpeak_C[6]*1 + cpeak_C[7]*1*x + cpeak_C[8]*1*1 + cpeak_C[9]*1*1 + cpeak_C[10]*1*-1+cpeak_C[11]*x*1 + cpeak_C[12]*x*1 + cpeak_C[13]*x*0 + cpeak_C[14]*1*-1 + cpeak_C[15]*0*1 + cpeak_C[16]*-1*0 + cpeak_C[17]*1^2 + cpeak_C[18]*x^2 + cpeak_C[19]*1^2 + cpeak_C[20]*1^2 + cpeak_C[21]*-1^2)+
# #   geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
# #   geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
# #   geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
# #   th
# 
# 
# 
# # Trough
# t2 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(heattime), y = trough)) +
#   #geom_line() +
#   stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*1 + ctrough_C[3]*x + ctrough_C[4]*1 + ctrough_C[5]*0 + ctrough_C[6]*1 + ctrough_C[7]*1*x + ctrough_C[8]*1*1 + ctrough_C[9]*1*1 + ctrough_C[10]*1*-1+ctrough_C[11]*x*1 + ctrough_C[12]*x*1 + ctrough_C[13]*x*0 + ctrough_C[14]*1*-1 + ctrough_C[15]*0*1 + ctrough_C[16]*-1*0 + ctrough_C[17]*1^2 + ctrough_C[18]*x^2 + ctrough_C[19]*1^2 + ctrough_C[20]*1^2 + ctrough_C[21]*-1^2)+
#   geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# # Breakdown
# b2 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(heattime), y = breakdown)) +
#   #geom_line() +
#   stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*1 + cbreak_C[3]*x + cbreak_C[4]*1 + cbreak_C[5]*0 + cbreak_C[6]*1 + cbreak_C[7]*1*x + cbreak_C[8]*1*1 + cbreak_C[9]*1*1 + cbreak_C[10]*1*-1+cbreak_C[11]*x*1 + cbreak_C[12]*x*1 + cbreak_C[13]*x*0 + cbreak_C[14]*1*-1 + cbreak_C[15]*0*1 + cbreak_C[16]*-1*0 + cbreak_C[17]*1^2 + cbreak_C[18]*x^2 + cbreak_C[19]*1^2 + cbreak_C[20]*1^2 + cbreak_C[21]*-1^2)+
#   geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # Final
# f2 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(heattime), y = final)) +
#   #geom_line() +
#   stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*1 + cfinal_C[3]*x + cfinal_C[4]*1 + cfinal_C[5]*0 + cfinal_C[6]*1 + cfinal_C[7]*1*x + cfinal_C[8]*1*1 + cfinal_C[9]*1*1 + cfinal_C[10]*1*-1+cfinal_C[11]*x*1 + cfinal_C[12]*x*1 + cfinal_C[13]*x*0 + cfinal_C[14]*1*-1 + cfinal_C[15]*0*1 + cfinal_C[16]*-1*0 + cfinal_C[17]*1^2 + cfinal_C[18]*x^2 + cfinal_C[19]*1^2 + cfinal_C[20]*1^2 + cfinal_C[21]*-1^2)+
#   geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # All plots with Hold time as x variable
# 
# # Peak
# p3 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(holdtime), y = peak)) +
#   #geom_line() +
#   stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*1 + cpeak_C[3]*0 + cpeak_C[4]*x + cpeak_C[5]*0 + cpeak_C[6]*1 + cpeak_C[7]*1*1 + cpeak_C[8]*1*x + cpeak_C[9]*1*1 + cpeak_C[10]*1*-1+cpeak_C[11]*1*x + cpeak_C[12]*1*1 + cpeak_C[13]*1*0 + cpeak_C[14]*x*-1 + cpeak_C[15]*x*1 + cpeak_C[16]*-1*0 + cpeak_C[17]*1^2 + cpeak_C[18]*1^2 + cpeak_C[19]*x^2 + cpeak_C[20]*1^2 + cpeak_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # Trough
# t3 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(holdtime), y = trough)) +
#   #geom_line() +
#   stat_function(fun = function(x)ctrough_C[1] +ctrough_C[2]*1 +ctrough_C[3]*0 +ctrough_C[4]*x +ctrough_C[5]*0 +ctrough_C[6]*1 +ctrough_C[7]*1*1 +ctrough_C[8]*1*x +ctrough_C[9]*1*1 +ctrough_C[10]*1*-1+cpeak_C[11]*1*x +ctrough_C[12]*1*1 +ctrough_C[13]*1*0 +ctrough_C[14]*x*-1 +ctrough_C[15]*x*1 +ctrough_C[16]*-1*0 +ctrough_C[17]*1^2 +ctrough_C[18]*1^2 +ctrough_C[19]*x^2 +ctrough_C[20]*1^2 +ctrough_C[21]*-1^2)+
#   geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # Breakdown
# b3 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(holdtime), y = breakdown)) +
#   #geom_line() +
#   stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*1 + cbreak_C[3]*0 + cbreak_C[4]*x + cbreak_C[5]*0 + cbreak_C[6]*1 + cbreak_C[7]*1*1 + cbreak_C[8]*1*x + cbreak_C[9]*1*1 + cbreak_C[10]*1*-1+cbreak_C[11]*1*x + cbreak_C[12]*1*1 + cbreak_C[13]*1*0 + cbreak_C[14]*x*-1 + cbreak_C[15]*x*1 + cbreak_C[16]*-1*0 + cbreak_C[17]*1^2 + cbreak_C[18]*1^2 + cbreak_C[19]*x^2 + cbreak_C[20]*1^2 + cbreak_C[21]*-1^2)+
#   geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# # Final
# 
# f3 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(holdtime), y = final)) +
#   #geom_line() +
#   stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*1 + cfinal_C[3]*0 + cfinal_C[4]*x + cfinal_C[5]*0 + cfinal_C[6]*1 + cfinal_C[7]*1*1 + cfinal_C[8]*1*x + cfinal_C[9]*1*1 + cfinal_C[10]*1*-1+cfinal_C[11]*1*x + cfinal_C[12]*1*1 + cfinal_C[13]*1*0 + cfinal_C[14]*x*-1 + cfinal_C[15]*x*1 + cfinal_C[16]*-1*0 + cfinal_C[17]*1^2 + cfinal_C[18]*1^2 + cfinal_C[19]*x^2 + cfinal_C[20]*1^2 + cfinal_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # All variables with stir rpm as the x variable
# 
# # Peak
# 
# p4 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(stirrpm), y = peak)) +
#   #geom_line() +
#   stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*1 + cpeak_C[3]*0 + cpeak_C[4]*1 + cpeak_C[5]*x + cpeak_C[6]*1 + cpeak_C[7]*1*1 + cpeak_C[8]*1*0 + cpeak_C[9]*1*x + cpeak_C[10]*1*-1 + cpeak_C[11]*1*0 + cpeak_C[12]*1*x + cpeak_C[13]*1*0 + cpeak_C[14]*0*x + cpeak_C[15]*0*1 + cpeak_C[16]*x*0 + cpeak_C[17]*1^2 + cpeak_C[18]*1^2 + cpeak_C[19]*0^2 + cpeak_C[20]*x^2 + cpeak_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# 
# # Trough
# 
# t4 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(stirrpm), y = trough)) +
#   #geom_line() +
#   stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*1 + ctrough_C[3]*0 + ctrough_C[4]*1 + ctrough_C[5]*x + ctrough_C[6]*1 + ctrough_C[7]*1*1 + ctrough_C[8]*1*0 + ctrough_C[9]*1*x + ctrough_C[10]*1*-1 + ctrough_C[11]*1*0 + ctrough_C[12]*1*x + ctrough_C[13]*1*0 + ctrough_C[14]*0*x + ctrough_C[15]*0*1 + ctrough_C[16]*x*0 + ctrough_C[17]*1^2 + ctrough_C[18]*1^2 + ctrough_C[19]*0^2 + ctrough_C[20]*x^2 + ctrough_C[21]*-1^2)+
#   geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # Breakdown
# 
# b4 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(stirrpm), y = breakdown)) +
#   #geom_line() +
#   stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*1 + cbreak_C[3]*0 + cbreak_C[4]*1 + cbreak_C[5]*x + cbreak_C[6]*1 + cbreak_C[7]*1*1 + cbreak_C[8]*1*0 + cbreak_C[9]*1*x + cbreak_C[10]*1*-1 + cbreak_C[11]*1*0 + cbreak_C[12]*1*x + cbreak_C[13]*1*0 + cbreak_C[14]*0*x + cbreak_C[15]*0*1 + cbreak_C[16]*x*0 + cbreak_C[17]*1^2 + cbreak_C[18]*1^2 + cbreak_C[19]*0^2 + cbreak_C[20]*x^2 + cbreak_C[21]*-1^2)+
#   geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # Final
# 
# f4 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(stirrpm), y = final)) +
#   #geom_line() +
#   stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*1 + cfinal_C[3]*0 + cfinal_C[4]*1 + cfinal_C[5]*x + cfinal_C[6]*1 + cfinal_C[7]*1*1 + cfinal_C[8]*1*0 + cfinal_C[9]*1*x + cfinal_C[10]*1*-1 + cfinal_C[11]*1*0 + cfinal_C[12]*1*x + cfinal_C[13]*1*0 + cfinal_C[14]*0*x + cfinal_C[15]*0*1 + cfinal_C[16]*x*0 + cfinal_C[17]*1^2 + cfinal_C[18]*1^2 + cfinal_C[19]*0^2 + cfinal_C[20]*x^2 + cfinal_C[21]*-1^2) +
#   geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# 
# 
# # All variables with starch g as the x variable
# 
# 
# # Peak
# 
# p5 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(starchg), y = peak)) +
#   #geom_line() +
#   stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*1 + cpeak_C[3]*0 + cpeak_C[4]*1 + cpeak_C[5]*1 + cpeak_C[6]*x + cpeak_C[7]*1*1 + cpeak_C[8]*1*0 + cpeak_C[9]*1*1 + cpeak_C[10]*1*x + cpeak_C[11]*1*0 + cpeak_C[12]*1*1 + cpeak_C[13]*1*x + cpeak_C[14]*0*1 + cpeak_C[15]*0*x + cpeak_C[16]*1*x + cpeak_C[17]*1^2 + cpeak_C[18]*1^2 + cpeak_C[19]*0^2 + cpeak_C[20]*x^2 + cpeak_C[21]*x^2) +
#   geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2) +
#   geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2) +
#   geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2) +
#   th
# 
# 
# # Trough
# 
# t5 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(starchg), y = trough)) +
#   #geom_line() +
#   stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*1 + ctrough_C[3]*0 + ctrough_C[4]*1 + ctrough_C[5]*1 + ctrough_C[6]*x + ctrough_C[7]*1*1 + ctrough_C[8]*1*0 + ctrough_C[9]*1*1 + ctrough_C[10]*1*x + ctrough_C[11]*1*0 + ctrough_C[12]*1*1 + ctrough_C[13]*1*x + ctrough_C[14]*0*1 + ctrough_C[15]*0*x + ctrough_C[16]*1*x + ctrough_C[17]*1^2 + ctrough_C[18]*1^2 + ctrough_C[19]*0^2 + ctrough_C[20]*x^2 + ctrough_C[21]*x^2)+
#   geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # Breakdown
# 
# b5 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(starchg), y = breakdown)) +
#   #geom_line() +
#   stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*1 + cbreak_C[3]*0 + cbreak_C[4]*1 + cbreak_C[5]*1 + cbreak_C[6]*x + cbreak_C[7]*1*1 + cbreak_C[8]*1*0 + cbreak_C[9]*1*1 + cbreak_C[10]*1*x + cbreak_C[11]*1*0 + cbreak_C[12]*1*1 + cbreak_C[13]*1*x + cbreak_C[14]*0*1 + cbreak_C[15]*0*x + cbreak_C[16]*1*x + cbreak_C[17]*1^2 + cbreak_C[18]*1^2 + cbreak_C[19]*0^2 + cbreak_C[20]*x^2 + cbreak_C[21]*x^2)+
#   geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# # Final
# 
# f5 <- df2_coded %>% 
#   ggplot(aes(x = as.factor(starchg), y = final)) +
#   #geom_line() +
#   stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*1 + cfinal_C[3]*0 + cfinal_C[4]*1 + cfinal_C[5]*1 + cfinal_C[6]*x + cfinal_C[7]*1*1 + cfinal_C[8]*1*0 + cfinal_C[9]*1*1 + cfinal_C[10]*1*x + cfinal_C[11]*1*0 + cfinal_C[12]*1*1 + cfinal_C[13]*1*x + cfinal_C[14]*0*1 + cfinal_C[15]*0*x + cfinal_C[16]*1*x + cfinal_C[17]*1^2 + cfinal_C[18]*1^2 + cfinal_C[19]*0^2 + cfinal_C[20]*x^2 + cfinal_C[21]*x^2) +
#   geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#   geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#   th
# 
# 
# 
# ggpubr::ggarrange(p1,p2,p3,p4,p5,t1,t2,t3,t4,t5,b1,b2,b3,b4,b5,f1,f2,f3,f4,f5, ncol = 5, nrow = 4)
# 
# 
# 
# 
 peak_graph <- function(dat, x_pick, x2, x3, x4, x5, theme = th, y_pick = peak) {
   
    

  #if (x_pick == "cooltime") {
    
    x_pick <- enquo(x_pick)
    y_pick <- enquo(y_pick) 

    
     out <- ggplot(data = dat, aes(x = as.factor(!!x_pick), y = !!y_pick)) +
     stat_function(fun = function(z) cpeak_C[1] + cpeak_C[2]*z + cpeak_C[3]*x2 + cpeak_C[4]*x3 + cpeak_C[5]*x4 + cpeak_C[6]*x5 + cpeak_C[7]*z*x2 + cpeak_C[8]*z*x3 + cpeak_C[9]*z*x4 + cpeak_C[10]*z*x5+cpeak_C[11]*x2*x3 + cpeak_C[12]*x2*x4 + cpeak_C[13]*x2*x5 + cpeak_C[14]*x3*x4 + cpeak_C[15]*x3*x5 + cpeak_C[16]*x4*x5 + cpeak_C[17]*z^2 + cpeak_C[18]*x2^2 + cpeak_C[19]*x3^2 + cpeak_C[20]*x4^2 + cpeak_C[21]*x5^2) +
     geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
     geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
     geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
       theme
  

  #}
  
  
  out
}



trough_graph <- function(dat, x_pick, x2, x3, x4, x5, theme = th, y_pick = trough) {
  
  x_pick <- enquo(x_pick)
  y_pick <- enquo(y_pick)
  
  ggplot(data = dat, aes(x = as.factor(!!x_pick), y = !!y_pick)) +
    stat_function(fun = function(z) ctrough_C[1] + ctrough_C[2]*x1 + ctrough_C[3]*z + ctrough_C[4]*x3 + ctrough_C[5]*x4 + ctrough_C[6]*x5 + ctrough_C[7]*x1*z + ctrough_C[8]*x1*x3 + ctrough_C[9]*x1*x4 + ctrough_C[10]*x1*x5+ctrough_C[11]*z*x3 + ctrough_C[12]*z*x4 + ctrough_C[13]*z*x5 + ctrough_C[14]*x3*x4 + ctrough_C[15]*x3*x5 + ctrough_C[16]*x4*x5 + ctrough_C[17]*x1^2 + ctrough_C[18]*z^2 + ctrough_C[19]*x3^2 + ctrough_C[20]*x4^2 + ctrough_C[21]*x5^2) +
    geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
    theme
  
  
}

# Breakdown - Lower = 4500; Upper = 4600; Expected = 4550

break_graph <- function(dat, x_pick, x2, x3, x4, x5, theme = th, y_pick = breakdown) {
  
  x_pick <- enquo(x_pick)
  y_pick <- enquo(y_pick)
  
  ggplot(data = dat, aes(x = as.factor(!!x_pick), y = !!y_pick)) +
    stat_function(fun = function(z) cbreak_C[1] + cbreak_C[2]*x1 + cbreak_C[3]*x2 + cbreak_C[4]*z + cbreak_C[5]*x4 + cbreak_C[6]*x5 + cbreak_C[7]*x1*x2 + cbreak_C[8]*x1*z + cbreak_C[9]*x1*x4 + cbreak_C[10]*x1*x5+cbreak_C[11]*x2*z + cbreak_C[12]*x2*x4 + cbreak_C[13]*x2*x5 + cbreak_C[14]*z*x4 + cbreak_C[15]*z*x5 + cbreak_C[16]*x4*x5 + cbreak_C[17]*x1^2 + cbreak_C[18]*x2^2 + cbreak_C[19]*zs^2 + cbreak_C[20]*x4^2 + cbreak_C[21]*x5^2) +
    geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
    theme
  
  
}




predict(peaklm_coded, newdata = data.frame(cooltime = 0, heattime = -.858, holdtime = .557, stirrpm = .009, starchg = .009))



