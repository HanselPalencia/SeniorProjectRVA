library(tidyverse)
library(readxl)
library(rsm)

dat_wheat <- read_xlsx("wheat_results_2018-12-11.xlsx")


# Hoping that we can use the same model for wheat? If we can then this will be really good.

peaklm_coded_w <- lm(peak ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = dat_wheat)
cpeak_C <- coef(peaklm_coded_w)
summary(peaklm_coded_w)
#R^2 = .95

troughlm_coded_w <- lm(trough ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = dat_wheat)
ctrough_C <- coef(troughlm_coded_w)
summary(troughlm_coded_w)
#R^2 = .95

breaklm_coded_w <- lm(breakdown ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = dat_wheat)
cbreak_C <- coef(breaklm_coded_w)
summary(breaklm_coded_w)
#R^2 = .84

finallm_coded_w <- lm(final ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = dat_wheat)
cfinal_C <- coef(finallm_coded_w)
summary(finallm_coded_w)
#R^2 = .95

# This is really good because it means we can make really good predictions from this, except for breakdown, but in reality we don't really care that much, because the breakdown is just the difference  between peak and trough









