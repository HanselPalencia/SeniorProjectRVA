# In this R Script I am going to attempt to solve the numeric equation. I will be using matrix algebra, specifically searching for the solution set and or the eigenvalues of the matrix.

library(tidyverse)
library(rsm)
library(pracma)

# x1 - cooltime
# x2 - heattime
# x3 - holdtime
# x4 - rpm
# x5 - starchg

dat <- read_csv("potato_starch_results_plus10_2018-11-27.csv")


peaklm <- lm(peak ~ FO(x1,x2,x3,x4,x5), data = dat)
cpeak <- coef(peaklm)
summary(peaklm)


troughlm <- lm(trough ~ FO(x1,x2,x3,x4,x5), data = dat)
ctrough <- coef(troughlm)
summary(troughlm)


breaklm <- lm(breakdown ~ FO(x1,x2,x3,x4,x5), data = dat)
cbreak <- coef(breaklm)
summary(breaklm)


finallm <- lm(final ~ FO(x1,x2,x3,x4,x5), data = dat)
cfinal <- coef(finallm)
summary(finallm)

dat <- rbind(cpeak, ctrough, cbreak, cfinal)

mymatrix <- as.matrix.data.frame(dat)

# Since my matrix is 4:21 then we will use SVD to solve.

rref(mymatrix)
forreducev<- t(mymatrix)
rref(forreducev)



# Y_1 lives between what
# Y_2 lives between what
# Y_3 lives between what
# Y_4 lives between what

library(lpSolve)
# Move intercept to other side and then say 
A1 <- mymatrix[,-1]
A2 <- mymatrix[,-1]

A <- rbind(A1,A2)


B <- c(6950, 2425, 4500, 2975, 7050, 2475, 4600, 3025)

cons_direction <- c("<=", "<=", "<=", "<=",">=", ">=", ">=", ">=")


mymatrix2 <- rbind(cpeak, ctrough, cbreak, cfinal)

mymatrix2 <- rowSums(t(mymatrix2))

C <- mymatrix2[-1]


optimum <- lp(direction = "min",
              objective.in = C,
              const.mat = A,
              const.dir = cons_direction,
              const.rhs = B,
              all.int = F)

print(optimum$status)

best_sol <- optimum$solution

print(round(best_sol))



# Redoing with coded data to verify and using rsm

dat <- read_csv("potato_starch_results_plus10_2018-11-27.csv")

df2_coded <- coded.data(dat,cooltime~(x1 - 228)/90, heattime~(x2 - 222)/90, holdtime~(x3 - 150)/90, 
                        stirrpm~(x4-160)/40, starchg~(x5-2)/.1 )

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

dat <- rbind(cpeak_C, ctrough_C, cbreak_C, cfinal_C)

mymatrix <- as.matrix.data.frame(dat)


# Since my matrix is 4:21 then we will use SVD to solve.

rref(mymatrix)
forreducev<- t(mymatrix)
rref(forreducev)



# Y_1 lives between what
# Y_2 lives between what
# Y_3 lives between what
# Y_4 lives between what

library(lpSolve)
# Move intercept to other side and then say 
A1 <- mymatrix[,-1]
A2 <- mymatrix[,-1]

A <- rbind(A1,A2)


B <- c(6950, 2425, 4500, 2975, 7050, 2475, 4600, 3025)

cons_direction <- c("<=", "<=", "<=", "<=",">=", ">=", ">=", ">=")


mymatrix2 <- rbind(cpeak_C, ctrough_C, cbreak_C, cfinal_C)

mymatrix2 <- rowSums(t(mymatrix2))

C <- mymatrix2[-1]


optimum <- lp(direction = "min",
              objective.in = C,
              const.mat = A,
              const.dir = cons_direction,
              const.rhs = B,
              all.int = F)

print(optimum$status)

best_sol <- optimum$solution

print(round(best_sol))

# From what I can tell there doesn't seem to be a numerical solution to this problem. This is good moving forward because we now focus on model selection to improve this issue



