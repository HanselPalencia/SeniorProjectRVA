library(rsm)
library(tidyverse)
#des <- ccd (peak + trough + breakdown + final ~ cooltime + heattime + hold + rpm, 
 #            generators = starchg~-cool*heat*hold*rpm, n0 = c(8, 2))

#des1 <- as.coded.data(des, cool~(x1 - 228)/90, heat~(x2 - 222)/90, hold~(x3 - 150)/90, 
 #                     rpm~(x4-160)/40, starchg~(x5-2)/.1)

des <- bbd(peak + trough + breakdown + final ~ cooltime + heattime + holdtime + stirrpm + starchg)
des1 <- as.coded.data(des, cooltime~(x1 - 228)/90, heattime~(x2 - 222)/90, holdtime~(x3 - 150)/90, 
                      stirrpm~(x4-160)/40, starchg~(x5-2)/.1)
des1
write_csv(decode.data(des1), "design.csv")

#df <- read_csv("C:/Users/drp36/OneDrive - BYU-Idaho 1/General/Design of Experiments/potato_starch_results_2018-11-15.csv")


df2 <- read_csv("potato_starch_results_plus10_2018-11-27.csv")

#df_coded <- coded.data(df,cooltime~(x1 - 228)/90, heattime~(x2 - 222)/90, holdtime~(x3 - 150)/90,  stirrpm~(x4-160)/40, starchg~(x5-2)/.1 )


df2_coded <- coded.data(df2,cooltime~(x1 - 228)/90, heattime~(x2 - 222)/90, holdtime~(x3 - 150)/90, 
                       stirrpm~(x4-160)/40, starchg~(x5-2)/.1 )




##############Combine the contour for all outputs#########
peak_model <- rsm(peak~SO(cooltime,heattime,holdtime,stirrpm,starchg), data=df2_coded)
trough_model <- rsm(trough~SO(cooltime,heattime,holdtime,stirrpm,starchg), data=df2_coded)
breakdown_model <- rsm(breakdown~SO(cooltime,heattime,holdtime,stirrpm,starchg), data=df2_coded)
final_model <- rsm(final~SO(cooltime,heattime,holdtime,stirrpm,starchg), data=df2_coded)

summary(peak_model)
summary(trough_model)
summary(breakdown_model)
summary(final_model)
########Create the contour plot for stir and starch########
x4_inputs<-(seq(140,180, by=1) - 160)/40
#length(x4_inputs)
#length(x5_inputs)
x5_inputs<-(seq(1.95,2.05, by=.0025)-2)/.1
#This adds constants for heat, cool, and hold and gets all the combinations of the other 2 vector elements
x4x5_contour_inputs<-cbind(.265,0,.431, expand.grid(x4_inputs,x5_inputs))
names(x4x5_contour_inputs) <- c( "cooltime", "heattime", "holdtime","stirrpm", "starchg")

####Use each model to get predicted values
peak_preds <- cbind(x4x5_contour_inputs, predict(peak_model, newdata=x4x5_contour_inputs))
trough_preds<-cbind(x4x5_contour_inputs, predict(trough_model, newdata=x4x5_contour_inputs))
breakdown_preds<-cbind(x4x5_contour_inputs, predict(breakdown_model, newdata=x4x5_contour_inputs))
final_preds<-cbind(x4x5_contour_inputs, predict(final_model, newdata=x4x5_contour_inputs))
#Rename all the prediction matrices so that the prediction column is named z, to prepare for combining
names(peak_preds)[6] <- "z"
names(trough_preds)[6] <- "z"
names(breakdown_preds)[6] <- "z"
names(final_preds)[6] <- "z"

#This combines the tables and labels the row according to its original dataframe
contour_inputs<-bind_rows(list(peak=peak_preds,trough=trough_preds, 
                               breakdown=breakdown_preds, final=final_preds), .id="df")


ggplot(contour_inputs) + geom_contour(aes(x=starchg, y=stirrpm, z=z, color=df))+theme_minimal()
###This ggplots is too messy, I just want to see the contours where the targets overlap

#The original Perten target for peak is 7022 +/- 24, but because the model tends to overpredict peak
#I subtracted 60 from it
#The original Perten target for final is 2992 +/- 5, but because the model tends to under predict final
#I added 40 to the target
narrow<-contour_inputs%>%filter((df=="peak" & between(z, (6950-60),(7050-60))) | 
                          (df=="trough" & between(z, 2425,2475)) |
                          (df=="breakdown" & between(z, 4500, 4600)) |
                          (df=="final" & between(z, (2975+40), (3025+40)))
                        )

np<-ggplot(narrow) + geom_contour(aes(x=starchg, y=stirrpm, z=z, color=df))+theme_minimal()
np +  
  labs(title = "Contour Plot of 4 Viscosity Profile Outputs",
        subtitle = "Hold Time = 151.17s, Heat Time = 222s, Cool Time = 244.65s",
        y="Stir Rate (rpm)", x="Starch (g)",
        color="Output Model Prediction", color="Model Prediction Output") +
  scale_color_discrete(labels = c("Peak 6890-6990", "Trough 2425-2475", "Breakdown 4500-4600",
                                  "Final 3015-3065")) +
  scale_x_continuous(labels=c(1.95, 1.975, 2, 2.025, 2.05)) +
  scale_y_continuous(labels=c(seq(140,180, by=10)))+
  guides(color="none") #this should remove the legend for color since I'm going to just do it once
#put it for all the graphs instead of repeating it in every graph


#np
#direct.label(np, list("far.from.others.borders", "calc.boxes", "enlarge.box", 
#                         box.color = NA, fill = "transparent", "draw.rects"))
#direct.label(np, list("bottom.pieces", colour='black'))

########Create the contour plot for heat and starch########
x2_inputs<-(seq(132,312, by=2.25) - 222)/90
length(x2_inputs)
x5_inputs<-(seq(1.9,2.1, by=.0025)-2)/.1
length(x5_inputs)
#This adds constants for heat, cool, and hold and gets all the combinations of the other 2 vector elements
x2x5_contour_inputs<-cbind(.265,.431, .075, expand.grid(x2_inputs,x5_inputs))
names(x2x5_contour_inputs) <- c( "cooltime", "holdtime","stirrpm", "heattime", "starchg")
x2x5_contour_inputs <- x2x5_contour_inputs[,c(1,4,2,3,5)]


####Use each model to get predicted values
peak_predsx2x5 <- cbind(x2x5_contour_inputs, predict(peak_model, newdata=x2x5_contour_inputs))
trough_predsx2x5<-cbind(x2x5_contour_inputs, predict(trough_model, newdata=x2x5_contour_inputs))
breakdown_predsx2x5<-cbind(x2x5_contour_inputs, predict(breakdown_model, newdata=x2x5_contour_inputs))
final_predsx2x5<-cbind(x2x5_contour_inputs, predict(final_model, newdata=x2x5_contour_inputs))
#Rename all the prediction matrices so that the prediction column is named z, to prepare for combining
names(peak_predsx2x5)[6] <- "z"
names(trough_predsx2x5)[6] <- "z"
names(breakdown_predsx2x5)[6] <- "z"
names(final_predsx2x5)[6] <- "z"

#This combines the tables and labels the row according to its original dataframe
contour_inputsx2x5<-bind_rows(list(peak=peak_predsx2x5,trough=trough_predsx2x5, 
                               breakdown=breakdown_predsx2x5, final=final_predsx2x5), .id="df")


ggplot(contour_inputsx2x5) + geom_contour(aes(x=starchg, y=heattime, z=z, color=df))+theme_minimal()
###This ggplots is too messy, I just want to see the contours where the targets overlap

narrowx2x5<-contour_inputsx2x5%>%filter((df=="peak" & between(z, (6950-60),(7050-60))) | 
                                          (df=="trough" & between(z, 2425,2475)) |
                                          (df=="breakdown" & between(z, 4500, 4600)) |
                                          (df=="final" & between(z, (2975+40), (3025+40)))
                                        )

np<-ggplot(narrowx2x5) + geom_contour(aes(x=starchg, y=heattime, z=z, color=df))+theme_minimal()
np
np +  labs(title = "Contour Plot of 4 Viscosity Profile Outputs",
           subtitle = "Hold Time = 151.17s, Stir Rate = 163rpm, Cool Time = 244.65s",
           y="Heat Time (s)" , x="Starch (g)",
           color="Output")+
  scale_x_continuous(labels=c(seq(1.95,2.05, by = .025))) +
  scale_y_continuous(labels=c(seq(132,312, by = 45))) +
  guides(color = "none")  #this should remove the legend for color since I'm going to just do it once
#put it for all the graphs instead of repeating it in every graph


########Create the contour plot for heat and stir########
x2_inputs<-(seq(177,267, by=4.5) - 222)/90
length(x2_inputs)
x4_inputs<-(seq(160,168, by=.4) - 160)/40
length(x4_inputs)

#This adds constants for heat, cool, and hold and gets all the combinations of the other 2 vector elements
x2x4_contour_inputs<-cbind(.265,.431, -.02, expand.grid(x2_inputs,x4_inputs))
names(x2x4_contour_inputs) <- c( "cooltime", "holdtime","starchg", "heattime", "stirrpm")
x2x4_contour_inputs <- x2x4_contour_inputs[,c(1,4,2,5,3)]

####Use each model to get predicted values
peak_predsx2x4 <- cbind(x2x4_contour_inputs, predict(peak_model, newdata=x2x4_contour_inputs))
trough_predsx2x4<-cbind(x2x4_contour_inputs, predict(trough_model, newdata=x2x4_contour_inputs))
breakdown_predsx2x4<-cbind(x2x4_contour_inputs, predict(breakdown_model, newdata=x2x4_contour_inputs))
final_predsx2x4<-cbind(x2x4_contour_inputs, predict(final_model, newdata=x2x4_contour_inputs))

#Rename all the prediction matrices so that the prediction column is named z, to prepare for combining
names(peak_predsx2x4)[6] <- "z"
names(trough_predsx2x4)[6] <- "z"
names(breakdown_predsx2x4)[6] <- "z"
names(final_predsx2x4)[6] <- "z"

#This combines the tables and labels the row according to its original dataframe
contour_inputsx2x4<-bind_rows(list(peak=peak_predsx2x4,trough=trough_predsx2x4, 
                                   breakdown=breakdown_predsx2x4, final=final_predsx2x4), .id="df")

narrowx2x4<-contour_inputsx2x4%>%filter((df=="peak" & between(z, (6950-60),(7050-60))) | 
                                          (df=="trough" & between(z, 2425,2475)) |
                                          (df=="breakdown" & between(z, 4500, 4600)) |
                                          (df=="final" & between(z, (2975+40), (3025+40)))
)

np25<-ggplot(narrowx2x4) + geom_contour(aes(x=stirrpm, y=heattime, z=z, color=df))+theme_minimal()
np25 + labs(title = "Contour Plot of 4 Viscosity Profile Outputs",
            subtitle = "Hold Time = 151.17s, Starch = 1.998g, Cool Time = 244.65s",
            y="Heat Time (s)" , x="Stir Rate (rpm)",
            color="Output")+
  scale_x_continuous(labels=c(seq(160,168, length.out=5))) +
  scale_y_continuous(labels=c(seq(177,267, length.out=5))) +
  guides(color = "none")  #this should remove the legend for color since I'm going to just do it once
#put it for all the graphs instead of repeating it in every graph



########Create the contour plot for hold and stir########
x3_inputs<-(seq(150,240, by=2.25/2) - 150)/90
#length(x3_inputs)
x4_inputs<-(seq(160,168, by=.1) - 160)/40
#length(x4_inputs)

#This adds constants for heat, cool, and hold and gets all the combinations of the other 2 vector elements
x3x4_contour_inputs<-cbind(.265,0, -.02, expand.grid(x3_inputs,x4_inputs))
names(x3x4_contour_inputs) <- c( "cooltime","heattime","starchg", "holdtime","stirrpm")
x3x4_contour_inputs <- x3x4_contour_inputs[,c(1,2,4,5,3)]

####Use each model to get predicted values
peak_predsx3x4 <- cbind(x3x4_contour_inputs, predict(peak_model, newdata=x3x4_contour_inputs))
trough_predsx3x4<-cbind(x3x4_contour_inputs, predict(trough_model, newdata=x3x4_contour_inputs))
breakdown_predsx3x4<-cbind(x3x4_contour_inputs, predict(breakdown_model, newdata=x3x4_contour_inputs))
final_predsx3x4<-cbind(x3x4_contour_inputs, predict(final_model, newdata=x3x4_contour_inputs))

#Rename all the prediction matrices so that the prediction column is named z, to prepare for combining
names(peak_predsx3x4)[6] <- "z"
names(trough_predsx3x4)[6] <- "z"
names(breakdown_predsx3x4)[6] <- "z"
names(final_predsx3x4)[6] <- "z"

#This combines the tables and labels the row according to its original dataframe
contour_inputsx3x4<-bind_rows(list(peak=peak_predsx3x4,trough=trough_predsx3x4, 
                                   breakdown=breakdown_predsx3x4, final=final_predsx3x4), .id="df")

narrowx3x4<-contour_inputsx3x4%>%filter((df=="peak" & between(z, (6950-60),(7050-60))) | 
                                          (df=="trough" & between(z, 2425,2475)) |
                                          (df=="breakdown" & between(z, 4500, 4600)) |
                                          (df=="final" & between(z, (2975+40), (3025+40)))
)

np34<-ggplot(narrowx3x4) + geom_contour(aes(x=stirrpm, y=holdtime, z=z, color=df))+theme_minimal()
np34 + labs(title = "Contour Plot of 4 Viscosity Profile Outputs",
            subtitle = "Heat Time = 222s, Starch = 1.998g, Cool Time = 244.65s",
            y="Hold Time (s)" , x="Stir Rate (rpm)",
            color="Output")+
  scale_x_continuous(labels=c(seq(160,168, length.out=5))) +
  scale_y_continuous(labels=c(seq(150,240, length.out=5))) +
  guides(color = "none")  #this should remove the legend for color since I'm going to just do it once
#put it for all the graphs instead of repeating it in every graph


########Create the contour plot for Heat and Hold########
x3_inputs<-(seq(150,240, by=2.25) - 150)/90 #hold
length(x3_inputs)
x2_inputs<-(seq(177,267, by=2.25) - 222)/90 #heat
length(x2_inputs)

#This adds constants for heat, cool, and hold and gets all the combinations of the other 2 vector elements
x2x3_contour_inputs<-cbind(.265,.075, -.02, expand.grid(x2_inputs,x3_inputs))
names(x2x3_contour_inputs) <- c( "cooltime","stirrpm","starchg", "heattime","holdtime")
x2x3_contour_inputs <- x2x3_contour_inputs[,c(1,4,5,2,3)]

####Use each model to get predicted values
peak_predsx2x3 <- cbind(x2x3_contour_inputs, predict(peak_model, newdata=x2x3_contour_inputs))
trough_predsx2x3<-cbind(x2x3_contour_inputs, predict(trough_model, newdata=x2x3_contour_inputs))
breakdown_predsx2x3<-cbind(x2x3_contour_inputs, predict(breakdown_model, newdata=x2x3_contour_inputs))
final_predsx2x3<-cbind(x2x3_contour_inputs, predict(final_model, newdata=x2x3_contour_inputs))

#Rename all the prediction matrices so that the prediction column is named z, to prepare for combining
names(peak_predsx2x3)[6] <- "z"
names(trough_predsx2x3)[6] <- "z"
names(breakdown_predsx2x3)[6] <- "z"
names(final_predsx2x3)[6] <- "z"

#This combines the tables and labels the row according to its original dataframe
contour_inputsx2x3<-bind_rows(list(peak=peak_predsx2x3,trough=trough_predsx2x3, 
                                   breakdown=breakdown_predsx2x3, final=final_predsx2x3), .id="df")

narrowx2x3<-contour_inputsx2x3%>%filter((df=="peak" & between(z, (6950-60),(7050-60))) | 
                                          (df=="trough" & between(z, 2425,2475)) |
                                          (df=="breakdown" & between(z, 4500, 4600)) |
                                          (df=="final" & between(z, (2975+40), (3025+40)))
)

np23<-ggplot(narrowx2x3) + geom_contour(aes(x=heattime, y=holdtime, z=z, color=df))+theme_minimal()
np23 + labs(title = "Contour Plot of 4 Viscosity Profile Outputs",
            subtitle = "Stir Rate = 163rpm, Starch = 1.998g, Cool Time = 244.65s",
            y="Hold Time (s)" , x="Heat Time (s)",
            color="Output")+
  scale_x_continuous(labels=c(seq(177,267, length.out=5))) +
  scale_y_continuous(labels=c(seq(150,240, length.out=5))) +
  guides(color = "none")  #this should remove the legend for color since I'm going to just do it once
#put it for all the graphs instead of repeating it in every graph


########Create the contour plot for Hold by Starch########
x3_inputs<-(seq(150,240, by=2.25/2) - 150)/90 #hold
length(x3_inputs)
x5_inputs<-(seq(1.95, 2.05, by=.0025/2) - 2)/.1 #heat
length(x5_inputs)

#This adds constants for heat, cool, and hold and gets all the combinations of the other 2 vector elements
x3x5_contour_inputs<-cbind(.265,.075, 0, expand.grid(x3_inputs,x5_inputs))
names(x3x5_contour_inputs) <- c( "cooltime","stirrpm","heattime","holdtime","starchg" )
x3x5_contour_inputs <- x3x5_contour_inputs[,c(1,3,4,2,5)]

####Use each model to get predicted values
peak_predsx3x5 <- cbind(x3x5_contour_inputs, predict(peak_model, newdata=x3x5_contour_inputs))
trough_predsx3x5<-cbind(x3x5_contour_inputs, predict(trough_model, newdata=x3x5_contour_inputs))
breakdown_predsx3x5<-cbind(x3x5_contour_inputs, predict(breakdown_model, newdata=x3x5_contour_inputs))
final_predsx3x5<-cbind(x3x5_contour_inputs, predict(final_model, newdata=x3x5_contour_inputs))

#Rename all the prediction matrices so that the prediction column is named z, to prepare for combining
names(peak_predsx3x5)[6] <- "z"
names(trough_predsx3x5)[6] <- "z"
names(breakdown_predsx3x5)[6] <- "z"
names(final_predsx3x5)[6] <- "z"

#This combines the tables and labels the row according to its original dataframe
contour_inputsx3x5<-bind_rows(list(peak=peak_predsx3x5,trough=trough_predsx3x5, 
                                   breakdown=breakdown_predsx3x5, final=final_predsx3x5), .id="df")

narrowx3x5<-contour_inputsx3x5%>%filter((df=="peak" & between(z, (6950-60),(7050-60))) | 
                                          (df=="trough" & between(z, 2425,2475)) |
                                          (df=="breakdown" & between(z, 4500, 4600)) |
                                          (df=="final" & between(z, (2975+40), (3025+40)))
)

np35<-ggplot(narrowx3x5) + geom_contour(aes(x=starchg, y=holdtime, z=z, color=df))+theme_minimal()
np35 + labs(title = "Contour Plot of 4 Viscosity Profile Outputs",
            subtitle = "Stir Rate = 163rpm, Heat Time = 222s, Cool Time = 244.65s",
            y="Hold Time (s)" , x="Starch (g)",
            color="Output")+
  scale_x_continuous(labels=c(seq(1.95,2.05, length.out=5))) +
  scale_y_continuous(labels=c(seq(150,240, length.out=5))) +
  guides(color = "none")  #this should remove the legend for color since I'm going to just do it once
#put it for all the graphs instead of repeating it in every graph




###########Can ignore everything below this line###########################










#############Keeping this as just archived code###############
########## Get Contours Based on the Settings found in JMP 2019-02-14
########### I realize the model output from R does not match the parameter estimates in JMP
#########I'm not sure why, but I will try to verify if the end result is the same
#########Otherwise I will need to create contour plots by hand from the very messy JMP coefficients
#####peak#####

peak_model <- rsm(peak~SO(cooltime,heattime,holdtime,stirrpm,starchg), data=df2_coded)
summary(peak_model)
#This first command is just to get a named vector. Then I reset it at the values I actually want
peak_cont_center<-canonical(peak_model)$xs
peak_cont_center[1:5] <- c(.265, 0, .431, .369, .387)
#contour(peak_model, ~cooltime+heattime+holdtime+stirrpm+starchg, at = peak_cont_center)
#,       levels=pretty(c(7028,8028),n=10))
#######Trough model##########
trough_model <- rsm(trough~SO(cooltime,heattime,holdtime,stirrpm,starchg), data=df2_coded)
#This adds zeros for heattime and gets all the combinations of the other 2 vector elements
x4x5_contour_inputs<-cbind(.265,0,.431, expand.grid(x4_inputs,x5_inputs))
names(x4x5_contour_inputs) <- c( "cooltime", "heattime", "holdtime","stirrpm", "starchg")
#Create the dataframe of the independent and dependent variable
trough_contour_inputs <- cbind(x4x5_contour_inputs, predict(trough_model, newdata=x4x5_contour_inputs))
names(trough_contour_inputs)[6] <- "trough_z"
head(trough_contour_inputs)
ptrough<-ggplot(trough_contour_inputs) + geom_contour(aes(x=starchg, y=stirrpm, z=trough_z, color=..level..), color="black")+theme_minimal()
direct.label(ptrough, list("bottom.pieces", colour='black'))


narrow_trough<-filter(trough_contour_inputs, between(trough_z, 2400,2500))
ptrough<-ggplot(narrow_trough) + geom_contour(aes(x=starchg, y=stirrpm, z=trough_z, color=..level..), color="black")+theme_minimal()
ptrough
############Creating my own contour plot########
library(directlabels)
#Peak model contour lines first###
x4_inputs<-(seq(120,160, by=2) - 160)/40
length(x4_inputs)
x5_inputs<-(seq(1.9,2.1, by=.01)-2)/.1
x5_inputs
#This adds zeros for heattime and gets all the combinations of the other 2 vector elements
x4x5_contour_inputs<-cbind(.265,0,.431, expand.grid(x4_inputs,x5_inputs))
names(x4x5_contour_inputs) <- c( "cooltime", "heattime", "holdtime","stirrpm", "starchg")
#Create the dataframe of the independent and dependent variable
peak_contour_inputs <- cbind(x4x5_contour_inputs, predict(peak_model, newdata=peak_contour_inputs))
names(peak_contour_inputs)[6] <- "peak_z"
narrow_peak<-filter(peak_contour_inputs, between(peak_z, 7000,7200))
p0<-ggplot(narrow_peak) + geom_contour(aes(x=starchg, y=stirrpm, z=peak_z), color="black")+theme_minimal()

p<-ggplot(narrow_peak) + geom_contour(aes(x=starchg, y=stirrpm, z=peak_z, color=..level..))+theme_minimal()
#p<-ggplot(narrow_peak) + geom_contour(aes(x=starchg, y=stirrpm, z=peak_z, color=..level..))+theme_minimal()
p
p1<-direct.label(p, list("bottom.pieces", colour='black'))
p1
p2<-direct.label(p, list("far.from.others.borders", "calc.boxes", "enlarge.box", 
                         box.color = NA, fill = "transparent", "draw.rects"))
p2
#  geom_dl(aes(label=peak_z), method="bottom.pieces")
#library(directlabels)
#direct.label(v, method="bottom.pieces")

#########peak model  - 3 parameter#########
#Theoretically cool and hold times shouldn't effect peak, so I took them out.
peak_model <- rsm(peak~SO(cooltime,heattime,holdtime,stirrpm,starchg), data=df2_coded)
summary(peak_model)
#Lack of fit is a problem. From what I've read, that can be fixed with more centerpoint runs.
par(mfrow = c(1,1))
plot(df$peak, peak_model$fitted.values)
#There's a couple of influential observations, which I think maybe I should look at more of a CCD design to 
#supplement the Box-Behnken I've already done.


#canonical(peak_model) 
canonical.path(peak_model, dist = seq(7,8,by=.1))
#It looks like the ideal point is at heattime=116, stir=126, and starch = 2
#Applying the threshold didn't help, still getting bad results.

par(mfrow = c(2,3))
contour(peak_model, ~heattime+stirrpm+starchg, image=TRUE, levels=pretty(c(7000,8000),n=10)) 
#Note I took out the starch level because it seems to be making little difference.
#I want to recenter the contour plots.
new.inputs <- data.frame(cooltime=c(237, (237-228)/90), heattime=c(222,0), holdtime=c(172, (172-150)/90), 
                         stirrpm=c(167, (167-160)/40), starchg=c(2,0))



########## Garbage for peak 5 parameter model ########

peak_cont_center<-canonical(peak_model, threshold = 2)$xs
peak_cont_center[1:5] <- c(.1,0,.244, .175, 0)
contour(peak_model, ~cooltime+heattime+holdtime+stirrpm+starchg, image=TRUE,
        at = peak_cont_center) 

peak.pred.inputs <- data.frame(cooltime=c((228-228)/90), heattime=c(0), holdtime=c((150-150)/90), 
                         stirrpm=c((135-160)/40), starchg=c(0))
predict(peak_model, newdata=peak.pred.inputs)
#canonical(peak_model, threshold=40)
#Only starch, cool time and stir are significant, so I'm only keep those
#peak_model <- rsm(peak~SO(cooltime, stirrpm, starchg), data=df_coded)
#summary(peak_model)
#contour(peak_model, ~cooltime+ stirrpm+ starchg, data=df_coded)
#canonical.path(peak_model, dist = seq(-1.5,1.5,by=.2))

##########trough model###########

trough_model <- rsm(trough~ SO( holdtime, stirrpm, starchg, heattime), data=df2_coded)
#I took out cool time because theoretically it should affect trough
summary(trough_model)
#Good lack of fit and good R-squared

par(mfrow=c(1,1))
plot(df$trough, trough_model$fitted.values)
#There is one particularly high value, but I think it is legit.
plot(trough_model) #assumptions/requirements appear to be met

par(mfrow=c(2,2))
contour(trough_model, ~holdtime+stirrpm+starchg, image=TRUE) 
center_points<-summary(trough_model)$canonical$xs
center_points[1:4]<-c(.431, .369, .387, 0)
center_points
contour(trough_model, ~holdtime+stirrpm+starchg+heattime, image=TRUE,
       at = center_points) 
#at= tells it to hold the values not in the plot constant at the stationary point
steepest(trough_model, dist=seq(0,5,by=.5))
#This is encouraging. If we use some value around 3000 for the target we get hold time about 140,
#Stir time around 151, starchg at 2 and heat time at 222.

#Or if we assumed the stir was about 140, we would have to change the hold time to about 220.

######Old Gargabe code for reference #############
#canonical(trough_model, threshold = 32) #Because the values for heat time are 
#really far from my starting point and really quite impossible, I limited the eigen values
#of that and starchg and stirrpm so that I could start from a stationary point that was more reasonable
thresh_xs<-canonical(trough_model, threshold = 32)$xs
contour(trough_model, ~cooltime+holdtime+stirrpm+starchg, image=TRUE,
        at = thresh_xs) 

#canonical.path(trough_model, dist = seq(-2,2,by=.1), threshold = 32)

########breakdown models
breakdown_model <- rsm(trough~ FO(cooltime, holdtime, stirrpm, starchg) +TWI(cooltime, holdtime, stirrpm, starchg) + PQ(cooltime, holdtime, stirrpm, starchg), data=df_coded)
summary(breakdown_model)

########## breakdown model ########
bd_model <- rsm(breakdown~SO(holdtime, stirrpm, starchg) , data=df_coded)
summary(bd_model)
#cool time theoretically shouldn't be affecting breakdown or trough.
#heat time doesn't appear to have an impact whatsoever so I took it out


par(mfrow = c(1,1))
plot(df$breakdown, bd_model$fitted.values)
#st.order point # 26 looks really suspicious
par(mfrow = c(2,2))
contour(bd_model, ~holdtime+stirrpm+starchg, image=TRUE)
#canonical(bd_model, threshold=3)$xs
canonical.path(bd_model, dist=seq(-7,7, by=.5))
#From this it looks like you would need 200 hold time, 191 stir rate and 1.8 grams of starch.
bd.inputs <- data.frame(holdtime=c(.542, -1), 
                         stirrpm=c(.78, .9), starchg=c(-1.9,(1.8-2)/.1))

bd.inputs
predict(bd_model, newdata=bd.inputs)

#########final model #################
final_model <- rsm(final~SO(cooltime, heattime, holdtime, stirrpm, starchg), data=df_coded)
summary(final_model)
#Lack of fit measure indicates things aren't fitting well. Lower order models don't perform as well.
par(mfrow=c(1,1))
plot(df$final, final_model$fitted.values)

par(mfrow = c(3,3))
contour(final_model, ~ cooltime+heattime+holdtime+stirrpm+starchg, image=TRUE) 
#I think a target of 3000 will be tricky to meet, but we can get pretty close to 3200 I think.
#final_thresh <- canonical(final_model, threshold=8)$xs

canonical.path(final_model, dist = seq(-2,2, by=.25), threshold=30)
#The threshold helps some, but still doesn't get us there. I wonder if there is a 3 way interaction
#we should be modeling.


new.inputs <- data.frame(cooltime=c(237, (237-228)/90), heattime=c(222,0), holdtime=c(172, (172-150)/90), 
                         stirrpm=c(167, (167-160)/40), starchg=c(2,0))

new.inputs
predict(trough_model, newdata=new.inputs)


######### With extra points peak model #########
#Theoretically cool and hold times shouldn't effect peak, so I took them out.########
peak_model2 <- rsm(peak~FO(heattime, stirrpm, starchg), data=df2_coded)
summary(peak_model2)
anova(peak_model, peak_model2)
