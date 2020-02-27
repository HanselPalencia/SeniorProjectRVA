library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(scales)
library(plotly)
library(highcharter)
library(readxl)
library(rsm)

#source("App_Graphics.R")
source("Data_PreProcess.R")


ui <- navbarPage(title = "RVA PredictoR",
                 windowTitle = "RVA PredictoR",
                 fluidPage(theme = shinytheme("darkly"),
                           sidebarLayout(
                             
                             sidebarPanel(width = 3,
                                          
                                          wellPanel(
                                            pickerInput(inputId = "Starch",
                                                        label = "Choose Your Starch",
                                                        choices = c("Potato", "Wheat", "Corn"),
                                                        selected = "Potato"
                                                        
                                                        ),
                                            
                                            sliderInput(inputId = "cool",
                                                        label = "Cool Time",
                                                        min = -1,
                                                        max = 1,
                                                        value = 0, step = .01),
                                            
                                            sliderInput(inputId = "heat",
                                                        label = "Heat Time",
                                                        min = -1,
                                                        max = 1,
                                                        value = 0, step = .01),
                                            
                                            sliderInput(inputId = "hold",
                                                        label = "Hold Time",
                                                        min = -1,
                                                        max = 1,
                                                        value = 0, step = .01),
                                            
                                            sliderInput(inputId = "stir",
                                                        label = "Stir RPM",
                                                        min = -1,
                                                        max = 1,
                                                        value = 0, step = .01),
                                            
                                            sliderInput(inputId = "starch",
                                                        label = "Starch (g)",
                                                        min = -1,
                                                        max = 1,
                                                        value = 0, step = .01)
                                            
                                            
                                            
                                            
                                            
                                            
                                          )
                                          
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(width = 8,
                                       
                                       # HTML(""),
                                       # HTML("<br>"),
                                       # HTML(""),
                                       
                                       tabsetPanel(type = "tabs",
                                                   
                                                   tabPanel("PredictoR Tool",
                                                            fluidRow(
                                                              plotOutput("allplots", height = "550px", width = "1000px"),
dataTableOutput("table1")
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_C", width = "100%",height = "700px")),
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_d", width = "100%",height = "700px"))
                                                            ))
                                              
                                                   
                                       ))
                             
                             
                           )
                 )
)






# Define server logic required to draw a histogram
server <- function(input, output, session) {


  
data <- reactive({
  
  req(input$Starch)
  
  if (input$Starch == "Potato") {
  
  dat <- dat %>% 
    filter(Starch == "Potato")
  
    } else if (input$Starch == "Wheat") {
  
  dat <- dat %>% 
    filter(Starch == "Wheat")
  
    }
  
})
  


  
# Peak Reactives
peaklm_coded <- reactive({
  
  lm(peak ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data())
  
  })
  
cpeak_C <- reactive({
  
  coef(peaklm_coded())
  
})

troughlm_coded <- reactive({
  
  lm(trough ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data())
  
})

ctrough_C <- reactive({
  
  coef(troughlm_coded())
  
})


breaklm_coded <- reactive({
  
  lm(breakdown ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data())
  
})

cbreak_C <- reactive({
  
  coef(breaklm_coded())
  
})


finallm_coded <- reactive({
  
  lm(final ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data())
  
})

cfinal_C <- reactive({
  
  coef(finallm_coded())
  
})

  
  #### Reactive funcions #####

  
#### WHEAT #####
  
  
### COOL GRAPHS #####  

output$allplots <- renderPlot({
  
  if (input$Starch == "Potato") {
  

  p1 <- data() %>% 
      ggplot(aes(x = as.factor(cooltime), y = peak)) +
      #geom_line() +
       stat_function(fun = function(x) cpeak_C()[1] + cpeak_C()[2]*x + cpeak_C()[3]*input$heat + cpeak_C()[4]*input$hold + cpeak_C()[5]*input$stir + cpeak_C()[6]*input$starch + cpeak_C()[7]*x*input$heat + cpeak_C()[8]*x*input$hold + cpeak_C()[9]*x*input$stir + cpeak_C()[10]*x*input$starch + cpeak_C()[11]*input$heat*input$hold + cpeak_C()[12]*input$heat*input$stir + cpeak_C()[13]*input$heat*input$starch + cpeak_C()[14]*input$hold*input$stir + cpeak_C()[15]*input$hold*input$starch + cpeak_C()[16]*input$stir*input$starch + cpeak_C()[17]*x^2 + cpeak_C()[18]*input$heat^2 + cpeak_C()[19]*input$hold^2 + cpeak_C()[20]*input$stir^2 + cpeak_C()[21]*input$starch^2) +
      scale_y_continuous(limits = c(6800, 7150), breaks = seq(6900, 7050, 100)) +
      geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
      geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
      geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
      geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
      th_left
      
    

  
  
    
t1 <- data() %>% 
    ggplot(aes(x = as.factor(cooltime), y = trough)) +
    #geom_line() +
     stat_function(fun = function(x) ctrough_C()[1] + ctrough_C()[2]*x + ctrough_C()[3]*input$heat + ctrough_C()[4]*input$hold + ctrough_C()[5]*input$stir + ctrough_C()[6]*input$starch + ctrough_C()[7]*x*input$heat + ctrough_C()[8]*x*input$hold + ctrough_C()[9]*x*input$stir + ctrough_C()[10]*x*input$starch + ctrough_C()[11]*input$heat*input$hold + ctrough_C()[12]*input$heat*input$stir + ctrough_C()[13]*input$heat*input$starch + ctrough_C()[14]*input$hold*input$stir + ctrough_C()[15]*input$hold*input$starch + ctrough_C()[16]*input$stir*input$starch + ctrough_C()[17]*x^2 + ctrough_C()[18]*input$heat^2 + ctrough_C()[19]*input$hold^2 + ctrough_C()[20]*input$stir^2 + ctrough_C()[21]*input$starch^2) +
    scale_y_continuous(limits = c(2300, 2600), breaks = seq(2400, 2500, 100)) +
    geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
    th_left
    
  
  
  
  
    
 b1 <- data() %>% 
    ggplot(aes(x = as.factor(cooltime), y = breakdown)) +
    #geom_line() +
     stat_function(fun = function(x) cbreak_C()[1] + cbreak_C()[2]*x + cbreak_C()[3]*input$heat + cbreak_C()[4]*input$hold + cbreak_C()[5]*input$stir + cbreak_C()[6]*input$starch + cbreak_C()[7]*x*input$heat + cbreak_C()[8]*x*input$hold + cbreak_C()[9]*x*input$stir + cbreak_C()[10]*x*input$starch + cbreak_C()[11]*input$heat*input$hold + cbreak_C()[12]*input$heat*input$stir + cbreak_C()[13]*input$heat*input$starch + cbreak_C()[14]*input$hold*input$stir + cbreak_C()[15]*input$hold*input$starch + cbreak_C()[16]*input$stir*input$starch + cbreak_C()[17]*x^2 + cbreak_C()[18]*input$heat^2 + cbreak_C()[19]*input$hold^2 + cbreak_C()[20]*input$stir^2 + cbreak_C()[21]*input$starch^2) +
    scale_y_continuous(limits = c(4350, 4750), breaks = seq(4450, 4650, 100)) +
    geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
    th_left
    
  
  
  
    
  f1 <-  data() %>% 
    ggplot(aes(x = as.factor(cooltime), y = final)) +
    #geom_line() +
     stat_function(fun = function(x) cfinal_C()[1] + cfinal_C()[2]*x + cfinal_C()[3]*input$heat + cfinal_C()[4]*input$hold + cfinal_C()[5]*input$stir + cfinal_C()[6]*input$starch + cfinal_C()[7]*x*input$heat + cfinal_C()[8]*x*input$hold + cfinal_C()[9]*x*input$stir + cfinal_C()[10]*x*input$starch + cfinal_C()[11]*input$heat*input$hold + cfinal_C()[12]*input$heat*input$stir + cfinal_C()[13]*input$heat*input$starch + cfinal_C()[14]*input$hold*input$stir + cfinal_C()[15]*input$hold*input$starch + cfinal_C()[16]*input$stir*input$starch + cfinal_C()[17]*x^2 + cfinal_C()[18]*input$heat^2 + cfinal_C()[19]*input$hold^2 + cfinal_C()[20]*input$stir^2 + cfinal_C()[21]*input$starch^2) +
    scale_y_continuous(limits = c(2825, 3150), breaks = seq(2925, 3050, 100)) +
    geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
    geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
    th_left
  
  
    
    ggpubr::ggarrange(p1, t1, b1, f1, ncol = 1 , nrow = 4)
    
    
  }
  
  
  else if (input$Starch == "Wheat") {
    
     p1 <- data() %>% 
       ggplot(aes(x = as.factor(cooltime), y = peak)) +
       #geom_line() +
       stat_function(fun = function(x) cpeak_C()[1] + cpeak_C()[2]*x + cpeak_C()[3]*input$heat + cpeak_C()[4]*input$hold + cpeak_C()[5]*input$stir + cpeak_C()[6]*input$starch + cpeak_C()[7]*x*input$heat + cpeak_C()[8]*x*input$hold + cpeak_C()[9]*x*input$stir + cpeak_C()[10]*x*input$starch + cpeak_C()[11]*input$heat*input$hold + cpeak_C()[12]*input$heat*input$stir + cpeak_C()[13]*input$heat*input$starch + cpeak_C()[14]*input$hold*input$stir + cpeak_C()[15]*input$hold*input$starch + cpeak_C()[16]*input$stir*input$starch + cpeak_C()[17]*x^2 + cpeak_C()[18]*input$heat^2 + cpeak_C()[19]*input$hold^2 + cpeak_C()[20]*input$stir^2 + cpeak_C()[21]*input$starch^2) +
       scale_y_continuous(limits = c(1100, 1500), breaks = seq(1100, 1500, 100)) +
       scale_x_discrete(limits = c(-1,1), breaks = seq(-1, 1, 1)) +
       geom_hline(aes(yintercept = 1200), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 1300), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 1400), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       th_left
    
    
    
    
    
    
     t1 <- data() %>% 
       ggplot(aes(x = as.factor(cooltime), y = trough)) +
       #geom_line() +
       stat_function(fun = function(x) ctrough_C()[1] + ctrough_C()[2]*x + ctrough_C()[3]*input$heat + ctrough_C()[4]*input$hold + ctrough_C()[5]*input$stir + ctrough_C()[6]*input$starch + ctrough_C()[7]*x*input$heat + ctrough_C()[8]*x*input$hold + ctrough_C()[9]*x*input$stir + ctrough_C()[10]*x*input$starch + ctrough_C()[11]*input$heat*input$hold + ctrough_C()[12]*input$heat*input$stir + ctrough_C()[13]*input$heat*input$starch + ctrough_C()[14]*input$hold*input$stir + ctrough_C()[15]*input$hold*input$starch + ctrough_C()[16]*input$stir*input$starch + ctrough_C()[17]*x^2 + ctrough_C()[18]*input$heat^2 + ctrough_C()[19]*input$hold^2 + ctrough_C()[20]*input$stir^2 + ctrough_C()[21]*input$starch^2) +
       scale_y_continuous(limits = c(900, 1300), breaks = seq(900, 1300, 100)) +
       scale_x_discrete(limits = c(-1,1), breaks = seq(-1, 1, 1)) +
       geom_hline(aes(yintercept = 1000), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 1200), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 1100), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       th_left
    
    
    
    
    
    
     b1 <- data() %>% 
       ggplot(aes(x = as.factor(cooltime), y = breakdown)) +
       #geom_line() +
       stat_function(fun = function(x) cbreak_C()[1] + cbreak_C()[2]*x + cbreak_C()[3]*input$heat + cbreak_C()[4]*input$hold + cbreak_C()[5]*input$stir + cbreak_C()[6]*input$starch + cbreak_C()[7]*x*input$heat + cbreak_C()[8]*x*input$hold + cbreak_C()[9]*x*input$stir + cbreak_C()[10]*x*input$starch + cbreak_C()[11]*input$heat*input$hold + cbreak_C()[12]*input$heat*input$stir + cbreak_C()[13]*input$heat*input$starch + cbreak_C()[14]*input$hold*input$stir + cbreak_C()[15]*input$hold*input$starch + cbreak_C()[16]*input$stir*input$starch + cbreak_C()[17]*x^2 + cbreak_C()[18]*input$heat^2 + cbreak_C()[19]*input$hold^2 + cbreak_C()[20]*input$stir^2 + cbreak_C()[21]*input$starch^2) +
       scale_y_continuous(limits = c(75, 275), breaks = seq(75, 275, 50)) +
       scale_x_discrete(limits = c(-1,1), breaks = seq(-1, 1, 1)) +
       geom_hline(aes(yintercept = 75), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 275), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 175), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       th_left
    
    
    
    
    
     f1 <-  data() %>% 
       ggplot(aes(x = as.factor(cooltime), y = final)) +
       #geom_line() +
       stat_function(fun = function(x) cfinal_C()[1] + cfinal_C()[2]*x + cfinal_C()[3]*input$heat + cfinal_C()[4]*input$hold + cfinal_C()[5]*input$stir + cfinal_C()[6]*input$starch + cfinal_C()[7]*x*input$heat + cfinal_C()[8]*x*input$hold + cfinal_C()[9]*x*input$stir + cfinal_C()[10]*x*input$starch + cfinal_C()[11]*input$heat*input$hold + cfinal_C()[12]*input$heat*input$stir + cfinal_C()[13]*input$heat*input$starch + cfinal_C()[14]*input$hold*input$stir + cfinal_C()[15]*input$hold*input$starch + cfinal_C()[16]*input$stir*input$starch + cfinal_C()[17]*x^2 + cfinal_C()[18]*input$heat^2 + cfinal_C()[19]*input$hold^2 + cfinal_C()[20]*input$stir^2 + cfinal_C()[21]*input$starch^2) +
       scale_y_continuous(limits = c(1000, 1400), breaks = seq(1000, 1400, 100)) +
       scale_x_discrete(limits = c(-1,1), breaks = seq(-1, 1, 1)) +
       geom_hline(aes(yintercept = 1100), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 1300), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = 1200), lwd = .8, lty = 2, alpha = .5) +
       geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
         th_left
    
    
    
    ggpubr::ggarrange(p1, t1, b1, f1, ncol = 1 , nrow = 4)
    
    
  }
    
  })


output$table1 <- renderDataTable({
  
pt <- predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))
  
tt <- predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))
  
bt <- predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))
  
ft <- predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))

test <- data.frame(pt,tt,bt,ft)


DT::datatable(test)
  
  
  
})

  
  
#   
#   
# #### HEAT GRAPHS ####  
#   
#   p2 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(heattime), y = peak)) +
#     #geom_line() +
#     stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*input$cool + cpeak_C[3]*x + cpeak_C[4]*input$hold + cpeak_C[5]*input$stir + cpeak_C[6]*input$starch + cpeak_C[7]*input$cool*x + cpeak_C[8]*input$cool*input$hold + cpeak_C[9]*input$cool*input$stir + cpeak_C[10]*input$cool*input$starch + cpeak_C[11]*x*input$hold + cpeak_C[12]*x*input$stir + cpeak_C[13]*x*input$starch + cpeak_C[14]*input$hold*input$stir + cpeak_C[15]*input$hold*input$starch + cpeak_C[16]*input$stir*input$starch + cpeak_C[17]*input$cool^2 + cpeak_C[18]*x^2 + cpeak_C[19]*input$hold^2 + cpeak_C[20]*input$stir^2 + cpeak_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(6900, 7050), breaks = seq(6900, 7050, 50)) +
#     geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(peaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
#   t2 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(heattime), y = trough)) +
#     #geom_line() +
#     stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*input$cool + ctrough_C[3]*x + ctrough_C[4]*input$hold + ctrough_C[5]*input$stir + ctrough_C[6]*input$starch + ctrough_C[7]*input$cool*x + ctrough_C[8]*input$cool*input$hold + ctrough_C[9]*input$cool*input$stir + ctrough_C[10]*input$cool*input$starch + ctrough_C[11]*x*input$hold + ctrough_C[12]*x*input$stir + ctrough_C[13]*x*input$starch + ctrough_C[14]*input$hold*input$stir + ctrough_C[15]*input$hold*input$starch + ctrough_C[16]*input$stir*input$starch + ctrough_C[17]*input$cool^2 + ctrough_C[18]*x^2 + ctrough_C[19]*input$hold^2 + ctrough_C[20]*input$stir^2 + ctrough_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(2400, 2500), breaks = seq(2400, 2500, 25)) +
#     geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(troughlm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
#   
#   b2 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(heattime), y = breakdown)) +
#     #geom_line() +
#     stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*input$cool + cbreak_C[3]*x + cbreak_C[4]*input$hold + cbreak_C[5]*input$stir + cbreak_C[6]*input$starch + cbreak_C[7]*input$cool*x + cbreak_C[8]*input$cool*input$hold + cbreak_C[9]*input$cool*input$stir + cbreak_C[10]*input$cool*input$starch + cbreak_C[11]*x*input$hold + cbreak_C[12]*x*input$stir + cbreak_C[13]*x*input$starch + cbreak_C[14]*input$hold*input$stir + cbreak_C[15]*input$hold*input$starch + cbreak_C[16]*input$stir*input$starch + cbreak_C[17]*input$cool^2 + cbreak_C[18]*x^2 + cbreak_C[19]*input$hold^2 + cbreak_C[20]*input$stir^2 + cbreak_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(4450, 4650), breaks = seq(4450, 4650, 50)) +
#     geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(breaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
#   f2 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(heattime), y = final)) +
#     #geom_line() +
#     stat_function(fun = function(x) cfinal_C()[1] + cfinal_C[2]*input$cool + cfinal_C[3]*x + cfinal_C[4]*input$hold + cfinal_C[5]*input$stir + cfinal_C[6]*input$starch + cfinal_C[7]*input$cool*x + cfinal_C[8]*input$cool*input$hold + cfinal_C[9]*input$cool*input$stir + cfinal_C[10]*input$cool*input$starch + cfinal_C[11]*x*input$hold + cfinal_C[12]*x*input$stir + cfinal_C[13]*x*input$starch + cfinal_C[14]*input$hold*input$stir + cfinal_C[15]*input$hold*input$starch + cfinal_C[16]*input$stir*input$starch + cfinal_C[17]*input$cool^2 + cfinal_C[18]*x^2 + cfinal_C[19]*input$hold^2 + cfinal_C[20]*input$stir^2 + cfinal_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(2925, 3025), breaks = seq(2925, 3050, 25)) +
#     geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(finallm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
# #### HOLD GRAPHS ####
#   
#     
#   p3 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(holdtime), y = peak)) +
#     #geom_line() +
#     stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*input$cool + cpeak_C[3]*input$heat + cpeak_C[4]*x + cpeak_C[5]*input$stir + cpeak_C[6]*input$starch + cpeak_C[7]*input$cool*input$heat + cpeak_C[8]*input$cool*x + cpeak_C[9]*input$cool*input$stir + cpeak_C[10]*input$cool*input$starch + cpeak_C[11]*input$heat*x + cpeak_C[12]*input$heat*input$stir + cpeak_C[13]*input$heat*input$starch + cpeak_C[14]*x*input$stir + cpeak_C[15]*x*input$starch + cpeak_C[16]*input$stir*input$starch + cpeak_C[17]*input$cool^2 + cpeak_C[18]*input$heat^2 + cpeak_C[19]*x^2 + cpeak_C[20]*input$stir^2 + cpeak_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(6900, 7050), breaks = seq(6900, 7050, 50)) +
#     geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(peaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
#   t3 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(holdtime), y = trough)) +
#     #geom_line() +
#     stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*input$cool + ctrough_C[3]*input$heat + ctrough_C[4]*x + ctrough_C[5]*input$stir + ctrough_C[6]*input$starch + ctrough_C[7]*input$cool*input$heat + ctrough_C[8]*input$cool*x + ctrough_C[9]*input$cool*input$stir + ctrough_C[10]*input$cool*input$starch + ctrough_C[11]*input$heat*x + ctrough_C[12]*input$heat*input$stir + ctrough_C[13]*input$heat*input$starch + ctrough_C[14]*x*input$stir + ctrough_C[15]*x*input$starch + ctrough_C[16]*input$stir*input$starch + ctrough_C[17]*input$cool^2 + ctrough_C[18]*input$heat^2 + ctrough_C[19]*x^2 + ctrough_C[20]*input$stir^2 + ctrough_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(2400, 2500), breaks = seq(2400, 2500, 25)) +
#     geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(troughlm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#     
#   
#   
#   b3 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(holdtime), y = breakdown)) +
#     #geom_line() +
#     stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*input$cool + cbreak_C[3]*input$heat + cbreak_C[4]*x + cbreak_C[5]*input$stir + cbreak_C[6]*input$starch + cbreak_C[7]*input$cool*input$heat + cbreak_C[8]*input$cool*x + cbreak_C[9]*input$cool*input$stir + cbreak_C[10]*input$cool*input$starch + cbreak_C[11]*input$heat*x + cbreak_C[12]*input$heat*input$stir + cbreak_C[13]*input$heat*input$starch + cbreak_C[14]*x*input$stir + cbreak_C[15]*x*input$starch + cbreak_C[16]*input$stir*input$starch + cbreak_C[17]*input$cool^2 + cbreak_C[18]*input$heat^2 + cbreak_C[19]*x^2 + cbreak_C[20]*input$stir^2 + cbreak_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(4450, 4650), breaks = seq(4450, 4650, 50)) +
#     geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(breaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
#   f3 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(holdtime), y = final)) +
#     #geom_line() +
#     stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*input$cool + cfinal_C[3]*input$heat + cfinal_C[4]*x + cfinal_C[5]*input$stir + cfinal_C[6]*input$starch + cfinal_C[7]*input$cool*input$heat + cfinal_C[8]*input$cool*x + cfinal_C[9]*input$cool*input$stir + cfinal_C[10]*input$cool*input$starch + cfinal_C[11]*input$heat*x + cfinal_C[12]*input$heat*input$stir + cfinal_C[13]*input$heat*input$starch + cfinal_C[14]*x*input$stir + cfinal_C[15]*x*input$starch + cfinal_C[16]*input$stir*input$starch + cfinal_C[17]*input$cool^2 + cfinal_C[18]*input$heat^2 + cfinal_C[19]*x^2 + cfinal_C[20]*input$stir^2 + cfinal_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(2925, 3050), breaks = seq(2925, 3050, 25)) +
#     geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(finallm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
# #### STIR GRAPHS #### 
#   
#    
#   p4 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(stirrpm), y = peak)) +
#     #geom_line() +
#     stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*input$cool + cpeak_C[3]*input$heat + cpeak_C[4]*input$hold + cpeak_C[5]*x + cpeak_C[6]*input$starch + cpeak_C[7]*input$cool*input$heat + cpeak_C[8]*input$cool*input$hold + cpeak_C[9]*input$cool*x + cpeak_C[10]*input$cool*input$starch + cpeak_C[11]*input$heat*input$hold + cpeak_C[12]*input$heat*x + cpeak_C[13]*input$heat*input$starch + cpeak_C[14]*input$hold*x + cpeak_C[15]*input$hold*input$starch + cpeak_C[16]*x*input$starch + cpeak_C[17]*input$cool^2 + cpeak_C[18]*input$heat^2 + cpeak_C[19]*input$hold^2 + cpeak_C[20]*x^2 + cpeak_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(6900, 7050), breaks = seq(6900, 7050, 50)) +
#     geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(peaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   t4 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(stirrpm), y = trough)) +
#     #geom_line() +
#     stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*input$cool + ctrough_C[3]*input$heat + ctrough_C[4]*input$hold + ctrough_C[5]*x + ctrough_C[6]*input$starch + ctrough_C[7]*input$cool*input$heat + ctrough_C[8]*input$cool*input$hold + ctrough_C[9]*input$cool*x + ctrough_C[10]*input$cool*input$starch + ctrough_C[11]*input$heat*input$hold + ctrough_C[12]*input$heat*x + ctrough_C[13]*input$heat*input$starch + ctrough_C[14]*input$hold*x + ctrough_C[15]*input$hold*input$starch + ctrough_C[16]*x*input$starch + ctrough_C[17]*input$cool^2 + ctrough_C[18]*input$heat^2 + ctrough_C[19]*input$hold^2 + ctrough_C[20]*x^2 + ctrough_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(2400, 2500), breaks = seq(2400, 2500, 25)) +
#     geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(troughlm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   b4 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(stirrpm), y = breakdown)) +
#     #geom_line() +
#     stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*input$cool + cbreak_C[3]*input$heat + cbreak_C[4]*input$hold + cbreak_C[5]*x + cbreak_C[6]*input$starch + cbreak_C[7]*input$cool*input$heat + cbreak_C[8]*input$cool*input$hold + cbreak_C[9]*input$cool*x + cbreak_C[10]*input$cool*input$starch + cbreak_C[11]*input$heat*input$hold + cbreak_C[12]*input$heat*x + cbreak_C[13]*input$heat*input$starch + cbreak_C[14]*input$hold*x + cbreak_C[15]*input$hold*input$starch + cbreak_C[16]*x*input$starch + cbreak_C[17]*input$cool^2 + cbreak_C[18]*input$heat^2 + cbreak_C[19]*input$hold^2 + cbreak_C[20]*x^2 + cbreak_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(4450, 4650), breaks = seq(4450, 4650, 50)) +
#     geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(breaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
#   
#   f4 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(stirrpm), y = final)) +
#     #geom_line() +
#     stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*input$cool + cfinal_C[3]*input$heat + cfinal_C[4]*input$hold + cfinal_C[5]*x + cfinal_C[6]*input$starch + cfinal_C[7]*input$cool*input$heat + cfinal_C[8]*input$cool*input$hold + cfinal_C[9]*input$cool*x + cfinal_C[10]*input$cool*input$starch + cfinal_C[11]*input$heat*input$hold + cfinal_C[12]*input$heat*x + cfinal_C[13]*input$heat*input$starch + cfinal_C[14]*input$hold*x + cfinal_C[15]*input$hold*input$starch + cfinal_C[16]*x*input$starch + cfinal_C[17]*input$cool^2 + cfinal_C[18]*input$heat^2 + cfinal_C[19]*input$hold^2 + cfinal_C[20]*x^2 + cfinal_C[21]*input$starch^2) +
#     scale_y_continuous(limits = c(2925, 3050), breaks = seq(2925, 3050, 25)) +
#     geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(finallm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
# 
# #### STARCH GRAPHS ####  
#   
#   
#   p5 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(starchg), y = peak)) +
#     #geom_line() +
#     stat_function(fun = function(x) cpeak_C[1] + cpeak_C[2]*input$cool + cpeak_C[3]*input$heat + cpeak_C[4]*input$hold + cpeak_C[5]*input$stir + cpeak_C[6]*x + cpeak_C[7]*input$cool*input$heat + cpeak_C[8]*input$cool*input$hold + cpeak_C[9]*input$cool*input$stir + cpeak_C[10]*input$cool*x + cpeak_C[11]*input$heat*input$hold + cpeak_C[12]*input$heat*input$stir + cpeak_C[13]*input$heat*x + cpeak_C[14]*input$hold*input$stir + cpeak_C[15]*input$hold*x + cpeak_C[16]*input$stir*x + cpeak_C[17]*input$cool^2 + cpeak_C[18]*input$heat^2 + cpeak_C[19]*input$hold^2 + cpeak_C[20]*input$stir^2 + cpeak_C[21]*x^2) +
#     scale_y_continuous(limits = c(6900, 7050), breaks = seq(6900, 7050, 50)) +
#     geom_hline(aes(yintercept = 6950), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7050), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 7000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(peaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th +
#       theme(legend.key = element_blank())
#   
#   t5 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(starchg), y = trough)) +
#     #geom_line() +
#     stat_function(fun = function(x) ctrough_C[1] + ctrough_C[2]*input$cool + ctrough_C[3]*input$heat + ctrough_C[4]*input$hold + ctrough_C[5]*input$stir + ctrough_C[6]*x + ctrough_C[7]*input$cool*input$heat + ctrough_C[8]*input$cool*input$hold + ctrough_C[9]*input$cool*input$stir + ctrough_C[10]*input$cool*x + ctrough_C[11]*input$heat*input$hold + ctrough_C[12]*input$heat*input$stir + ctrough_C[13]*input$heat*x + ctrough_C[14]*input$hold*input$stir + ctrough_C[15]*input$hold*x + ctrough_C[16]*input$stir*x + ctrough_C[17]*input$cool^2 + ctrough_C[18]*input$heat^2 + ctrough_C[19]*input$hold^2 + ctrough_C[20]*input$stir^2 + ctrough_C[21]*x^2) +
#     scale_y_continuous(limits = c(2400, 2500), breaks = seq(2400, 2500, 25)) +
#     geom_hline(aes(yintercept = 2425), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2450), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 2475), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(troughlm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   
#   b5 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(starchg), y = breakdown)) +
#     #geom_line() +
#     stat_function(fun = function(x) cbreak_C[1] + cbreak_C[2]*input$cool + cbreak_C[3]*input$heat + cbreak_C[4]*input$hold + cbreak_C[5]*input$stir + cbreak_C[6]*x + cbreak_C[7]*input$cool*input$heat + cbreak_C[8]*input$cool*input$hold + cbreak_C[9]*input$cool*input$stir + cbreak_C[10]*input$cool*x + cbreak_C[11]*input$heat*input$hold + cbreak_C[12]*input$heat*input$stir + cbreak_C[13]*input$heat*x + cbreak_C[14]*input$hold*input$stir + cbreak_C[15]*input$hold*x + cbreak_C[16]*input$stir*x + cbreak_C[17]*input$cool^2 + cbreak_C[18]*input$heat^2 + cbreak_C[19]*input$hold^2 + cbreak_C[20]*input$stir^2 + cbreak_C[21]*x^2) +
#     scale_y_continuous(limits = c(4450, 4650), breaks = seq(4450, 4650, 50)) +
#     geom_hline(aes(yintercept = 4500), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4600), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 4550), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(breaklm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
#   f5 <- df2_coded %>% 
#     ggplot(aes(x = as.factor(starchg), y = final)) +
#     #geom_line() +
#     stat_function(fun = function(x) cfinal_C[1] + cfinal_C[2]*input$cool + cfinal_C[3]*input$heat + cfinal_C[4]*input$hold + cfinal_C[5]*input$stir + cfinal_C[6]*x + cfinal_C[7]*input$cool*input$heat + cfinal_C[8]*input$cool*input$hold + cfinal_C[9]*input$cool*input$stir + cfinal_C[10]*input$cool*x + cfinal_C[11]*input$heat*input$hold + cfinal_C[12]*input$heat*input$stir + cfinal_C[13]*input$heat*x + cfinal_C[14]*input$hold*input$stir + cfinal_C[15]*input$hold*x + cfinal_C[16]*input$stir*x + cfinal_C[17]*input$cool^2 + cfinal_C[18]*input$heat^2 + cfinal_C[19]*input$hold^2 + cfinal_C[20]*input$stir^2 + cfinal_C[21]*x^2) +
#     scale_y_continuous(limits = c(2925, 3050), breaks = seq(2925, 3050, 25)) +
#     geom_hline(aes(yintercept = 2975), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3025), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = 3000), lwd = .8, lty = 2, alpha = .5) +
#     geom_hline(aes(yintercept = predict(finallm_coded, newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "firebrick") +
#     th
#   
  

  
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)




###############################################