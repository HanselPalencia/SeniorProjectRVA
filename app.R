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
                 fluidPage(theme = shinytheme("cerulean"),
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
dataTableOutput("table1"),
dataTableOutput("table2")
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_C", width = "100%",height = "700px")),
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_d", width = "100%",height = "700px"))
                                                            )),
                                                  tabPanel("Contour Plots",
                                                           fluidRow(
                                                             
                                                             plotOutput("contour")
                                                           ))

                                              
                                                   
                                       )
)
                             
                             
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

# Trough Reactives

troughlm_coded <- reactive({
  
  lm(trough ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data())
  
})

ctrough_C <- reactive({
  
  coef(troughlm_coded())
  
})


# Breakdown Reatives

breaklm_coded <- reactive({
  
  lm(breakdown ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data())
  
})

cbreak_C <- reactive({
  
  coef(breaklm_coded())
  
})


# Final Reactives

finallm_coded <- reactive({
  
  lm(final ~ SO(cooltime,heattime,holdtime,stirrpm,starchg), data = data())
  
})

cfinal_C <- reactive({
  
  coef(finallm_coded())
  
})


# Data Reactives

COOL <- reactive({
  
  rep(input$cool, 201)
  
})

HEAT <- reactive({
  
  rep(input$heat, 201)
  
})

HOLD <- reactive({
  
  rep(input$hold, 201)
  
})


STIR <- reactive({
  
  rep(input$stir, 201)
  
})


STARCH <- reactive({
  
  rep(input$starch, 201)
  
})


  
  #### Reactive funcions #####

  
#### POTATO #####
  
  
### COOL GRAPHS #####  

output$allplots <- renderPlot({
  
  if (input$Starch == "Potato") {
  

    
    p1 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(5000, 9000), breaks = seq(5000, 9000, 500)) +
      geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
      labs(y = "Peak") +
      th_left
    
    p2 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(5000, 9000), breaks = seq(5000, 9000, 500)) +
      geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
      th
    
    p3 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(5000, 9000), breaks = seq(5000, 9000, 500)) +
      geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
      labs(y = "Peak") +
      th
    
    p4 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
      scale_y_continuous(limits = c(5000, 9000), breaks = seq(5000, 9000, 500)) +
      geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
      labs(y = "Peak") +
      th
    
    p5 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
      scale_y_continuous(limits = c(5000, 9000), breaks = seq(5000, 9000, 500)) +
      geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
      labs(y = "Peak") +
      th
    
    
    
    #### TROUGH GRAPHS ####
    
    t1 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(1500, 5000), breaks = seq(1500, 5000, 500)) +
      geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
      labs(y = "Trough") +
      th_left
    
    t2 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(1500, 5000), breaks = seq(1500, 5000, 500)) +
      geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
      labs(y = "Trough") +
      th
    
    t3 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(1500, 5000), breaks = seq(1500, 5000, 500)) +
      geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
      labs(y = "Trough") +
      th
    
    t4 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
      scale_y_continuous(limits = c(1500, 5000), breaks = seq(1500, 5000, 500)) +
      geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
      labs(y = "Trough") +
      th
    
    t5 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
      scale_y_continuous(limits = c(1500, 5000), breaks = seq(1500, 5000, 500)) +
      geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
      labs(y = "Trough") +
      th
    
    
    #### BREAKDOWN GRAPHS #####   
    
    b1 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(2500, 6000), breaks = seq(2500, 6000, 500)) +
      geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
      labs(y = "Breakdown") +
      th_left
    
    b2 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(2500, 6000), breaks = seq(2500, 6000, 500)) +
      geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
      labs(y = "Breakdown") +
      th
    
    b3 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(2500, 6000), breaks = seq(2500, 6000, 500)) +
      geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
      labs(y = "Breakdown") +
      th
    
    b4 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
      scale_y_continuous(limits = c(2500, 6000), breaks = seq(2500, 6000, 500)) +
      geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
      labs(y = "Breakdown") +
      th
    
    b5 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
      scale_y_continuous(limits = c(2500, 6000), breaks = seq(2500, 6000, 500)) +
      geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
      labs(y = "Breakdown") +
      th
    
    
    
    ##### FINAL GRAPHS #####    
    
    f1 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(1955, 5455), breaks = seq(1955, 5455, 750)) +
      geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
      labs(y = "Final", x = "Cool Time") +
      th_left_bottom
    
    f2 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(1955, 5455), breaks = seq(1955, 5455, 750)) +
      geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
      labs(y = "Final", x = "Hold Time") +
      th_bottom
    
    f3 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
      scale_y_continuous(limits = c(1955, 5455), breaks = seq(1955, 5455, 750)) +
      geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
      labs(y = "Final", x = "Heat Time") +
      th_bottom
    
    f4 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
      scale_y_continuous(limits = c(1955, 5455), breaks = seq(1955, 5455, 750)) +
      geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
      labs(y = "Final", x = "Stir RPM") +
      th_bottom
    
    f5 <- ggplot() +
      geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
      scale_y_continuous(limits = c(1955, 5455), breaks = seq(1955, 5455, 750)) +
      geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
      geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
      labs(y = "Final", x = "Starch (g)") +
      th_bottom
    
    
    
    ggpubr::ggarrange(p1, p2, p3, p4, p5, t1, t2, t3, t4, t5, b1, b2, b3, b4, b5, f1, f2, f3, f4, f5, ncol = 5 , nrow = 4)
    
    
  }
  
##### WHEAT #####
  
  
  else if (input$Starch == "Wheat") {
    
     p1 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(900, 1800), breaks = seq(900, 1800, 200)) +
       geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
       labs(y = "Peak") +
       th_left
    
     p2 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(900, 1800), breaks = seq(900, 1800, 200)) +
       geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
       th
     
     p3 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(900, 1800), breaks = seq(900, 1800, 200)) +
       geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
       labs(y = "Peak") +
       th
     
     p4 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
       scale_y_continuous(limits = c(900, 1800), breaks = seq(900, 1800, 200)) +
       geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
       labs(y = "Peak") +
       th
     
     p5 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(peaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
       scale_y_continuous(limits = c(900, 1800), breaks = seq(900, 1800, 200)) +
       geom_hline(aes(yintercept = predict(peaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
       labs(y = "Peak") +
       th
    
    
    
#### TROUGH GRAPHS ####
    
     t1 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(700, 1600), breaks = seq(700, 1600, 200)) +
       geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
       labs(y = "Trough") +
       th_left
     
     t2 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(700, 1600), breaks = seq(700, 1600, 200)) +
       geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
       labs(y = "Trough") +
       th
     
     t3 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(700, 1600), breaks = seq(700, 1600, 200)) +
       geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
       labs(y = "Trough") +
       th
     
     t4 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
       scale_y_continuous(limits = c(700, 1600), breaks = seq(700, 1600, 200)) +
       geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
       labs(y = "Trough") +
       th
    
     t5 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(troughlm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
       scale_y_continuous(limits = c(700, 1600), breaks = seq(700, 1600, 200)) +
       geom_hline(aes(yintercept = predict(troughlm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
       labs(y = "Trough") +
       th
    
    
#### BREAKDOWN GRAPHS #####   
    
     b1 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(130, 230), breaks = seq(130, 230, 20)) +
       geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
       labs(y = "Breakdown") +
       th_left
     
     b2 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(130, 230), breaks = seq(130, 230, 20)) +
       geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
       labs(y = "Breakdown") +
       th
     
     b3 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(130, 230), breaks = seq(130, 230, 20)) +
       geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
       labs(y = "Breakdown") +
       th
     
     b4 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
       scale_y_continuous(limits = c(130, 230), breaks = seq(130, 230, 20)) +
       geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
       labs(y = "Breakdown") +
       th
     
     b5 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(breaklm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
       scale_y_continuous(limits = c(130, 230), breaks = seq(130, 230, 20)) +
       geom_hline(aes(yintercept = predict(breaklm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
       labs(y = "Breakdown") +
       th
    
    
    
##### FINAL GRAPHS #####    
   
     f1 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = my_sample, holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(800, 1700), breaks = seq(800, 1700, 100)) +
       geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$cool), alpha = .5, color = "red") +
       labs(y = "Final", x = "Cool Time") +
         th_left_bottom
     
     f2 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = my_sample, heattime = HEAT(), stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(800, 1700), breaks = seq(800, 1700, 100)) +
       geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$hold), alpha = .5, color = "red") +
       labs(y = "Final", x = "Hold Time") +
       th_bottom
     
     f3 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = my_sample, stirrpm = STIR(), starchg = STARCH())))) +
       scale_y_continuous(limits = c(800, 1700), breaks = seq(800, 1700, 100)) +
       geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$heat), alpha = .5, color = "red") +
       labs(y = "Final", x = "Heat Time") +
       th_bottom
     
     f4 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = my_sample, starchg = STARCH())))) +
       scale_y_continuous(limits = c(800, 1700), breaks = seq(800, 1700, 100)) +
       geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$stir), alpha = .5, color = "red") +
       labs(y = "Final", x = "Stir RPM") +
       th_bottom
     
     f5 <- ggplot() +
       geom_line(aes(x = my_sample, y = predict(finallm_coded(), newdata = data.frame(cooltime = COOL(), holdtime = HOLD(), heattime = HEAT(), stirrpm = STIR(), starchg = my_sample)))) +
       scale_y_continuous(limits = c(800, 1700), breaks = seq(800, 1700, 100)) +
       geom_hline(aes(yintercept = predict(finallm_coded(), newdata = data.frame(cooltime = input$cool, heattime = input$heat , holdtime = input$hold, stirrpm = input$stir, starchg = input$starch))), lwd = 1, col = "steelblue") +
       geom_vline(aes(xintercept = input$starch), alpha = .5, color = "red") +
       labs(y = "Final", x = "Starch (g)") +
       th_bottom
    
    
    
    ggpubr::ggarrange(p1, p2, p3, p4, p5, t1, t2, t3, t4, t5, b1, b2, b3, b4, b5, f1, f2, f3, f4, f5, ncol = 5 , nrow = 4)
    
    
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
  
  
cdat <- reactive({
    
    cbind(COOL(), HEAT(), HOLD(), expand.grid(my_sample, my_sample))
  
  })
  

ppred <- reactive({
  
  predict(peaklm_coded(), newdata = data.frame(cooltime = cdat()[,1], heattime = cdat()[,2], holdtime = cdat()[,3], stirrpm = cdat()[,4], starchg = cdat()[,5])) 
  
})


tpred <- reactive({
  
  predict(troughlm_coded(), newdata = data.frame(cooltime = cdat()[,1], heattime = cdat()[,2], holdtime = cdat()[,3], stirrpm = cdat()[,4], starchg = cdat()[,5])) 
  
})



bpred <- reactive({
  
  predict(breaklm_coded(), newdata = data.frame(cooltime = cdat()[,1], heattime = cdat()[,2], holdtime = cdat()[,3], stirrpm = cdat()[,4], starchg = cdat()[,5])) 

  
})

fpred <- reactive({
  
  predict(finallm_coded(), newdata = data.frame(cooltime = cdat()[,1], heattime = cdat()[,2], holdtime = cdat()[,3], stirrpm = cdat()[,4], starchg = cdat()[,5]))  
  
})
  
  
full_cdat <- reactive({
  
  pdat <- data.frame(cbind(cdat(), ppred()))
  tdat <- data.frame(cbind(cdat(), tpred()))
  bdat <- data.frame(cbind(cdat(), bpred()))
  fdat <- data.frame(cbind(cdat(), fpred()))
  
  names(pdat) <- c("cooltime", "heattime", "holdtime", "stirrpm", "starchg")
  names(tdat) <- c("cooltime", "heattime", "holdtime", "stirrpm", "starchg")
  names(bdat) <- c("cooltime", "heattime", "holdtime", "stirrpm", "starchg")
  names(fdat) <- c("cooltime", "heattime", "holdtime", "stirrpm", "starchg")
  
  pdat$df <- "peak"
  tdat$df <- "trough"
  bdat$df <- "breakdown"
  fdat$df <- "final"
  
  names(pdat)[6] <- "z"
  names(tdat)[6] <- "z"
  names(bdat)[6] <- "z"
  names(fdat)[6] <- "z"
  
  rbind(pdat, tdat, bdat, fdat)
  
  
})


  


filtered_full_cdat <- reactive({
  
  
  
  req(input$Starch)
  
  if (input$Starch == "Potato") {
    
    full_cdat() %>% 
      filter((df == "peak" & between(z, (6950-60),(7050-60))) | 
               (df == "trough" & between(z, 2425,2475)) |
               (df == "breakdown" & between(z, 4500, 4600)) |
               (df == "final" & between(z, (2975+40), (3025+40))))
    
  } else if (input$Starch == "Wheat") {
    
    full_cdat() %>% 
      filter((df == "peak" & between(z, (6950-60),(7050-60))) | 
               (df == "trough" & between(z, 2425,2475)) |
               (df == "breakdown" & between(z, 4500, 4600)) |
               (df == "final" & between(z, (2975+40), (3025+40))))
    
  }
  
  
})
  
  
  

output$contour <- renderPlot({
  
  
   filtered_full_cdat() %>% 
     ggplot() +
     geom_contour(aes(x = stirrpm, starchg, z = z, color = df)) +
     theme_minimal()
  
  
  
})

  
  
  
  
  

  






  
  
}

# Run the application
shinyApp(ui = ui, server = server)




###############################################