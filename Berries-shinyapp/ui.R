library(shiny)
library(tidyverse)
library(ggpubr)

# load data
bearing=read.csv('b_berry_bearing.csv')
market=read.csv('b_berry_market.csv')

fluidPage(
  mainPanel(
    headerPanel('Blueberry Data from USDA'),
    tabsetPanel(
      tabPanel("Chemical & Fertilizer Bearing",
               br(),
               sidebarPanel(
                 selectInput('domain','Domain',
                             choices=sort(unique(bearing$Domain))),
                 uiOutput('ui_category'),
                 uiOutput('ui_unit'),
                 uiOutput('ui_state'),
                 uiOutput('ui_year')
               ),
               mainPanel(
                 h3('Plot of bearing'),
                 plotOutput('p_bearing'),
                 h3('Data selected'),
                 dataTableOutput('t_bearing')
               )
      ),
      tabPanel("Production & Price Related", 
               br(), 
               sidebarPanel(
                 selectInput('state2','State',
                             choices=sort(unique(market$State))),
                 uiOutput('ui2_year'),
                 uiOutput('ui2_unit')
               ),
               mainPanel(
                 h3('Plot of market'),
                 plotOutput('p_market'),
                 h3('Data selected'),
                 dataTableOutput('t_market')
               )
      )
    )
  )
)