library(shiny)
library(tidyverse)
library(ggpubr)
# load data
bearing=read.csv('b_berry_bearing.csv')
market=read.csv('b_berry_market.csv')

function(input,output){
# Interactive inputs
  ## interactive input of category  
  s_category=reactive({
    sort(unique(bearing$Category[bearing$Domain%in%input$domain]))
  })
  output$ui_category=renderUI({
    d_selected=ifelse('(TOTAL)'%in%s_category(),'(TOTAL)',NA)
    selectInput('category','Category',s_category(),d_selected)
  })
  ## interactive input of unit
  s_unit=reactive({
    sort(unique(bearing$Unit[bearing$Domain%in%input$domain
                              &bearing$Category%in%input$category]))
  })
  output$ui_unit=renderUI({
    checkboxGroupInput('unit','Unit',s_unit(),s_unit()[1])
  })
  ## interactive input of state
  s_state=reactive({
    sort(unique(bearing$State[bearing$Domain%in%input$domain
                              &bearing$Category%in%input$category
                              &bearing$Unit%in%input$unit]))
  })
  output$ui_state=renderUI({
    checkboxGroupInput('state','State',s_state(),s_state()[1])
  })
  ## interactive input of year
  s_year=reactive({
    sort(unique(bearing$Year[bearing$Domain%in%input$domain
                             &bearing$Category%in%input$category
                             &bearing$Unit%in%input$unit
                             &bearing$State%in%input$state]))
  })
  output$ui_year=renderUI({
    checkboxGroupInput('year','Year',s_year(),s_year()[1])
  })
# Data used to plot

  p_bearing=reactive({
    filter(bearing,
           Domain%in%input$domain,
           Category%in%input$category,
           Unit%in%input$unit,
           State%in%input$state,
           Year%in%input$year)%>%select(Year,State,Commodity,Unit,Value)
  })
# Export data frame and plot
  output$t_bearing=renderDataTable(p_bearing())
  ## FERTILIZER
  f=filter(bearing,Domain=='FERTILIZER',Unit=='LB')
  f$Value[f$Value==' (D)']=0
  f$Value=as.numeric(str_replace_all(f$Value,c(','='')))
  f_2019=ggplot(f[f$Year=='2019',])+
    geom_bar(aes(x=State,weight=Value,fill=Category),position='dodge')+
    labs(x='State',y='Measured in LB',title='FERTILIZER of Different States in 2019')
  f_2015=ggplot(f[f$Year=='2015',])+
    geom_bar(aes(x=State,weight=Value,fill=Category),position='dodge')+
    labs(x='State',y='Measured in LB',title='FERTILIZER of Different States in 2015')
  ## CHEMICAL
  c=filter(bearing,Unit=='LB',Category=='(TOTAL)')
  c$Value[c$Value==' (D)']=0
  c$Value=as.numeric(str_replace_all(c$Value,c(','='')))
  c_2019=ggplot(c[c$Year=='2019',])+
    geom_bar(aes(x=State,weight=Value,fill=Domain),position='dodge')+
    labs(x='State',y='Measured in LB',title='CHEMICAL of Different States in 2019')
  c_2017=ggplot(c[c$Year=='2017',])+
    geom_bar(aes(x=State,weight=Value,fill=Domain),position='dodge')+
    labs(x='State',y='Measured in LB',title='CHEMICAL of Different States in 2017')
  c_2015=ggplot(c[c$Year=='2015',])+
    geom_bar(aes(x=State,weight=Value,fill=Domain),position='dodge')+
    labs(x='State',y='Measured in LB',title='CHEMICAL of Different States in 2015')
  ## PLOT
  plot_bearing=reactive({
      if(input$domain=='FERTILIZER'){
        if('2015'%in%input$year){
          if('2019'%in%input$year)ggpubr::ggarrange(f_2015,f_2019,ncol=1)
          else f_2015
        }
        else f_2019
      }
      else if('2015'%in%input$year){
        if('2017'%in%input$year){
          if('2019'%in%input$year)ggpubr::ggarrange(c_2015,c_2017,c_2019,ncol=1)
          else ggpubr::ggarrange(c_2015,c_2017,ncol=1)
        }
        else c_2015
      }
    
  })
  output$p_bearing=renderPlot(plot_bearing())
# Interactive Input
  ## interactive input of year
  s2_year=reactive({
    sort(unique(market$Year[market$State%in%input$state2]))
  })
  output$ui2_year=renderUI({
    checkboxGroupInput('year2','Year',s2_year(),s2_year()[1])
  })
  ## interactive input of unit
  market$MU=paste(market$Measurement,', MEASURED IN ',market$Unit)
  s2_unit=reactive({
    sort(unique(market[market$State%in%input$state2
                       &market$Year%in%input$year2,]$MU))
  })
  output$ui2_unit=renderUI({
    checkboxGroupInput('unit2','Measurement & Unit',s2_unit(),s2_unit()[1])
  })
# Data used to plot
  p_market=reactive({
    filter(market,
           State%in%input$state2,
           MU%in%input$unit2,
           Year%in%input$year2)%>%select(Year,State,Commodity,Measurement,Unit,Value)
  })
# Export data frame and plot
  output$t_market=renderDataTable(p_market())
  ## Plot
  plot_market=reactive({
    m=filter(market,
             State%in%input$state2,
             Measurement%in%c('PRODUCTION','ACRES HARVESTED'),
             Year%in%input$year2)
    m$Value[m$Value==' (D)']=0
    m$Value=as.numeric(str_replace_all(m$Value,c(','='')))
    m_1=ggplot(m[m$Measurement=='PRODUCTION',])+
      geom_bar(aes(x=as.factor(Year),weight=Value))+
      labs(x='Year',y='Measured in LB',title=paste('Production of ',input$state2))
    m_2=ggplot(m[m$Measurement=='ACRES HARVESTED',])+
      geom_bar(aes(x=as.factor(Year),weight=Value))+
      labs(x='Year',y='Measured in ACRE',title=paste('ACRES HARVESTED from ',input$state2))
    ggpubr::ggarrange(m_1,m_2,ncol=1)
  })
  output$p_market=renderPlot(plot_market())
  
}
