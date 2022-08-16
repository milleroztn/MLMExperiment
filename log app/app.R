library(tidyverse)
library(stringr)
library(modelr)
library(broom)
library(lmtest)
library(sandwich)
library(stargazer)
library(shiny)
library(shinycssloaders)
library(shinydashboard)

mlm <- readRDS("../data/mlm_2022_clean.rds")

shinyApp(
  ui = fluidPage(
    titlePanel("Logarithmic Transformations")
    , verticalLayout(selectInput("variable", "Choose dependent variable:",
                                 c("interest", "earnings", "earnleast", "earnmost", "over6", "expenses")
                                 )
                     , box(withSpinner(plotOutput("histograms")), width = 9, title = "Residual Histograms")
                     , box(withSpinner(plotOutput("qqs")), width = 9, title = "Residual Q-Q Plots")
                     )
    )
  
  , server = function(input, output) {
      
    m <- reactive({
      lm(as.formula(paste0(input$variable, " ~ treatment")), data = mlm)
    })
    
    mlog <- reactive({
      lm(as.formula(paste0("l", input$variable, " ~ treatment")), data = mlm)
    })
      
    results <- reactive({
      augment(m(), mlm %>% filter(!is.na(get(input$variable)))) %>%
        mutate(model = input$variable)
    })
    
    logresults <- reactive({
      augment(mlog(), mlm %>% filter(!is.na(get(input$variable)))) %>%
        mutate(model = paste0("log(", input$variable, ")"))
    })
    
    compare <- reactive({
      rbind(results(), logresults()) %>%
        mutate(model = as_factor(model) %>% 
                        fct_relevel(input$variable)
               )
    })
      
    output$histograms <- renderPlot(
      ggplot(compare()) +
        geom_histogram(aes(.resid), bins = 10) +
        facet_wrap(~model, scales = "free")
      )
    
    output$qqs <- renderPlot(
      ggplot(compare(), aes(sample = .resid)) + 
        stat_qq() +
        stat_qq_line() +
        facet_wrap(~model, scales = "free")
      )
        
    })