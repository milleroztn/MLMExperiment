library(tidyverse)
library(broom)
library(Hmisc)
library(shiny)
library(shinycssloaders)
library(shinydashboard)


mlm <- readRDS("../data/mlm_2022_clean.rds")

dependents <- c("interest", "earnings", "earnleast", "earnmost", "over6", "expenses")
key_ind <- c("numeracy", "finance", "EVtest", "educ_hs", "educ_sc", "educ_cg", "educ_pg", "earnbta")
other <- c("age", "man", "woman", "black", "white", "hispanic", "other", 
           "inc_0", "inc_25", "inc_50", "inc_100", "rel_nr", "rel_lr", "rel_sr", "rel_vr", 
           "risk", "knownMLM", "wasMLM", "MLMrecruited")

rcorrlist <- mlm %>%
              select(all_of(dependents), all_of(key_ind), all_of(other)) %>% 
              as.matrix() %>% 
              rcorr(type="pearson")

shinyApp(
  ui = fluidPage(
    titlePanel("Only Significant Correlations")
    , verticalLayout(numericInput("p", label = "p-value", 0.1, min = 0, max = 1)
                     ) 
    , mainPanel(withSpinner(verbatimTextOutput("table")), width=9)
    )
  
  , server = function(input, output) {
    
    f <- reactive({
      as.vector(rcorrlist$P < input$p)
    })
      
    output$table <- renderPrint(
      as_tibble(rcorrlist$r) %>% 
      mutate(variable = row.names(rcorrlist$r)) %>% 
      pivot_longer(c(-"variable")) %>%
      filter(f()) %>%
      pivot_wider() %>%
      filter(!variable %in% dependents) %>% 
      filter(!(is.na(interest) & is.na(earnings))) %>%
      select(variable, interest, earnings)
      )
    
    })