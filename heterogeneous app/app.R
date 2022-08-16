library(tidyverse)
library(stringr)
library(modelr)
library(broom)
library(lmtest)
library(sandwich)
library(stargazer)
library(shiny)
library(shinycssloaders)


mlm <- readRDS("../data/mlm_2022_clean.rds")

shinyApp(
  ui = fluidPage(
    titlePanel("Heterogeneous Treatment Effects")
    , verticalLayout(selectInput("covariate", "Choose a covariate:",
                               c("Age", "Gender: Woman", "Numeracy", "Education: High School",
                                 "Education: Some College", "Education: College Degree", 
                                 "Education: Post-graduate", "Finance", "EVtest", "Earnbta")
                               )
                     )
    , mainPanel(withSpinner(verbatimTextOutput("table")), width = 9)
    )
  
  , server = function(input, output) {
      
      varname <- reactive({
        if_else(input$covariate == "Numeracy", "numeracy",
        if_else(input$covariate == "Education: High School", "educ_hs", 
        if_else(input$covariate == "Education: Some College", "educ_sc", 
        if_else(input$covariate == "Education: College Degree", "educ_cg", 
        if_else(input$covariate == "Education: Post-graduate", "educ_pg",
        if_else(input$covariate == "Finance", "finance", 
        if_else(input$covariate == "Earnbta", "earnbta4",
        if_else(input$covariate == "Age", "age",
        if_else(input$covariate == "Gender: Woman", "woman",
                input$covariate)))))))))
      })
      
      cov <- reactive({
        if (input$covariate == "Age" || input$covariate == "Gender: Woman") {
          paste0("+ woman + age + income + black + white + hispanic + relig + risk + knownMLM + wasMLM",
                 "+ numeracy + education + finance + EVtest + earnbta")
        } else {
          "+ woman + age + income + black + white + hispanic + relig + risk + knownMLM + wasMLM"
        }
      })
    
      m_int <- reactive({
        lm(as.formula(paste0("linterest ~ treatment*", varname())), data = mlm)
      })
      
      m_int_c <- reactive({
        lm(as.formula(paste0("linterest ~ treatment*", varname(), cov())), data = mlm)
      })
      
      m_earn <- reactive({
        lm(as.formula(paste0("learnings ~ treatment*", varname())), data = mlm)
      })
      
      m_earn_c <- reactive({
        lm(as.formula(paste0("learnings ~ treatment*", varname(), cov())), data = mlm)
      })
      
      keep <- reactive({
        c("Company Disclosure", "Graphical Disclosure", varname())
      })
      
      labels <- reactive({
        c("Treatment 1", "Treatment 2", input$covariate
          , paste("Treatment 1 x", input$covariate)
          , paste("Treatment 2 x", input$covariate)
          )
      })
      
      ses <- reactive({
        list(sqrt(diag(vcovHC(m_int(), type = "HC1"))),
             sqrt(diag(vcovHC(m_int_c(), type = "HC1"))),
             sqrt(diag(vcovHC(m_earn(), type = "HC1"))),
             sqrt(diag(vcovHC(m_earn_c(), type = "HC1")))
             )
      })
            
      output$table <- renderPrint(
        stargazer(m_int(), m_int_c(), m_earn(), m_earn_c(),
                    se = ses(),  
                    keep = keep(),
                    header=FALSE,
                    column.labels = c("Interest", "Interest", "Earnings", "Earnings"),
                    dep.var.labels.include = FALSE,
                    model.numbers          = FALSE,
                    covariate.labels = labels(),
                    digits = 2,
                    digits.extra = 1,
                    omit.stat = c("f","ser","rsq"),
                    report= c("vc*sp"),
                    notes.align = "l",
                    add.lines = list(c("Controls", "", "X", "", "X")),
                    type="text")
        )
        
    })