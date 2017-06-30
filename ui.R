#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(heatmaply)

# Define UI for application that draws a histogram



shinyUI(navbarPage(
  title = "World Happiness",
  tabPanel("Dendrogram",
           titlePanel("Dendrogram of Factors affecting Happiness"),
           selectizeInput("variable", "Variable:",
                          c("Economy" = "economy",
                            "Family" = "family",
                            "Health" = "health",
                            "Freedom" = "freedom",
                            "Trust" = "trust",
                            "Generosity" = "generosity"), multiple = TRUE,
                          selected = c("economy", "family", "health", "freedom",
                                       "trust", "generosity")),

           
           plotOutput(outputId = "dend_plot", height = "300px")
           #plotlyOutput(outputId = "heat", height = "300px")
          
  ),
  tabPanel("Heatmap",
           titlePanel("Heatmap of factor correlation"),
           plotlyOutput(outputId = "heat", height = "300px")
  )
))