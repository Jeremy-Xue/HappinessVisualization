# library(gganimate)
library(shiny)
library(plotly)
library(heatmaply)
library(radarchart)

    shinyUI(navbarPage(
      title = "World Happiness",
      tabPanel("World Map",
               titlePanel("Worldwide Comparison of Happiness 
                        and its Contributing Factors"),
               
               selectInput("ariable", label = h3("Variable:"), 
                           choices = list("Happiness Score" = "Happiness Score", 
                                          "Economy (GDP per Capita)" = "Economy", 
                                          "Family" = "Family",
                                          "Health (Life Expectancy)" = "Health",
                                          "Freedom" = "Freedom",
                                          "Trust (Government Corruption)" = "Trust",
                                          "Generosity" = "Generosity"), 
                           selected = "Happiness Score"),
               
               plotlyOutput(outputId = "main", width = "800px", height = "600px")
               
      ),
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

    tabPanel("Scatterplot",
             titlePanel("Happiness vs. other variables"),


             selectInput(inputId = "Variable",
                         label = "Variable:",
                         choices = c('Economy','Family',
                                     'Health', 'Freedom',
                                     'Trust',
                                     'Generosity'),
                         selected = 'Economy'),

             plotlyOutput(outputId = "scatvar", height = "300px")

    ),
    tabPanel("Radar Chart",
             titlePanel("Importance of Different Factors"),
             
             
             selectizeInput("region", label = h3("Region:"), 
                            choices = list("Australia and New Zealand" = 
                                             "Australia and New Zealand",
                                           "Central and Eastern Europe" = 
                                             "Central and Eastern Europe",
                                           "Eastern Asia" = 
                                             "Eastern Asia",
                                           "Latin America and Caribbean" = 
                                             "Latin America and Caribbean",
                                           "Middle East and Northern Africa" = 
                                             "Middle East and Northern Africa",
                                           "North America" = 
                                             "North America",
                                           "Southeastern Asia" = 
                                             "Southeastern Asia",
                                           "Southern Asia" = 
                                             "Southern Asia",
                                           "Sub-Saharan Africa" = 
                                             "Sub-Saharan Africa",
                                           "Western Europe" = 
                                             "Western Europe"), 
                            selected = c("North America", "Sub-Saharan Africa"), 
                            multiple = TRUE,
                            options = NULL),
             
             chartJSRadarOutput("radar_plot", width = "450", height = "300")
             
    ),
    tabPanel("Boxplot",
             titlePanel("boxplot"),
             checkboxInput("dots", "Show Dots", FALSE),
             # checkboxInput("SA", "Southern Asia", FALSE),
             # checkboxInput("CEE", "Central and Eastern Europe", FALSE),
             # checkboxInput("MENA", "Middle East and Northern Africa", FALSE),
             # checkboxInput("SSA", "Sub-Saharan Africa", FALSE),
             # checkboxInput("LAC", "Latin America and Caribbean", FALSE),
             # checkboxInput("ANZ", "Australia and New Zealand", FALSE),
             # checkboxInput("WE", "Western Europe", FALSE),
             # checkboxInput("SA", "Southeastern Asia", FALSE),
             # checkboxInput("NAA", "North America", FALSE),
             # checkboxInput("EA", "Eastern Asia", FALSE),
             
             selectizeInput("Region", label = "Region:", 
                            choices = list("Australia and New Zealand" = 
                                             "Australia and New Zealand",
                                           "Central and Eastern Europe" = 
                                             "Central and Eastern Europe",
                                           "Eastern Asia" = 
                                             "Eastern Asia",
                                           "Latin America and Caribbean" = 
                                             "Latin America and Caribbean",
                                           "Middle East and Northern Africa" = 
                                             "Middle East and Northern Africa",
                                           "North America" = 
                                             "North America",
                                           "Southeastern Asia" = 
                                             "Southeastern Asia",
                                           "Southern Asia" = 
                                             "Southern Asia",
                                           "Sub-Saharan Africa" = 
                                             "Sub-Saharan Africa",
                                           "Western Europe" = 
                                             "Western Europe"), 
                            selected = c("North America"), 
                            multiple = TRUE,
                            options = NULL),
             plotlyOutput(outputId = "box", height = "300px")
    ),

    tabPanel("Heatmap",
             titlePanel("Heatmap of factor correlation"),
             plotlyOutput(outputId = "heat", height = "300px")
    )

  ))

