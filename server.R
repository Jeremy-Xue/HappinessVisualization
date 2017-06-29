#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dendextend)
library(MASS)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Read files
  avg <- read_csv("https://raw.githubusercontent.com/joyce0917/happiness/master/happy.csv")
  
  
  #Dendrogram code
  weurope <- filter(avg, Region == "Western Europe")
  na <- filter(avg, Region == "North America")
  au <- filter(avg, Region == "Australia and New Zealand")
  mideast <- filter(avg, Region == "Middle East and Northern Africa")
  easia <- filter(avg, Region == "Eastern Asia")
  sea <- filter(avg, Region == "Southeastern Asia")
  sasia <- filter(avg, Region == "Southern Asia")
  sahara <- filter(avg, Region == "Sub-Saharan Africa")
  latin <- filter(avg, Region == "Latin America and Caribbean")
  ceeurope <- filter(avg, Region == "Central and Eastern Europe")
  
  weurope <- summarize(weurope,
                       Region = " Western Europe",
                       Economy = mean(Economy),
                       Family = mean(Family),
                       Health = mean(Health),
                       Freedom = mean(Freedom),
                       Trust = mean(Trust),
                       Generosity = mean(Generosity)
                       )
  na <- summarize(na, Economy = mean(Economy),
                  Region = " North America",
                       Family = mean(Family),
                       Health = mean(Health),
                       Freedom = mean(Freedom),
                       Trust = mean(Trust),
                       Generosity = mean(Generosity)
                  )
  au <- summarize(au, Economy = mean(Economy),
                  Region = " Australia/NZ",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  mideast <- summarize(mideast, Economy = mean(Economy),
                       Region = " Middle East",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  easia <- summarize(easia, Economy = mean(Economy),
                     Region = " Eastern Asia",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  sea <- summarize(sea, Economy = mean(Economy),
                   Region = " Southeastern Asia",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  sasia <- summarize(sahara, Economy = mean(Economy),
                     Region = " Southern Asia",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  sahara <- summarize(sahara, Economy = mean(Economy),
                      Region = " Sub-Saharan Africa",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  latin <- summarize(latin, Economy = mean(Economy),
                     Region = " Latin America",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  ceeurope <- summarize(ceeurope, Economy = mean(Economy),
                        Region = " Eastern Europe",
                  Family = mean(Family),
                  Health = mean(Health),
                  Freedom = mean(Freedom),
                  Trust = mean(Trust),
                  Generosity = mean(Generosity)
  )
  
  all_region_means <- rbind(weurope, na, au, mideast, easia,
                            sea, sasia, sahara, latin, ceeurope)
  
  region_cont <- dplyr::select(all_region_means, Economy, Family,
                               Health, Freedom, Trust, Generosity)
  region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
  region_dend <- region_dend %>%
    set("labels", all_region_means$Region,
        order_value = TRUE)
  
  `%notin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
  
  #Heatmap code
  region_cont2 <- dplyr::select(avg, Economy, Family,
                               Health, Freedom, Trust, Generosity)
  correlation_matrix <- cor(region_cont2)
  library(reshape2)
  library(heatmaply)
  melted_cormat <- melt(correlation_matrix)
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  correlation_matrix <- reorder_cormat(correlation_matrix)
  
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  correlation_matrix <- get_lower_tri(correlation_matrix)
  melted_cormat <- melt(correlation_matrix, na.rm = T)
  
  heatmap <- heatmaply(correlation_matrix, Rowv = F, Colv = F)
  
  output$dend_plot <- renderPlot({
    
    if("economy" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Economy)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("family" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Family)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("health" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Health)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("freedom" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Freedom)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("trust" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Trust)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("generosity" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Generosity)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    
    region_dend <- region_dend %>%
      ggplot(horiz = T)
    
    print(region_dend)
  })
  
  output$heat <- renderPlotly({
    print(heatmap)
  })
  
})

