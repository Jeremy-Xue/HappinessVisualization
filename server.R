library(plotly)
library(ggplot2)
library(tidyverse)
library(forcats)
library(dplyr)
library(shiny)
library(dendextend)
library(MASS)
library(leaflet)
library(data.table)



################################################################################
# Set up the data - map
################################################################################

# load in happiness data
happiness <- read_csv("https://raw.githubusercontent.com/joyce0917/happiness/master/happy.csv")

# rename countries
happiness$Country[happiness$Country == "United States"] = 
  "USA"
happiness$Country[happiness$Country == "Congo (Kinshasa)"] = 
  "Democratic Republic of the Congo"
happiness$Country[happiness$Country == "Congo (Brazzaville)"] = 
  "Republic of Congo"
happiness$Country[happiness$Country == "United Kingdom"] = "UK"

# load in world map coordinates
world_data <- map_data("world")

# join the data frames
world_data_map <- left_join(world_data, happiness,
                            by = c("region" = "Country"))

################################################################################
# set up data - radar
################################################################################

happy_means <- happiness %>% group_by(Region) %>%
  summarize("Economy" = mean(Economy),
            "Family" = mean(Family),
            "Health" = mean(Health),
            "Freedom" = mean(Freedom),
            "Trust" = mean(Trust),
            "Generosity" = mean(Generosity))

# transform matrix
happy_means_1 <- data.frame(t(data.frame(happy_means[,-1])))

# add column titles
names(happy_means_1) = data.frame(happy_means)[,1]

# add row label colummn
data.table::setDT(happy_means_1, keep.rownames = T)


shinyServer(function(input, output) {
  happy <- read_csv("https://raw.githubusercontent.com/joyce0917/happiness/master/happy.csv")
  
  seojinm_315_theme <-  theme_bw() + # White background, black and white theme
    theme(axis.text = element_text(size = 10, color="grey"),
          text = element_text(size = 14, face = "bold", color = "darkslategrey"),
          plot.background = element_rect(fill = "white"))
  
  
  output$scatvar <- renderPlotly({
    graphic <- ggplot(happy, aes_string(x = input$Variable, y = "Happiness_Score", 
                                        fill = "Region"
                                          )) +
      labs(y = "Happiness") +
      geom_point(aes(text = paste(Country)), color = NA) + seojinm_315_theme
    ggplotly(graphic,tooltip = "text",height = 600,width = 800)
  })
  
  happy2<-happy
  happy2$Happiness_Rank<-NULL
  happy2$Happiness_Score<-NULL
  happy2$X1 <-NULL
  library(reshape2)
  
 
  
  happy_func <- reactive({

    happy3 <- happy2[happy2$Region %in% input$Region,]

  })

  
  output$box <- renderPlotly({
      graphic <- ggplot() + geom_boxplot(data = melt(happy2), aes(variable, value)) + seojinm_315_theme
      if(input$dots){
        # if(input$SA){
          graphic<-graphic + geom_point(data = melt(happy_func()),aes(variable,value, 
                                                            text = paste(Country), color = Region),
                                         position = position_jitter(width = 0.2), size = 1)
        # }
        
      }
      ggplotly(graphic,tooltip = "text",height = 500,width = 500)
  })
  
  
  
  
  
  ######################
  # choropleth map code
  output$main <- renderPlotly({
    
    if (input$ariable == "Happiness Score") {
      
      main_map <- ggplot(world_data_map) +
        geom_polygon(aes(x = long, y = lat, group = group,
                         fill = Happiness_Score, 
                         text = sprintf("%s<br>Happiness: %s",
                                        region, round(Happiness_Score, 3))),
                     color = "black") +
        theme_void() +
        scale_fill_gradient(low = "white", high = "midnightblue") +
        labs(fill = "Happiness") + seojinm_315_theme
      
    }
    
    if (input$ariable == "Economy") {
      
      main_map <- ggplot(world_data_map) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = Economy,
                         text = sprintf("Country: %s<br>Economy: %s",
                                        region, round(Economy, 3))), 
                     color = "black") +
        theme_void() +
        scale_fill_gradient(low = "white", high = "midnightblue")
      
    }  
    
    if (input$ariable == "Family") {
      
      main_map <- ggplot(world_data_map) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = Family,
                         text = sprintf("Country: %s<br>Family: %s",
                                        region, round(Family, 3))), 
                     color = "black") +
        theme_void() +
        scale_fill_gradient(low = "white", high = "midnightblue")
      
    }
    
    if (input$ariable == "Health") {
      
      main_map <- ggplot(world_data_map) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = Health,
                         text = sprintf("Country: %s<br>Health: %s",
                                        region, round(Health, 3))), 
                     color = "black") +
        theme_void() +
        scale_fill_gradient(low = "white", high = "midnightblue")
      
    }
    
    if (input$ariable == "Freedom") {
      
      main_map <- ggplot(world_data_map) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = Freedom,
                         text = sprintf("Country: %s<br>Freedom: %s",
                                        region, round(Freedom, 3))), 
                     color = "black") +
        theme_void() +
        scale_fill_gradient(low = "white", high = "midnightblue")
      
    }
    
    if (input$ariable == "Trust") {
      
      main_map <- ggplot(world_data_map) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = Trust,
                         text = sprintf("Country: %s<br>Trust: %s",
                                        region, round(Trust, 3))), 
                     color = "black") +
        theme_void() +
        scale_fill_gradient(low = "white", high = "midnightblue")
      
    }
    
    if (input$ariable == "Generosity") {
      
      main_map <- ggplot(world_data_map) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = Generosity,
                         text = sprintf("Country: %s<br>Generosity: %s",
                                        region, round(Generosity, 3))), 
                     color = "black") +
        theme_void() +
        scale_fill_gradient(low = "white", high = "midnightblue")
      
    }
    
    ggplotly(main_map, tooltip = "text", height = 550, width = 900)
    
  })
  
  # select certain regions for radar chart
  select_happy <- reactive({
    
    data.frame(happy_means_1)[,names(happy_means_1) %in% 
                                c(names(happy_means_1)[1],input$region)]
    
  })
  
  # create radar chart
  output$radar_plot <- renderChartJSRadar({
    
    chartJSRadar(select_happy(), showToolTipLabel = F)
    
  })
  
  
  
  
  
  ################
  
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
    dendextend::set("labels", all_region_means$Region,
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
  
  heatmap <- heatmaply(correlation_matrix, Rowv = F, Colv = F,
                       scale_fill_gradient_fun = scale_fill_gradient2(),
                       low = "dark blue", high = "white")
  
  output$dend_plot <- renderPlot({
    
    if("economy" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Economy)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        dendextend::set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("family" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Family)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        dendextend::set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("health" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Health)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        dendextend::set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("freedom" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Freedom)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        dendextend::set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("trust" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Trust)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        dendextend::set("labels", all_region_means$Region,
            order_value = TRUE)
    }
    if("generosity" %notin% input$variable){
      region_cont <- dplyr::select(region_cont, -Generosity)
      region_dend <- region_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
      region_dend <- region_dend %>%
        dendextend::set("labels", all_region_means$Region,
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



