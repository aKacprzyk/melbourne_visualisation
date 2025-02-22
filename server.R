#Biblioteki
library(shiny)
library(CAST)
library(sf)
library(readr)
library(dplyr)
library(tidyverse)
library(tmap)
library(tmaptools)

#Wczytywanie danych
data <- read.csv(
  "C:/Users/Ola/Documents/sgh_master/sem1/wiz/projekt - nieruchomosci/[114125]_[115073]/Melbourne_housing_FULL.csv", 
  header = TRUE, sep = ","
)

#Czyszczenie danych
data_final <- data %>% 
  select(-c(Bedroom2, Bathroom, Car, Landsize, BuildingArea)) %>%
  mutate(YearInfo = ifelse(is.na(YearBuilt), 0, 1)) %>%
  select(-YearBuilt) %>%
  na.omit()

#Grupowanie danych
data_clustered <- data_final %>%
  group_by(Longtitude, Lattitude) %>%
  summarize(count = n(), .groups = "drop") %>%
  st_as_sf(coords = c("Longtitude", "Lattitude"), crs = 4326)

#Załadowanie mapy Melbourne
melbourne <- read_sf(
  "C:/Users/Ola/Documents/sgh_master/sem1/wiz/projekt - nieruchomosci/[114125]_[115073]/street-names/street-names.shp"
) %>%
  st_transform(4326)

#server
shinyServer(function(input, output, session) {
  filtered_data <- reactive({
    if (input$wybranaGestosc == "wszystko") {
      data_clustered
    } else {
      filtered <- subset(data_clustered, count == as.numeric(input$wybranaGestosc))
    }
  })
  
  #skala
  dynamic_scale <- reactive({
    if (input$wybranaGestosc == "wszystko") {
      2  
    } else {
      0.5
    }
  })
  
  output$mapa <- renderTmap({
    
    tmap_mode("plot")
    
    data <- filtered_data()
    tm_shape(melbourne) + 
      tm_lines(col = "gray80", lwd = 0.5) +
      tm_shape(data) +  
      tm_bubbles(size = "count", 
                 col = "count", 
                 palette = "viridis", 
                 alpha = 0.8, 
                 border.col = "white", 
                 scale = dynamic_scale(),
                 legend.size.show = TRUE) + 
      tm_layout(
        legend.outside = TRUE, 
        main.title = "Analiza gęstości nieruchomości w Melbourne", 
        main.title.size = 1.2
      ) 
  })
})
