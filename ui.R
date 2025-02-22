library(shiny)

shinyUI(fluidPage(
  titlePanel("Rynek nieruchomości w Melbourne"),
  
  sidebarLayout(
    sidebarPanel(
      p("Autorzy: Aleksandra Kacprzyk 114125, Sofiya Iadkouskaya 115079"),
      selectInput(
        inputId = "wybranaGestosc", 
        label = "Wybierz zakres gęstości do analizy:",
        choices = c("wszystko", unique(data_clustered$count)),  
        selected = "wszystko"
      )
    ),
    
    mainPanel(
      p("Mapa"),
      tmapOutput("mapa")
    )
  )
))
