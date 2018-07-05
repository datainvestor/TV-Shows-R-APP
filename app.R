library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(rgdal)
library(leaflet)

#glowny zbior
shows <- read.csv("finalR.csv", header=TRUE)
print(str(shows))

#tworze dataseta dla mapy
#WAZNE! dataset pobrany zosta³ ze storny https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html 
#plik zip wypokowac do folderu i ten folder umiescic w tym samym folderze co apka 

states <- readOGR("cb_2017_us_state_500k/cb_2017_us_state_500k.shp",
                  layer = "cb_2017_us_state_500k", GDAL1_integer64_policy = TRUE)
counted<-shows %>%
  group_by(Location) %>%  
  summarise(n = n() )     
colnames(counted)[1] <- "NAME"
states@data<-left_join(states@data, counted)
states@data$n[is.na(states@data$n)] <- 0


#MAIN ui

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Raleway:400,700,800');
                    h1 {
                    font-family: 'Raleway';
                    font-weight: 800;
                    text-transform: uppercase;
                    margin:0 auto;
                    }
            
                  h5 {
                  float:right;
                  }
                  "))
    ),
  navbarPage(h1("Olek/Trinh"), 
             h5("Projekt na PIWZD- Viet Anh Aleksander Trinh, 64364"),
             h2("Analiza top 50 najpopularniejszych seriali z IMDB")),
  
  tabsetPanel(
    tabPanel(h2("Scatterplot"),
             plotlyOutput("myplot"),
             hr(),
             fluidRow(
               column(3,
                      h4("Opcje filtrowania"), 
                      uiOutput("ratingOutput"),
                      br(),
                      sliderInput("voteInput", "Votes", min = 0, max = 155000, value = c(2500, 40000)),
                      br(),
                      sliderInput("lenInput", "Length", min = 0, max = 110, value = c(0, 60))
                    
               ),
               column(4, 
                      h6("Skasowac `All` przed filtrowaniem"),
                      uiOutput("seriesOutput"),
                      br(),
                      uiOutput("genreOutput"),
                      br(),
                      uiOutput("locOutput")
               ),
               column(4,offset = 0.5,
                      h4('Opcje wyboru zmiennych na osiach'),
                      selectInput('xvar', 'X', choice=c("Length", "Ep.Rating", "Votes", "Year","Series.Rating"), selected="Votes"),
                      selectInput('yvar', 'Y', choice=c("Length", "Ep.Rating", "Votes", "Year", "Series.Rating"), selected="Ep.Rating")
               ))
             
             ),
    tabPanel(h2("Table"), DT::dataTableOutput("results")),
    tabPanel(h2("Map"), leafletOutput("mymap")),
    tabPanel(h2("Barcharts"), plotOutput("mybar"),
             br(),
             selectInput('type', 'Zmienna X', choice=c("Series", "Year", "Location"), selected="Series"),
             radioButtons('var', 'Analizowana zmienna', c('Length', 'Episode Rating'), selected='Episode Rating'),
             radioButtons('calc', 'Statystyka', c("Mean", "Count", "Max"), selected="Mean")
             )
  )
)


#server

server <- function(input, output) {
  
  #active ui output
  
  output$ratingOutput <- renderUI({
    selectInput("ratingInput", "Ratings",
                c("All", as.character(sort(unique(shows$TV.Rating)))),
                selected = "All")
  })
  output$seriesOutput <- renderUI({
    selectInput("seriesInput", "Series",
                c("All", as.character(sort(unique(shows$Series)))),
                selected = "All", multiple = TRUE)
  })  
  output$genreOutput <- renderUI({
    selectInput("genreInput", "Genre",
                c("All", as.character(unique(unlist(shows[c("Genre.1", "Genre2","Genre3")])))),
                multiple = TRUE, selected="All")
  })  
  output$locOutput <- renderUI({
    selectInput("locInput", "Location",
                c("All", as.character(unique(shows$Location))),
                multiple = TRUE, selected="All")
  })  
  
  
  #filtered scatterplot
  filtered<-reactive({
    if (is.null(input$ratingInput)) {
      return(NULL)
    }
    
    if (is.null(input$seriesInput)) {
      return(NULL)
    }
    if (is.null(input$genreInput)) {
      return(NULL)
    }
    if (is.null(input$locInput)) {
      return(NULL)
    }
    
    shows %>%
      filter(Votes >= input$voteInput[1],
             Votes <= input$voteInput[2],
             Length >= input$lenInput[1],
             Length <= input$lenInput[2],
             
             if (input$ratingInput != "All") {
               TV.Rating == input$ratingInput
             } else TRUE,
             
             if (input$seriesInput != "All") {
               Series == input$seriesInput
             } else TRUE,
             
             if (input$genreInput != "All") {
               Genre.1==input$genreInput | Genre2==input$genreInput | Genre3==input$genreInput
             } else TRUE,
             
             if (input$locInput != "All") {
               Location==input$locInput
             } else TRUE
             
      )
    
  })
  
  #filtered histograms:
  barplottest <- reactive({
    if ( "Series" %in% input$type && "Count" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Series) %>% summarise(n=n()), aes(Series,n)) +
                                               geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if ( "Series" %in% input$type && "Mean" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Series) %>% summarise(mean.ep=mean(Ep.Rating)), aes(Series,mean.ep)) +
                                                geom_bar(stat="identity", fill="aquamarine3") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if( "Series" %in% input$type && "Max" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Series) %>% summarise(max.ep=max(Ep.Rating)), aes(Series,max.ep)) +
                                                                    geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                                                                    geom_text(aes(label=max.ep), vjust=1.6, color="white", size=3.5))
    
    if ( "Location" %in% input$type && "Count" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Location) %>% summarise(n=n()), aes(Location,n)) +
                                                                        geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if ( "Location" %in% input$type && "Mean" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Location) %>% summarise(mean.ep=mean(Ep.Rating)), aes(Location,mean.ep)) +
                                                                       geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if( "Location" %in% input$type && "Max" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Location) %>% summarise(max.ep=max(Ep.Rating)), aes(Location,max.ep)) +
                                                                    geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                                                                    geom_text(aes(label=max.ep), vjust=1.6, color="white", size=3.5)) 
    
    if ( "Year" %in% input$type && "Count" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Year) %>% summarise(n=n()), aes(Year,n)) +
                                                                          geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if ( "Year" %in% input$type && "Mean" %in% input$calc && 'Episode Rating' %in% input$var ) return(ggplot(shows %>% group_by(Year) %>% summarise(mean.ep=mean(Ep.Rating)), aes(Year,mean.ep)) +
                                                                         geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if( "Year" %in% input$type && "Max" %in% input$calc && 'Episode Rating' %in% input$var) return(ggplot(shows %>% group_by(Year) %>% summarise(max.ep=max(Ep.Rating)), aes(Year,max.ep)) +
                                                                      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                                                                      geom_text(aes(label=max.ep), vjust=1.6, color="white", size=3.5)) 
    
    if ( "Series" %in% input$type && "Count" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Series) %>% summarise(sumt=sum(Length)), aes(Series,sumt)) +
                                                                                                          geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if ( "Series" %in% input$type && "Mean" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Series) %>% summarise(mean.ep=mean(Length)), aes(Series,mean.ep)) +
                                                                       geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if( "Series" %in% input$type && "Max" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Series) %>% summarise(max.ep=max(Length)), aes(Series,max.ep)) +
                                                                    geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                                                                    geom_text(aes(label=max.ep), vjust=1.6, color="white", size=3.5))
    
    if ( "Location" %in% input$type && "Count" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Location) %>% summarise(sumt=sum(Length)), aes(Location,sumt)) +
                                                                                                  geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if ( "Location" %in% input$type && "Mean" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Location) %>% summarise(mean.ep=mean(Length)), aes(Location,mean.ep)) +
                                                                         geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if( "Location" %in% input$type && "Max" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Location) %>% summarise(max.ep=max(Length)), aes(Location,max.ep)) +
                                                                      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                                                                      geom_text(aes(label=max.ep), vjust=1.6, color="white", size=3.5)) 
    
    if ( "Year" %in% input$type && "Count" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Year) %>% summarise(sumt=sum(Length)), aes(Year,sumt)) +
                                                                                                  geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if ( "Year" %in% input$type && "Mean" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Year) %>% summarise(mean.ep=mean(Length)), aes(Year,mean.ep)) +
                                                                     geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    if( "Year" %in% input$type && "Max" %in% input$calc && 'Length' %in% input$var) return(ggplot(shows %>% group_by(Year) %>% summarise(max.ep=max(Length)), aes(Year,max.ep)) +
                                                                  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                                                                  geom_text(aes(label=max.ep), vjust=1.6, color="white", size=3.5)) 
    
  })
  
  
  #LEAFLET
  statesPopup <- paste0( 
    states@data$NAME,
    " -liczba odcinkow: ", 
    states@data$n )
  
  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = states@data$n)
  
  
  #map output
  output$mymap <- renderLeaflet({ leaflet(states) %>% setView(-95, 40, zoom = 3) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(n),
                highlightOptions = highlightOptions(color = "blue", weight = 1, bringToFront = TRUE), popup=statesPopup) 
  })
  
  #scatterplot output
  output$myplot <- renderPlotly({
    if (is.null(filtered())) {
      return()
    }
    
    a <- ggplot(filtered(), aes_string(x=input$xvar, y=input$yvar))  +
      geom_point(shape=1, aes(text=paste('Name:',filtered()$Ep.name, '<br>Season:', filtered()$Season, '<br>Ep number:', filtered()$Ep.Number, '<br>Series:', filtered()$Series)))
    
    ggplotly(a, tooltip = c("x", "y", "text"))
    
  })
  
  #barchart output
  output$mybar <- renderPlot({   
    dataplots = barplottest()
    print(dataplots)
  }) 
  
  #table output
  output$results <- DT::renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)