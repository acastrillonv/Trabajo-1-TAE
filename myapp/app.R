library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

datos <- readRDS("dataInicial.rds")

# See above for the definitions of ui and server
ui <- fluidPage(tabPanel("Interactive map",
                      div(class="outer",
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("ZIP explorer"),
                                        selectInput("grupo", "Seleccione Grupo", choices = list("1" = "1" ,
                                                                                                "2" = "2",
                                                                                                "3" = "3",
                                                                                                "4" = "4"),
                                                                                                selected = "1")
                                        
                                        )
                                        
                          )
                          
                         
                      )
             )
        

server <- function(input, output) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(datos) %>% addTiles() %>% setView(lng = -93.85, lat = 37.45, zoom = 4)%>%
      addCircles(lng = ~Longitud, lat = ~Latitud, weight = 1,
                 radius = ~sqrt(Costo_Anual_Estudio)*5)
    
  })
}

shinyApp(ui = ui, server = server)