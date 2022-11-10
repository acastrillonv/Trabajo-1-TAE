library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

datos <- readRDS("dataInicial.rds")

# Data per group
grupo1 <- subset(datos, datos$Clasificacion==1)
grupo2 <- subset(datos, datos$Clasificacion==2)
grupo3 <- subset(datos, datos$Clasificacion==3)
grupo4 <- subset(datos, datos$Clasificacion==4)

# Table
var_cate <- sapply(datos,is.factor)
data_cate <- datos[var_cate]
data_nume <- datos[!var_cate]
data_cate$Clasificacion <- datos$Clasificacion
data_nume <- subset(data_nume, select = -c(Institucion, Latitud, Longitud))
info <- as.data.frame(aggregate(.~Clasificacion, data= data_nume, mean))

info1 <- subset(info, info$Clasificacion==1)
info1 <- subset(info1, select=-c(Clasificacion))

info2 <- subset(info, info$Clasificacion==2)
info2 <- subset(info2, select=-c(Clasificacion))

info3 <- subset(info, info$Clasificacion==3)
info3 <- subset(info3, select=-c(Clasificacion))

info4 <- subset(info, info$Clasificacion==4)
info4 <- subset(info4, select=-c(Clasificacion))

# Groups info
conteo <- as.data.frame(count(data_cate,data_cate$Clasificacion, data_cate$Tipo_de_Entidad))
colnames(conteo) <- c("Clasificacion", "Tipo de Entidad", "Cantidad")

conteo1 <- subset(conteo, conteo$Clasificacion==1)
conteo1 <- subset(conteo1, select=-c(Clasificacion))

conteo2 <- subset(conteo, conteo$Clasificacion==2)
conteo2 <- subset(conteo2, select=-c(Clasificacion))

conteo3 <- subset(conteo, conteo$Clasificacion==3)
conteo3 <- subset(conteo3, select=-c(Clasificacion))

conteo4 <- subset(conteo, conteo$Clasificacion==4)
conteo4 <- subset(conteo4, select=-c(Clasificacion))

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
                                        width = 520, height = "auto",
                                        
                                        h2("Agrupamiento de instituciones de Estado Unidos",style='text-align:center;'),
                                        selectInput("grupo", "Seleccione Grupo", choices = list("1" = "1" ,
                                                                                                "2" = "2",
                                                                                                "3" = "3",
                                                                                                "4" = "4"),
                                                                                                selected = "1"),
                                        htmlOutput('table'),
                                        div(tableOutput('table2'),style='display:flex;justify-content:center;')
                                        
                                        )
                                        
                          )
                          
                         
                      )
             )
        

server <- function(input, output) {
  
  
  ## Interactive Map ###########################################
  
  
  # Create the map
  output$map <- renderLeaflet({
    if(input$grupo == '1'){dataView<- grupo1
    output$table <- renderUI({
      tagList(
      p(paste('El precio promedio de el costo anual de estudio es: $',round(info1[1,1],2))),
      br(),
      p(paste('El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info1[1,2]*100,2),'%')),
      br(),
      p(paste('El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info1[1,3]*100,2), '%')),
      br(),
      p(paste('La ganancia promedio por cada estudiante es: $',round(info1[1,4],2))),
      br(),
      h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo1)
    }
    else if(input$grupo == '2'){dataView<- grupo2
    output$table <- renderUI({
      tagList(
        p(paste('El precio promedio de el costo anual de estudio es: $',round(info2[1,1],2))),
        br(),
        p(paste('El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info2[1,2]*100,2),'%')),
        br(),
        p(paste('El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info2[1,3]*100,2), '%')),
        br(),
        p(paste('La ganancia promedio por cada estudiante es: $',round(info2[1,4],2))),
        br(),
        h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo2)}
    else if(input$grupo == '3'){dataView<- grupo3
    output$table <- renderUI({
      tagList(
        p(paste('El precio promedio de el costo anual de estudio es: $',round(info3[1,1],2))),
        br(),
        p(paste('El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info3[1,2]*100,2),'%')),
        br(),
        p(paste('El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info3[1,3]*100,2), '%')),
        br(),
        p(paste('La ganancia promedio por cada estudiante es: $',round(info3[1,4],2))),
        br(),
        h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo3)}
    else{dataView<- grupo4
    output$table <- renderUI({
      tagList(
        p(paste('El precio promedio de el costo anual de estudio es: $',round(info4[1,1],2))),
        br(),
        p(paste('El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info4[1,2]*100,2),'%')),
        br(),
        p(paste('El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info4[1,3]*100,2), '%')),
        br(),
        p(paste('La ganancia promedio por cada estudiante es: $',round(info4[1,4],2))),
        br(),
        h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo4)}
    
    
    leaflet(dataView) %>% addTiles() %>% setView(lng = -93.85, lat = 37.45, zoom = 4)%>%
      addCircles(lng = ~Longitud, lat = ~Latitud, weight = 0.5,
                 radius = 1000, popup = paste(dataView$Institucion," | $",dataView$Costo_Anual_Estudio))
    
  })
}

shinyApp(ui = ui, server = server)