library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(fdm2id)
library(flexclust)

datos <- readRDS("dataInicial.rds")
load("agrupamiento.RData")
load("valor_max.RData")
load("valor_min.RData")

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
                          
                          absolutePanel(id = "controls", class = "panel panel-info", fixed = TRUE,
                                        draggable = FALSE, top = 20, left = "auto", right = 20, bottom = "auto",
                                        width = 400, height = '90vh', style='overflow-y:scroll;',
                                        
                                        h3("Características de los grupos",style='text-align:center;font-weight: 700;'),
                                        selectInput("grupo", "Seleccione Grupo", choices = list("1" = "1" ,
                                                                                                "2" = "2",
                                                                                                "3" = "3",
                                                                                                "4" = "4"),
                                                                                                selected = "1"),
                                        htmlOutput('table'),
                                        div(tableOutput('table2'),style='display:flex;justify-content:center;'),
                                        h4('Instituciones del grupo', style='text-align:center; font-weight:700;'),
                                        div(tableOutput('table_u'),style='display:flex;justify-content:center;height: 35vh; overflow-y:scroll;')
                                        
                                        ),
                            absolutePanel(id='infoPanel', class='panel panel-info', fixed = TRUE, draggable = FALSE,
                                          top = 50, right = 'auto', left = 10, width = 350, height = '90vh', style='overflow-y:scroll;',
                                          h3("Agrupamiento de instituciones de Estados Unidos",style='text-align:center;font-weight: 700;'),
                                          h5('En esta página, te ayudamos a identificar que grupo de universidades se ajustan a las
                                            características que tu desees.
                                            Existen 4 grupos de instituciones de educación superior que se identificaron
                                            en  un base datos, por medio de un modelo estadístico de agrupamieto. Esta aplicación va dirigida a usuarios 
                                             que esten en la búsqueda de instituciones de educación superior', style='text-align:justify;'),
                                          h5('Para conocer el número del grupo ingresa los siguientes datos:'),
                                          br(),
                                          selectInput("var0", h5("Seleccione el tipo de institución"), choices = list(
                                            "Pública" = "PUBLIC" ,
                                            "Privado con ánimo de lucro" = "PRIVATE_FOR_PROFIT",
                                            "Privada sin ánimo de lucro" = "PRIVATE_NONPROFIT"),
                                          selected = "PUBLIC"),
                                          textInput("var1", h5("Costo anual de estudio"), value = 0),
                                          textInput("var2", h5("Porcentaje de estudiantes con beca PELL"), value = 0),
                                          textInput("var3", h5("Porcentaje de estudiantes mayores a 25"), value = 0),
                                          textInput("var4", h5("Ganancia de la institución por estudiante"), value = 0),
                                          br(),
                                          div(
                                            actionButton("btn", label = h5("Determinar grupo",style='color:#0d7ef3, display:none')),
                                            align='center'
                                          ),
                                          br(),
                                          h5('El grupo que más se acomoda a tus preferencias es el número: ', style='text-align:center;font-weight: 700;'),
                                          h1(uiOutput("group",align='center')),
                                          h5('A continuación, ingresa el número del grupo en el panel derecho 
                                             para poder conocer las características y universidades que hay en este grupo.', style='text-align:justify'),
                                          h5('Para saber más del funcionamiento mira el siguiente ',a('video', href='https://youtu.be/J9ok8tUQ7Zw',target="_blank")),
                                          h5('Para conocer el desarrollo de este trabajo ingresa ',a('aquí', href='https://acastrillonv.github.io/Trabajo-1-TAE/',target="_blank")),
                                          )
                                        
                          )
                          
                         
                      )
             )
        

server <- function(input, output) {
  
  
  output$group <- renderUI(
    if(input$btn){
      data_num <- data.frame(
        Costo_Anual_Estudio=as.numeric(input$var1),
        Becados_Pell=as.numeric(input$var2)/100,
        Estudiante_Mayor_25=as.numeric(input$var3)/100,
        GananciaEstudiantes=as.numeric(input$var4)
      )
      PUBLIC <- 0
      PRIVATE_FOR_PROFIT <- 0
      PRIVATE_NONPROFIT <- 0
      if(input$var0=='PUBLIC'){
        PUBLIC <- 1
      }
      else if(input$var0=='PRIVATE_FOR_PROFIT'){
        PRIVATE_FOR_PROFIT <- 1
      }
      else {
        PRIVATE_NONPROFIT <- 1
      }
      data_cat <- data.frame(
        PRIVATE_FOR_PROFIT = PRIVATE_FOR_PROFIT,
        PRIVATE_NONPROFIT = PRIVATE_NONPROFIT,
        PUBLIC = PUBLIC
      )
      data_num_norm <- scale(data_num, center = valor_min,scale = (valor_max-valor_min))
      data_modelo <-as.data.frame(cbind(data_num_norm,data_cat))
      prediccion <- as.data.frame(predict(agrupamiento, data_modelo ))
      colnames(prediccion) <- c("Grupo")
      res <- prediccion$Grupo
      res
    }
    else{
      res <- '-'
      res
    }
  )
  
  # Create the map
  output$map <- renderLeaflet({
    if(input$grupo == '1'){dataView<- grupo1
    output$table <- renderUI({
      tagList(
      h4(paste('- El precio promedio de el costo anual de estudio es: $',round(info1[1,1],2))),
      h4(paste('- El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info1[1,2]*100,2),'%')),
      h4(paste('- El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info1[1,3]*100,2), '%')),
      h4(paste('- La ganancia promedio por cada estudiante es: $',round(info1[1,4],2))),
      br(),
      h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo1)
    output$table_u <- renderTable(grupo1$Institucion)
    }
    else if(input$grupo == '2'){dataView<- grupo2
    output$table <- renderUI({
      tagList(
        h4(paste('- El precio promedio de el costo anual de estudio es: $',round(info2[1,1],2))),
        h4(paste('- El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info2[1,2]*100,2),'%')),
        h4(paste('- El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info2[1,3]*100,2), '%')),
        h4(paste('- La ganancia promedio por cada estudiante es: $',round(info2[1,4],2))),
        br(),
        h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo2)
    output$table_u <- renderTable(grupo2$Institucion)}
    else if(input$grupo == '3'){dataView<- grupo3
    output$table <- renderUI({
      tagList(
        h4(paste('- El precio promedio de el costo anual de estudio es: $',round(info3[1,1],2))),
        h4(paste('- El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info3[1,2]*100,2),'%')),
        h4(paste('- El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info3[1,3]*100,2), '%')),
        h4(paste('La ganancia promedio por cada estudiante es: $',round(info3[1,4],2))),
        br(),
        h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo3)
    output$table_u <- renderTable(grupo3$Institucion)}
    else{dataView<- grupo4
    output$table <- renderUI({
      tagList(
        h4(paste('- El precio promedio de el costo anual de estudio es: $',round(info4[1,1],2))),
        h4(paste('- El porcentaje promedio de los estudiantes con beca \'Pell\' es: ',round(info4[1,2]*100,2),'%')),
        h4(paste('- El porcentaje promedio de los estudiantes mayores de 25 años es: ',round(info4[1,3]*100,2), '%')),
        h4(paste('- La ganancia promedio por cada estudiante es: $',round(info4[1,4],2))),
        br(),
        h4('El número de instituciones en este grupo son:',style='text-align:center;')
      ) 
    })
    output$table2 <- renderTable(conteo4)
    output$table_u <- renderTable(grupo4$Institucion)}
    
    
    leaflet(dataView) %>% addProviderTiles("Stamen.TonerLite",options = providerTileOptions(noWrap = FALSE)) %>% setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addCircles(lng = ~Longitud, lat = ~Latitud, weight = 2, 
                 radius = 1000, popup = paste(dataView$Institucion," | $",dataView$Costo_Anual_Estudio))
    
  })
}

shinyApp(ui = ui, server = server)