library(rsconnect)
library(sendmailR)
library(readxl)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)
library(shinythemes)
library(bs4Dash)
library(lubridate)


library(png)


library(readxl)
balcarce_EMC <- read_excel("balcarce_EMC.xlsx", 
                           col_types = c("date", "text", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "text", "numeric", "text", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))

# ruta_excel <- "E:/TRABAJO/CERBAS/GrupoAgrometeorologia/App_Meteo/balcarce_EMC.xlsx"
# 
# # Leer la Hoja6 del archivo Excel
# balcarce_resumen <- read_excel(ruta_excel, sheet = "Hoja6")


datos <- balcarce_EMC 
datos <- subset(datos, select = -c(Direccion_Viento_200cm, Direccion_Viento_1000cm))
datos$Fecha <- as.Date(datos$Fecha, format = "%Y-%m-%d")
datos <- datos[order(datos$Fecha, decreasing = TRUE), ]
datos

ultima_fecha <- max(datos$Fecha)
ultimos_datos <- datos[datos$Fecha == ultima_fecha, ]
lluvia_ultimo_dia <- ultimos_datos$Precipitacion_Pluviometrica
Tmax_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Maxima
Tmin_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Minima


datos_historicos <- datos %>%
  filter(Año >= 1991 & Año <= 2020)



# enviar_correo <- function(comentario) {
#   correo_destino <- "lewczuk.nuria@inta.gob.ar"  
#   
#   # Configurar los detalles del correo electrónico
#   from <- "tu_correo@ejemplo.com"
#   to <- correo_destino
#   subject <- "Nuevo comentario recibido"
#   body <- paste("Nuevo comentario recibido:", comentario)
#   
#   # Enviar el correo electrónico
#   sendmail(from = from,
#            to = to,
#            subject = subject,
#            msg = body,
#            smtp = list(host.name = "smtp.tu_servidor_smtp.com"))
# }


# Define UI ----
ui <- dashboardPage(
  
  header = dashboardHeader(
    title = div(
      style = "font-size: 24px; font-weight: bold; text-align: center;",
      "EMC Balcarce",
      tags$style(HTML('.navbar { background-color: #2596be; }'))
    )
  ),
  
  sidebar = dashboardSidebar(
    
    width = 400,
    
    selectInput(
      inputId = "ano_selector", 
      label = "Selecciona el Año:",
      choices = c("Todos los años", unique(datos$Año)),
      selected = "Todos los años"
    ),
    
    checkboxGroupInput("mes_selector", "Selecciona los meses:",
                       choices = c("Mostrar todos los meses", 
                                   "enero", "febrero", "marzo", "abril",
                                   "mayo", "junio", "julio", "agosto", 
                                   "septiembre", "octubre", "noviembre", "diciembre"),
                       selected = "Mostrar todos los meses"),
    br(),
    tags$p(
          strong("Nuestras Redes sociales"),
          br(),
          tags$a(
            icon("instagram"), "Instagram", href= "https://www.instagram.com/agromet_inta.balcarce/#"),
          br(),
          tags$a(
            icon("twitter"), "Twitter", href= "https://twitter.com/agrometbalcarce"),
          br(),
          tags$p(
            strong("Para comunicarse con el grupo "),
            tags$h6(
              "Dra. Nuria Lewczuk : lewczuk.nuria@inta.gob.ar"),
            tags$h6(
              "Dra. Laura Echarte : echarte.laura@inta.gob.ar")
            ),
          br(),
          tags$img(src = "Logo_INTA_Balcarce.png",
                   height = "40px",
                   width = "220px"
          ))
  ),
  
  
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box {height: 80px; 
                    text-align:center;
                    display: flex; 
                    flex-direction: column; 
                    justify-content: center;
        }
        .fixed-footer {
                      position: fixed;
                      bottom: 0;
                      left: 50px;
                      width: 100%;
                      padding: 10px 0;
        }
        
        "))
    ),
    
    tabsetPanel(
      tabPanel(
        "Gráficos",
        fluidRow(
          infoBoxOutput(width = 2, "value5"),
          infoBoxOutput(width = 2, "value1"),
          infoBoxOutput(width = 2, "value2"),
          infoBoxOutput(width = 2, "value3"),
          infoBoxOutput(width = 2, "value4")
        ),
        br(),
        fluidRow(
          infoBoxOutput(width = 4, "precipitation_info_box"),
          infoBoxOutput(width = 4, "tempMax_info_box"),
          infoBoxOutput(width = 4, "tempMin_info_box")
        ),
        br(),
        fluidRow( 
          box(
            title = "Precipitaciones diarias (mm)"
            ,status = "gray"
            ,solidHeader = TRUE 
            ,collapsible = TRUE
            ,plotlyOutput("grafico_lluvia", height = "300px")
          ),
          box(
            title = "Precipitaciones y ETo mensuales (mm)"
            ,status = "gray"
            ,solidHeader = TRUE 
            ,collapsible = TRUE
            ,plotlyOutput("grafico_lluvia_etp_acum", height = "300px")
          ),
          box(
            title = "Temperaturas diarias (ºC)"
            ,status = "gray"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotlyOutput("grafico_temperatura", height = "300px")
          ),
          box(
            title = "Número de días con heladas"
            ,status = "gray"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotlyOutput("grafico_heladas", height = "300px")
          ) 
        )
      ),
      tabPanel(
        "Mapas",
        fluidRow(
            column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; margin-bottom: 10px; margin-top: 20px; ",  
              box(
                title = "Consumo de agua",
                status = "navy",
                solidHeader = FALSE,
                width = 10,  
            div(
              style = "display: flex; align-items: center;",
              div(
                style = "flex: 1; text-align: center;",
                tags$img(
                  src = "agua.jpg",
                  width = 700,
                  alt = "Consumo de agua"
                )
              ),
            div(
              style = "margin-top: 20px; text-align: left; padding: 10px; border: 2px solid #ddd; background-color: #f9f9f9;",
              "El consumo de agua o evapotranspiración real (ETR) es la
                cantidad de agua que es transpirada por la cubierta vegetal y aquella que
                es perdida desde la superficie del suelo por evaporación.
               El consumo de agua puede ser utilizado para detectar la ocurrencia
                de deficiencias de agua, cuando su valor no alcanza el requerido por el
                cultivo."
            )
              ),
          tags$figcaption(
            "Evapotranspiración real máxima (en el periodo de 10 días) expresada en
              mm/día estimada mediante el uso de imágenes del sensor VIIRS del satélite
              Suomi-NPP con una resolución espacial de 500 metros. Elaborado por Instituto de
              Clima y Agua, INTA Castelar. Recorte: Patricio Oricchio."
          )
            )
          )
        ),

          br(),
          
        column(
          width = 12,
          div(
            style = "display: flex; justify-content: center; margin-bottom: 10px; margin-top: 20px; ",  
            box(
              title = "% de agua útil",
              status = "navy",
              solidHeader = FALSE,
              width = 10,  
              div(
                style = "display: flex; align-items: center;",
                div(
                  style = "flex: 1; text-align: center;",
                  tags$img(
                    src = "agua_util.jpg",
                    width = 700,
                    alt = "agua útil"
                  )
                ),
                div(
                  style = "margin-top: 20px; text-align: left; padding: 10px; border: 2px solid #ddd; background-color: #f9f9f9;",
                  "El porcentaje de agua útil en el suelo (es decir, aquella porción de agua
                    que puede ser extraída por las plantas) puede ser estimado a través
                    de un balance de agua; donde se considera información del suelo, el
                    aporte de agua por lluvias y el consumo de agua de la cubierta
                    vegetal."
                )
              ),
              tags$figcaption(
                "Porcentaje de agua en el suelo.Resolución espacial: 500 m. 
                  Mapa elaborado por Instituto de Clima y Agua, INTA Castelar. Recorte: Lucas Gusmerotti."
              )
            )
          )
        )
      )
    ),
      tabPanel(
        "Pronósticos",
        fluidRow( 
          box(title = "Pronóstico semanal del 15 al 20 de agosto 2024"
              ,status = "navy"
              ,solidHeader = FALSE
              ,div(
                style = "text-align: center;",
                tags$img(
                  src = "pronostico_lluvia.png",
                  width = 600,
                  alt = "Pronóstico semanal de lluvia"
                )
              )
          ),
          box(title = "Pronóstico trimestral - AGOSTO / SEPTIEMBRE / OCTUBRE"
              ,status = "navy"
              ,solidHeader = FALSE
              ,div(
                style = "text-align: center;",
                tags$img(
                  src = "pronostico_tri.png",
                  width = 600,
                  alt = "Pronóstico trimestral"
                )
              )
          ),
        )
      ),
      tabPanel(
        "Informes",
        fluidRow(
          column(
            width = 3,
          div(
            style = "margin-left: 100px; margin-top: 50px;",  
            tags$img(
              src = "IMA.jpg",
              width = 300,
              height = 460,
              alt = "Informe Mensual Agropecuario"
              ),
            tags$br(),
            tags$a(
              icon("arrow-right"), "Descarga el informe completo aquí", href= "https://bit.ly/IMA-JUL24")
            )),
          column(
            width = 3,
          div(
            style = "margin-left: 100px; margin-top: 50px;",  
            tags$img(
              src = "chicharrita.jpg",
              width = 300,
              alt = "Achaparramiento del Maíz"
            ),
            tags$br(),
            tags$a(
              icon("arrow-right"), "Descarga el informe completo aquí", href= "https://www.argentina.gob.ar/sites/default/files/2018/09/el_achaparramiento_del_maiz_y_las_decisiones_agricolas_en_argentina_mesatecnicanacional_inta.pdf")
          ),
        )
      )
    ),
      tabPanel(
        "Datos disponibles",
        dataTableOutput("datos"),
        br(),
        "Selecciona el periodo:",
        fluidRow(
          column(2,
                 dateInput("fecha_inicio", "Fecha de Inicio", value = NULL, format = "dd/mm/yyyy")),
          column(2,
                 dateInput("fecha_fin", "Fecha de Fin", value = NULL, format = "dd/mm/yyyy"))
        ),
        selectInput(
          "variables", 
          "Seleccionar Variables:",
          choices = c("Fecha", "Temperatura_Abrigo_150cm",
                      "Temperatura_Abrigo_150cm_Maxima", "Temperatura_Abrigo_150cm_Minima",
                      "Temperatura_Intemperie_5cm_Minima", "Temperatura_Intemperie_50cm_Minima",	
                      "Temperatura_Suelo_5cm_Media", "Temperatura_Suelo_10cm_Media",
                      "Temperatura_Inte_5cm", "Temperatura_Intemperie_150cm_Minima",
                      "Humedad_Suelo", "Precipitacion_Pluviometrica", "Granizo",
                      "Nieve", "Heliofania_Efectiva", "Heliofania_Relativa", "Tesion_Vapor_Media", 
                      "Humedad_Media", "Humedad_Media_8_14_20", "Rocio_Medio",
                      "Duracion_Follaje_Mojado", "Velocidad_Viento_200cm_Media",
                      "Direccion_Viento_200cm", "Velocidad_Viento_1000cm_Media",
                      "Direccion_Viento_1000cm", "Velocidad_Viento_Maxima", "Presion_Media",
                      "Radiacion_Global", "Radiacion_Neta", "Evaporacion_Tanque",	
                      "Evapotranspiracion_Potencial", "Profundidad_Napa", "Horas_Frio",	
                      "Unidad_Frio"
          ),
          selected = "Fecha",
          multiple = TRUE
        ),
        downloadButton("Datos_meteo_Balcarce", "Descargar .csv")
      )
  )))
  


# Define server logic ----
server <- function(input, output, session) {
  
  # Cálculo del promedio de precipitaciones acumuladas anuales para el periodo 1991-2020
  promedio_historico <- mean(
    aggregate(Precipitacion_Pluviometrica ~ Año, data = datos_historicos, FUN = sum)$Precipitacion_Pluviometrica
  )
  
  # Cálculo del promedio de temp max anuales para el periodo 1991-2020
  promedio_historico_ttmax <- mean(
    aggregate(Temperatura_Abrigo_150cm_Maxima ~ Año, data = datos_historicos, FUN = mean)$Temperatura_Abrigo_150cm_Maxima
  )
  
  # Cálculo del promedio de temp min anuales para el periodo 1991-2020
  promedio_historico_ttmin <- mean(
    aggregate(Temperatura_Abrigo_150cm_Minima ~ Año, data = datos_historicos, FUN = mean)$Temperatura_Abrigo_150cm_Minima
  )
  
  
  # Calculating current year cumulative precipitation
  current_year <- max(datos$Año)
  pp_acum <- sum(subset(datos, Año == current_year)$Precipitacion_Pluviometrica)
  
  # Calculating current temp max
  ttmax_anual <- mean(subset(datos, Año == current_year)$Temperatura_Abrigo_150cm_Maxima)
  
  # Calculating current temp min
  ttmin_anual <- mean(subset(datos, Año == current_year)$Temperatura_Abrigo_150cm_Minima, na.rm = TRUE)
  
  
  
  ultima_fecha <- max(datos$Fecha)
  ultimos_datos <- datos[datos$Fecha == ultima_fecha, ]
  lluvia_ultimo_dia <- ultimos_datos$Precipitacion_Pluviometrica
  Tmax_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Maxima
  Tmin_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Minima
  
  
 datasetInput <- reactive({
    if (input$ano_selector == "Todos los años") {
      datos_filtrados <- datos
    } else {
      datos_filtrados <- subset(datos, Año == input$ano_selector)
    }
    
    # Filtrar los datos por los meses seleccionados si no se elige "Mostrar todos los meses"
    if (!"Mostrar todos los meses" %in% input$mes_selector) {
      meses_seleccionados <- input$mes_selector
      datos_filtrados <- subset(datos_filtrados, Mes %in% meses_seleccionados)
    }
    
    return(datos_filtrados)
  })
 
   output$value1 <- renderInfoBox({
    infoBox(
      title = div(p("Ultima fecha", 
                    style = "text-align: center; font-size: 14px;"), 
                  style = "margin-bottom: 6px;"),  
      value = div(format(ultima_fecha, "%d/%m/%Y"), 
                  style = "text-align: center; font-size: 18px;"),
      icon = icon("calendar"),
      color = "orange",
      fill = TRUE
    )
  })
  
  output$value2 <- renderInfoBox({
    infoBox(
      title = div(p("Lluvia", 
                    style = "text-align: center;font-size: 14px;"), 
                  style = "margin-bottom: 6px;"),  
      value = div(round(lluvia_ultimo_dia, 1), 
                  style = "text-align: center; font-size: 18px;"),
      icon = icon("tint"),
      color = "info",
      fill = TRUE
    )
  })
  
  output$value3 <- renderInfoBox({
    infoBox(
      title = div(p("Temperatura Máxima", 
                    style = "text-align: center;font-size: 14px;"), 
                  style = "margin-bottom: 6px;"),  
      value = div(round(Tmax_ultimo_dia, 1), 
                  style = "text-align: center; font-size: 18px;"),  
      icon = icon("sun"),
      color = "danger",
      fill = TRUE
    )
  })
  
  output$value4 <- renderInfoBox({
    infoBox(
      title = div(p("Temperatura Mínima", 
                    style = "text-align: center;font-size: 14px;"), 
                  style = "margin-bottom: 6px;"),  
      value = div(round(Tmin_ultimo_dia, 1), 
                  style = "text-align: center; font-size: 18px;"),
      icon = icon("snowflake"),
      color = "warning",
      fill = TRUE
    )
  })
  
  output$precipitation_info_box <- renderInfoBox({
    infoBox(
      title = "Precipitaciones Acumuladas",
      value = paste("Acumulado Año ", current_year, ": ", round(pp_acum, 0), "mm"),
      subtitle = paste("Promedio Histórico anual (1991-2020): ", round(promedio_historico, 0), "mm"),
      icon = icon("tint"),
      color = "info"
      
    )
  })
  
  output$tempMax_info_box <- renderInfoBox({
    infoBox(
      title = "Temperaturas Máximas",
      value = paste("Promedio Año ", current_year, ": ", round(ttmax_anual, 0), "ºC"),
      subtitle = paste("Promedio Histórico anual (1991-2020): ", round(promedio_historico_ttmax, 0), "ºC"),
      icon = icon("sun"),
      color = "danger"
    )
  })
  
  output$tempMin_info_box <- renderInfoBox({
    infoBox(
      title = "Temperaturas Mínimas",
      value = paste("Promedio Año ", current_year, ": ", round(ttmin_anual, 0),"ºC"),
      subtitle = paste("Promedio Histórico anual (1991-2020): ", round(promedio_historico_ttmin, 0), "ºC"), 
      icon = icon("snowflake"),
      color = "warning"
    )
  })
  
  
  output$grafico_lluvia <- renderPlotly({
    gg <- ggplot(datasetInput(), aes(x = Fecha)) +
      geom_bar(aes(y = Precipitacion_Pluviometrica), stat = "identity", fill = "#003459", alpha = 0.8) +
      labs(x = "", y = "mm") +
      ggtitle("") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),  # Elimina las líneas de la grilla
        panel.grid.minor = element_blank(),  # Elimina las líneas de la grilla
        axis.ticks = element_blank(),         # Elimina los ticks del eje
        axis.line = element_line(color = "black"))
    
    ggplotly(gg)
  })
  
  output$grafico_lluvia_etp_acum <- renderPlotly({
    dataset_acumulado <- datasetInput() %>%
      mutate(Mes = month(Fecha, label = TRUE)) %>%
      group_by(Mes) %>%
      summarise(Precipitacion_Acumulada = sum(Precipitacion_Pluviometrica, na.rm = TRUE),
                Evapotranspiracion_Acumulada = round(sum(Evapotranspiracion_Potencial, na.rm = TRUE)), 2)
    
    
    # Crear el gráfico de barras superpuestas
    acum <- ggplot(dataset_acumulado, aes(x = Mes)) +
      geom_bar(aes(y = Precipitacion_Acumulada), stat = "identity", fill = "#007EA7", color = "#003459",alpha = 0.5) +
      geom_bar(aes(y = Evapotranspiracion_Acumulada), stat = "identity", fill = "#BF4342", color = "#8C1C13", alpha = 0.5) +
      scale_fill_manual(values = c("Precipitaciones acumuladas" = "#007EA7", "Evapotranspiracion Potencial" = "#BF4342"), name = "") +
      labs(x = "", y = "Valor acumulado mensual (mm)", fill = "") +  
      ggtitle("") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.grid.major = element_blank(),  # Elimina las líneas de la grilla
            panel.grid.minor = element_blank(),  # Elimina las líneas de la grilla
            axis.ticks = element_blank(),         # Elimina los ticks del eje
            axis.line = element_line(color = "black"))
    
    ggplotly(acum) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = 1.2))  
  })
  
  output$grafico_temperatura <- renderPlotly({
    tt <- ggplot(datasetInput(), aes(x = Fecha)) +
      geom_line(aes(y = Temperatura_Abrigo_150cm, color = "Temperatura Media"), na.rm = TRUE, show.legend = FALSE) +
      geom_line(aes(y = Temperatura_Abrigo_150cm_Maxima, color = "Temperatura Máxima"), na.rm = TRUE, show.legend = FALSE) +
      geom_line(aes(y = Temperatura_Abrigo_150cm_Minima, color = "Temperatura Mínima"), na.rm = TRUE, show.legend = FALSE) +
      labs(title = "",
           x = "",
           y = "°C") +
      scale_color_manual(values = c("Temperatura Media" = "#136F63", 
                                    "Temperatura Máxima" = "#D00000",
                                    "Temperatura Mínima" = "#FFBA08")) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),  # Elimina las líneas de la grilla
        panel.grid.minor = element_blank(),  # Elimina las líneas de la grilla
        axis.ticks = element_blank(),         # Elimina los ticks del eje
        axis.line = element_line(color = "black"))
    
    ggplotly(tt) %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = 1.2))
  })
  
  
  output$grafico_heladas <- renderPlotly({
    
    promedio_heladas <- datasetInput() %>%
      filter(Temperatura_Abrigo_150cm_Minima < 3) %>%
      group_by(Mes) %>%
      summarise(Dias_Temperatura_Minima_Menor_3C = n()) %>%
      mutate(Mes = factor(substr(Mes, 1, 3), levels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"),
                          ordered = TRUE)) # Aquí se establecen los niveles ordenados
    
    
    hh <- ggplot(promedio_heladas, aes(x = Mes, y = Dias_Temperatura_Minima_Menor_3C)) +
      geom_bar(stat = "identity", fill = "#FFBA08", color = "#FF9F1C") +  
      labs(title = "",
           x = "",
           y = "Número de días con\nTemperatura mínimas < 3ºC") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.grid.major = element_blank(),  # Elimina las líneas de la grilla
            panel.grid.minor = element_blank(),  # Elimina las líneas de la grilla
            axis.ticks = element_blank(),         # Elimina los ticks del eje
            axis.line = element_line(color = "black")) 
    
    ggplotly(hh) %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = 1.2))
  })
  
  output$datos <- renderDT (
    datos,
    rownames = FALSE,
    options = list(
      lengthchange = TRUE,
      scrollX = TRUE,
      scrollY = "400px",  
      scrollCollapse = TRUE
    )
  )
  
  output$Datos_meteo_Balcarce <- downloadHandler(
    filename = function() {
      paste("Datos_metereologicos_balcarce", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Filtra los datos según las variables seleccionadas
      datos_filtrados3 <- datos
      
      # Filtrar por el rango de fechas seleccionado si se han proporcionado ambas fechas
      if (!is.null(input$fecha_inicio) && !is.null(input$fecha_fin)) {
        datos_filtrados3 <- subset(datos_filtrados3, Fecha >= input$fecha_inicio & Fecha <= input$fecha_fin)
      } else if (!is.null(input$fecha_inicio)) {
        datos_filtrados3 <- subset(datos_filtrados3, Fecha >= input$fecha_inicio)
      } else if (!is.null(input$fecha_fin)) {
        datos_filtrados3 <- subset(datos_filtrados3, Fecha <= input$fecha_fin)
      }
      
      # Seleccionar solo las variables elegidas por el usuario
      datos_filtrados3 <- datos_filtrados3[, c("Fecha", input$variables), drop = FALSE]
      
      # Guardar el archivo CSV
      write.csv(datos_filtrados3, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$enviar, {
    comentario <- input$text
    if (comentario != "Aquí...") {
      enviar_correo(comentario)
      showNotification("Mensaje enviado", duration = 5)
    }
  })

  
}

 
  


# Run the app ----
shinyApp(ui = ui, server = server)
# 
# rsconnect::setAccountInfo(name='8js2kq-nuria-lewczuk',
#                             token='111A9791794261A82E719E515597DD66',
#                             secret='m+wC8DYne2Mr69yhsLI/2i4QF7Zy5PQUFPIe5Xkr')
# rsconnect::deployApp(appDir = "E:/TRABAJO/CERBAS/GrupoAgrometeorologia/App_Meteo", appPrimaryDoc = "ui.R",
#                      appName = "MiAppMeteo", account = '8js2kq-nuria-lewczuk', server = 'shinyapps.io')