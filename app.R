library(rsconnect)
library(sendmailR)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(tidyr) 
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
# datos_historicos

# promedios_diarios_historicos <- datos_historicos %>%
#   group_by(month = month(Fecha), day = day(Fecha)) %>%  
#   summarise(
#     Promedio_Temperatura = mean(Temperatura_Abrigo_150cm, na.rm = TRUE),
#     Promedio_Precipitacion = mean(Precipitacion_Pluviometrica, na.rm = TRUE),
#     Promedio_ETP = mean(Evapotranspiracion_Potencial, na.rm = TRUE)
#   ) %>%
#   ungroup()
# promedios_diarios_historicos



#################

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
      choices = unique(datos$Año),
      selected = "2024"
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
            title = "Precipitaciones acumuladas mensuales (mm)"
            ,status = "gray"
            ,solidHeader = TRUE 
            ,collapsible = TRUE
            ,plotlyOutput("grafico_lluvia", height = "300px")
          ),
          box(
            title = "Precipitaciones y ETo acumuladas mensuales (mm)"
            ,status = "gray"
            ,solidHeader = TRUE 
            ,collapsible = TRUE
            ,plotlyOutput("grafico_lluvia_etp_acum", height = "300px")
          ),
          box(
            title = "Temperaturas medias mensuales (ºC)"
            ,status = "gray"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotlyOutput("grafico_temperatura", height = "300px")
          ),
          box(
            title = "Número de días mensuales con heladas"
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
        "Balance de agua",
        br(),
        h5(strong("Cálculo de balance de agua para MAÍZ de ciclo largo")),
        h6(HTML("Usted puede estimar el balance de agua diario de su campo.
               <br>
               Puede ingresar los datos solicitados en los recuadros o bien utilizar los valores por default.")),
        
        br(),
        
        fluidRow(
          # Fecha de siembra 
          column(12,
                 dateInput("fecha_siembra",
                           label = strong("Ingrese la fecha de siembra:"),
                           value = "2024-01-01")
          )
        ),
        fluidRow(
          # Fecha de siembra 
          column(6,
                 div(style = "background-color: #f2f2f2; padding: 15px; border-radius: 10px;",
                 h4(strong("Datos de manejo"))
                 )
          ),
          column(3,
                 div(style = "background-color: #e6ffe6; padding: 15px; border-radius: 10px;",
                 h4(strong("Datos de cultivo"))
                 )
          )
        ),

          fluidRow(
            
            # "Datos de manejo"
            column(3,
                   div(style = "background-color: #f2f2f2; padding-left: 15px; padding-right: 5px; padding-bottom: 15px; border-radius: 10px;",
                       numericInput("profundidad",  
                                    label = strong("Profundidad máxima (cm)"),
                                    value = 100),
                       br(),
                       br(),
                       numericInput("capacidad_campo",  
                                    label = strong(HTML("Capacidad de Campo (mm/cm)
                                                        <br><small>
                                                        (Límite Máximo de almacenamiento de agua)</small>")), 
                                    value = 3.70),
                       textOutput("almacenamiento_maximo")
                   )
            ),
            column(3,
                   div(style = "background-color: #f2f2f2; padding-left: 5px; padding-bottom: 15px; border-radius: 10px;",
                       numericInput("fraccion_min",  
                                    label = strong("Fracción de almacenamiento mínimo respecto del máximo (0 - 1)"), 
                                    value = 0.55),
                       textOutput("almacenamiento_minimo"),
                       textOutput("agua_util_total"),
                       br(),
                       numericInput("fraccion_inicial",  
                                    label = strong("Fracción inicial de agua útil (0 - 1)"), 
                                    value = 0.50)
                   )
            ),
            
            # "Datos de cultivo"
            column(3,
                   div(style = "background-color: #e6ffe6; padding: 15px; border-radius: 10px;",
                       numericInput("umbral_et",  
                                    label = strong(HTML("Umbral de fracción de agua útil 
                                                        <br><small>
                                                        (Debajo del cual se reduce la evapotranspiración)</small>")), 
                                    value = 0.8),
                       textOutput("disminucion_et")
                   )
            )
          ),
        br(),
          
       fluidRow( 
          box(
            title = "Balance de agua",
            status = "gray-dark",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("consumo_agua", height = "300px")
          ),
          box(
            title = "Deficiencias hídricas y precipitaciones",
            status = "gray-dark",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("deficit_agua", height = "300px")
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
                  style = "max-width: 80%; height: auto;", 
                  
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
                  style = "max-width: 80%; height: auto;",
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
  
  ### InfoBox Outputs
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
  
  ## Gráficos ##
  
  #### lluvias mensuales ####
  output$grafico_lluvia <- renderPlotly({
    # 
    dataset_acumulado <- datasetInput() %>%
      mutate(Mes = month(Fecha, label = TRUE)) %>%
      group_by(Mes) %>%
      summarise(Precipitacion_Acumulada = round(sum(Precipitacion_Pluviometrica, 
                                                    na.rm = TRUE), 1)) %>%
      ungroup()
    
    # 
    historical_precipitation <- datos_historicos %>%
      mutate(Mes = month(Fecha, label = TRUE)) %>%
      group_by(Año, Mes) %>%
      summarise(Precipitacion_Historica = sum(Precipitacion_Pluviometrica, 
                                              na.rm = TRUE), 
                .groups = 'drop') 
    
    historical_precipitation_mensual <- historical_precipitation %>%
      group_by(Mes) %>%
      summarise(Precipitacion_Historica_Mensual = round(mean(Precipitacion_Historica, 
                                                             na.rm = TRUE), 1),
                .groups = 'drop')
    
    # 
    dataset_completo <- dataset_acumulado %>%
      left_join(historical_precipitation_mensual, by = "Mes")
    
    # 
    anio_seleccionado_label <- input$ano_selector

    # 
    dataset_completo_long <- dataset_completo %>%
      pivot_longer(cols = c(Precipitacion_Historica_Mensual, 
                            Precipitacion_Acumulada),
                   names_to = "Tipo_Precipitacion",
                   values_to = "Precipitacion") %>%
      mutate(Tipo_Precipitacion = factor(Tipo_Precipitacion,
                                         levels = c("Precipitacion_Historica_Mensual", 
                                                    "Precipitacion_Acumulada"),
                                         labels = c("Histórico (1991 - 2020)", 
                                                    paste("Año", anio_seleccionado_label))))
    
    ll <- ggplot(dataset_completo, aes(x = Mes)) +
      geom_bar(aes(y = Precipitacion_Historica_Mensual), 
               stat = "identity", 
               fill = "#6C757D", 
               color = "#495057", 
               alpha = 0.5) +
      geom_bar(aes(y = Precipitacion_Acumulada), 
               stat = "identity", 
               fill = "#007EA7", 
               color = "#003459", 
               alpha = 0.5) +
      labs(x = "", y = "Precipitación acumulada \nmensual (mm)", fill = "") +
      ggtitle("") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_line(color = "black"))
    
    ggplotly(ll) %>%
      layout(legend = list(orientation = "h", 
                           x = 0.3, 
                           y = 1.2))    
  })
  
  ##### lluvia y ETP ####
  output$grafico_lluvia_etp_acum <- renderPlotly({
    dataset_acumulado <- datasetInput() %>%
      mutate(Mes = month(Fecha, label = TRUE)) %>%
      group_by(Mes) %>%
      summarise(Precipitacion_Acumulada = round(sum(Precipitacion_Pluviometrica, 
                                              na.rm = TRUE), 1),
                Evapotranspiracion_Acumulada = round(sum(Evapotranspiracion_Potencial, 
                                                         na.rm = TRUE)), 1)
    
    
    #
    acum <- ggplot(dataset_acumulado, aes(x = Mes)) +
      geom_bar(aes(y = Precipitacion_Acumulada), 
               stat = "identity", 
               fill = "#007EA7", 
               color = "#003459", 
               alpha = 0.5) +
      geom_bar(aes(y = Evapotranspiracion_Acumulada), 
               stat = "identity", 
               fill = "#BF4342", 
               color = "#8C1C13", 
               alpha = 0.5) +
      scale_fill_manual(values = c("Precipitaciones acumuladas" = "#007EA7", 
                                   "Evapotranspiracion Potencial" = "#BF4342"), 
                        name = "") +
      labs(x = "", y = "Acumulado mensual (mm)", 
           fill = "") +  
      ggtitle("") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, 
                                       hjust = 1),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),  
            axis.ticks = element_blank(),         
            axis.line = element_line(color = "black"))
    
    ggplotly(acum) %>%
      layout(legend = list(orientation = "h", 
                           x = 0.1, 
                           y = 1.2))  
  })
  
  #### temp mensuales ####
  output$grafico_temperatura <- renderPlotly({
    
    # Datos de temperaturas para el año seleccionado
    temperaturas_mensuales <- datasetInput() %>%
      mutate(Mes = month(Fecha, 
                         label = TRUE)) %>%
      group_by(Mes) %>%
      summarise(Temperatura_Maxima = round(mean(Temperatura_Abrigo_150cm_Maxima, 
                                                na.rm = TRUE), 1),
                Temperatura_Minima = round(mean(Temperatura_Abrigo_150cm_Minima, 
                                                na.rm = TRUE), 1))
    
    # Datos históricos de temperaturas
    historico_temperaturas_mensual <- datos_historicos %>%
      mutate(Mes = month(Fecha, 
                         label = TRUE)) %>%
      group_by(Mes) %>%
      summarise(Temp_Max_Historica = round(mean(Temperatura_Abrigo_150cm_Maxima, 
                                                na.rm = TRUE), 1),
                Temp_Min_Historica = round(mean(Temperatura_Abrigo_150cm_Minima, 
                                                na.rm = TRUE), 1)) %>%
      ungroup()
    
    
    #
    dataset_completo_temperatura <- temperaturas_mensuales %>%
      left_join(historico_temperaturas_mensual, 
                by = "Mes")
    
    dataset_completo_temperatura_long <- dataset_completo_temperatura %>%
      pivot_longer(cols = c(Temp_Max_Historica, Temp_Min_Historica, 
                            Temperatura_Maxima, Temperatura_Minima),
                   names_to = "Temperatura",
                   values_to = "temperatura") %>%
      mutate(Temperatura = factor(Temperatura,
                                       levels = c("Temperatura_Maxima",
                                                  "Temp_Max_Historica", 
                                                  "Temperatura_Minima",
                                                  "Temp_Min_Historica"),                                                  
                                       labels = c("Máxima Año Seleccionado",
                                                  "Máxima Histórica (1991 - 2020)",
                                                  "Mínima Año Seleccionado",
                                                  "Mínima Histórica (1991 - 2020)")))
  
  
    temp_plot <- ggplot(dataset_completo_temperatura_long, aes(x = Mes, 
                                                               y = temperatura, 
                                                               color = Temperatura, 
                                                               group = Temperatura)) +
      geom_line(size = 1) +
      geom_point(size = 1) +
      scale_color_manual(values = c("Máxima Año Seleccionado" = "#D00000",
                                    "Máxima Histórica (1991 - 2020)" = "#FCB9B2",
                                    "Mínima Año Seleccionado" = "#FFBA08",
                                    "Mínima Histórica (1991 - 2020)" = "#EDDEA4"
                                    )) +
      labs(x = "", y = "Temperatura media mensual (ºC)", 
           color = "") +
      ggtitle("") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, 
                                       hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_line(color = "black"))
    
    ggplotly(temp_plot) %>%
      layout(legend = list(orientation = "v", 
                           x = 0.4, 
                           y = 1.2))
  })
  
  
  output$grafico_heladas <- renderPlotly({
    
    promedio_heladas <- datasetInput() %>%
      filter(Temperatura_Abrigo_150cm_Minima < 3) %>%
      group_by(Mes) %>%
      summarise(Dias_Temperatura_Minima_Menor_3C = n()) %>%
      mutate(Mes = factor(substr(Mes, 1, 3), levels = c("ene", "feb", "mar", "abr", 
                                                        "may", "jun", "jul", "ago", 
                                                        "sep", "oct", "nov", "dic"),
                          ordered = TRUE)) 
    
    
    hh <- ggplot(promedio_heladas, aes(x = Mes, 
                                       y = Dias_Temperatura_Minima_Menor_3C)) +
      geom_bar(stat = "identity", fill = "#FFBA08", color = "#FF9F1C") +  
      labs(title = "",
           x = "",
           y = "Número de días con\nTemperatura mínimas < 3ºC") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),  
            axis.ticks = element_blank(),         
            axis.line = element_line(color = "black")) 
    
    ggplotly(hh) %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = 1.2))
  })
  
  ##### Balance de agua ########
  
  # Calcular Almacenamiento máximo de agua
  almacenamiento_maximo <- reactive({
    input$capacidad_campo * input$profundidad
  })
  
  # Calcular Almacenamiento mínimo de agua
  almacenamiento_minimo <- reactive({
    almacenamiento_maximo() * input$fraccion_min
  })
  
  # Calcular Agua útil total
  agua_util_total <- reactive({
    almacenamiento_maximo() - almacenamiento_minimo()
  })
  
  # Calcular Disminución de ET
  disminucion_et <- reactive({
    1 / input$umbral_et
  })
  
   
  output$almacenamiento_maximo <- renderText({
    paste("Almacenamiento máximo de agua (mm):", round(almacenamiento_maximo(), 2))
  })
  
  output$almacenamiento_minimo <- renderText({
    paste("Almacenamiento mínimo de agua (mm):", round(almacenamiento_minimo(), 2))
  })
  
  output$agua_util_total <- renderText({
    paste("Agua útil total (mm):", round(agua_util_total(), 2))
  })
  
  output$disminucion_et <- renderText({
    paste("Disminución de evapotranspiración:", round(disminucion_et(), 2))
  })
  
  
  balance_agua <- reactive({
    fecha_siembra <- as.Date(input$fecha_siembra)
    # fecha_siembra <- as.Date("2024-01-01") 
    fecha_dia_18 <- fecha_siembra + 17
    
    datos_filtrados <- datos %>%
      filter(Fecha >= fecha_siembra) %>%
      select(Fecha, Temperatura_Abrigo_150cm, Precipitacion_Pluviometrica, Evapotranspiracion_Potencial)
    
    datos_filtrados <- datos_filtrados %>%
      mutate(Dia_Mes = format(Fecha, "%m-%d"))
    
    datos_historicos_avg <- datos_historicos %>%
      mutate(Dia_Mes = format(Fecha, "%m-%d")) %>%
      group_by(Dia_Mes) %>%
      summarise(
        Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE),
        Evapotranspiracion_media = mean(Evapotranspiracion_Potencial, na.rm = TRUE),
        .groups = "drop"
      ) 
    
    fraccion_inicial <- input$fraccion_inicial
    agua_util_total_val <- agua_util_total()
    disminucion_et_val <- disminucion_et()
    
    # fraccion_inicial <- 0.5
    # agua_util_total_val <- 166.5
    # disminucion_et_val <- 1.25
    
    
    datos_filtrados <- datos_filtrados %>%
      left_join(datos_historicos_avg, by = "Dia_Mes") %>%
      arrange(Fecha) %>%
      mutate(
        Temperatura_Abrigo_150cm = coalesce(Temperatura_Abrigo_150cm, Temperatura_media), 
        Evapotranspiracion_Potencial = coalesce(Evapotranspiracion_Potencial, Evapotranspiracion_media),
        
        
        TTB = if_else(Fecha >= fecha_dia_18, 
                      if_else(Temperatura_Abrigo_150cm - 8 < 0, 
                                                    0, 
                                                    Temperatura_Abrigo_150cm - 8), NA_real_),
        GD_acum = case_when(
          Fecha >= fecha_dia_18 ~ cumsum(replace_na(TTB, 0)),
          TRUE ~ 0),
        Ttrelativo = if_else(Fecha >= fecha_dia_18,  
                             GD_acum / 1790, 
                             0),
        
        Kc = if_else(Ttrelativo > 0.16, 
                     2.988041 * Ttrelativo^4 - 4.052411 * Ttrelativo^3 - 3.999317 * Ttrelativo^2 + 6.015032 * Ttrelativo - 0.390632, 
                     0.4),
        ETM = Kc * Evapotranspiracion_Potencial,
        
        Fr_agua_util = NA_real_,
        agua_util = NA_real_,
        ETR = NA_real_,
        deficiencia = NA_real_
      ) 
    
    datos_filtrados$Fr_agua_util[1] <- fraccion_inicial
    datos_filtrados$agua_util[1] <- fraccion_inicial * agua_util_total_val
    
    for (i in 2:nrow(datos_filtrados)) {
      
      datos_filtrados$ETR[i] <- if_else(
        is.na(datos_filtrados$Fr_agua_util[i - 1]) | is.na(datos_filtrados$ETM[i]), 
        NA_real_, 
        if_else(
          datos_filtrados$Fr_agua_util[i - 1] >= agua_util_total_val, 
          datos_filtrados$ETM[i], 
          disminucion_et_val * datos_filtrados$Fr_agua_util[i - 1] * datos_filtrados$ETM[i]
        )
      )
      
      datos_filtrados$agua_util[i] <- if_else(
        is.na(datos_filtrados$agua_util[i - 1]) | is.na(datos_filtrados$Precipitacion_Pluviometrica[i]) | is.na(datos_filtrados$ETR[i]),
        NA_real_,
        if_else(
          datos_filtrados$agua_util[i - 1] + datos_filtrados$Precipitacion_Pluviometrica[i] - datos_filtrados$ETR[i] > agua_util_total_val,
          agua_util_total_val,
          datos_filtrados$agua_util[i - 1] + datos_filtrados$Precipitacion_Pluviometrica[i] - datos_filtrados$ETR[i]
        )
      )
      
      datos_filtrados$Fr_agua_util[i] <- if_else(
        is.na(datos_filtrados$agua_util[i]) | is.na(agua_util_total_val),
        NA_real_,
        datos_filtrados$agua_util[i] / agua_util_total_val
      )
      
      datos_filtrados$deficiencia[i] <- if_else(
        is.na(datos_filtrados$ETR[i]) | is.na(datos_filtrados$ETM[i]),
        NA_real_,
        datos_filtrados$ETR[i] - datos_filtrados$ETM[i]
      )
    }
    return(datos_filtrados)
    # write_xlsx(datos_filtrados, "datos_filtrados2.xlsx")
  })
  
  ## Gráficos balance de agua ##
  output$consumo_agua <- renderPlotly({
    df_siembra <- balance_agua()
    
    
    cons_agua <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_line(aes(y = ETM, color = "ETM")) +
      geom_line(aes(y = ETR, color = "ETR")) +
      geom_line(aes(y = Fr_agua_util, color = "Fr_agua_util")) +  
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_color_manual(values = c("#E76F51", "#2A9D8F", "#E9C46A"),
                         labels = c("ETM", 
                                    "ETR", 
                                    "Fracción de Agua Útil")) +
      guides(color = guide_legend(title = NULL))
    
    ggplotly(cons_agua) %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = 1.2),
             annotations = list(
               x = 0.6, 
               y = -0.18, 
               text = "ETM: Máximo consumo de agua si no hubiera deficiencias de agua\nETR: Consumo de agua REAL",
               showarrow = FALSE,
               xref = 'paper', 
               yref = 'paper', 
               xanchor = 'left', 
               yanchor = 'left', 
               font = list(size = 8)
             ))
  })
  
  
  output$deficit_agua <- renderPlotly({
    df_siembra <- balance_agua()
    
    def_agua <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_bar(aes(y = Precipitacion_Pluviometrica, fill = "Precipitacion_Pluviometrica"),
               stat = "identity", position = "dodge") +
      geom_bar(aes(y = deficiencia, fill = "deficiencia"),
               stat = "identity", position = "dodge") +
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_fill_manual(values = c("#007EA7", "#BC4749"),
                         labels = c("Precipitacion", "Deficiencia hídrica")) +
      guides(fill = guide_legend(title = NULL))
    
    ggplotly(def_agua) %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = 1.2))
  })
  
  
  ## Descarga de datos ##
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
        datos_filtrados3 <- subset(datos_filtrados3, 
                                   Fecha >= input$fecha_inicio & Fecha <= input$fecha_fin)
      } else if (!is.null(input$fecha_inicio)) {
        datos_filtrados3 <- subset(datos_filtrados3, 
                                   Fecha >= input$fecha_inicio)
      } else if (!is.null(input$fecha_fin)) {
        datos_filtrados3 <- subset(datos_filtrados3, 
                                   Fecha <= input$fecha_fin)
      }
      
      # SSelección de variables
      datos_filtrados3 <- datos_filtrados3[, c("Fecha", input$variables), 
                                           drop = FALSE]
      
      #
      write.csv(datos_filtrados3, 
                file, 
                row.names = FALSE)
    }
  )
  
  # observeEvent(input$enviar, {
  #   comentario <- input$text
  #   if (comentario != "Aquí...") {
  #     enviar_correo(comentario)
  #     showNotification("Mensaje enviado", duration = 5)
  #   }
  # })
  # 
  
}





# Run the app ----
shinyApp(ui = ui, server = server)
# 
# rsconnect::setAccountInfo(name='8js2kq-nuria-lewczuk',
#                             token='111A9791794261A82E719E515597DD66',
#                             secret='m+wC8DYne2Mr69yhsLI/2i4QF7Zy5PQUFPIe5Xkr')
# rsconnect::deployApp(appDir = "E:/TRABAJO/CERBAS/GrupoAgrometeorologia/App_Meteo", appPrimaryDoc = "ui.R",
#                      appName = "MiAppMeteo", account = '8js2kq-nuria-lewczuk', server = 'shinyapps.io')