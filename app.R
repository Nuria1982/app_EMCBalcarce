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
library(writexl)
library(leaflet)



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
dalbulus <- read_excel("dalbulus.xlsx")

datos_EMC <- balcarce_EMC 
datos_EMC <- datos_EMC %>%
  filter(Año >= 1970)
# datos_EMC <- subset(datos, select = -c(Direccion_Viento_200cm, Direccion_Viento_1000cm))
datos_EMC$Fecha <- as.Date(datos_EMC$Fecha, format = "%Y-%m-%d")
datos_EMC <- datos_EMC[order(datos_EMC$Fecha, decreasing = TRUE), ]
head(datos_EMC)


datos_actuales <- datos_EMC[!is.na(datos_EMC$Precipitacion_Pluviometrica) & 
                          !is.na(datos_EMC$Temperatura_Abrigo_150cm_Maxima) & 
                          !is.na(datos_EMC$Temperatura_Abrigo_150cm_Minima), ]


ultima_fecha <- max(datos_actuales$Fecha)
ultimos_datos <- datos_actuales[datos_actuales$Fecha == ultima_fecha, ]
lluvia_ultimo_dia <- ultimos_datos$Precipitacion_Pluviometrica
Tmax_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Maxima
Tmin_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Minima

## historicos 1991-2020 ##
datos_historicos <- datos_EMC %>%
  filter(Año >= 1991 & Año <= 2020)
# datos_historicos

datos_historicos_avg <- datos_historicos %>%
  mutate(Dia_Mes = format(Fecha, "%m-%d")) %>%
  group_by(Dia_Mes) %>%
  reframe(
    Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE),
    Evapotranspiracion_media = mean(Evapotranspiracion_Potencial, na.rm = TRUE),
    Precipitacion_media = mean(Precipitacion_Pluviometrica, na.rm = TRUE),
    .groups = "drop"
  )

datos_EMC <- datos_EMC %>%
  mutate(Dia_Mes = format(Fecha, "%m-%d"))

datos <- datos_EMC %>%
  left_join(datos_historicos_avg, by = "Dia_Mes") %>%
  mutate(
    Evapotranspiracion_Potencial = coalesce(Evapotranspiracion_Potencial, Evapotranspiracion_media),
    Temperatura_Abrigo_150cm = coalesce(Temperatura_Abrigo_150cm, Temperatura_media)
  ) %>%
  select(-Evapotranspiracion_media)
# datos
# write_xlsx(datos, "datos.xlsx")

dalbulus <- dalbulus



#################

# Define UI ----
ui <- dashboardPage(
  
  
  title = "Agrometeorología Balcarce",
  skin = "#2596be",
  
  header = dashboardHeader(
    title = div(
      style = "font-size: 24px; font-weight: bold; text-align: center;",
      "EMC Balcarce",
      tags$style(HTML('.navbar { background-color: #2596be; }'))
    )
  ),
  
  sidebar = dashboardSidebar(
    
    
    
    fluid = FALSE,
    position = "left",
    # collapsible = TRUE,
    # collapsed = FALSE,
    br(),
    br(),
    div(
      style = "text-align: center;",
      tags$img(src = "EstacionBalcarce.jpg",
               height = "80px",
               width = "220px")
    ),
    
    br(),
    br(),
    
    sidebarMenu(id = "siderbarID",
                menuItem("Condiciones actuales", 
                         tabName = "condiciones",
                         icon = icon("calendar")),
                menuItem("Cambio climático", 
                         tabName = "cambio_climatico",
                         icon = icon("earth-americas")),
                menuItem("Mapas",
                         tabName = "mapas", 
                         icon = icon("map")),
                menuItem("Manejo de los cultivos", 
                         icon = icon("cogs"),
                         menuSubItem("Ambiente",
                                     tabName = "ambiente"),
                         menuSubItem("Balance de agua",
                                     tabName = "balance")
                         # ,
                         # menuSubItem("Dalbulus",
                         #             tabName = "Dalbulus")
                         ),
                menuItem("Pronósticos",
                         tabName = "pronosticos", 
                         icon = icon("bar-chart")),
                menuItem("informes",
                         tabName = "informes", 
                         icon = icon("file")),
                menuItem("Descarga de datos",
                         tabName = "descarga", 
                         icon = icon("download")),
                menuItem("Referencias bibliográficas",
                         tabName = "referencias", 
                         icon = icon("book"))),
    br(),
    br(),
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
      
      div(
        style = "text-align: center;",
        tags$img(src = "IPADS.png",
                 height = "80px",
                 width = "150px"
        ),
        tags$img(src = "Logo_Red_Agromet.jpg",
                 height = "80px",
                 width = "200px")
      )
    )
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
    
    tabItems(
      tabItem(tabName = "condiciones",
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
                column(6,
                       fluidRow(
                         column(6,
                                selectInput(
                                  inputId = "ano_selector", 
                                  label = "Selecciona el Año:",
                                  choices = unique(datos$Año),
                                  selected = "2024"
                                )
                         ),
                         column(6,
                                selectInput(
                                  inputId = "mes_selector", 
                                  label = "Selecciona los meses:",
                                  choices = c("Mostrar todos los meses", 
                                              "enero", "febrero", "marzo", "abril",
                                              "mayo", "junio", "julio", "agosto", 
                                              "septiembre", "octubre", "noviembre", "diciembre"),
                                  selected = "Mostrar todos los meses",
                                  multiple = TRUE
                                )
                         )
                       )
                )
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
      
      tabItem(tabName = "cambio_climatico",
              fluidRow(
                
                column(12,
                       div(style = "background-color: #2A9D8F60; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                           h5(HTML("<strong>Variabilidad Climática</strong>")),
                           p("Es el resultado de ciclos naturales relacionados con la órbita terrestre, la radiación solar, la composición atmosférica, y la circulación oceánica y biosférica. Esta variabilidad puede manifestarse a corto plazo, como se observa en los episodios recurrentes de El Niño y La Niña, que están asociados con patrones de presión atmosférica y circulación oceánica.")
                       )
                )
              ),
              fluidRow(
                
                column(12,
                       div(style = "background-color: #F4A26160; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                           h5(HTML("<strong>Cambio Climático</strong>")),
                           p("Se refiere a las variaciones en el estado del clima, identificables por cambios en el valor medio o la variabilidad de sus componentes, como la temperatura, que deben ser persistentes durante décadas o más. Estos cambios pueden ser resultado de procesos naturales internos o de forzamientos externos, como las fluctuaciones de los ciclos solares, erupciones volcánicas o alteraciones antropogénicas en la composición atmosférica y el uso del suelo.")
                       )
                )
              ),
              br(),
              fluidRow(
                column(3),
                column(12,
                       div(style = "text-align: center;",
                           p("Esta aplicación presenta datos climáticos relevantes, incluyendo temperaturas medias, máximas y mínimas y precipitaciones acumuladas, para ayudar a comprender los impactos del cambio climático en nuestra región."),
                           p("Utiliza el filtro para ver los datos anuales desde 1970, para un mes en particular o para el promedio anual:"),
                           br(),
                           div(style = "width: 200px; margin: 0 auto;",
                               selectInput(
                                 inputId = "mes_climatico", 
                                 label = "Selecciona el mes:",
                                 choices = c("Anual", 
                                             "enero", "febrero", "marzo", "abril",
                                             "mayo", "junio", "julio", "agosto", 
                                             "septiembre", "octubre", "noviembre", "diciembre"),
                                 selected = "Anual"
                               )
                           )
                       )
                       
                )
              ),
              
              br(),
              
              fluidRow(
                column(4, 
                       box(
                         title = "Temperaturas medias (ºC)",
                         status = "orange",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         plotlyOutput("grafico_temp_climatico", 
                                      height = "300px", 
                                      width = "100%"),
                         width = 12
                       )
                ),
                column(4, 
                       box(
                         title = "Precipitaciones acumuladas (mm)",
                         status = "orange",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         plotlyOutput("grafico_pp_climatico", 
                                      height = "300px", 
                                      width = "100%"),
                         width = 12
                       )
                ),
                column(4, 
                       box(
                         title = "Días con temperaturas máximas > a 25ºC y < 3ºC",
                         status = "orange",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         plotlyOutput("grafico_heladas_climatico", 
                                      height = "300px", 
                                      width = "100%"),
                         width = 12
                       )
                )
              ),
      ),
      
      tabItem(
        tabName = "mapas",
        h4(HTML("<strong>Mapas de suelo</strong>")),
        h5(HTML("Recortes de la zona de influencia de EEA Balcarce, realizados por el Inst. de Clima y agua - INTA Castelar")),
        br(),
        br(),
        fluidRow(
          box(title = "Consumo de agua"
              ,status = "navy"
              ,solidHeader = FALSE
              ,div(
                style = "text-align: center;",
                tags$img(
                  src = "agua.jpg",
                  style = "max-width: 80%; height: auto;",
                  alt = "Mapa-Consumo de agua"
                )
              ),
              tags$figcaption("Evapotranspiración real máxima (en el periodo de 10 días) expresada en
              mm/día estimada mediante el uso de imágenes del sensor VIIRS del satélite
              Suomi-NPP con una resolución espacial de 500 metros. Elaborado por Instituto de
              Clima y Agua, INTA Castelar. Recorte: Patricio Oricchio."),
              div(
                style = "margin-top: 20px; text-align: left; padding: 10px; border: 2px solid #ddd; background-color: #f9f9f9;",
                "El consumo de agua o evapotranspiración real (ETR) es la
                cantidad de agua que es transpirada por la cubierta vegetal y aquella que
                es perdida desde la superficie del suelo por evaporación.
               El consumo de agua puede ser utilizado para detectar la ocurrencia
                de deficiencias de agua, cuando su valor no alcanza el requerido por el
                cultivo.")
          ),
          box(title = "% de agua útil"
              ,status = "navy"
              ,solidHeader = FALSE
              ,div(
                style = "text-align: center;",
                tags$img(
                  src = "agua_util.jpg",
                  style = "max-width: 80%; height: auto;",
                  alt = "Mapa-agua útil"
                )
              ),
              br(),
              br(),
              tags$figcaption("Porcentaje de agua en el suelo.Resolución espacial: 500 m. 
                  Mapa elaborado por Instituto de Clima y Agua, INTA Castelar. Recorte: Lucas Gusmerotti."),
              div(
                style = "margin-top: 20px; text-align: left; padding: 10px; border: 2px solid #ddd; background-color: #f9f9f9;",
                "El porcentaje de agua útil en el suelo (es decir, aquella porción de agua
                    que puede ser extraída por las plantas) puede ser estimado a través
                    de un balance de agua; donde se considera información del suelo, el
                    aporte de agua por lluvias y el consumo de agua de la cubierta
                    vegetal.")
          ))
      ),
      
      tabItem(
        tabName = "ambiente",
        br(),
        h4(HTML("<strong>Ambiente de los cultivos</strong>")),
        h5(HTML("Te mostramos como fue cambiando el ambiente del cultivo durante el perdíodo crítico (PC), según la fecha de siembra desde el año 1991.")),
        
        br(),
        
        fluidRow(
          # elección cultivo 
          column(3,
                 div(style = "background-color: #81B29A80; padding: 10px;  border-radius: 10px;",
                     selectInput("cultivo_ambiente",
                                 label = strong("Cultivo:"),
                                 choices = list("Maíz" = "maiz",
                                                "Soja" = "soja"
                                                # ,
                                                # "Girasol" = "girasol",
                                                # "Hortalizas" = "hortalizas"
                                 ),
                                 selected = "maiz")
                 )
          ),
          column(3,
                 div(style = "background-color: #81B29A80; padding: 10px;  border-radius: 10px;",
                     numericInput("dia_siembra_ambiente",
                                  label = strong("Día de siembra:"),
                                  value = 1, 
                                  min = 1, max = 31)
                 )
          ),
          column(3,
                 div(style = "background-color: #81B29A80; padding: 10px; border-radius: 10px;",
                     selectInput("mes_siembra_ambiente",
                                 label = strong("Mes de siembra:"),
                                 choices = list("Octubre" = 10, "Noviembre" = 11, 
                                                "Diciembre" = 12, "Enero" = 1),
                                 selected = 10)
                 )
          ),
          # column(3,
          #        div(style = "background-color: #E0E1DD80; padding: 10px; border-radius: 10px;",
          #            fileInput("ambiente_precip_riego", "Subir archivo de precipitaciones y riego (opcional)",
          #                      accept = c(".csv", ".xlsx")),
          #            helpText("El archivo debe contener los datos diarios y las columnas: Fecha, Lluvia, Riego")
          #        )
          # ),
        ),
        br(),
        br(),
        
        fluidRow( 
          box(
            title = "Precipitaciones y ET0 acumuladas durante el período crítico",
            status = "olive",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("pp_acum_PC", height = "300px")
          ),
          box(
            title = "Radiación y días con temp. máx. > 35ºC durante el período crítico",
            status = "olive",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("rad_temp", height = "300px")
          ),
          h6(HTML("ET0: Evapotranspiración de referencia (mm) o demanda de agua del cultivo<br />Valores decádicos corresponden a la mediana."))
        ),
        
        br(),
        br(),
        
        h5(HTML("Podes comparar el ambiente de tu cultivo en 3 fechas de siembra 
                diferentes")),
        h6(HTML("(consideramos desde el año 2010, 
                donde se observa un cambio en las temperaturas a nivel país).")),
        
        br(),
        
        fluidRow(
          column(3,
                 div(style = "background-color: #81B29A80; padding: 10px;  border-radius: 10px;",
                     selectInput("cultivo_fecha",
                                 label = strong("Cultivo:"),
                                 choices = list("Maíz" = "maiz",
                                                "Soja" = "soja"
                                                # ,
                                                # "Girasol" = "girasol",
                                                # "Hortalizas" = "hortalizas"
                                 ),
                                 selected = "maiz")
                 ),
                 br(),
                 br(),
                 column(12, align = "center",
                        actionButton("btn_calcular", label = "Calcular", icon = icon("calculator"),
                                     style = "color: white; background-color: #81B29A; border-color: #81B29A; border-radius: 10px; padding: 10px;")
                 )
                 
          ),
          column(3,
                 div(style = "background-color: #E07A5F80; padding: 10px;  border-radius: 10px;",
                     h6(HTML("<strong>Fecha 1:</strong>")),
                     
                     selectInput("mes_siembra1",
                                 label = strong("Mes de siembra:"),
                                 choices = list("Octubre" = 10, "Noviembre" = 11, 
                                                "Diciembre" = 12, "Enero" = 1),
                                 selected = 10),
                     numericInput("dia_siembra1",
                                  label = strong("Día de siembra:"),
                                  value = 1, 
                                  min = 1, max = 31),
                     br(),
                     textOutput("fecha_inicio_pc1"),
                     textOutput("fecha_fin_pc1")
                 )
          ),
          column(3,
                 div(style = "background-color: #3D405B80; padding: 10px;  border-radius: 10px;",
                     h6(HTML("<strong>Fecha 2:</strong>")),
                     
                     selectInput("mes_siembra2",
                                 label = strong("Mes de siembra:"),
                                 choices = list("Octubre" = 10, "Noviembre" = 11, 
                                                "Diciembre" = 12, "Enero" = 1),
                                 selected = 10),
                     numericInput("dia_siembra2",
                                  label = strong("Día de siembra:"),
                                  value = 1, 
                                  min = 1, max = 31),
                     br(),
                     textOutput("fecha_inicio_pc2"),
                     textOutput("fecha_fin_pc2")
                 )
          ),
          column(3,
                 div(style = "background-color: #AEC3B080; padding: 10px;  border-radius: 10px;",
                     h6(HTML("<strong>Fecha 3:</strong>")),
                     
                     selectInput("mes_siembra3",
                                 label = strong("Mes de siembra:"),
                                 choices = list("Octubre" = 10, "Noviembre" = 11, 
                                                "Diciembre" = 12, "Enero" = 1),
                                 selected = 10),
                     numericInput("dia_siembra3",
                                  label = strong("Día de siembra:"),
                                  value = 1, 
                                  min = 1, max = 31),
                     br(),
                     textOutput("fecha_inicio_pc3"),
                     textOutput("fecha_fin_pc3")
                 )
          )
        ),
        
        br(),
        
        fluidRow(
          column(4, 
                 box(
                   title = "Precipitaciones y ET0 acumuladas",
                   status = "olive",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotlyOutput("gg_lluvia", height = "300px", width = "100%"),
                   width = 12
                 )
          ),
          column(4, 
                 box(
                   title = "Radiación global",
                   status = "olive",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotlyOutput("gg_radiacion", height = "300px", width = "100%"),
                   width = 12
                 )
          ),
          column(4, 
                 box(
                   title = "Días con temperaturas máximas > a 35ºC",
                   status = "olive",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotlyOutput("gg_dias_35", height = "300px", width = "100%"),
                   width = 12
                 )
          ),
        )
      ),
      
      tabItem(
        tabName = "balance",
        br(),
        h4(HTML("<strong>Cálculo de balance de agua</strong>")),
        h5(HTML("A partir de los datos del suelo y del cultivo seleccionado, podemos calcular el balance de agua diario de tu campo. <br> 
                Podes ingresar los datos en los recuadros o usar los valores predeterminados.")),
        
        br(),
        
        fluidRow(
          # elección cultivo 
          column(3,
                 selectInput("cultivo",
                             label = strong("Seleccione el cultivo:"),
                             choices = list("Maíz" = "maiz",
                                            "Soja" = "soja"
                                            # ,
                                            # "Girasol" = "girasol",
                                            # "Hortalizas" = "hortalizas"
                             ),
                             selected = "maiz")
          ),
          column(3,
                 dateInput("fecha_siembra",
                           label = strong("Ingrese la fecha de siembra:"),
                           value = "2024-01-01")
          )
        ),
        fluidRow(
          column(6,
                 div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                     h4(HTML(("<strong>Datos de suelo<sup>2</sup></strong>"))),
                     fluidRow(
                       column(6,
                              numericInput("profundidad",  
                                           label = strong("Profundidad (cm)"),
                                           value = 100),
                              br(),
                              numericInput("capacidad_campo",  
                                           label = strong(HTML("Capacidad de Campo (mm/cm)
                                                        <br><small>
                                                        (Límite máximo de almacenamiento de agua)</small>")), 
                                           value = 3.70),
                              textOutput("almacenamiento_maximo")
                       ),
                       column(6,
                              numericInput("fraccion_min",  
                                           label = strong("Fracción de almacenamiento mínimo respecto del máximo (0 - 1)"), 
                                           value = 0.55,
                                           min = 0,
                                           max = 1),
                              textOutput("almacenamiento_minimo"),
                              textOutput("agua_util_total"),
                              br(),
                              numericInput("fraccion_inicial",  
                                           label = strong("Fracción inicial de agua útil (0 - 1)"), 
                                           value = 0.50,
                                           min = 0,
                                           max = 1)
                       )
                     )
                 )
          ),
          column(3,
                 div(style = "background-color: #21838040; padding: 15px; border-radius: 10px;",
                     h4(strong("Datos de cultivo")),
                     numericInput("umbral_et",  
                                  label = strong(HTML("Umbral de fracción de agua útil 
                                                        <br><small>
                                                        (Debajo del cual se reduce la evapotranspiración)</small>")), 
                                  value = NULL),
                     textOutput("disminucion_et"),
                     br(),
                     textOutput("GD"),
                 )
          ),
          # column(3,
          #        div(style = "background-color: #E0E1DD40; padding: 10px; border-radius: 10px;",
          #            fileInput("balance_precip_riego", "Subir archivo de precipitaciones y riego (opcional)",
          #                      accept = c(".csv", ".xlsx")),
          #            helpText("El archivo debe contener las columnas: Fecha, Precipitacion, Riego")
          #        )
          # )
        ),
        br(),
        
        fluidRow( 
          box(
            title = "Fracción de agua útil",
            status = "lightblue",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("agua_util", height = "300px")
          ),
          box(
            title = "Consumo de agua",
            status = "lightblue",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("consumo_agua", height = "300px")
          ),
          box(
            title = "Balance de agua",
            status = "lightblue",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("deficit_agua", height = "300px")
          ),
          column(6,
                 div(tags$img(
                   src = "Mapa_Estacion_Met.png",
                   style = "max-width: 60%; height: 100%; display: block; margin: 0 auto;",
                   alt = "ubicacion_EMC"
                 )
                 )
          )
        ),
        
        fluidRow(  
          column(12,
                 br(),
                 br(),
                 div(uiOutput("mensaje_cultivo"),
                 )
          ),
          
        )
      ),
      
      # tabItem(
      #   tabName = "Dalbulus",
      #   br(),
      #   h4(HTML("<strong>Mapa de probabilidad de presencia de Dalbulus</strong>")),
      #   br(),
      #   h5(HTML("Al seleccionar la fecha, se muestra la probabilidad de emergencia de maíz guacho")),
      #   br(),
      #   
      #   fluidRow(
      #     dateInput("fecha_dalbulus",
      #               label = strong("Ingrese la fecha:"),
      #               value = "2024-01-01") 
      #     ),
      #     
      #     leafletOutput("mapa_arg") 
      #   ),
      
      tabItem(
        tabName = "pronosticos",
        br(),
        h4(HTML("<strong>Pronósticos meteorológicos del área de influencia de EEA Balcarce</strong>")),
        h6(HTML("Elaborados por el SMN y el Instituto de Clima y Agua - INTA Castelar.")),
        br(),
        br(),
        
        fluidRow( 
          box(title = "Pronóstico semanal"
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
          box(title = "Pronóstico trimestral"
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
      
      tabItem(
        tabName = "informes",
        br(),
        h4(HTML("<strong>Informes</strong>")),
        fluidRow(
          column(
            width = 3,
            div(
              style = "margin-left: 100px; margin-top: 50px;",  
              tags$img(
                src = "IMA.jpeg",
                width = 300,
                height = 420,
                alt = "Informe Mensual Agropecuario"
              ),
              tags$br(),
              tags$a(
                "Descarga el informe completo aquí", href= "https://bit.ly/IMA-SEP24")
            )),
          column(
            width = 3,
            div(
              style = "margin-left: 100px; margin-top: 50px;",  
              tags$img(
                src = "chicharrita.jpg",
                width = 300,
                height = 420,
                alt = "Achaparramiento del Maíz"
              ),
              tags$br(),
              tags$a(
                "Descarga el informe completo aquí", href= "https://www.argentina.gob.ar/sites/default/files/2018/09/el_achaparramiento_del_maiz_y_las_decisiones_agricolas_en_argentina_mesatecnicanacional_inta.pdf")
            ),
          ),
          column(
            width = 3,
            div(
              style = "margin-left: 100px; margin-top: 50px;",  
              tags$img(
                src = "ENSO.jpg",
                width = 300,
                height = 420,
                alt = "ENSO y precipitaciones"
              ),
              tags$br(),
              downloadLink(
                outputId = "downloadReport", 
                label = "Descarga el informe completo aquí"
              ),
            )
          )
        )),
      
      tabItem(
        tabName = "descarga",
        br(),
        h4(HTML("<strong>Datos disponibles de la EMA Balcarce</strong>")),
        br(),
        
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
          choices = c("Temperatura_Abrigo_150cm",
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
          selected = "Temperatura_Abrigo_150cm",
          multiple = TRUE
        ),
        downloadButton("Datos_meteo_Balcarce", "Descargar (.xlsx)")
      ),
      
      tabItem(
        tabName = "referencias",
        br(),
        h4(HTML("<strong>Referencias bibliográficas</strong>")),
        br(),
        br(),
        h6(HTML("<strong>Allen</strong>, R.G.; Pereira, L.S. Raes, D. Y D. Smith. 1998. Crop evapotranspiration. Guides for computing crop water requirements. FAO Irrig. Drain. Nº 56. Italy, 300 p.")),
        h6(HTML("<strong>Andrade</strong>, FH., Otegui, ME., Cirilo, A., Uhart, S. 2023.  “Ecofisiología y manejo del cultivo de maíz”.  Maizar.")),
        h6(HTML("<strong>Cerrudo</strong>, A, Di Matteo J, Fernandez E, Robles M, Pico LO, Andrade FH. 2013. Yield components of maize as affected by short shading periods and thinning. Crop and Pasture Science 64, 580.")),
        h6(HTML("<strong>Della Maggiora</strong>, A.I., A.I. Irigoyen, J. M. Gardiol, O. Caviglia and L. Echarte. 2002/03. Evaluación de un balance de agua en el suelo para maíz. Revista Argentina de Agrometeorología, 2(2):167-176.")),
        h6(HTML("<strong>Echarte</strong>, L., Otegui, M.E. 2023. Consumo y eficiencia en el uso del agua. En: Andrade, FH., Otegui, ME., Cirilo, A., Uhart, S. (Eds) “Ecofisiología y manejo del cultivo de maíz” (pp. 221-244).  Maizar.")),
        h6(HTML("<strong>Gardiol</strong>, J.M.; Della Maggiora, A. Irigoyen, A. 2002. Curvas de coeficientes de cultivo de maíz, girasol y soja. IX Reunión Argentina de Agrometeorología. Córdoba.")),
        h6(HTML("<strong>Gardiol</strong>, J. M., Leonardo, A. S., & Aida, I. D.M. 2003. Modeling evapotranspiration of corn (Zea mays) under different plant densities. Journal of Hydrology, 271, 291–308. https://doi.org/10.1016/S0022-1694(02)00347-5.")),
        h6(HTML("<strong>Gardiol</strong>, J.M.; Della Maggiora, A.; Irigoyen, A. 2006. Coeficientes de cultivo de soja basados en la evapotranspiración de referencia Penman-Monteith.")),
        h6(HTML("<strong>Monzon</strong>, J. P., Cafaro La Menza, N., Cerrudo, A., Canepa, M., Rattalino Edreira, J. I., Specht, J., et al. 2021. Critical period for seed number determination in soybean as determined by crop growth rate, duration, and dry matter accumulation. Field Crops Res. 261:108016. doi: 10.1016/j.fcr.2020.108016.")),
        h6(HTML("<strong>RECSO</strong>, Base de datos de RECSO Balcarce periodo 2013-2023. Compilada por Marina Montoya, INTA Balcarce. Agosto 2023. Colaboradores: Auxilares Walter Suarez, Silvio Giuliano, Carlos Antonelli, Mauro Zabaleta, Mariano Ruberto (INTA Balcarce). Fuente: Información publicada anualmente por Comunicaciones INTA Balcarce. Actividades incluidas en el convenio INTA-ASA.")),
      )
    )
  )
)



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
  current_year <- max(datos_actuales$Año)
  pp_acum <- sum(subset(datos_actuales, Año == current_year)$Precipitacion_Pluviometrica)
  
  # Calculating current temp max
  ttmax_anual <- mean(subset(datos_actuales, Año == current_year)$Temperatura_Abrigo_150cm_Maxima)
  
  # Calculating current temp min
  ttmin_anual <- mean(subset(datos_actuales, Año == current_year)$Temperatura_Abrigo_150cm_Minima, na.rm = TRUE)
  
  
  
  ultima_fecha <- max(datos_actuales$Fecha)
  ultimos_datos <- datos_actuales[datos_actuales$Fecha == ultima_fecha, ]
  lluvia_ultimo_dia <- ultimos_datos$Precipitacion_Pluviometrica
  Tmax_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Maxima
  Tmin_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Minima
  
  
  datasetInput <- reactive({
    if (input$ano_selector == "Todos los años") {
      datos_filtrados <- datos_actuales
    } else {
      datos_filtrados <- subset(datos_actuales, Año == input$ano_selector)
    }
    
    if (!"Mostrar todos los meses" %in% input$mes_selector) {
      meses_seleccionados <- input$mes_selector
      datos_filtrados <- subset(datos_filtrados, Mes %in% meses_seleccionados)
    } else {
      datos_filtrados <- datos_filtrados
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
      value = paste("Promedio Año ", current_year, ": ", round(ttmax_anual, 2), "ºC"),
      subtitle = paste("Promedio Histórico anual (1991-2020): ", round(promedio_historico_ttmax, 2), "ºC"),
      icon = icon("sun"),
      color = "danger"
    )
  })
  
  output$tempMin_info_box <- renderInfoBox({
    infoBox(
      title = "Temperaturas Mínimas",
      value = paste("Promedio Año ", current_year, ": ", round(ttmin_anual, 2),"ºC"),
      subtitle = paste("Promedio Histórico anual (1991-2020): ", round(promedio_historico_ttmin, 2), "ºC"), 
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
      scale_fill_manual(name = "") +
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
      geom_line(linewidth = 1) +
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
  
  ##### CAMBIO CLIMATICO ##########
  
  datasetInput_climatico <- reactive({
    
    datos_filtrados_climatico <- datos
    
    if (!is.null(input$mes_climatico) && !"Anual" %in% input$mes_climatico) {
      datos_filtrados_climatico <- subset(datos_filtrados_climatico, Mes == input$mes_climatico)
    }
    
    return(datos_filtrados_climatico)
  })
  

  output$grafico_temp_climatico <- renderPlotly({
    
    datos_filtrados_climatico <- datasetInput_climatico()
    
    datos_historicos_avg_1991_2020 <- datos %>%
      filter(Año >= 1991 & Año <= 2020) %>%
      summarise(Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE))
    
    datos_historicos_avg_1981_2010 <- datos %>%
      filter(Año >= 1981 & Año <= 2010) %>%
      summarise(Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE))
    
    datos_historicos_avg_1971_2000 <- datos %>%
      filter(Año >= 1971 & Año <= 2000) %>%
      summarise(Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE))
    
    
    
    if (input$mes_climatico == "Anual") {
      # Mostrar el promedio anual de temperaturas
      tt_media <- datos %>%
        group_by(Año) %>%
        summarise(Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE))
      
      etiquetas_10_anios <- tt_media %>%
        filter(Año %% 10 == 0)
      
      tt_climatico <- ggplot(tt_media, 
                             aes(x = Año, 
                                 y = Temperatura_media)) +
        geom_line(color = "#FFBA08", linewidth = 1) +  
        geom_point(size = 1, color = "#FFBA08") +
        geom_smooth(method = "lm", color = "red", linewidth = 0.5, se = TRUE) + 
        geom_text(data = etiquetas_10_anios, aes(label = round(Temperatura_media, 1)), 
                  vjust = -0.5, color = "black", size = 3) +
        labs(x = "", y = "ºC", title = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))   
        
        # geom_hline(data = datos_historicos_avg_1991_2020, 
        #            aes(yintercept = Temperatura_media), 
        #            linetype = "dashed", color = "green") +
        # geom_hline(data = datos_historicos_avg_1981_2010, 
        #            aes(yintercept = Temperatura_media), 
        #            linetype = "dashed", color = "blue") +
        # geom_hline(data = datos_historicos_avg_1971_2000, 
        #            aes(yintercept = Temperatura_media), 
        #            linetype = "dashed", color = "red") +
        
    } else {
      
      tt_media <- datos_filtrados_climatico %>%
        group_by(Año) %>%
        summarise(Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE))
      
      etiquetas_10_anios <- tt_media %>%
        filter(Año %% 10 == 0)
      
      tt_climatico <- ggplot(tt_media, 
                             aes(x = Año, 
                                 y = Temperatura_media)) +
        geom_line(color = "#FFBA08", linewidth = 1) +  
        geom_point(size = 1, color = "#FFBA08") +
        geom_smooth(method = "lm", color = "red", linewidth = 0.5, se = TRUE) + 
        geom_text(data = etiquetas_10_anios, aes(label = round(Temperatura_media, 1)), 
                  vjust = -0.5, color = "black", size = 3) +
        labs(x = "", y = "ºC", title = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
      
    }
    
    ggplotly(tt_climatico)
  })
        

  
  
  output$grafico_pp_climatico <- renderPlotly({
    
    datos_filtrados_climatico <- datasetInput_climatico()
    
    # Calcular promedios históricos
    datos_historicos_avg_1991_2020 <- datos %>%
      filter(Año >= 1991 & Año <= 2020) %>%
      group_by(Año) %>%
      summarise(Precipitacion_Acumulada = sum(Precipitacion_Pluviometrica, na.rm = TRUE)) %>%
      summarise(Precipitacion_media = mean(Precipitacion_Acumulada, na.rm = TRUE))
    
    datos_historicos_avg_1981_2010 <- datos %>%
      filter(Año >= 1981 & Año <= 2010) %>%
      group_by(Año) %>%
      summarise(Precipitacion_Acumulada = sum(Precipitacion_Pluviometrica, na.rm = TRUE)) %>%
      summarise(Precipitacion_media = mean(Precipitacion_Acumulada, na.rm = TRUE))
    
    datos_historicos_avg_1971_2000 <- datos %>%
      filter(Año >= 1971 & Año <= 2000) %>%
      group_by(Año) %>%
      summarise(Precipitacion_Acumulada = sum(Precipitacion_Pluviometrica, na.rm = TRUE)) %>%
      summarise(Precipitacion_media = mean(Precipitacion_Acumulada, na.rm = TRUE))
    
    if (input$mes_climatico == "Anual") {
      
      # Acumulado por año
      pp_acum <- datos %>%
        group_by(Año) %>%
        summarise(Precipitacion_Acumulada = sum(Precipitacion_Pluviometrica, na.rm = TRUE))
      
      etiquetas_10_anios <- pp_acum %>%
        filter(Año %% 10 == 0) %>%
        mutate(Año_medio = Año + 2,
               Precipitacion_media = round(Precipitacion_Acumulada, 0)) 
      max_y <- max(pp_acum$Precipitacion_Acumulada)
      
      pp_climatico <- ggplot(pp_acum, 
                             aes(x = factor(Año), 
                                 y = Precipitacion_Acumulada)) +
        geom_bar(stat = "identity", 
                 fill = "#007EA7", 
                 color = "#003459", 
                 alpha = 0.5) +
        labs(x = "", y = "mm", title = "") +
        scale_x_discrete(breaks = pp_acum$Año[pp_acum$Año %% 10 == 0]) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        geom_text(data = etiquetas_10_anios, 
                  aes(x = factor(Año_medio), y = max_y, label = Precipitacion_media), 
                  vjust = -0.5, color = "black", size = 3.5)
      
    } else {
    
      # Acumulado por mes
      pp_acum <- datos_filtrados_climatico %>%
        group_by(Año) %>%
        summarise(Precipitacion_Acumulada = sum(Precipitacion_Pluviometrica, na.rm = TRUE))
      
      etiquetas_10_anios <- pp_acum %>%
        filter(Año %% 10 == 0) %>%
        mutate(Año_medio = Año + 2,
               Precipitacion_media = round(Precipitacion_Acumulada, 0))
      max_y <- max(pp_acum$Precipitacion_Acumulada)
      
      
      pp_climatico <- ggplot(pp_acum, 
                             aes(x = Año, 
                                 y = Precipitacion_Acumulada)) +
        geom_bar(stat = "identity", 
                 fill = "#007EA7", 
                 color = "#003459", 
                 alpha = 0.5) +
        labs(x = "", 
             y = "mm", 
             title = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, 
                                         hjust = 1)) +
        geom_text(data = etiquetas_10_anios,
                  aes(x = Año_medio, y = max_y, label = Precipitacion_media),
                  vjust = -0.5, color = "black", size = 3.5)
    }
    
    ggplotly(pp_climatico) 
  })
    
  
  output$grafico_heladas_climatico <- renderPlotly({
    
    datos_filtrados_climatico <- datasetInput_climatico()

    dias_extremos <- datos_filtrados_climatico %>%
  mutate(dia_bajo = Temperatura_Abrigo_150cm_Minima < 3,
         dia_alto = Temperatura_Abrigo_150cm_Maxima > 25) %>%
    group_by(Año) %>%
    summarise(
      dias_bajos = sum(dia_bajo, na.rm = TRUE),
      dias_altos = sum(dia_alto, na.rm = TRUE)
    ) %>%
    arrange(Año)
    
    
    if (input$mes_climatico == "Anual") {
      
      # Gráfico para el número de días por año
      dias_extremos_long <- dias_extremos %>%
        pivot_longer(cols = c(dias_bajos, dias_altos), 
                     names_to = "Tipo", 
                     values_to = "Numero_de_dias")%>%
        mutate(Tipo = factor(Tipo,
                             levels = c("dias_bajos",
                                        "dias_altos"),                                                  
                             labels = c("Días Temp. Mín. < 3ºC",
                                        "Días Temp. Máx. > 25ºC")))
      
     
      
      
      tt_dias_extremos <- ggplot(dias_extremos_long, 
                                 aes(x = Año, y = Numero_de_dias, color = Tipo, group = Tipo)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", aes(group = Tipo), se = TRUE, linetype = "dashed", size = 0.8) +
        labs(x = "", y = "Días", 
             title = "",
             color = "") +
        scale_color_manual(values = c("Días Temp. Mín. < 3ºC" = "#A8DADC", "Días Temp. Máx. > 25ºC" = "#E63946")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
      
    } else {
      
      # Gráfico para el número de días por mes
      dias_extremos_mes <- datos_filtrados_climatico %>%
        mutate(dia_bajo = Temperatura_Abrigo_150cm_Minima < 3,
               dia_alto = Temperatura_Abrigo_150cm_Maxima > 25) %>%
        group_by(Año) %>%
        summarise(
          dias_bajos = sum(dia_bajo, na.rm = TRUE),
          dias_altos = sum(dia_alto, na.rm = TRUE)
        ) 
      # %>%
      #   mutate(Mes = factor(Mes,
      #                       
      #                       labels = c("enero", "febrero", "marzo", "abril",
      #                                  "mayo", "junio", "julio", "agosto", 
      #                                  "septiembre", "octubre", "noviembre", "diciembre")))
      
      dias_extremos_long_mes <- dias_extremos_mes %>%
        pivot_longer(cols = c(dias_bajos, dias_altos), 
                     names_to = "Tipo", 
                     values_to = "Numero_de_dias") %>%
        mutate(Tipo = factor(Tipo,
                             levels = c("dias_bajos",
                                        "dias_altos"),                                                  
                             labels = c("Días < 3ºC",
                                        "Días > 25ºC")))
      
      tt_dias_extremos <- ggplot(dias_extremos_long_mes, 
                                 aes(x = Año, y = Numero_de_dias, color = Tipo, group = Tipo)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", aes(group = Tipo), se = TRUE, linetype = "dashed", size = 0.8) +
        labs(x = "", y = "Número de Días", 
             title = "",
             color = "") +
        scale_color_manual(values = c("Días < 3ºC" = "#A8DADC", "Días > 25ºC" = "#E63946")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, 
                                         hjust = 1))
    }
    
    ggplotly(tt_dias_extremos) %>% 
      layout(legend = list(orientation = "h", 
                           x = 0.1, y = 1.3))
  })
  
  
  ##### AMBIENTE #####
  
  # data_usuario <- reactive({
  #   if (is.null(input$file_precip_riego)) {
  #     return(NULL)  # Si no hay archivo subido, devuelve NULL
  #   }
  #   
  #   ext <- tools::file_ext(input$file_precip_riego$name)
  #   
  #   if (ext == "csv") {
  #     data <- read.csv(input$file_precip_riego$datapath)
  #   } else if (ext == "xlsx") {
  #     data <- readxl::read_xlsx(input$file_precip_riego$datapath)
  #   } else {
  #     showNotification("Formato de archivo no soportado.", type = "error")
  #     return(NULL)
  #   }
  #   
  #   # Verificar si el archivo tiene las columnas requeridas
  #   required_columns <- c("Fecha", "Lluvia", "Riego")
  #   if (all(required_columns %in% colnames(data))) {
  #     showNotification("Archivo subido correctamente.", type = "message")
  #   } else {
  #     showNotification("El archivo no tiene las columnas requeridas: Fecha, Lluvia, Riego.", type = "error")
  #     return(NULL)  # Si no tiene las columnas requeridas, devolver NULL
  #   }
  #   
  #   return(data)
  # })
  
  # datos_actualizados <- reactive({
  #   # Si no hay archivo subido, usar los datos originales
  #   if (is.null(data_usuario())) {
  #     return(datos)  # Usar los datos originales si no se sube archivo
  #   }
  #   
  #   # Si hay archivo subido, procesar los datos del usuario
  #   data_user <- data_usuario() %>%
  #     mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))
  #   
  #   # Combinamos los datos del archivo con los datos existentes
  #   datos_actualizados <- datos %>%
  #     mutate(Fecha = as.Date(Fecha)) %>%
  #     left_join(data_user, by = "Fecha", suffix = c("", "_usuario")) %>%
  #     mutate(
  #       Precipitacion_Pluviometrica = coalesce(Lluvia_usuario, Precipitacion_Pluviometrica),  # Reemplazar precipitaciones
  #       Riego = Riego_usuario  # Agregar la columna de riego
  #     ) %>%
  #     select(-Lluvia_usuario, -Riego_usuario)  # Limpiar columnas extra
  #   
  #   return(datos_actualizados)
  # })
  
 periodo_critico <- reactive({
    
    req(input$mes_siembra_ambiente, input$dia_siembra_ambiente)
    
    dia_siembra <- as.numeric(input$dia_siembra_ambiente)
    mes_siembra <- as.numeric(input$mes_siembra_ambiente)
    
    # fecha_siembra_dia_mes <- sprintf("%02d-%02d", dia_siembra, mes_siembra)
    # fecha_siembra_dia_mes <- as.Date(fecha_siembra_dia_mes, format = "%d-%m")
    fecha_siembra_dia_mes <- as.Date(sprintf("%02d-%02d", mes_siembra, dia_siembra), format = "%m-%d")
    
    cultivo <- input$cultivo_ambiente

    if (cultivo == "maiz") {
      gd_min <- 670
      gd_max <- 1120
    } else if (cultivo == "soja") {
      # gd_min <- 620
      # gd_max <- 1010
      
      
      dia_juliano <- as.numeric(format(fecha_siembra_dia_mes, "%j"))
      
      if (dia_juliano < 60) {
        dia_juliano <- dia_juliano + 365
      }
      
      gd_min <- round(((-0.0476 * (dia_juliano^2)) + (30.212 * (dia_juliano)) - 4047.4), 0)
      gd_max <- round(((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9), 0)
    }
    
    historicos_periodo_critico <- data.frame(
      ano = integer(),
      lluvia_acumulada = numeric(),
      etp_acumulada = numeric(),
      radiacion_global_media = numeric(),
      dias_mayores_35 = numeric()
    )
    
    
    for (ano in 1991:2024) {
      
      # Filtrar los datos de cada año específico
      datos_ano <- datos %>%
        filter(
          (format(Fecha, "%Y") == as.character(ano) & format(Fecha, "%m-%d") >= format(fecha_siembra_dia_mes, "%m-%d")) |
            (format(Fecha, "%Y") == as.character(ano + 1) & format(Fecha, "%m-%d") <= "12-30")
        ) %>%
        arrange(Fecha) %>%
        mutate(
          Dia_Mes = format(Fecha, "%m-%d")
        ) %>%
        mutate(
          TTB = case_when(
            cultivo == "maiz" ~ if_else(Temperatura_Abrigo_150cm - 8 < 0, 0, Temperatura_Abrigo_150cm - 8),
            cultivo == "soja" ~ if_else(Temperatura_Abrigo_150cm - 11 < 0, 0, Temperatura_Abrigo_150cm - 11)
            # ,
            # TRUE ~ if_else(Temperatura_Abrigo_150cm - 9 < 0, 0, Temperatura_Abrigo_150cm - 9)
          ),
          GD_acum = cumsum(TTB)
        )

      datos_periodo_critico <- datos_ano %>%
        filter(GD_acum >= gd_min & GD_acum <= gd_max)
      
      lluvia_acumulada <- 0
      etp_acumulada <- 0
      radiacion_global_media <- NA
      dias_mayores_35 <- 0
      
      if (nrow(datos_periodo_critico) > 0) {
        lluvia_acumulada <- sum(round(datos_periodo_critico$Precipitacion_Pluviometrica, 0), na.rm = TRUE)
        etp_acumulada <- sum(round(datos_periodo_critico$Evapotranspiracion_Potencial, 0), na.rm = TRUE)
        radiacion_global_media <- mean(round(datos_periodo_critico$Radiacion_Global, 0), na.rm = TRUE)
        dias_mayores_35 <- sum(datos_periodo_critico$Temperatura_Abrigo_150cm_Maxima >= 35, na.rm = TRUE)
      }
      
      
      historicos_periodo_critico <- add_row(
        historicos_periodo_critico,
        data.frame(
          ano = ano, 
          lluvia_acumulada = lluvia_acumulada,
          etp_acumulada = etp_acumulada,
          radiacion_global_media = radiacion_global_media,
          dias_mayores_35 = dias_mayores_35
        )
      )
    }
    
    return(historicos_periodo_critico)

  })

  output$pp_acum_PC <- renderPlotly({
    historicos_periodo_critico <- periodo_critico()

    historicos_periodo_critico$decada <- ifelse(historicos_periodo_critico$ano >= 2010,
                                                2010, 
                                                floor(historicos_periodo_critico$ano / 10) * 10)
    historicos_periodo_critico <- historicos_periodo_critico %>%
      filter(decada <= 2024)
    
    mediana_lluvia_decada <- historicos_periodo_critico %>%
      group_by(decada) %>%
      reframe(mediana_lluvia = median(lluvia_acumulada, na.rm = TRUE),
                mediana_etp = median(etp_acumulada, na.rm = TRUE),
                decada_fin = ifelse(decada == 2010, 2024, decada + 10))
    
    pp_acum <- ggplot(historicos_periodo_critico, 
                      aes(x = ano)) +
      geom_bar(aes(y = etp_acumulada, fill = "ET0 Acumulada"), 
               stat = "identity", color = "#8C1C13", alpha = 0.5) +
      geom_bar(aes(y = lluvia_acumulada, fill = "Lluvia Acumulada"), 
               stat = "identity", color = "#003459", alpha = 0.5) +
      geom_segment(data = mediana_lluvia_decada,
                   aes(x = decada, xend = decada_fin,
                       y = mediana_lluvia + 100, yend = mediana_lluvia + 100),
                   linetype = "dashed", color = "#284B63", linewidth = 0.5) +
      geom_text(data = mediana_lluvia_decada,
                aes(x = (decada + decada_fin) / 2, y = mediana_lluvia + 120,
                    label = paste0(round(mediana_lluvia, 0), " mm")),
                color = "#284B63", size = 3, vjust = -0.5) +
      ylim(0, 350) +
      labs(x = "Año", 
           y = "(mm)", 
           fill = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_x_continuous(breaks = seq(min(historicos_periodo_critico$ano), 
                                      max(historicos_periodo_critico$ano), by = 5)) +
      scale_fill_manual(values = c("ET0 Acumulada" = "#BF4342", "Lluvia Acumulada" = "#007EA7")) 
    
    ggplotly(pp_acum) %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = 1.2))
    
  })
  
  output$rad_temp <- renderPlotly({
    historicos_periodo_critico <- periodo_critico()

    historicos_periodo_critico$decada <- ifelse(historicos_periodo_critico$ano >= 2011,
                                                2011, 
                                                floor(historicos_periodo_critico$ano / 11) * 11)
    historicos_periodo_critico <- historicos_periodo_critico %>%
      filter(decada <= 2024)
    
    mediana_dias_35_decada <- historicos_periodo_critico %>%
      group_by(decada) %>%
      reframe(mediana_dias_mayores_35 = median(dias_mayores_35, na.rm = TRUE),
                decada_fin = ifelse(decada == 2011, 2024, decada + 10))
    
    
    historicos_long <- historicos_periodo_critico %>%
      pivot_longer(cols = c(radiacion_global_media, dias_mayores_35), 
                   names_to = "variable", 
                   values_to = "valor")
    
    promedio_radiacion <- mean(historicos_periodo_critico$radiacion_global_media, na.rm = TRUE)
    promedio_dias_mayores_35 <- median(historicos_periodo_critico$dias_mayores_35, na.rm = TRUE)
    
    rad_temp <- ggplot(historicos_long, aes(x = ano, 
                                            y = valor, 
                                            fill = variable)) +
      geom_bar(stat = "identity", 
               position = "dodge") +
      annotate("text", x = 2016, y = 47, label = paste0("Radiación\n(1991-2020): ", round(promedio_radiacion, 0), " Mj / m2 * día"),
               color = "#FFBC42", size = 3.5, vjust = 2) +
      annotate("text", x = 1995, y = 47, label = paste0("Días temp. máx. > 35ºC\n(1991-2020): ", round(promedio_dias_mayores_35, 0)),
               color = "#D00000", size = 3.5, vjust = 2) +
      # geom_segment(data = mediana_dias_35_decada,
      #              aes(x = decada, xend = decada + 10,
      #                  y = mediana_dias_mayores_35 + 20, yend = mediana_dias_mayores_35 + 20),
      #              linetype = "dashed", color = "#D00000", size = 1) +
      # geom_text(data = mediana_dias_35_decada,
      #           aes(x = decada + 5, y = mediana_dias_mayores_35 + 30,
      #               label = paste0(round(mediana_dias_mayores_35, 0), " mm")),
      #           color = "#D00000", size = 3.5, vjust = -0.5) +
      labs(
        title = "",
        x = "Año",
        y = "",
        color = "Variables",
        fill = ""
      ) +
      ylim(0, 50) +
      scale_fill_manual(name = "", values = c("radiacion_global_media" = "#FFBC42",
                                              "dias_mayores_35" = "#D00000")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_x_continuous(breaks = seq(min(historicos_periodo_critico$ano), 
                                      max(historicos_periodo_critico$ano), by = 5)) +
      guides(fill = "none")

    ggplotly(rad_temp) %>%
      layout(legend = list(orientation = "h", x = 0.05, y = 1.2))
    
  })
  
  ####### Comparación fechas de siembra ############
    
    calcular_periodo_critico <- function(mes_siembra, dia_siembra, cultivo, datos, datos_historicos_avg) {
      
      
      dia_siembra <- as.numeric(dia_siembra)
      mes_siembra <- as.numeric(mes_siembra)
      
      fecha_siembra_dia_mes <- as.Date(sprintf("%02d-%02d", mes_siembra, dia_siembra), format = "%m-%d")
      
      # cultivo <- input$cultivo_ambiente
      
      if (cultivo == "maiz") {
        
        gd_min <- 670
        gd_max <- 1120
        
      } else if (cultivo == "soja") {
        
        dia_juliano <- as.numeric(format(fecha_siembra_dia_mes, "%j"))
        
        if (dia_juliano < 40) {
          dia_juliano <- dia_juliano + 365
        }
        
        gd_min <- round(((-0.0476 * (dia_juliano^2)) + (30.212 * (dia_juliano)) - 4047.4), 0)
        gd_max <- round(((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9), 0)
      }
      
      
      historicos_periodo_critico <- data.frame(
        ano = integer(),
        lluvia_acumulada = numeric(),
        etp_acumulada = numeric(),
        radiacion_global_media = numeric(),
        dias_mayores_35 = numeric(),
        inicio = as.Date(character()),  
        fin = as.Date(character())
      )
      
      for (ano in 2010:2025) {
        datos_ano <- datos %>%
          filter(
            (format(Fecha, "%Y") == as.character(ano) & format(Fecha, "%m-%d") >= format(fecha_siembra_dia_mes, "%m-%d")) |
              (format(Fecha, "%Y") == as.character(ano + 1) & format(Fecha, "%m-%d") < "12-31")
          ) %>%
          arrange(Fecha) %>%
          mutate(
            Dia_Mes = format(Fecha, "%m-%d")
          ) %>%
          mutate(
            TTB = case_when(
              cultivo == "maiz" ~ if_else(Temperatura_Abrigo_150cm - 8 < 0, 0, Temperatura_Abrigo_150cm - 8),
              cultivo == "soja" ~ if_else(Temperatura_Abrigo_150cm - 11 < 0, 0, Temperatura_Abrigo_150cm - 11)
              # ,
              # TRUE ~ if_else(Temperatura_Abrigo_150cm - 9 < 0, 0, Temperatura_Abrigo_150cm - 9)
            ),
            GD_acum = cumsum(TTB)
          )
        
        datos_periodo_critico <- datos_ano %>%
          filter(GD_acum >= gd_min & GD_acum <= gd_max)
        
        fecha_inicio <- if (nrow(datos_periodo_critico) > 0) min(datos_periodo_critico$Fecha) else NA
        fecha_fin <- if (nrow(datos_periodo_critico) > 0) max(datos_periodo_critico$Fecha) else NA
        
        
        
        lluvia_acumulada <- if (nrow(datos_periodo_critico) > 0) {
          sum(round(datos_periodo_critico$Precipitacion_Pluviometrica, 0), na.rm = TRUE)
        } else 0
        
        etp_acumulada <- if (nrow(datos_periodo_critico) > 0) {
          sum(round(datos_periodo_critico$Evapotranspiracion_Potencial, 0), na.rm = TRUE)
        } else 0
        
        radiacion_global_media <- if (nrow(datos_periodo_critico) > 0) {
          mean(round(datos_periodo_critico$Radiacion_Global, 0), na.rm = TRUE)
        } else NA
        
        dias_mayores_35 <- if (nrow(datos_periodo_critico) > 0) {
          sum(datos_periodo_critico$Temperatura_Abrigo_150cm_Maxima >= 35, na.rm = TRUE)
        } else 0
        
        historicos_periodo_critico <- rbind(
          historicos_periodo_critico,
          data.frame(
            ano = ano,
            lluvia_acumulada = lluvia_acumulada,
            etp_acumulada = etp_acumulada,
            radiacion_global_media = radiacion_global_media,
            dias_mayores_35 = dias_mayores_35,
            inicio = fecha_inicio,  
            fin = fecha_fin 
          )
        )
      }
      
     
      # Convertir las fechas a día juliano
      historicos_periodo_critico$inicio_juliano <- yday(historicos_periodo_critico$inicio)
      historicos_periodo_critico$fin_juliano <- yday(historicos_periodo_critico$fin)
      
      historicos_periodo_critico$inicio_juliano <- ifelse(historicos_periodo_critico$inicio_juliano < 100, 
                                                          historicos_periodo_critico$inicio_juliano + 365, 
                                                          historicos_periodo_critico$inicio_juliano)

      
      
      # Calcular el promedio de los días julianos
      promedio_inicio_juliano <- mean(historicos_periodo_critico$inicio_juliano, na.rm = TRUE)
      promedio_fin_juliano <- mean(historicos_periodo_critico$fin_juliano, na.rm = TRUE)
      
      
      # Convertir los días julianos promedio de vuelta a fechas
      fecha_promedio_inicio <- as.Date(promedio_inicio_juliano - 1, origin = "2024-01-01")
      fecha_promedio_fin <- as.Date(promedio_fin_juliano - 1, origin = "2024-01-01")
      
      
      fecha_inicio <- format(fecha_promedio_inicio, "%d-%m")
      fecha_fin <- format(fecha_promedio_fin, "%d-%m")

      return(
        list(
          historicos = historicos_periodo_critico,
          fecha_inicio = fecha_inicio,
          fecha_fin = fecha_fin
        )
      )
    }
  
    periodo_critico1 <- reactive({
      req(input$mes_siembra1, input$dia_siembra1, input$cultivo_fecha)
      calcular_periodo_critico(input$mes_siembra1, input$dia_siembra1, input$cultivo_fecha, datos, datos_historicos_avg)
    })
    
    periodo_critico2 <- reactive({
      req(input$mes_siembra2, input$dia_siembra2, input$cultivo_fecha)
      calcular_periodo_critico(input$mes_siembra2, input$dia_siembra2, input$cultivo_fecha, datos, datos_historicos_avg)
    })
    
    periodo_critico3 <- reactive({
      req(input$mes_siembra3, input$dia_siembra3, input$cultivo_fecha)
      calcular_periodo_critico(input$mes_siembra3, input$dia_siembra3, input$cultivo_fecha, datos, datos_historicos_avg)
    })
    
    
  
  observeEvent(input$btn_calcular, {
    historicos_periodo_critico1 <- periodo_critico1()
    historicos_periodo_critico2 <- periodo_critico2()
    historicos_periodo_critico3 <- periodo_critico3()
    
    output$fecha_inicio_pc1 <- renderText({
      paste("Fecha inicio periodo crítico:", as.character(periodo_critico1()$fecha_inicio))
    })
    
    output$fecha_fin_pc1 <- renderText({
      paste("Fecha fin periodo crítico:", as.character(periodo_critico1()$fecha_fin))
    })
    
    output$fecha_inicio_pc2 <- renderText({
      paste("Fecha inicio periodo crítico:", as.character(periodo_critico2()$fecha_inicio))
    })
    
    output$fecha_fin_pc2 <- renderText({
      paste("Fecha fin periodo crítico:", as.character(periodo_critico2()$fecha_fin))
    })
    
    output$fecha_inicio_pc3 <- renderText({
      paste("Fecha inicio periodo crítico:", as.character(periodo_critico3()$fecha_inicio))
    })
    
    output$fecha_fin_pc3 <- renderText({
      paste("Fecha fin periodo crítico:", as.character(periodo_critico3()$fecha_fin))
    })
    
    #Lluvia
  output$gg_lluvia <- renderPlotly({
    historicos_periodo_critico1 <- periodo_critico1()$historicos
    historicos_periodo_critico2 <- periodo_critico2()$historicos
    historicos_periodo_critico3 <- periodo_critico3()$historicos
    
    fechas <- c(paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
                paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
                paste(input$dia_siembra3, input$mes_siembra3,  sep = "-"))
    
    # Verificar si las fechas son únicas
    if (length(unique(fechas)) < length(fechas)) {
      # Si hay duplicados, mostrar un gráfico en blanco con un mensaje
      plot_blank <- ggplot() + 
        theme_void() + 
        annotate("text", x = 1, y = 1, label = "Por favor, elija 3 fechas diferentes.", size = 5, color = "red") +
        labs(title = "")
      
      return(plot_blank)
      }
    
    # Calcular estadísticas
    mediana_lluvia1 <- median(round(historicos_periodo_critico1$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp1 <- median(round(historicos_periodo_critico1$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion1 <- mean(round(historicos_periodo_critico1$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_1 <- median(round(historicos_periodo_critico1$dias_mayores_35, 0), na.rm = TRUE)
    
    mediana_lluvia2 <- median(round(historicos_periodo_critico2$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp2 <- median(round(historicos_periodo_critico2$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion2 <- mean(round(historicos_periodo_critico2$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_2 <- median(round(historicos_periodo_critico2$dias_mayores_35, 0), na.rm = TRUE)
    
    mediana_lluvia3 <- median(round(historicos_periodo_critico3$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp3 <- median(round(historicos_periodo_critico3$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion3 <- mean(round(historicos_periodo_critico3$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_3 <- median(round(historicos_periodo_critico3$dias_mayores_35, 0), na.rm = TRUE)
    
    # Crear un nuevo df con las estadísticas
    estadisticas <- data.frame(
      fecha = fechas,
      lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
      etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
      radiacion_media = c(promedio_radiacion1, promedio_radiacion2, promedio_radiacion3),
      dias_mayores_35 = c(mediana_dias_mayores_35_1, mediana_dias_mayores_35_2, mediana_dias_mayores_35_3)
    )
    
    estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)
    
    
    # Graficar
    gg_lluvia <- ggplot(estadisticas, 
                           aes(x = fecha, 
                               y = lluvia_acumulada, 
                               fill = fecha)) +
      geom_bar(aes(y = lluvia_acumulada, fill = fecha), 
               stat = "identity", 
               position = position_dodge(0.9)) +
      geom_bar(aes(y = etp_acumulada, fill = fecha), 
               stat = "identity", 
               alpha = 0.4,   
               position = position_dodge(0.9)) +
      geom_text(aes(y = lluvia_acumulada, label = lluvia_acumulada), 
                position = position_dodge(0.9),  
                vjust = -0.5, size = 3) +
      geom_text(aes(y = etp_acumulada, label = etp_acumulada), 
                position = position_dodge(0.9), 
                vjust = -0.5, size = 3, color = "black") +
      labs(title = "",
           x = "",
           y = " mm",
           fill = NULL) +
      theme_minimal() +
      scale_fill_manual(name = "", values = c("#E07A5F", "#3D405B", "#AEC3B0")) +
      theme(axis.text.x = element_text(size = 10),
            axis.ticks.x = element_blank()) +
      guides(fill = "none")
    
    
    ggplotly(gg_lluvia) 
  })
  
  
    #Radiacion
  output$gg_radiacion <- renderPlotly({
    historicos_periodo_critico1 <- periodo_critico1()$historicos
    historicos_periodo_critico2 <- periodo_critico2()$historicos
    historicos_periodo_critico3 <- periodo_critico3()$historicos
    
    fechas <- c(paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
                paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
                paste(input$dia_siembra3, input$mes_siembra3,  sep = "-"))
    
    # Verificar si las fechas son únicas
    if (length(unique(fechas)) < length(fechas)) {
      # Si hay duplicados, mostrar un gráfico en blanco con un mensaje
      plot_blank <- ggplot() + 
        theme_void() + 
        annotate("text", x = 1, y = 1, label = "Por favor, elija 3 fechas diferentes.", size = 5, color = "red") +
        labs(title = "")
      
      return(plot_blank)
    }
    
    # Calcular estadísticas
    mediana_lluvia1 <- median(round(historicos_periodo_critico1$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp1 <- median(round(historicos_periodo_critico1$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion1 <- mean(round(historicos_periodo_critico1$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_1 <- median(round(historicos_periodo_critico1$dias_mayores_35, 0), na.rm = TRUE)
    
    mediana_lluvia2 <- median(round(historicos_periodo_critico2$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp2 <- median(round(historicos_periodo_critico2$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion2 <- mean(round(historicos_periodo_critico2$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_2 <- median(round(historicos_periodo_critico2$dias_mayores_35, 0), na.rm = TRUE)
    
    mediana_lluvia3 <- median(round(historicos_periodo_critico3$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp3 <- median(round(historicos_periodo_critico3$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion3 <- mean(round(historicos_periodo_critico3$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_3 <- median(round(historicos_periodo_critico3$dias_mayores_35, 0), na.rm = TRUE)
    
    # Crear un nuevo dataframe con las estadísticas
    estadisticas <- data.frame(
      fecha = fechas,
      lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
      etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
      radiacion_media = c(promedio_radiacion1, promedio_radiacion2, promedio_radiacion3),
      dias_mayores_35 = c(mediana_dias_mayores_35_1, mediana_dias_mayores_35_2, mediana_dias_mayores_35_3)
    )
    
    estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)
    
    # Graficar
    gg_radiacion <- ggplot(estadisticas, aes(x = fecha, y = radiacion_media, fill = fecha)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(radiacion_media)), 
                position = position_stack(vjust = 1.05),  
                size = 3) +
      labs(title = "",
           x = "",
           y = " Mj / m2 * día") +
      theme_minimal() +
      scale_fill_manual(name = "", values = c("#E07A5F", "#3D405B", "#AEC3B0")) +
      theme(axis.text.x = element_text(size = 10),
            axis.ticks.x = element_blank()) +
      guides(fill = "none") 
    
    
    ggplotly(gg_radiacion) 
  })
  
  #Dias>35ºC
  output$gg_dias_35 <- renderPlotly({
    historicos_periodo_critico1 <- periodo_critico1()$historicos
    historicos_periodo_critico2 <- periodo_critico2()$historicos
    historicos_periodo_critico3 <- periodo_critico3()$historicos
    
    fechas <- c(paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
                paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
                paste(input$dia_siembra3, input$mes_siembra3,  sep = "-"))
    
    # Verificar si las fechas son únicas
    if (length(unique(fechas)) < length(fechas)) {
      # Si hay duplicados, mostrar un gráfico en blanco con un mensaje
      plot_blank <- ggplot() + 
        theme_void() + 
        annotate("text", x = 1, y = 1, label = "Por favor, elija 3 fechas diferentes.", size = 5, color = "red") +
        labs(title = "")
      
      return(plot_blank)
    }
    
    # Calcular estadísticas
    mediana_lluvia1 <- median(round(historicos_periodo_critico1$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp1 <- median(round(historicos_periodo_critico1$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion1 <- mean(round(historicos_periodo_critico1$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_1 <- median(round(historicos_periodo_critico1$dias_mayores_35, 0), na.rm = TRUE)
    
    mediana_lluvia2 <- median(round(historicos_periodo_critico2$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp2 <- median(round(historicos_periodo_critico2$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion2 <- mean(round(historicos_periodo_critico2$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_2 <- median(round(historicos_periodo_critico2$dias_mayores_35, 0), na.rm = TRUE)
    
    mediana_lluvia3 <- median(round(historicos_periodo_critico3$lluvia_acumulada, 0), na.rm = TRUE)
    mediana_etp3 <- median(round(historicos_periodo_critico3$etp_acumulada, 0), na.rm = TRUE)
    promedio_radiacion3 <- mean(round(historicos_periodo_critico3$radiacion_global_media, 0), na.rm = TRUE)
    mediana_dias_mayores_35_3 <- median(round(historicos_periodo_critico3$dias_mayores_35, 0), na.rm = TRUE)
    
    # Crear un nuevo dataframe con las estadísticas
    estadisticas <- data.frame(
      fecha = fechas,
      lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
      etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
      radiacion_media = c(promedio_radiacion1, promedio_radiacion2, promedio_radiacion3),
      dias_mayores_35 = c(mediana_dias_mayores_35_1, mediana_dias_mayores_35_2, mediana_dias_mayores_35_3)
    )
    
    estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)
    
    # Graficar
    gg_dias_35 <- ggplot(estadisticas, aes(x = fecha, y = dias_mayores_35, fill = fecha)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = dias_mayores_35), 
                position = position_stack(vjust = 1.05),  
                size = 3) +
      labs(title = "",
           x = "",
           y = "# días") +
      theme_minimal() +
      scale_fill_manual(name = "", values = c("#E07A5F", "#3D405B", "#AEC3B0")) +
      theme(axis.text.x = element_text(size = 10),
            axis.ticks.x = element_blank()) +
      guides(fill = "none") 
    
    
    ggplotly(gg_dias_35) 
  })
  })
  
  
  ##### Balance de agua ########
  
  observeEvent(input$cultivo, {
    
    if (input$cultivo == "maiz") {
      updateNumericInput(session, "umbral_et", value = 0.8)

    } else if (input$cultivo == "soja") {
      updateNumericInput(session, "umbral_et", value = 0.5)
    }
    
  })

  output$mensaje_cultivo <- renderUI({
    if (input$cultivo == "maiz") {
      tagList(
        p("Híbrido de maíz de 1890 GD (grados-días) a madurez fisiológica 
            (aprox. 160-170 días de siembra a madurez fisiológica, para siembras de mediados de octubre en Balcarce)."),
        p("Los recuadros en los gráficos indican el período crítico, correspondiente a 670 - 1120 GD (grados-días) desde la siembra."),
        p(tags$sup("2"), ": Valores de referencia para un suelo Argiudol típico.")
      )
    } else if (input$cultivo == "soja") {
      tagList(
        p("Variedad de soja de grupo 3."),
        p("Los recuadros en los gráficos indican el período crítico."),
        p(tags$sup("2"), ": Valores de referencia para un suelo Argiudol típico.")
      )
    }
  })
  
  
  
  
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
  
  # GD por cultivo
  GD <- reactive({
    
   if (input$cultivo == "maiz") {
      return(1890)
      
    } else if (input$cultivo == "soja") {
      
      dia_juliano <- yday(input$fecha_siembra)
      
      if (dia_juliano < 60) {
        dia_juliano <- dia_juliano + 365
      }

      GD_soja <- round(((-0.0466 * (dia_juliano^2)) + (26.344 * (dia_juliano)) - 2479.9), 0)


      return(round(GD_soja))
    } 
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
  
  output$GD <- renderText({
    paste("Grados-Días:", GD())
  })
  
  balance_agua <- reactive({
    fecha_siembra <- as.Date(input$fecha_siembra)
    
    datos_filtrados <- datos %>%
      filter(Fecha >= fecha_siembra) %>%
      select(Fecha, Temperatura_Abrigo_150cm, Precipitacion_Pluviometrica, Evapotranspiracion_Potencial)
    
    # if (is.null(input$file1)) {
    #   # Si no se sube archivo, usar los datos originales
    #   datos_filtrados <- datos %>%
    #     filter(Fecha >= fecha_siembra) %>%
    #     select(Fecha, Temperatura_Abrigo_150cm, Precipitacion_Pluviometrica, Evapotranspiracion_Potencial)
    # } else {
    #   # Si se sube archivo, utilizar los datos subidos
    #   datos_actualizados <- read.csv(input$file1$datapath)  # Leer archivo subido
    #   datos_filtrados <- datos_actualizados %>%
    #     filter(Fecha >= fecha_siembra) %>%
    #     select(Fecha, Temperatura_Abrigo_150cm, Precipitacion_Pluviometrica, Evapotranspiracion_Potencial)
    # }
    
    datos_filtrados <- datos_filtrados %>%
      mutate(Dia_Mes = format(Fecha, "%m-%d"))
    
    fraccion_inicial <- input$fraccion_inicial
    agua_util_total_val <- agua_util_total()
    disminucion_et_val <- disminucion_et()
    GD <- GD()
    
    
    datos_filtrados <- datos_filtrados %>%
      # left_join(datos_historicos_avg, by = "Dia_Mes") %>%
      arrange(Fecha) %>%
      mutate(
        TTB = case_when(
          input$cultivo == "maiz" ~ if_else(Temperatura_Abrigo_150cm - 8 < 0, # Umbral para maíz
                                            0, 
                                            Temperatura_Abrigo_150cm - 8),  
          input$cultivo == "soja" ~ if_else(Temperatura_Abrigo_150cm - 11 < 0, # Umbral para soja
                                            0, 
                                            Temperatura_Abrigo_150cm - 11)
          # ,  
          # TRUE ~ if_else(Temperatura_Abrigo_150cm - 9 < 0, 
          #                0, 
          #                Temperatura_Abrigo_150cm - 9)  
        ),
        GD_acum = cumsum(TTB),
        Ttrelativo = GD_acum / GD,
        
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
      
      if (datos_filtrados$GD_acum[i - 1] > GD) {
        break
      }
      
      datos_filtrados$ETR[i] <- if_else(
        is.na(datos_filtrados$Fr_agua_util[i - 1]) | is.na(datos_filtrados$ETM[i]), 
        NA_real_, 
        if_else(
          datos_filtrados$Fr_agua_util[i - 1] >= input$umbral_et, 
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
    
  })
  
  ## Gráficos balance de agua ##
  output$agua_util <- renderPlotly({
    GD <- GD()
    df_siembra <- balance_agua()
    df_siembra <- df_siembra %>% filter(GD_acum <= GD)
    
    fecha_actual <- Sys.Date()
    
    if (input$cultivo == "maiz") {
      fecha_min <- min(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= 670], na.rm = TRUE)
      fecha_max <- max(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= 1120], na.rm = TRUE)
      color_rect <- "cornsilk3"
      
    } else if (input$cultivo == "soja") {

      dia_juliano <- yday(input$fecha_siembra)
      
      if (dia_juliano < 60) {
        dia_juliano <- dia_juliano + 365
      }

      GD_R3 <- round(((-0.0476 * (dia_juliano^2)) + (30.212 * (dia_juliano)) - 4047.4), 0)
      GD_R6 <- round(((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9), 0)

      
      fecha_min <- min(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_R3], na.rm = TRUE)
      fecha_max <- max(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_R6], na.rm = TRUE)

      color_rect <- "darkgreen"
      
    } else {
      fecha_min <- NA
      fecha_max <- NA
    }
    
    agua_util <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_rect(aes(xmin = fecha_min, 
                    xmax = fecha_max, 
                    ymin = 0, ymax = 1),
                fill = color_rect, 
                alpha = 0.2, 
                color = NA) +
      geom_line(aes(y = Fr_agua_util , color = "Fr_agua_util")) +
      labs(title = "", x = "", 
           y = "Fracción de Agua Útil (0 - 1)") +
      theme_minimal() +
      scale_color_manual(values = c("#E9C46A")) +
      guides(color = "none") +
      coord_cartesian(ylim = c(0, NA))
    
    ggplotly(agua_util)  %>% 
      plotly::style(name = "Fracción de Agua Útil", traces = 1) 
  })
  
  output$consumo_agua <- renderPlotly({
    GD <- GD()
    df_siembra <- balance_agua()
    df_siembra <- df_siembra %>% filter(GD_acum <= GD)
    
    fecha_actual <- Sys.Date()
    
    if (input$cultivo == "maiz") {
      fecha_min <- min(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= 670], na.rm = TRUE)
      fecha_max <- max(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= 1120], na.rm = TRUE)
      color_rect <- "cornsilk3"
      
    } else if (input$cultivo == "soja") {

      dia_juliano <- yday(input$fecha_siembra)
      
      if (dia_juliano < 60) {
        dia_juliano <- dia_juliano + 365
      }

      GD_R3 <- round(((-0.0476 * (dia_juliano^2)) + (30.212 * (dia_juliano)) - 4047.4), 0)
      GD_R6 <- round(((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9), 0)
      
      fecha_min <- min(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_R3], na.rm = TRUE)
      fecha_max <- max(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_R6], na.rm = TRUE)
      color_rect <- "darkgreen"
      
    } else {
      
      # Valores por defecto o para otros cultivos
      fecha_min <- NA
      fecha_max <- NA
    }
    
    ymax_ETM <- max(df_siembra$ETM,
                    na.rm = TRUE)
    
    cons_agua <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_line(aes(y = ETM, color = "ETM")) +
      geom_line(aes(y = ETR, color = "ETR")) +
      geom_rect(aes(xmin = fecha_min,
                    xmax = fecha_max,
                    ymin = 0, ymax = ymax_ETM),
                fill = color_rect,
                alpha = 0.2,
                color = NA) +
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_color_manual(values = c("#E76F51", "#2A9D8F")) +
      guides(color = guide_legend(title = NULL))
    
    ggplotly(cons_agua) %>% 
      layout(legend = list(orientation = "v", x = 0.1, y = 1.3)) %>% 
      plotly::style(name = "ETM: Máximo consumo de agua si no hubiera deficiencias de agua", traces = 1)  %>% 
      plotly::style(name = "ETR: Consumo de agua REAL", traces = 2) 
  })
  
  
  output$deficit_agua <- renderPlotly({
    GD <- GD()
    df_siembra <- balance_agua()
    df_siembra <- df_siembra %>% filter(GD_acum <= GD)
    
    if (input$cultivo == "maiz") {
      
      fecha_min <- min(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= 670], na.rm = TRUE)
      fecha_max <- max(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= 1120], na.rm = TRUE)
      color_rect <- "cornsilk3"
      
    } else if (input$cultivo == "soja") {

      dia_juliano <- yday(input$fecha_siembra)
      
      if (dia_juliano < 60) {
        dia_juliano <- dia_juliano + 365
      }
      
      GD_R3 <- round(((-0.0476 * (dia_juliano^2)) + (30.212 * (dia_juliano)) - 4047.4), 0)
      GD_R6 <- round(((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9), 0)
      
      fecha_min <- min(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_R3], na.rm = TRUE)
      fecha_max <- max(df_siembra$Fecha[!is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_R6], na.rm = TRUE)
      color_rect <- "darkgreen"
      
    } else {
      
      # Valores por defecto o para otros cultivos
      fecha_min <- NA
      fecha_max <- NA
    }
    
    ymax_pp <- max(df_siembra$Precipitacion_Pluviometrica,
                   na.rm = TRUE)
    
    def_agua <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_bar(aes(y = Precipitacion_Pluviometrica, fill = "Precipitacion_Pluviometrica"),
               stat = "identity", position = "dodge") +
      geom_bar(aes(y = deficiencia, fill = "deficiencia"),
               stat = "identity", position = "dodge") +
      geom_rect(aes(xmin = fecha_min,
                    xmax = fecha_max,
                    ymin = 0, ymax = ymax_pp),
                fill = color_rect,
                alpha = 0.2,
                color = NA) +
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_fill_manual(values = c("#BC4749", "#007EA7")) +
      guides(fill = guide_legend(title = NULL))
    
    ggplotly(def_agua) %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = 1.2)) %>% 
      plotly::style(name = "Precipitación", traces = 1) %>% 
      plotly::style(name = "Déficit hídrico", traces = 2)
  })
  
  
  ## Dalbulus ##
 
  dalbulus_filtrados <- reactive({
    filtered_data <- dalbulus %>% filter(Fecha == as.Date(input$fecha_dalbulus, format = "%d/%m/%Y"))
  })
  
  
  pal <- colorFactor("viridis", levels = unique(dalbulus$MG))
  pal_poligono <- colorFactor(c("#BC4B51", "#F4A259", "#8CB369"), levels = c("Zona Alta", "Zona transición", "Zona Baja"))
  
  output$mapa_arg <- renderLeaflet({
    
    dalbulus_data <- dalbulus_filtrados()
    
    leaflet(data = dalbulus_data) %>%
      addTiles() %>%
      setView(lng = -65.0, lat = -31.5, zoom = 4) %>%
      
      addPolygons(
        lng = c(-66.9, -66.9, -56.80, -55.57, -56.04, -58.55, -57.67, -62.79),
        lat = c(-21.9, -29.8, -29.8, -28.15, -27.32, -27.24, -25.34, -21.9),
        color = "#BC4B51", fillColor = "#BC4B51", weight = 2, fillOpacity = 0.4,
        label = "Zona Alta Carga"
      ) %>%

      addPolygons(
        lng = c(-66.9, -66.9, -58.40, -57.50),
        lat = c(-29.8, -32.4, -32.4, -29.8),
        color = "#F4A259", fillColor = "#F4A259", weight = 2, fillOpacity = 0.4,
        label = "Zona Transición"
      ) %>%

      addPolygons(
        lng = c(-66.9, -66.9, -57.90, -56.77, -58.44, -58.40),
        lat = c(-32.4, -38.5, -38.5, -36.34, -34.57, -32.4),
        color = "#8CB369", fillColor = "#8CB369", weight = 2, fillOpacity = 0.4,
        label = "Zona Baja Carga"
      ) %>%
      
      addCircles(~lng, ~lat, radius = 30000) %>%
      addCircleMarkers(~lng, ~lat, color = ~pal(MG),
                       popup = ~paste0("<b>Estación: </b>", Nombre, "<hr>",
                                       "<b>Probabilidad de maíz guacho: </b>", MG),
                       label = ~Nombre) %>%
      addLegend(position = "bottomright",
                pal = pal_poligono, values = c("Zona Alta", "Zona transición", "Zona Baja"),
                title = "Capacidad de supervivencia del vector",
                opacity = 1)
  })

  
  
  
  
  ## Descarga de informes ##
  output$downloadReport <- downloadHandler(
    filename = function() {
      "ENSO.pdf"
    },
    content = function(file) {
      file.copy("www/ENSO.pdf", file)
    }
  )
  
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
      paste("Datos_metereologicos_balcarce_", Sys.Date(), ".xlsx", sep=",")
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
      write_xlsx(datos_filtrados3, 
                 file)
    }
  )
}





# Run the app ----
shinyApp(ui = ui, server = server)
# 
# rsconnect::setAccountInfo(name='8js2kq-nuria-lewczuk',
#                             token='111A9791794261A82E719E515597DD66',
#                             secret='m+wC8DYne2Mr69yhsLI/2i4QF7Zy5PQUFPIe5Xkr')
# rsconnect::deployApp(appDir = "E:/TRABAJO/CERBAS/GrupoAgrometeorologia/App_Meteo", appPrimaryDoc = "ui.R",
#                      appName = "MiAppMeteo", account = '8js2kq-nuria-lewczuk', server = 'shinyapps.io')