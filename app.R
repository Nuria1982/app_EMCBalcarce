library(rsconnect)
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
library(webshot)
library(httr)
library(jsonlite)
library(broom)
library(purrr)
library(minpack.lm)
library(httr2)
library(usethis)
library(zoo)


usethis::use_git_ignore(".Renviron")

#Cuando actualizamos Rstudio##
# renv::install("rsconnect")
# renv::install("rstudioapi")
# renv::snapshot()
#
# .rs.restartR()

# Forecast de Meteored ----
source("config.R", local = TRUE)

# message("After config.R - option length = ", nchar(getOption("METEORED_API_KEY")))

get_meteored_key <- function() {
  key <- Sys.getenv("METEORED_API_KEY")
  if (!nzchar(key)) {
    key <- getOption("METEORED_API_KEY")
  }
  if (!nzchar(key)) {
    stop("METEORED_API_KEY is missing (env var and option are empty).")
  }
  key
}

assert_meteored_key <- function() {
  key <- get_meteored_key()
  # message("Using METEORED key length = ", nchar(key))
  invisible(key)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

get_meteored_key <- function() {
  key <- Sys.getenv("METEORED_API_KEY")
  if (!nzchar(key)) {
    key <- getOption("METEORED_API_KEY")
  }
  if (!nzchar(key)) {
    stop("METEORED_API_KEY falta (env y option vacías).")
  }
  key
}

# (opcional) para logs
assert_meteored_key <- function() {
  key <- get_meteored_key()
  message("Using METEORED key length = ", nchar(key))
  invisible(key)
}

search_locations_txt <- function(text, base_url = "https://api.meteored.com") {
  api_key <- get_meteored_key()

  req <- httr2::request(paste0(
    base_url,
    "/api/location/v1/search/txt/",
    URLencode(text, reserved = TRUE)
  )) |>
    httr2::req_headers(`x-api-key` = api_key) |>
    httr2::req_timeout(20)

  j <- httr2::req_perform(req) |> httr2::resp_body_json()
  stopifnot(isTRUE(j$ok))

  locs <- j$data$locations
  if (length(locs) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(lapply(locs, function(x) {
    tibble::tibble(
      hash = x$hash,
      name = x$name,
      description = x$description,
      country_name = x$country_name
    )
  }))
}

get_forecast_daily5_full <- function(
  hash,
  base_url = "https://api.meteored.com"
) {
  api_key <- get_meteored_key()

  if (length(hash) != 1 || !nzchar(hash) || is.na(hash)) {
    stop("Hash inválido para forecast: ", paste(hash, collapse = ","))
  }

  url <- paste0(base_url, "/api/forecast/v1/daily/", hash)

  req <- httr2::request(url) |>
    httr2::req_headers(`x-api-key` = api_key) |>
    httr2::req_timeout(20)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    body_txt <- tryCatch(httr2::resp_body_string(resp), error = function(e) {
      "<no body>"
    })
    stop("Meteored HTTP ", status, " | ", substr(body_txt, 1, 400))
  }

  j <- httr2::resp_body_json(resp)
  if (!isTRUE(j$ok)) {
    stop("Meteored: ok != TRUE")
  }

  d <- j$data
  days <- NULL
  if (
    is.list(d) && length(d) > 0 && is.list(d[[1]]) && !is.null(d[[1]]$start)
  ) {
    days <- d
  }
  if (is.null(days) && is.list(d) && !is.null(d$days)) {
    days <- d$days
  }
  if (is.null(days) && is.list(d) && !is.null(d$forecast)) {
    days <- d$forecast
  }

  if (is.null(days) || length(days) == 0) {
    return(tibble::tibble(
      Fecha = as.Date(character()),
      Tmin = numeric(),
      Tmax = numeric(),
      Precipitacion_Pluviometrica = numeric(),
      Riego = numeric(),
      Temperatura_Abrigo_150cm = numeric(),
      Temperatura_Abrigo_150cm_Minima = numeric(),
      Temperatura_Abrigo_150cm_Maxima = numeric()
    ))
  }

  dplyr::bind_rows(lapply(days, function(x) {
    tmin <- as.numeric(x$temperature_min %||% x$temperature$min %||% NA_real_)
    tmax <- as.numeric(x$temperature_max %||% x$temperature$max %||% NA_real_)
    start_ms <- x$start %||% x$utime$start %||% NA_real_

    tibble::tibble(
      Fecha = as.Date(as.POSIXct(
        start_ms / 1000,
        origin = "1970-01-01",
        tz = "America/Argentina/Buenos_Aires"
      )),
      Tmin = tmin,
      Tmax = tmax,
      Precipitacion_Pluviometrica = as.numeric(x$rain %||% 0),
      Riego = 0
    )
  })) |>
    dplyr::arrange(Fecha) |>
    dplyr::distinct(Fecha, .keep_all = TRUE) |>
    dplyr::mutate(
      Temperatura_Abrigo_150cm = (Tmin + Tmax) / 2,
      Temperatura_Abrigo_150cm_Minima = Tmin,
      Temperatura_Abrigo_150cm_Maxima = Tmax
    )
}


HASH_BALCARCE <- "157d85bea5cc30774a422ca476dd1b73"
LAT_BALCARCE <- -37.8462


ra_mj_m2_day <- function(lat_deg, doy) {
  Gsc <- 0.0820
  phi <- lat_deg * pi / 180
  dr <- 1 + 0.033 * cos(2 * pi / 365 * doy)
  delta <- 0.409 * sin(2 * pi / 365 * doy - 1.39)
  ws <- acos(pmax(-1, pmin(1, -tan(phi) * tan(delta))))
  (24 * 60 / pi) *
    Gsc *
    dr *
    (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
}

et0_hargreaves <- function(tmax, tmin, tmean, ra) {
  td <- pmax(0, tmax - tmin) # avoid negative due to bad data
  0.00115 * (tmean + 5.15) * sqrt(td) * ra
}

add_et0_hargreaves_fc <- function(df_fc, lat_deg) {
  df_fc |>
    mutate(
      doy = yday(Fecha),
      Tmin = Temperatura_Abrigo_150cm_Minima,
      Tmax = Temperatura_Abrigo_150cm_Maxima,
      Ra = ra_mj_m2_day(lat_deg, doy),
      Evapotranspiracion_Potencial = et0_hargreaves(
        Tmax,
        Tmin,
        Temperatura_Abrigo_150cm,
        Ra
      )
    ) |>
    select(-doy, -Tmin, -Tmax, -Ra)
}

# Colores para heladas ----

col_agro <- "#0072B2"
col_meteo <- "#D55E00"

###############################################################################
# Actualizar df ----

balcarce_EMC <- read_excel(
  "balcarce_EMC.xlsx",
  col_types = c(
    "date",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "text",
    "numeric",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)

#dalbulus <- read_excel("dalbulus.xlsx")

datos_EMC <- balcarce_EMC
datos_EMC <- datos_EMC %>%
  filter(Año >= 1970)
datos_EMC$Fecha <- as.Date(datos_EMC$Fecha, format = "%Y-%m-%d")
datos_EMC <- datos_EMC[order(datos_EMC$Fecha, decreasing = TRUE), ]


datos_actuales <- datos_EMC[
  !is.na(datos_EMC$Precipitacion_Pluviometrica) &
    !is.na(datos_EMC$Temperatura_Abrigo_150cm_Maxima) &
    !is.na(datos_EMC$Temperatura_Abrigo_150cm_Minima),
]

ultima_fecha <- max(datos_actuales$Fecha)
ultimos_datos <- datos_actuales[datos_actuales$Fecha == ultima_fecha, ]
lluvia_ultimo_dia <- ultimos_datos$Precipitacion_Pluviometrica
Tmax_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Maxima
Tmin_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Minima


datos_historicos <- datos_EMC %>%
  filter(Año >= 1991 & Año <= 2020)

datos_historicos_avg <- datos_historicos %>%
  mutate(Dia_Mes = format(Fecha, "%m-%d")) %>%
  group_by(Dia_Mes) %>%
  reframe(
    Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE),
    Evapotranspiracion_media = median(
      Evapotranspiracion_Potencial,
      na.rm = TRUE
    ),
    Precipitacion_media = median(Precipitacion_Pluviometrica, na.rm = TRUE),
    .groups = "drop"
  )

datos_EMC <- datos_EMC %>%
  mutate(Dia_Mes = format(Fecha, "%m-%d"))

datos <- datos_EMC %>%
  left_join(datos_historicos_avg, by = "Dia_Mes") %>%
  mutate(
    Evapotranspiracion_Potencial = coalesce(
      Evapotranspiracion_Potencial,
      Evapotranspiracion_media
    ),
    Temperatura_Abrigo_150cm = coalesce(
      Temperatura_Abrigo_150cm,
      Temperatura_media
    )
  ) %>%
  select(-Evapotranspiracion_media)

# funciones para obtener fechas max y min ----
min_safe_date <- function(x, na.rm = TRUE) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.Date(NA))
  }
  min(x)
}

max_safe_date <- function(x, na.rm = TRUE) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.Date(NA))
  }
  max(x)
}

max_safe_num <- function(x, na.rm = TRUE) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x, na.rm = na.rm)
}

# ui ----

ui <-
  dashboardPage(
    title = "Agrometeorología Balcarce",
    skin = "#2596be",

    dashboardHeader(
      disable = FALSE,
      title = div(
        style = "font-size: 24px; font-weight: bold; text-align: center; color: black;",
        "Agrometeorología Balcarce"
      )
    ),

    dashboardSidebar(
      width = 350,
      tags$style(HTML(
        "
      .main-sidebar {
        background-color: white; 
      }
      .main-sidebar .sidebar-menu > li > a {
        color: black; 
      }
      .user-info-box {
      border: 2px solid #83C5BE; 
      background-color: #f0f8ff; 
      padding: 15px; 
      border-radius: 10px; 
      margin-top: 20px;
      margin-bottom: 20px;
      }
    .user-info-line {
      display: flex;
      align-items: center;
    }
    .user-info-line p {
      margin-right: 10px;  
    }
    "
      )),

      fluid = FALSE,
      position = "left",
      disable = FALSE,
      collapsed = FALSE,

      br(),

      div(
        style = "text-align: center;",
        tags$img(src = "EstacionBalcarce.jpg", height = "80px", width = "220px")
      ),

      br(),

      sidebarMenu(
        id = "siderbarID",

        menuItem(
          "Tiempo y Clima",
          tabName = "tiempo",
          icon = icon("calendar")
        ),

        menuItem(
          "Cambio climático",
          tabName = "cambio_climatico",
          icon = icon("earth-americas")
        ),

        menuItem("Heladas", tabName = "heladas", icon = icon("snowflake")),

        menuItem(
          "Manejo de los cultivos",
          icon = icon("seedling"),
          menuSubItem("Ambiente", tabName = "ambiente"),
          menuSubItem("Balance de agua", tabName = "balance")
        ),

        menuItem("informes", tabName = "informes", icon = icon("file")),

        menuItem(
          "Descarga de datos",
          tabName = "descarga",
          icon = icon("download")
        ),

        menuItem(
          "Referencias bibliográficas",
          tabName = "referencias",
          icon = icon("book")
        )
      ),

      br(),

      tags$head(
        tags$script(
          src = "https://www.googletagmanager.com/gtag/js?id=G-TW1X22VFRM"
        ),

        tags$script(HTML(
          "
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', 'G-TW1X22VFRM');
        "
        ))
      ),

      tags$p(
        strong("Nuestras Redes sociales"),

        br(),

        tags$a(
          icon("instagram"),
          "Instagram",
          href = "https://www.instagram.com/agromet_inta.balcarce/#"
        ),

        br(),

        tags$p(
          strong("Para comunicarse con el grupo "),
          tags$h6(
            "Dra. Nuria Lewczuk : lewczuk.nuria@inta.gob.ar"
          ),
          tags$h6(
            "Dra. Laura Echarte : echarte.laura@inta.gob.ar"
          )
        ),

        div(
          style = "text-align: center;",
          tags$img(src = "IPADS.png", height = "80px", width = "150px"),
          tags$img(
            src = "Logo_Red_Agromet.jpg",
            height = "80px",
            width = "200px"
          )
        )
      )
    ),

    dashboardBody(
      tags$head(
        tags$style(HTML(
          "
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
        .small-box .icon {
          font-size: 50px; /* Tamaño del icono */
          opacity: 0.6; /* Transparencia del icono */
        }
        .small-box h3 {
          font-size: 20px; /* Tamaño del valor */
          font-weight: bold;
          text-align: center;
        }
        .small-box p {
          font-size: 16px; /* Tamaño del subtítulo */
          font-weight: bold;
          text-align: center;
        }
        
        info-box {
    min-height: 110px;
    white-space: normal !important;
  }

  .info-box .info-box-content {
    white-space: normal !important;
    overflow-wrap: break-word;
    word-break: normal;
  }

  .info-box .info-box-text,
  .info-box .info-box-number {
    white-space: normal !important;
    overflow-wrap: break-word;
    line-height: 1.2;
  }

  @media (max-width: 768px) {
    .info-box {
      min-height: 130px;
    }

    .info-box .info-box-text {
      font-size: 13px;
    }

    .info-box .info-box-number {
      font-size: 18px;
    }
  }
        "
        ))
      ),

      tabItems(
        tabItem(
          tabName = "tiempo",
          fluidRow(
            infoBoxOutput(width = 3, "value1"),
            infoBoxOutput(width = 3, "value2"),
            infoBoxOutput(width = 3, "value3"),
            infoBoxOutput(width = 3, "value4")
          ),
          br(),
          fluidRow(
            infoBoxOutput(width = 4, "precipitation_info_box"),
            infoBoxOutput(width = 4, "tempMax_info_box"),
            infoBoxOutput(width = 4, "tempMin_info_box")
          ),
          br(),
          fluidRow(
            column(
              6,
              fluidRow(
                column(
                  6,
                  selectInput(
                    inputId = "ano_selector",
                    label = "Selecciona el Año:",
                    choices = unique(datos$Año),
                    selected = "2026"
                  )
                )
              )
            )
          ),

          br(),

          fluidRow(
            
            box(
              title = "Precipitaciones acumuladas mensuales (mm)",
              status = "gray",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("grafico_lluvia", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            ),
            
            box(
              title = "Precipitaciones y ETo acumuladas mensuales (mm)",
              status = "gray",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("grafico_lluvia_etp_acum", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            ),
            
            box(
              title = "Temperaturas máximas diarias (ºC)",
              status = "gray",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("tempmax_diaria", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            ),
            
            box(
              title = "Temperaturas mínimas diarias (ºC)",
              status = "gray",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("tempmin_diaria", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            )
          ),
          
          br(),
          
          fluidRow(
            box(
              title = "Temperaturas medias mensuales (ºC)",
              status = "gray",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("grafico_temperatura", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            ),
            
            box(
              title = "Número de días mensuales con heladas agrometeorológicas (< 3ºC)",
              status = "gray",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("grafico_heladas", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            )
          ),

          h6(HTML(
            "<strong>Cita:</strong><br>
              Lewczuk, N.  & Echarte, L. (",
            format(Sys.Date(), "%Y"),
            "). <i>Monitoreo de condiciones meteorológicas en Balcarce</i> [Aplicación web]. 
              Laboratorio de Agrometeorología, IPADS CONICET-INTA."
          )),
        ),

        tabItem(
          tabName = "cambio_climatico",
          fluidRow(
            column(
              12,
              div(
                style = "background-color: #2A9D8F60; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                h5(HTML("<strong>Variabilidad Climática</strong>")),
                p(
                  "Es el resultado de ciclos naturales relacionados con la órbita terrestre, la radiación solar, la composición atmosférica, y la circulación oceánica y biosférica. Esta variabilidad puede manifestarse a corto plazo, como se observa en los episodios recurrentes de El Niño y La Niña, que están asociados con patrones de presión atmosférica y circulación oceánica."
                )
              )
            )
          ),

          fluidRow(
            column(
              12,
              div(
                style = "background-color: #F4A26160; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                h5(HTML("<strong>Cambio Climático</strong>")),
                p(
                  "Se refiere a las variaciones en el estado del clima, identificables por cambios en el valor medio o la variabilidad de sus componentes, como la temperatura, que deben ser persistentes durante décadas o más. Estos cambios pueden ser resultado de procesos naturales internos o de forzamientos externos, como las fluctuaciones de los ciclos solares, erupciones volcánicas o alteraciones antropogénicas en la composición atmosférica y el uso del suelo."
                )
              )
            )
          ),

          br(),

          fluidRow(
            column(3),
            column(
              12,
              div(
                style = "text-align: center;",
                p(
                  "Esta aplicación presenta datos climáticos relevantes, incluyendo temperaturas medias, máximas y mínimas y precipitaciones acumuladas, para ayudar a comprender los impactos del cambio climático en nuestra región."
                ),
                p(
                  "Utiliza el filtro para ver los datos anuales desde 1970, para un mes en particular o para el promedio anual:"
                ),
                br(),
                div(
                  style = "width: 200px; margin: 0 auto;",
                  selectInput(
                    inputId = "mes_climatico",
                    label = "Selecciona el mes:",
                    choices = c(
                      "Anual",
                      "enero",
                      "febrero",
                      "marzo",
                      "abril",
                      "mayo",
                      "junio",
                      "julio",
                      "agosto",
                      "septiembre",
                      "octubre",
                      "noviembre",
                      "diciembre"
                    ),
                    selected = "Anual"
                  )
                )
              )
            )
          ),

          br(),

          fluidRow(
            column(
              4,
              box(
                title = "Temperaturas medias (ºC)",
                status = "orange",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "grafico_temp_climatico",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Precipitaciones acumuladas (mm)",
                status = "orange",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "grafico_pp_climatico",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Días con temp. máximas > a 25ºC y temp. mínimas < 3ºC",
                status = "orange",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "grafico_heladas_climatico",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            )
          ),

          fluidRow(
            column(
              4,
              box(
                title = "Fechas de primera helada agrometeorológica",
                status = "orange",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "grafico_primera_helada_agro",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Fechas de última helada agrometeorológica",
                status = "orange",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "grafico_ultima_helada_agro",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Número de días libres de heladas",
                status = "orange",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "grafico_dias_libres_heladas_agro",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            )
          ),

          br(),
          br(),

          h6(HTML(
            "<strong>Cita:</strong><br>
          Lewczuk, N.  & Echarte, L. (",
            format(Sys.Date(), "%Y"),
            "). <i>Análisis de variabilidad y cambio climático en Balcarce</i> [Aplicación web]. 
          Laboratorio de Agrometeorología, IPADS CONICET-INTA."
          )),
        ),

        tabItem(
          tabName = "heladas",

          fluidRow(
            column(
              6,
              div(
                style = "background-color: #0072B260; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                h5(HTML("<strong>Heladas agrometeorológicas</strong>")),
                p(
                  "Ocurre cuando la temperatura medida en el abrigo meteorológico es igual o inferior a 3 °C, 
                           equivalente a 0 °C o menos a la intemperie en superficie."
                ),
              )
            ),
            column(
              6,
              div(
                style = "background-color: #D55E0060; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                h5(HTML("<strong>Heladas meteorológicas</strong>")),
                p(
                  "Se produce cuando la temperatura del aire medida en abrigo meteorológico es igual o inferior a 0 °C."
                )
              )
            )
          ),

          fluidRow(
            column(
              6,
              box(
                title = "Probabilidad de tener al menos 1 día en el mes con heladas agrometeorológicas (1991 - 2020)",
                status = "lightblue",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "prob_helada_ene_dic_3",
                    height = "500px",
                    width = "100%"
                  ),
                  image = getOption("imagen_nieve"),
                  type = 5,
                  color = col_agro,
                  size = 0.5
                ),

                width = 12
              )
            ),
            column(
              6,
              box(
                title = "Probabilidad de tener al menos 1 día en el mes con heladas meteorológicas (1991 - 2020)",
                status = "lightblue",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "prob_helada_ene_dic_0",
                    height = "500px",
                    width = "100%"
                  ),
                  image = getOption("imagen_nieve"),
                  type = 5,
                  color = col_meteo,
                  size = 0.5
                ),
                width = 12
              )
            )
          ),
          fluidRow(
            column(
              6,
              box(
                title = "Frecuencia acumulada de primera helada agrometeorológica",
                status = "lightblue",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "periodo_prim_helada_3",
                      label = "Período:",
                      choices = c(
                        # "1971-2000", "1981-2010",
                        "1991-2020",
                        "2016-2025"
                      ),
                      selected = "1991-2020"
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "dia_prim_helada_3",
                      label = "Día:",
                      value = 17,
                      min = 1,
                      max = 31
                    )
                  ),
                  column(
                    4,
                    selectInput(
                      "mes_prim_helada_3",
                      label = "Mes:",
                      choices = list(
                        "febrero" = 2,
                        "marzo" = 3,
                        "abril" = 4,
                        "mayo" = 5
                      ),
                      selected = 4
                    )
                  )
                ),
                withSpinner(
                  plotlyOutput(
                    "acum_primera_helada_3",
                    height = "300px",
                    width = "100%"
                  ),
                  # image = getOption("imagen_nieve"),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                )
              )
            ),
            column(
              6,
              box(
                title = "Frecuencia acumulada de primera helada meteorológica",
                status = "lightblue",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "periodo_prim_helada_0",
                      label = "Período:",
                      choices = c(
                        "1991-2020",
                        "2016-2025"
                      ),
                      selected = "1991-2020"
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "dia_prim_helada_0",
                      label = "Día:",
                      value = 27,
                      min = 1,
                      max = 31
                    )
                  ),
                  column(
                    4,
                    selectInput(
                      "mes_prim_helada_0",
                      label = "Mes:",
                      choices = list(
                        "abril" = 4,
                        "mayo" = 5,
                        "junio" = 6,
                        "julio" = 7
                      ),
                      selected = 5
                    )
                  )
                ),
                withSpinner(
                  plotlyOutput(
                    "acum_primera_helada_0",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                )
              )
            )
          ),

          fluidRow(
            column(
              6,
              box(
                title = "Frecuencia acumulada de última helada agrometeorológica",
                status = "lightblue",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "periodo_ult_helada_3",
                      label = "Período:",
                      choices = c(
                        "1991-2020",
                        "2016-2025"
                      ),
                      selected = "1991-2020"
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "dia_ult_helada_3",
                      label = "Día:",
                      value = 15,
                      min = 1,
                      max = 31
                    )
                  ),
                  column(
                    4,
                    selectInput(
                      "mes_ult_helada_3",
                      label = "Mes:",
                      choices = list(
                        "octubre" = 10,
                        "noviembre" = 11,
                        "diciembre" = 12
                      ),
                      selected = 11
                    )
                  )
                ),
                withSpinner(
                  plotlyOutput(
                    "acum_ultima_helada_3",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                )
              )
            ),
            column(
              6,
              box(
                title = "Frecuencia acumulada de última helada meteorológica",
                status = "lightblue",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "periodo_ult_helada_0",
                      label = "Período:",
                      choices = c(
                        "1991-2020",
                        "2016-2025"
                      ),
                      selected = "1991-2020"
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "dia_ult_helada_0",
                      label = "Día:",
                      value = 27,
                      min = 1,
                      max = 31
                    )
                  ),
                  column(
                    4,
                    selectInput(
                      "mes_ult_helada_0",
                      label = "Mes:",
                      choices = list(
                        "agosto" = 8,
                        "septiembre" = 9,
                        "octubre" = 10,
                        "noviembre" = 11,
                        "diciembre" = 12
                      ),
                      selected = 9
                    )
                  )
                ),
                withSpinner(
                  plotlyOutput(
                    "acum_ultima_helada_0",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                )
              )
            )
          ),

          p(
            HTML("Podés descargar el informe de heladas haciendo clic "),
            tags$a(
              href = "Heladas.pdf",
              "aquí",
              target = "Heladas"
            )
          ),

          br(),

          fluidRow(
            column(
              12,
              div(
                style = "background-color: #00AFB9; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                h5(HTML(
                  "<strong>Fechas de ocurrencia de heladas por año</strong>"
                )),
                p(
                  "Visualizá para cada año cuándo comienza y termina el período con riesgo de heladas, 
               diferenciando eventos agrometeorológicos y meteorológicos."
                )
              )
            )
          ),

          fluidRow(
            column(3),

            column(
              6,
              box(
                width = 12,
                title = "Primera y última helada",
                status = "primary",
                solidHeader = TRUE,

                selectInput(
                  inputId = "anio_heladas",
                  label = "Seleccionar año",
                  choices = 1970:max(datos$Año, na.rm = TRUE),
                  selected = max(datos$Año, na.rm = TRUE)
                ),

                fluidRow(
                  infoBoxOutput("box_heladas_agro", width = 6),
                  infoBoxOutput("box_heladas_meteo", width = 6)
                )
              )
            )
          ),
          br(),

          h6(HTML(paste0(
            "<strong>Cita:</strong><br>
              Lewczuk, N., & Echarte, L. (",
            format(Sys.Date(), "%Y"),
            "). <i>Climatología de heladas agrometeorológicas</i> [Aplicación web]. 
              Laboratorio de Agrometeorología, IPADS CONICET-INTA."
          )))
        ),

        # tabItem(
        #   tabName = "mapas",
        #   h4(HTML("<strong>Mapas de suelo</strong>")),
        #   h5(HTML("Recortes de la zona de influencia de EEA Balcarce, realizados por el Inst. de Clima y agua - INTA Castelar")),
        #   br(),
        #   br(),
        #   fluidRow(
        #     box(title = "Variación de agua disponible"
        #         ,status = "navy"
        #         ,solidHeader = FALSE
        #         ,div(
        #           style = "text-align: center;",
        #           tags$img(
        #             src = "agua.jpg",
        #             style = "max-width: 100%; height: auto;",
        #             alt = "Mapa-Consumo de agua"
        #           )
        #         ),
        #         br(),
        #         br(),
        #         tags$figcaption("Variación del agua disponible hasta 2 metros, expresada en
        #         mm, estimada mediante el uso de imágenes del satélite S-NPP con una resolución espacial de 500 metros.
        #         Acumulado a la fecha con respecto a la fecha anterior.
        #                         Elaborado por Instituto de Clima y Agua, INTA Castelar. Recorte: Patricio Oricchio."),
        #         div(
        #           style = "margin-top: 20px; text-align: left; padding: 10px; border: 2px solid #ddd; background-color: #f9f9f9;",
        #           "Representa el cambio en el agua disponible (expresado en mm) al final de un período de 10
        #           días con respecto al final del período anterior. Permite analizar la magnitud del aumento
        #           (período actual mayor al anterior) o reducción (período actual menor al anterior) del agua
        #           disponible en el perfil en hasta 2 metros de profundidad.")
        #     ),
        #     box(title = "% de agua útil"
        #         ,status = "navy"
        #         ,solidHeader = FALSE
        #         ,div(
        #           style = "text-align: center;",
        #           tags$img(
        #             src = "agua_util.jpg",
        #             style = "max-width: 100%; height: auto;",
        #             alt = "Mapa-agua útil"
        #           )
        #         ),
        #         br(),
        #         br(),
        #         tags$figcaption("Agua en el suelo con respecto al máximo posible (% de agua útil).Resolución espacial: 500 m.
        #             Mapa elaborado por Instituto de Clima y Agua, INTA Castelar. Recorte: Lucas Gusmerotti."),
        #         div(
        #           style = "margin-top: 20px; text-align: left; padding: 10px; border: 2px solid #ddd; background-color: #f9f9f9;",
        #           "El porcentaje de agua útil en el suelo (es decir, aquella porción de agua
        #               que puede ser extraída por las plantas) puede ser estimado a través
        #               de un balance de agua; donde se considera información del suelo, el
        #               aporte de agua por lluvias y el consumo de agua de la cubierta
        #               vegetal.")
        #     ))
        # ),

        tabItem(
          tabName = "ambiente",
          br(),
          h4(HTML("<strong>Ambiente de los cultivos</strong>")),
          h5(HTML(
            "Te mostramos como fue cambiando el ambiente del cultivo durante el perdíodo crítico (PC), según la fecha de siembra desde el año 1991."
          )),

          br(),

          fluidRow(
            # elección cultivo
            column(
              4,
              div(
                style = "background-color: #81B29A80; padding: 10px;  border-radius: 10px;",
                selectInput(
                  "cultivo_ambiente",
                  label = strong("Cultivo:"),
                  choices = list(
                    "Maíz ciclo largo" = "maiz_largo",
                    "Maíz ciclo corto" = "maiz_corto",
                    "Soja" = "soja"
                    # ,
                    # "Girasol" = "girasol",
                    # "Hortalizas" = "hortalizas"
                  ),
                  selected = "maiz"
                )
              )
            ),
            column(
              4,
              div(
                style = "background-color: #81B29A80; padding: 10px;  border-radius: 10px;",
                numericInput(
                  "dia_siembra_ambiente",
                  label = strong("Día de siembra:"),
                  value = 1,
                  min = 1,
                  max = 31
                )
              )
            ),
            column(
              4,
              div(
                style = "background-color: #81B29A80; padding: 10px; border-radius: 10px;",
                selectInput(
                  "mes_siembra_ambiente",
                  label = strong("Mes de siembra:"),
                  choices = list(
                    "Octubre" = 10,
                    "Noviembre" = 11,
                    "Diciembre" = 12,
                    "Enero" = 1
                  ),
                  selected = 10
                )
              )
            ),
          ),
          br(),
          br(),

          fluidRow(
            box(
              title = "Precipitaciones y ET0 acumuladas durante el período crítico",
              status = "olive",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("pp_acum_PC", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            ),
            box(
              title = "Radiación y días con temp. máx. > 35ºC durante el período crítico",
              status = "olive",
              solidHeader = TRUE,
              collapsible = TRUE,
              withSpinner(
                plotlyOutput("rad_temp_PC", height = "300px"),
                type = 5,
                color = "#0dc5c1",
                size = 0.5
              )
            ),
            h6(HTML(
              "ET0: Evapotranspiración de referencia (mm) o demanda de agua del ambiente<br />Valores decádicos corresponden a la mediana."
            ))
          ),

          br(),
          br(),

          h5(HTML(
            "Podes comparar el ambiente de tu cultivo en 3 fechas de siembra 
                diferentes"
          )),
          h6(HTML(
            "(consideramos desde el año 2010, 
                donde se observa un cambio en las temperaturas a nivel país)."
          )),

          br(),

          fluidRow(
            column(
              3,
              div(
                style = "background-color: #81B29A80; padding: 10px;  border-radius: 10px;",
                selectInput(
                  "cultivo_fecha",
                  label = strong("Cultivo:"),
                  choices = list(
                    "Maíz ciclo largo" = "maiz_largo",
                    "Maíz ciclo corto" = "maiz_corto",
                    "Soja" = "soja"
                    # ,
                    # "Girasol" = "girasol",
                    # "Hortalizas" = "hortalizas"
                  ),
                  selected = "maiz"
                )
              ),
              br(),
              br(),
              column(
                12,
                align = "center",
                actionButton(
                  "btn_calcular",
                  label = "Calcular",
                  icon = icon("calculator"),
                  style = "color: white; background-color: #81B29A; border-color: #81B29A; border-radius: 10px; padding: 10px;"
                )
              )
            ),
            column(
              3,
              div(
                style = "background-color: #E07A5F80; padding: 10px;  border-radius: 10px;",
                h6(HTML("<strong>Fecha 1:</strong>")),

                selectInput(
                  "mes_siembra1",
                  label = strong("Mes de siembra:"),
                  choices = list(
                    "Octubre" = 10,
                    "Noviembre" = 11,
                    "Diciembre" = 12,
                    "Enero" = 1
                  ),
                  selected = 10
                ),
                numericInput(
                  "dia_siembra1",
                  label = strong("Día de siembra:"),
                  value = 1,
                  min = 1,
                  max = 31
                ),
                br(),
                textOutput("fecha_inicio_pc1"),
                textOutput("fecha_fin_pc1")
              )
            ),
            column(
              3,
              div(
                style = "background-color: #3D405B80; padding: 10px;  border-radius: 10px;",
                h6(HTML("<strong>Fecha 2:</strong>")),

                selectInput(
                  "mes_siembra2",
                  label = strong("Mes de siembra:"),
                  choices = list(
                    "Octubre" = 10,
                    "Noviembre" = 11,
                    "Diciembre" = 12,
                    "Enero" = 1
                  ),
                  selected = 10
                ),
                numericInput(
                  "dia_siembra2",
                  label = strong("Día de siembra:"),
                  value = 1,
                  min = 1,
                  max = 31
                ),
                br(),
                textOutput("fecha_inicio_pc2"),
                textOutput("fecha_fin_pc2")
              )
            ),
            column(
              3,
              div(
                style = "background-color: #AEC3B080; padding: 10px;  border-radius: 10px;",
                h6(HTML("<strong>Fecha 3:</strong>")),

                selectInput(
                  "mes_siembra3",
                  label = strong("Mes de siembra:"),
                  choices = list(
                    "Octubre" = 10,
                    "Noviembre" = 11,
                    "Diciembre" = 12,
                    "Enero" = 1
                  ),
                  selected = 10
                ),
                numericInput(
                  "dia_siembra3",
                  label = strong("Día de siembra:"),
                  value = 1,
                  min = 1,
                  max = 31
                ),
                br(),
                textOutput("fecha_inicio_pc3"),
                textOutput("fecha_fin_pc3")
              )
            )
          ),

          br(),

          fluidRow(
            column(4, ),
            column(
              4,
              div(
                style = "background-color: #FB850080; padding: 10px; border-radius: 10px; text-align: center; align-items: center;",
                h5(HTML(
                  "<strong>Condiciones ambientales en el período crítico</strong>"
                ))
              )
            )
          ),
          br(),

          fluidRow(
            column(
              4,
              box(
                title = "Precipitaciones y ET0 acumuladas",
                status = "olive",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput("pc_lluvia", height = "300px", width = "100%"),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Radiación global",
                status = "olive",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "pc_radiacion",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Días con temperaturas máximas > a 35ºC",
                status = "olive",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput("pc_dias_35", height = "300px", width = "100%"),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
          ),
          br(),

          fluidRow(
            column(4, ),
            column(
              4,
              div(
                style = "background-color: #FB850080; padding: 10px;  border-radius: 10px; text-align: center; align-items: center;",
                h5(HTML(
                  "<strong>Condiciones ambientales durante el llenado de grano</strong>"
                ))
              )
            )
          ),
          br(),

          fluidRow(
            column(
              4,
              box(
                title = "Precipitaciones y ET0 acumuladas",
                status = "olive",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput("lg_lluvia", height = "300px", width = "100%"),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Radiación global",
                status = "olive",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput(
                    "lg_radiacion",
                    height = "300px",
                    width = "100%"
                  ),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
            column(
              4,
              box(
                title = "Días con temperaturas mínimas < a 2ºC",
                status = "olive",
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(
                  plotlyOutput("lg_dias_2", height = "300px", width = "100%"),
                  type = 5,
                  color = "#0dc5c1",
                  size = 0.5
                ),
                width = 12
              )
            ),
          ),
          h6(HTML(
            "<strong>Cita:</strong><br>
              Lewczuk, N. & Echarte, L.(2025). <i>Ambiente en cultivos extensivos de Balcarce</i> [Aplicación web]. 
              Laboratorio de Agrometeorología, IPADS CONICET-INTA. 
              https://doi.org/10.5281/zenodo.18926557"
          )),
        ),

        tabItem(
          tabName = "balance",
          tabsetPanel(
            id = "balance_agua",
            tabPanel(
              title = "Balcarce",
              br(),
              h4(HTML(
                "<strong>Cálculo de balance de agua<sup>1</sup></strong>"
              )),
              h5(HTML(
                "A partir de los datos del suelo y del cultivo seleccionado, podes calcular el balance de agua diario de tu campo. <br> 
                Podes ingresar los datos en los recuadros o usar los valores predeterminados."
              )),

              br(),

              fluidRow(
                column(
                  3,
                  # elección cultivo
                  fluidRow(
                    column(
                      12,
                      selectInput(
                        "cultivo_balcarce",
                        label = strong("Seleccione el cultivo:"),
                        choices = list(
                          "Maíz ciclo largo" = "maiz_largo",
                          "Maíz ciclo corto" = "maiz_corto",
                          "Soja" = "soja"
                          # ,
                          # "Girasol" = "girasol",
                          # "Hortalizas" = "hortalizas"
                        ),
                        selected = "maiz_largo"
                      )
                    ),
                    column(
                      12,
                      conditionalPanel(
                        condition = "input.cultivo_balcarce == 'maiz_largo'",
                        selectInput(
                          "densidad_siembra",
                          label = HTML(
                            "<strong>Seleccione la densidad de plantas<br>(plantas / m<sup>2</sup>):</strong>"
                          ),
                          choices = c("4" = 4, "8" = 8),
                          selected = 4
                        )
                      )
                    )
                  )
                ),
                column(
                  3,
                  dateInput(
                    "fecha_siembra_balcarce",
                    label = strong("Ingrese la fecha de siembra:"),
                    value = "2025-10-01"
                  )
                )
              ),
              fluidRow(
                column(12, div(uiOutput("mensaje_cultivo_balcarce1")))
              ),
              br(),
              fluidRow(
                column(
                  6,
                  div(
                    style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                    h4(HTML(("<strong>Datos de suelo</strong>"))),
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "profundidad_balcarce",
                          label = strong("Profundidad (cm)"),
                          value = 100
                        ),
                        br(),
                        numericInput(
                          "capacidad_campo_balcarce",
                          label = strong(HTML(
                            "Capacidad de Campo (mm/cm)
                                                        <br><small>
                                                        (Límite máximo de almacenamiento de agua)</small>"
                          )),
                          value = 3.70
                        ),
                        textOutput("almacenamiento_maximo_balcarce")
                      ),
                      column(
                        6,
                        numericInput(
                          "fraccion_min_balcarce",
                          label = strong(HTML(
                            "Fracción de almacenamiento mínimo respecto del máximo (0 - 1)<sup>3</sup>"
                          )),
                          value = 0.50,
                          min = 0,
                          max = 1
                        ),
                        textOutput("almacenamiento_minimo_balcarce"),
                        textOutput("agua_util_total_balcarce"),
                        br(),
                        numericInput(
                          "fraccion_inicial_balcarce",
                          label = strong(
                            "Fracción inicial de agua útil (0 - 1)"
                          ),
                          value = NULL,
                          max = 1
                        )
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    style = "background-color: #21838040; padding: 15px; border-radius: 10px;",
                    h4(strong("Datos de cultivo")),
                    numericInput(
                      "umbral_et_balcarce",
                      label = strong(HTML(
                        "Umbral de fracción de agua útil<sup>2</sup>"
                      )),
                      value = NULL
                    ),
                    textOutput("disminucion_et_balcarce"),
                    br(),
                    textOutput("GD_balcarce"),
                  )
                ),

                column(
                  3,
                  div(
                    style = "background-color: #E0E1DD80; padding: 10px; border-radius: 10px;",
                    h4(strong("Datos ambientales propios")),
                    downloadButton(
                      "descarga_modelo_balcarce",
                      "Descargar modelo de archivo para completar"
                    ),
                    br(),
                    br(),
                    fileInput(
                      "clima_balcarce",
                      "Subir tabla con datos (.csv ó .xlsx) (opcional)",
                      accept = c(".csv", ".xlsx")
                    ),
                    helpText(
                      "El archivo debe contener datos diarios (sin datos faltantes) y las columnas: Fecha, Lluvia, Riego (primera letra mayúscula)"
                    )
                  )
                )
              ),
              fluidRow(
                column(12, br(), div(uiOutput("mensaje_cultivo_balcarce3")))
              ),
              br(),

              fluidRow(
                box(
                  title = "Fracción de agua útil",
                  status = "lightblue",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(
                    plotlyOutput("agua_util_balcarce", height = "300px"),
                    type = 5,
                    color = "#0dc5c1",
                    size = 0.5
                  )
                ),
                box(
                  title = "Consumo de agua",
                  status = "lightblue",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(
                    plotlyOutput("consumo_agua_balcarce", height = "300px"),
                    type = 5,
                    color = "#0dc5c1",
                    size = 0.5
                  )
                ),
                box(
                  title = "Balance de agua",
                  status = "lightblue",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(
                    plotlyOutput("deficit_agua_balcarce", height = "300px"),
                    type = 5,
                    color = "#0dc5c1",
                    size = 0.5
                  )
                ),
                column(
                  6,
                  h6(HTML(
                    ("<strong>ETM: </strong>MÁXIMO consumo de agua, si no hubiera deficiencias de agua.")
                  )),
                  h6(HTML(("<strong>ETR: </strong>Consumo de agua REAL."))),
                  br(),
                  uiOutput("mensaje_cultivo_balcarce2"),
                  br(),
                  h6(HTML(
                    ("<strong>Aviso:</strong> 
                        A partir del <strong>último día con datos observados</strong>, los valores de 
                        <em>T<sub>max</sub>, T<sub>min</sub> y precipitaciones</em> corresponden a 
                        <u>estimaciones basadas en pronósticos de Meteored (5 días)</u>. 
                        En los gráficos, estos datos pronosticados se representan mediante 
                        <strong>líneas discontinuas</strong> y <strong>barras semitransparentes</strong>, 
                        y se utilizan únicamente para proyectar el consumo de agua y el balance hídrico.")
                  )),
                )
              ),

              br(),
              br(),

              fluidRow(
                column(
                  12,
                  div(
                    style = "background-color: #E0E1DD80; padding: 10px; border-radius: 10px;",
                    h4(strong("Cálculo de la huella hídrica de su cultivo")),
                    br(),
                    fluidRow(
                      column(
                        4,
                        offset = 1,
                        div(
                          style = "display: flex; flex-direction: column; align-items: center;",
                          infoBoxOutput("ETMacum_balcarce", width = 12),
                          infoBoxOutput("ETRacum_balcarce", width = 12)
                        )
                      ),
                      br(),
                      column(
                        4,
                        offset = 1,
                        div(
                          style = "display: flex; flex-direction: column; align-items: center;",
                          numericInput(
                            "rendimiento_balcarce",
                            label = "Ingrese el rendimiento obtenido (kg / ha):",
                            value = 0
                          ),
                          infoBoxOutput("huella_hidrica_balcarce", width = 12)
                        )
                      ),
                    )
                  )
                )
              ),
              h6(HTML(
                "<strong>Cita:</strong><br>
              Echarte, L. & Lewczuk, N. (2025). <i>Balance hídrico en cultivos extensivos de Balcarce</i> [Aplicación web]. 
              Laboratorio de Agrometeorología, IPADS CONICET-INTA. 
              https://doi.org/10.5281/zenodo.18925301"
              ))
            ),

            tabPanel(
              title = "Otros sitios",
              br(),
              h4(HTML(
                "Para aquellas zonas fuera del radio de influencia de la EEA Balcarce, podes ingresar tus propios datos."
              )),
              br(),
              h6(HTML(
                "Desde la web <a href='https://siga.inta.gob.ar/#/' target='_blank'>siga.inta.gob.ar</a>, podes seleccionar la estación meteorológica más cercana a tu campo y descargar los datos de lluvia, temperaturas y evapotranspiración."
              )),
              h6(HTML(
                "Si no encontras valores de evapotranspiración para tu campo, podes consultarnos y te ayudamos con el cálculo: <strong>echarte.laura@inta.gob.ar</strong>"
              )),
              br(),
              fluidRow(
                div(
                  style = "background-color: #1B263B40; border: 1px solid #dee2e6; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p(
                    "El consumo de agua del cultivo se estima considerando curvas de coeficiente de cultivos (es decir, kc = ETc/ETo) 
          calibradas para las condiciones del Sudeste de Buenos Aires. Al inicio del crecimiento del cultivo, 
          cuando la canopia cubre parcialmente el suelo, el consumo de agua está muy afectado por la evaporación de agua 
          directamente desde el suelo. Esta depende de (i) intervalo de tiempo entre eventos de lluvia y riego, (ii) poder 
          evaporante de la atmósfera, (iii) magnitud del evento de humedecimiento. En particular, las condiciones para las 
          que se calibraron los modelos presentaban un intervalo de humedecimiento de aproóx. 5 a 7 días, una ETo media de 
          apróx. 5,2 mm d",
                    tags$sup("-1"),
                    "y un promedio de lámina de agua de lluvia/riego de 15 mm. En ambientes con más frecuencia de 
          humedecimiento inicial se puede esperar más evaporación, mientras que en ambientes con menor frecuencia de mojado 
          se puede esperar una evaporación más restringida. Asimismo, en ambientes con mayor demanda evaporativa que la del 
          Sudeste de Buenos Aires, el suelo se secará más rápidamente entre eventos de mojado y menor será el valor promedio 
          temporal de Kc para un período dado."
                  )
                )
              ),
              br(),
              fluidRow(
                # elección cultivo
                column(
                  3,
                  selectInput(
                    "cultivo",
                    label = strong("Seleccione el cultivo:"),
                    choices = list(
                      "Maíz ciclo largo" = "maiz_largo",
                      "Maíz ciclo corto" = "maiz_corto",
                      "Soja" = "soja"
                      # ,
                      # "Girasol" = "girasol",
                      # "Hortalizas" = "hortalizas"
                    ),
                    selected = "maiz_largo"
                  )
                ),

                column(
                  3,
                  dateInput(
                    "fecha_siembra",
                    label = strong("Ingrese la fecha de siembra:"),
                    value = "2024-01-01"
                  )
                )
              ),
              fluidRow(
                column(12, div(uiOutput("mensaje_cultivo1"), )),
              ),
              br(),
              fluidRow(
                column(
                  6,
                  div(
                    style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                    h4(HTML(("<strong>Datos de suelo</strong>"))),
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "profundidad",
                          label = strong("Profundidad (cm)"),
                          value = 100
                        ),
                        br(),
                        numericInput(
                          "capacidad_campo",
                          label = strong(HTML(
                            "Capacidad de Campo (mm/cm)
                                                        <br><small>
                                                        (Límite máximo de almacenamiento de agua)</small>"
                          )),
                          value = 3.70
                        ),
                        textOutput("almacenamiento_maximo")
                      ),
                      column(
                        6,
                        numericInput(
                          "fraccion_min",
                          label = strong(HTML(
                            "Fracción de almacenamiento mínimo respecto del máximo (0 - 1)<sup>3</sup>"
                          )),
                          value = 0.55,
                          min = 0,
                          max = 1
                        ),
                        textOutput("almacenamiento_minimo"),
                        textOutput("agua_util_total"),
                        br(),
                        numericInput(
                          "fraccion_inicial",
                          label = strong(
                            "Fracción inicial de agua útil (0 - 1)"
                          ),
                          value = 0.50,
                          min = 0,
                          max = 1
                        )
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    style = "background-color: #21838040; padding: 15px; border-radius: 10px;",
                    h4(strong("Datos de cultivo")),
                    numericInput(
                      "umbral_et",
                      label = strong(HTML(
                        "Umbral de fracción de agua útil<sup>2</sup> 
                                                        <br><small>
                                                        (Debajo del cual se reduce la evapotranspiración)</small>"
                      )),
                      value = NULL
                    ),
                    textOutput("disminucion_et"),
                    br(),
                    textOutput("GD"),
                  )
                ),
                column(
                  3,
                  div(
                    style = "background-color: #E0E1DD80; padding: 10px; border-radius: 10px;",
                    h4(strong("Datos ambientales")),
                    downloadButton(
                      "descarga_modelo",
                      "Descargar modelo de archivo para completar"
                    ),
                    br(),
                    br(),
                    fileInput(
                      "otros_clima",
                      "Subir tabla con datos (.csv ó .xlsx)",
                      accept = c(".csv", ".xlsx")
                    ),
                    helpText(
                      "El archivo debe contener datos diarios y las columnas (sin datos faltantes): Fecha, Lluvia, Riego, Temperatura_Media, Temperatura_Minima y ET0 (primera letra mayúscula)"
                    )
                  )
                )
              ),
              br(),
              fluidRow(
                column(12, div(uiOutput("mensaje_cultivo3"), )),
              ),
              br(),

              fluidRow(
                box(
                  title = "Fracción de agua útil",
                  status = "lightblue",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(
                    plotlyOutput("agua_util", height = "300px"),
                    type = 5,
                    color = "#0dc5c1",
                    size = 0.5
                  )
                ),
                box(
                  title = "Consumo de agua",
                  status = "lightblue",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(
                    plotlyOutput("consumo_agua", height = "300px"),
                    type = 5,
                    color = "#0dc5c1",
                    size = 0.5
                  )
                ),
                box(
                  title = "Balance de agua",
                  status = "lightblue",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  withSpinner(
                    plotlyOutput("deficit_agua", height = "300px"),
                    type = 5,
                    color = "#0dc5c1",
                    size = 0.5
                  )
                ),

                column(
                  6,
                  h6(HTML(
                    ("<strong>ETM: </strong>MÁXIMO consumo de agua, si no hubiera deficiencias de agua.")
                  )),
                  h6(HTML(("<strong>ETR: </strong>Consumo de agua REAL."))),
                  br(),
                  uiOutput("mensaje_cultivo2")
                )
              ),

              br(),
              br(),

              fluidRow(
                column(
                  12,
                  div(
                    style = "background-color: #E0E1DD80; padding: 10px; border-radius: 10px;",
                    h4(strong("Cálculo de la huella hídrica de su cultivo")),
                    br(),
                    fluidRow(
                      column(
                        4,
                        offset = 1,
                        div(
                          style = "display: flex; flex-direction: column; align-items: center;",
                          infoBoxOutput("ETMacum", width = 12),
                          infoBoxOutput("ETRacum", width = 12)
                        )
                      ),
                      br(),
                      column(
                        4,
                        offset = 1,
                        div(
                          style = "display: flex; flex-direction: column; align-items: center;",
                          numericInput(
                            "rendimiento",
                            label = "Ingrese el rendimiento obtenido (kg / ha):",
                            value = 0
                          ),
                          infoBoxOutput("huella_hidrica", width = 12)
                        )
                      ),
                    )
                  )
                )
              )
            ),

            h6(HTML(
              "<strong>Cita:</strong><br>
              Echarte, L. & Lewczuk, N. (2025). <i>Balance hídrico en cultivos extensivos</i> [Aplicación web]. 
              Laboratorio de Agrometeorología, IPADS CONICET-INTA."
            ))
          )
        ),

        tabItem(
          tabName = "huella_hidrica",

          br(),

          numericInput(
            "rto_huella",
            "Rendimiento del cultivo (kg/ha):",
            value = 1000,
            min = 0
          ),
          actionButton(
            "calcular",
            "Calcular huella hídrica",
            style = "background-color: #00BFFF; color: white; border: none;"
          ),

          br(),

          valueBoxOutput("calcular_huella_hídrica"),

          br(),

          plotlyOutput("grafico_huella"),
          textOutput("info_huella")
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
        #     leafletOutput("mapa_arg"),
        #   downloadButton("downloadMap", "Descargar mapa")
        #   ),

        # tabItem(
        #   tabName = "pronosticos",
        #   br(),
        #   h4(HTML("<strong>Pronósticos meteorológicos del área de influencia de EEA Balcarce</strong>")),
        #   h6(HTML("Elaborados por el SMN y el Instituto de Clima y Agua - INTA Castelar.")),
        #   br(),
        #   br(),
        #
        #   fluidRow(
        #     box(title = "Pronóstico semanal"
        #         ,status = "navy"
        #         ,solidHeader = FALSE
        #         ,div(
        #           style = "text-align: center;",
        #           tags$img(
        #             src = "pronostico_lluvia.png",
        #             style = "max-width: 80%; height: auto;",
        #             alt = "Pronóstico semanal de lluvia"
        #           )
        #         )
        #     ),
        #     box(title = "Pronóstico trimestral"
        #         ,status = "navy"
        #         ,solidHeader = FALSE
        #         ,div(
        #           style = "text-align: center;",
        #           tags$img(
        #             src = "pronostico_tri.png",
        #             style = "max-width: 80%; height: auto;",
        #             alt = "Pronóstico trimestral"
        #           )
        #         )
        #     ),
        #   )
        # ),

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
                  width = 250,
                  height = 300,
                  alt = "Informe Mensual Agropecuario"
                ),
                tags$br(),
                tags$a(
                  "Descarga el informe completo aquí",
                  href = "https://bit.ly/IMA-NOV25"
                )
              )
            ),
            column(
              width = 3,
              div(
                style = "margin-left: 100px; margin-top: 50px;",
                tags$img(
                  src = "chicharrita.jpg",
                  width = 250,
                  height = 300,
                  alt = "Achaparramiento del Maíz"
                ),
                tags$br(),
                tags$a(
                  "Descarga el informe completo aquí",
                  href = "https://www.argentina.gob.ar/sites/default/files/2018/09/el_achaparramiento_del_maiz_y_las_decisiones_agricolas_en_argentina_mesatecnicanacional_inta.pdf"
                )
              ),
            ),
            column(
              width = 3,
              div(
                style = "margin-left: 100px; margin-top: 50px;",
                tags$img(
                  src = "ENSO.jpg",
                  width = 250,
                  height = 300,
                  alt = "ENSO y precipitaciones"
                ),
                tags$br(),
                tags$a(
                  href = "ENSO.pdf",
                  "Descarga el informe completo",
                  target = "ENSO"
                ),
              )
            ),
            column(
              width = 3,
              div(
                style = "margin-left: 100px; margin-top: 50px;",
                tags$img(
                  src = "Heladas.png",
                  width = 250,
                  height = 300,
                  alt = "Caracterización del régimen de heladas agrometeorológicas en Balcarce desde 1971 a 2025"
                ),
                tags$br(),
                tags$a(
                  href = "Heladas.pdf",
                  "Descarga el informe completo",
                  target = "Heladas"
                ),
              )
            )
          )
        ),

        tabItem(
          tabName = "descarga",
          br(),
          h4(HTML("<strong>Datos disponibles de la EMC Balcarce</strong>")),
          br(),

          dataTableOutput("datos"),
          br(),
          "Selecciona el periodo:",
          fluidRow(
            column(
              2,
              dateInput(
                "fecha_inicio",
                "Fecha de Inicio",
                value = NULL,
                format = "dd/mm/yyyy"
              )
            ),
            column(
              2,
              dateInput(
                "fecha_fin",
                "Fecha de Fin",
                value = NULL,
                format = "dd/mm/yyyy"
              )
            )
          ),
          selectInput(
            "variables",
            "Seleccionar Variables:",
            choices = c(
              "Temperatura_Abrigo_150cm",
              "Temperatura_Abrigo_150cm_Maxima",
              "Temperatura_Abrigo_150cm_Minima",
              "Temperatura_Intemperie_5cm_Minima",
              "Temperatura_Intemperie_50cm_Minima",
              "Temperatura_Suelo_5cm_Media",
              "Temperatura_Suelo_10cm_Media",
              "Temperatura_Inte_5cm",
              "Temperatura_Intemperie_150cm_Minima",
              "Humedad_Suelo",
              "Precipitacion_Pluviometrica",
              "Granizo",
              "Nieve",
              "Heliofania_Efectiva",
              "Heliofania_Relativa",
              "Tesion_Vapor_Media",
              "Humedad_Media",
              "Humedad_Media_8_14_20",
              "Rocio_Medio",
              "Duracion_Follaje_Mojado",
              "Velocidad_Viento_200cm_Media",
              "Direccion_Viento_200cm",
              "Velocidad_Viento_1000cm_Media",
              "Direccion_Viento_1000cm",
              "Velocidad_Viento_Maxima",
              "Presion_Media",
              "Radiacion_Global",
              "Radiacion_Neta",
              "Evaporacion_Tanque",
              "Evapotranspiracion_Potencial",
              "Profundidad_Napa",
              "Horas_Frio",
              "Unidad_Frio"
            ),
            selected = "Temperatura_Abrigo_150cm",
            multiple = TRUE
          ),
          downloadButton("Datos_meteo_Balcarce", "Descargar (.xlsx)"),
          br(),
          h6(HTML(
            "<strong>Cita:</strong><br>
              Lewczuk, N. & Echarte, L.(2025). <i>Base de datos meteorológicos de la Estación Meteorológica Convencional Balcarce</i> [Aplicación web]. 
              Laboratorio de Agrometeorología, IPADS CONICET-INTA"
          )),
        ),

        tabItem(
          tabName = "referencias",
          br(),
          h4(HTML("<strong>Referencias bibliográficas</strong>")),
          br(),
          br(),
          h6(HTML(
            "<strong>Allen</strong>, R.G., Pereira, L.S. Raes, D. Y D. Smith. 1998. Crop evapotranspiration. Guides for computing crop water requirements. FAO Irrig. Drain. Nº 56. Italy, 300 p."
          )),
          h6(HTML(
            "<strong>Andrade</strong>, FH., Otegui, ME., Cirilo, A., Uhart, S. 2023.  “Ecofisiología y manejo del cultivo de maíz”.  Maizar."
          )),
          h6(HTML(
            "<strong>Cerrudo</strong>, A, Di Matteo J, Fernandez E, Robles M, Pico LO, Andrade FH. 2013. Yield components of maize as affected by short shading periods and thinning. Crop and Pasture Science 64, 580."
          )),
          h6(HTML(
            "<strong>Chang et al.</strong> (2021). shiny: Web Application Framework for R. R package version 1.7.1, <https://CRAN.R-project.org/package=shiny>"
          )),
          h6(HTML(
            "<strong>Della Maggiora</strong>, A.I., A.I. Irigoyen, J. M. Gardiol, O. Caviglia and L. Echarte. 2002/03. Evaluación de un balance de agua en el suelo para maíz. Revista Argentina de Agrometeorología, 2(2):167-176."
          )),
          h6(HTML(
            "<strong>Echarte</strong>, L., Otegui, M.E. 2023. Consumo y eficiencia en el uso del agua. En: Andrade, FH., Otegui, ME., Cirilo, A., Uhart, S. (Eds) “Ecofisiología y manejo del cultivo de maíz” (pp. 221-244).  Maizar."
          )),
          h6(HTML(
            "<strong>Gardiol</strong>, J.M., Della Maggiora, A. Irigoyen, A. 2002. Curvas de coeficientes de cultivo de maíz, girasol y soja. IX Reunión Argentina de Agrometeorología. Córdoba."
          )),
          h6(HTML(
            "<strong>Gardiol</strong>, J. M., Leonardo, A. S., & Aida, I. D.M. 2003. Modeling evapotranspiration of corn (Zea mays) under different plant densities. Journal of Hydrology, 271, 291–308. https://doi.org/10.1016/S0022-1694(02)00347-5."
          )),
          h6(HTML(
            "<strong>Gardiol</strong>, J.M., Della Maggiora, A., Irigoyen, A. 2006. Coeficientes de cultivo de soja basados en la evapotranspiración de referencia Penman-Monteith."
          )),
          h6(HTML(
            "<strong>Monzon</strong>, J. P., Cafaro La Menza, N., Cerrudo, A., Canepa, M., Rattalino Edreira, J. I., Specht, J., et al. 2021. Critical period for seed number determination in soybean as determined by crop growth rate, duration, and dry matter accumulation. Field Crops Res. 261:108016. doi: 10.1016/j.fcr.2020.108016."
          )),
          h6(HTML(
            "<strong>RECSO</strong>, Base de datos de RECSO Balcarce periodo 2013-2023. Compilada por Marina Montoya, INTA Balcarce. Agosto 2023. Colaboradores: Auxilares Walter Suarez, Silvio Giuliano, Carlos Antonelli, Mauro Zabaleta, Mariano Ruberto (INTA Balcarce). Fuente: Información publicada anualmente por Comunicaciones INTA Balcarce. Actividades incluidas en el convenio INTA-ASA."
          )),
          # h6(HTML("<strong>Sievert</strong>, C, Iannone R, Allaire J, Borges B (2023). flexdashboard: R Markdown Format for Flexible Dashboards. R package version 0.6.1.9000, <https://pkgs.rstudio.com/flexdashboard/, https://github.com/rstudio/flexdashboard/>")),
          h6(HTML(
            "<strong>Winston</strong>, C. and, Borges Ribeiro, B. shinydashboard: Create Dashboards with 'Shiny'. http://rstudio.github.io/shinydashboard/,  https://github.com/rstudio/shinydashboard/>"
          )),
          br(),
          br(),
          h6(HTML(
            "La aplicación fue desarrollada por <a href='https://github.com/Nuria1982/app_EMCBalcarce' target='_blank'>Nuria Lewczuk</a> utilizando el paquete Shiny y Shinydashboard de R."
          ))
        )
      )
    )
  )


# Server ----

server <- function(input, output, session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$tab)) {
      updateTabItems(session, "siderbarID", selected = query$tab)
    }
  })

  ##### EMC Balcarce #####

  # periodo 1991-2020
  promedio_historico <- mean(
    aggregate(
      Precipitacion_Pluviometrica ~ Año,
      data = datos_historicos,
      FUN = sum
    )$Precipitacion_Pluviometrica
  )

  promedio_historico_ttmax <- mean(
    aggregate(
      Temperatura_Abrigo_150cm_Maxima ~ Año,
      data = datos_historicos,
      FUN = mean
    )$Temperatura_Abrigo_150cm_Maxima
  )

  promedio_historico_ttmin <- mean(
    aggregate(
      Temperatura_Abrigo_150cm_Minima ~ Año,
      data = datos_historicos,
      FUN = mean
    )$Temperatura_Abrigo_150cm_Minima
  )

  current_year <- max(datos_actuales$Año)
  pp_acum <- sum(
    subset(datos_actuales, Año == current_year)$Precipitacion_Pluviometrica
  )

  ttmax_anual <- mean(
    subset(datos_actuales, Año == current_year)$Temperatura_Abrigo_150cm_Maxima
  )

  ttmin_anual <- mean(
    subset(datos_actuales, Año == current_year)$Temperatura_Abrigo_150cm_Minima,
    na.rm = TRUE
  )

  ultima_fecha <- max(datos_actuales$Fecha)
  ultimos_datos <- datos_actuales[datos_actuales$Fecha == ultima_fecha, ]
  lluvia_ultimo_dia <- ultimos_datos$Precipitacion_Pluviometrica
  Tmax_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Maxima
  Tmin_ultimo_dia <- ultimos_datos$Temperatura_Abrigo_150cm_Minima

  output$value1 <- renderInfoBox({
    infoBox(
      title = div(
        p(
          "Última fecha",
          style = "text-align: center; font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 6px;"
      ),
      value = div(
        format(ultima_fecha, "%d/%m/%Y"),
        style = "text-align: center; font-size: 28px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-calendar",
        style = "font-size: 60px; opacity: 0.6;"
      ),
      color = "orange",
      fill = TRUE
    )
  })

  output$value2 <- renderInfoBox({
    infoBox(
      title = div(
        p(
          "Lluvia",
          style = "text-align: center;font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 6px;"
      ),
      value = div(
        paste(round(lluvia_ultimo_dia, 1), "mm"),
        style = "text-align: center; font-size: 28px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-tint",
        style = "font-size: 60px; opacity: 0.6;"
      ),
      color = "info",
      fill = TRUE
    )
  })

  output$value3 <- renderInfoBox({
    infoBox(
      title = div(
        p(
          "Temp. Máxima",
          style = "text-align: center;font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 6px;"
      ),
      value = div(
        paste(round(Tmax_ultimo_dia, 1), "ºC"),
        style = "text-align: center; font-size: 28px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-sun",
        style = "font-size: 60px; opacity: 0.6;"
      ),
      color = "danger",
      fill = TRUE
    )
  })

  output$value4 <- renderInfoBox({
    infoBox(
      title = div(
        p(
          "Temp. Mínima",
          style = "text-align: center;font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 6px;"
      ),
      value = div(
        paste(round(Tmin_ultimo_dia, 1), "ºC"),
        style = "text-align: center; font-size: 28px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-snowflake",
        style = "font-size: 60px; opacity: 0.6;"
      ),
      color = "warning",
      fill = TRUE
    )
  })

  ultima_fecha <- max(datos_actuales$Fecha)

  mes_ultimo <- lubridate::month(ultima_fecha)
  dia_ultimo <- lubridate::day(ultima_fecha)

  datos_historicos_hasta_fecha <- datos_historicos %>%
    mutate(
      Fecha = as.Date(Fecha),
      mes = lubridate::month(Fecha),
      dia = lubridate::day(Fecha)
    ) %>%
    filter(
      mes < mes_ultimo |
        (mes == mes_ultimo & dia <= dia_ultimo)
    )

  promedio_historico_hasta_fecha <- median(
    aggregate(
      Precipitacion_Pluviometrica ~ Año,
      data = datos_historicos_hasta_fecha,
      FUN = sum
    )$Precipitacion_Pluviometrica,
    na.rm = TRUE
  )

  promedio_historico_ttmax_hasta_fecha <- mean(
    aggregate(
      Temperatura_Abrigo_150cm_Maxima ~ Año,
      data = datos_historicos_hasta_fecha,
      FUN = mean
    )$Temperatura_Abrigo_150cm_Maxima,
    na.rm = TRUE
  )

  promedio_historico_ttmin_hasta_fecha <- mean(
    aggregate(
      Temperatura_Abrigo_150cm_Minima ~ Año,
      data = datos_historicos_hasta_fecha,
      FUN = mean
    )$Temperatura_Abrigo_150cm_Minima,
    na.rm = TRUE
  )

  output$precipitation_info_box <- renderInfoBox({
    infoBox(
      title = "Precipitaciones Acumuladas",
      value = paste(
        "Acumulado hasta el ",
        format(ultima_fecha, "%d/%m"),
        ": ",
        round(pp_acum, 0),
        "mm"
      ),
      subtitle = HTML(
        paste(
          "Acumulada mediana histórica hasta el ",
          format(ultima_fecha, "%d/%m"),
          " : ",
          round(promedio_historico_hasta_fecha, 0),
          " mm ",
          "<br>",
          "Acumulada mediana anual (1991-2020): ",
          round(promedio_historico, 0),
          " mm"
        )
      ),
      icon = icon("tint"),
      color = "info"
    )
  })

  output$tempMax_info_box <- renderInfoBox({
    infoBox(
      title = "Temperaturas Máximas",
      value = paste(
        "Media hasta el ",
        format(ultima_fecha, "%d/%m"),
        ": ",
        round(ttmax_anual, 2),
        "ºC"
      ),
      subtitle = HTML(paste(
        "Media histórica hasta el ",
        format(ultima_fecha, "%d/%m"),
        " : ",
        round(promedio_historico_ttmax_hasta_fecha, 2),
        " ºC ",
        "<br>",
        "Media anual (1991-2020): ",
        round(promedio_historico_ttmax, 2),
        " ºC"
      )),
      icon = icon("sun"),
      color = "danger"
    )
  })

  output$tempMin_info_box <- renderInfoBox({
    infoBox(
      title = "Temperaturas Mínimas",
      value = paste(
        "Media hasta el ",
        format(ultima_fecha, "%d/%m"),
        ": ",
        round(ttmin_anual, 2),
        "ºC"
      ),
      subtitle = HTML(
        paste0(
          "Media histórica hasta el ",
          format(ultima_fecha, "%d/%m"),
          " : ",
          round(promedio_historico_ttmin_hasta_fecha, 2),
          " ºC",
          "<br>",
          "Media anual (1991-2020): ",
          round(promedio_historico_ttmin, 2),
          " ºC"
        )
      ),
      icon = icon("snowflake"),
      color = "warning"
    )
  })

  datasetInput <- reactive({
    if (input$ano_selector == "Todos los años") {
      datos_filtrados <- datos_actuales
    } else {
      datos_filtrados <- subset(datos_actuales, Año == input$ano_selector)
    }

    return(datos_filtrados)
  })

  output$grafico_lluvia <- renderPlotly({
    niveles_meses <- c(
      "Ene",
      "Feb",
      "Mar",
      "Abr",
      "May",
      "Jun",
      "Jul",
      "Ago",
      "Sep",
      "Oct",
      "Nov",
      "Dic"
    )

    dataset_acumulado <- datasetInput() %>%
      mutate(
        Mes = factor(
          niveles_meses[month(Fecha)],
          levels = niveles_meses
        )
      ) %>%
      group_by(Mes) %>%
      summarise(
        Precipitacion_Acumulada = round(
          sum(Precipitacion_Pluviometrica, na.rm = TRUE),
          1
        ),
        .groups = "drop"
      )

    dataset_acumulado <- tibble(
      Mes = factor(
        niveles_meses,
        levels = niveles_meses
      )
    ) %>%
      left_join(dataset_acumulado, by = "Mes") %>%
      mutate(
        Precipitacion_Acumulada = replace_na(
          Precipitacion_Acumulada,
          0
        )
      )

    historical_precipitation <- datos_historicos %>%
      mutate(
        Mes = factor(
          niveles_meses[month(Fecha)],
          levels = niveles_meses
        )
      ) %>%
      group_by(Año, Mes) %>%
      summarise(
        Precipitacion_Historica = sum(
          Precipitacion_Pluviometrica,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    historical_precipitation_mensual <- historical_precipitation %>%
      group_by(Mes) %>%
      summarise(
        Precipitacion_Historica_Mensual = round(
          mean(Precipitacion_Historica, na.rm = TRUE),
          1
        ),
        .groups = "drop"
      )

    historical_precipitation_mensual <- tibble(
      Mes = factor(niveles_meses, levels = niveles_meses)
    ) %>%
      left_join(historical_precipitation_mensual, by = "Mes")

    dataset_completo <- dataset_acumulado %>%
      left_join(historical_precipitation_mensual, by = "Mes")

    anio_seleccionado_label <- input$ano_selector

    lluvia <- ggplot(dataset_completo, aes(x = Mes)) +

      geom_bar(
        aes(
          y = Precipitacion_Historica_Mensual,
          text = paste0(
            "Mes: ",
            Mes,
            "<br>Histórico: ",
            round(Precipitacion_Historica_Mensual, 0),
            " mm"
          )
        ),
        stat = "identity",
        fill = "#6C757D",
        color = "#495057",
        alpha = 0.4
      ) +

      geom_bar(
        aes(
          y = Precipitacion_Acumulada,
          text = paste0(
            "Mes: ",
            Mes,
            "<br>",
            anio_seleccionado_label,
            ": ",
            round(Precipitacion_Acumulada, 0),
            " mm"
          )
        ),
        stat = "identity",
        fill = "#007EA7",
        color = "#003459",
        alpha = 0.5
      ) +

      labs(
        x = "",
        y = "Precipitación acumulada\nmensual (mm)"
      ) +

      theme_minimal() +

      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          face = "bold",
          color = "black"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black")
      )

    ggplotly(lluvia, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.3,
          y = 1.15
        )
      )
  })

  output$grafico_lluvia_etp_acum <- renderPlotly({
    niveles_meses <- c(
      "Ene",
      "Feb",
      "Mar",
      "Abr",
      "May",
      "Jun",
      "Jul",
      "Ago",
      "Sep",
      "Oct",
      "Nov",
      "Dic"
    )

    dataset_acumulado <- datasetInput() %>%
      mutate(
        Mes = factor(
          niveles_meses[month(Fecha)],
          levels = niveles_meses
        )
      ) %>%
      group_by(Mes) %>%
      summarise(
        Precipitacion_Acumulada = round(
          sum(Precipitacion_Pluviometrica, na.rm = TRUE),
          1
        ),
        Evapotranspiracion_Acumulada = round(
          sum(Evapotranspiracion_Potencial, na.rm = TRUE),
          1
        ),
        .groups = "drop"
      )

    dataset_acumulado <- tibble(
      Mes = factor(
        niveles_meses,
        levels = niveles_meses
      )
    ) %>%
      left_join(dataset_acumulado, by = "Mes") %>%
      mutate(
        Precipitacion_Acumulada = replace_na(
          Precipitacion_Acumulada,
          0
        ),
        Evapotranspiracion_Acumulada = replace_na(
          Evapotranspiracion_Acumulada,
          0
        )
      )

    acum_pp_etp <- ggplot(dataset_acumulado, aes(x = Mes)) +

      geom_bar(
        aes(
          y = Precipitacion_Acumulada,
          text = paste0(
            "Mes: ",
            Mes,
            "<br>Precipitación: ",
            round(Precipitacion_Acumulada, 0),
            " mm"
          )
        ),
        stat = "identity",
        fill = "#007EA7",
        color = "#003459",
        alpha = 0.5
      ) +

      geom_bar(
        aes(
          y = Evapotranspiracion_Acumulada,
          text = paste0(
            "Mes: ",
            Mes,
            "<br>ETP: ",
            round(Evapotranspiracion_Acumulada, 0),
            " mm"
          )
        ),
        stat = "identity",
        fill = "#BF4342",
        color = "#8C1C13",
        alpha = 0.5
      ) +

      labs(
        x = "",
        y = "Acumulado mensual (mm)"
      ) +

      theme_minimal() +

      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          face = "bold",
          color = "black"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black")
      )

    ggplotly(acum_pp_etp, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.1,
          y = 1.15
        )
      )
  })


  output$tempmax_diaria <- renderPlotly({
    
    req(input$ano_selector)
    
    anio_sel <- as.numeric(input$ano_selector)
    
    ultima_fecha_anio <- datos_actuales %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      filter(year(Fecha) == anio_sel) %>%
      summarise(ultima = max(Fecha, na.rm = TRUE)) %>%
      pull(ultima)
    
    doy_hoy <- yday(ultima_fecha_anio)
    
    meses_pos <- yday(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")))
    meses_lab <- c("ene", "feb", "mar", "abr", "may", "jun",
                   "jul", "ago", "sept", "oct", "nov", "dic")
    
    temp_actual <- datos_actuales %>%
      mutate(
        Fecha = as.Date(Fecha),
        anio = year(Fecha),
        doy = yday(Fecha)
      ) %>%
      filter(anio == anio_sel) %>%
      group_by(doy) %>%
      summarise(
        Tmax_actual = mean(Temperatura_Abrigo_150cm_Maxima, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(doy) %>%
      mutate(
        Tmax_suavizada = zoo::rollmean(
          Tmax_actual,
          k = 7,
          fill = "extend",
          align = "center"
        )
      )
    
    temp_historica <- datos_historicos %>%
      mutate(
        Fecha = as.Date(Fecha),
        doy = yday(Fecha)
      ) %>%
      group_by(doy) %>%
      summarise(
        tmin = min(Temperatura_Abrigo_150cm_Maxima, na.rm = TRUE),
        tmax = max(Temperatura_Abrigo_150cm_Maxima, na.rm = TRUE),
        media = mean(Temperatura_Abrigo_150cm_Maxima, na.rm = TRUE),
        .groups = "drop"
      )
    
    df_plot <- tibble(doy = 1:365) %>%
      left_join(temp_historica, by = "doy") %>%
      left_join(temp_actual, by = "doy")
    
    df_plot <- df_plot %>%
      mutate(
        fecha = as.Date(doy - 1, origin = "2021-01-01")
      ) 
    
    df_real <- df_plot %>% filter(doy <= doy_hoy)
    df_futuro <- df_plot %>% filter(doy > doy_hoy)
    
    plot_ly() %>%
      
      add_ribbons(
        data = df_plot,
        x = ~doy,
        ymin = ~tmin,
        ymax = ~tmax,
        name = "Rango histórico (min-max)",
        fillcolor = "rgba(252,185,178,0.60)",
        line = list(color = "rgba(252,185,178,0)"),
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>Mín: ", round(tmin, 1), " °C",
          "<br>Máx: ", round(tmax, 1), " °C"
        )
      ) %>%
      
      add_lines(
        data = df_plot,
        x = ~doy,
        y = ~media,
        name = "Media histórica",
        line = list(
          color = "rgba(252,185,178,0.9)",
          width = 2,
          dash = "dash"
        ),
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>Media histórica: ",
          round(media, 1), " °C"
        )
      ) %>%
      
      add_lines(
        data = df_real,
        x = ~doy,
        y = ~Tmax_suavizada,
        name = as.character(anio_sel),
        line = list(color = "#D00000", width = 3),
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>", anio_sel, ": ",
          round(Tmax_suavizada, 1), " °C"
        )
      ) %>%
      
      add_lines(
        data = df_futuro,
        x = ~doy,
        y = ~Tmax_suavizada,
        name = paste0(anio_sel, " futuro"),
        line = list(color = "rgba(208,0,0,0.35)", width = 3, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>", anio_sel, ": sin datos reales"
        )
      ) %>%
      
      layout(
        xaxis = list(
          title = "",
          tickvals = meses_pos,
          ticktext = meses_lab,
          range = c(1, 365)
        ),
        yaxis = list(
          title = "Temperatura máxima diaria (°C)"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.15
        )
      )
  })
  
  
  output$tempmin_diaria <- renderPlotly({
    
    req(input$ano_selector)
    
    anio_sel <- as.numeric(input$ano_selector)
    
    ultima_fecha_anio <- datos_actuales %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      filter(year(Fecha) == anio_sel) %>%
      summarise(ultima = max(Fecha, na.rm = TRUE)) %>%
      pull(ultima)
    
    doy_hoy <- yday(ultima_fecha_anio)
    
    meses_pos <- yday(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")))
    meses_lab <- c("ene", "feb", "mar", "abr", "may", "jun",
                   "jul", "ago", "sept", "oct", "nov", "dic")
    
    temp_actual <- datos_actuales %>%
      mutate(
        Fecha = as.Date(Fecha),
        anio = year(Fecha),
        doy = yday(Fecha)
      ) %>%
      filter(anio == anio_sel) %>%
      group_by(doy) %>%
      summarise(
        Tmin_actual = mean(Temperatura_Abrigo_150cm_Minima, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(doy) %>%
      mutate(
        Tmin_suavizada = zoo::rollmean(
          Tmin_actual,
          k = 7,
          fill = "extend",
          align = "center"
        )
      )
    
    temp_historica <- datos_historicos %>%
      mutate(
        Fecha = as.Date(Fecha),
        doy = yday(Fecha)
      ) %>%
      group_by(doy) %>%
      summarise(
        tmin = min(Temperatura_Abrigo_150cm_Minima, na.rm = TRUE),
        tmax = max(Temperatura_Abrigo_150cm_Minima, na.rm = TRUE),
        media = mean(Temperatura_Abrigo_150cm_Minima, na.rm = TRUE),
        .groups = "drop"
      )
    
    df_plot <- tibble(doy = 1:365) %>%
      left_join(temp_historica, by = "doy") %>%
      left_join(temp_actual, by = "doy")
    
    df_plot <- df_plot %>%
      mutate(
        fecha = as.Date(doy - 1, origin = "2021-01-01")
      )
    
    df_real <- df_plot %>% filter(doy <= doy_hoy)
    df_futuro <- df_plot %>% filter(doy > doy_hoy)
    
    plot_ly() %>%
      
      add_ribbons(
        data = df_plot,
        x = ~doy,
        ymin = ~tmin,
        ymax = ~tmax,
        name = "Rango histórico (min-max)",
        fillcolor = "rgba(237,222,164,0.60)",
        line = list(color = "rgba(237,222,164,0)"),
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>Mín histórica: ", round(tmin, 1), " °C",
          "<br>Máx histórica: ", round(tmax, 1), " °C"
        )
      ) %>%
      
      add_lines(
        data = df_plot,
        x = ~doy,
        y = ~media,
        name = "Media histórica",
        line = list(
          color = "rgba(237,222,164,0.90)",
          width = 2,
          dash = "dash"
        ),
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>Media histórica: ",
          round(media, 1), " °C"
        )
      ) %>%
      
      add_lines(
        data = df_real,
        x = ~doy,
        y = ~Tmin_suavizada,
        name = as.character(anio_sel),
        line = list(color = "#FFBA08", width = 3),
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>", anio_sel, ": ",
          round(Tmin_suavizada, 1), " °C"
        )
      ) %>%
      
      add_lines(
        data = df_futuro,
        x = ~doy,
        y = ~Tmin_suavizada,
        name = paste0(anio_sel, " futuro"),
        line = list(color = "rgba(255,186,8,0.35)", width = 3, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~paste0(
          format(fecha, "%d/%m"),
          "<br>", anio_sel, ": sin datos reales"
        )
      ) %>%
      
      layout(
        xaxis = list(
          title = "",
          tickvals = meses_pos,
          ticktext = meses_lab,
          range = c(1, 365)
        ),
        yaxis = list(
          title = "Temperatura mínima diaria (°C)"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.15
        )
      )
  })
  
  output$grafico_temperatura <- renderPlotly({
    
    niveles_meses <- c(
      "Ene", "Feb", "Mar", "Abr", "May", "Jun",
      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"
    )
    
    anio_sel <- as.numeric(input$ano_selector)
    
    ultima_fecha_anio <- datasetInput() %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      filter(year(Fecha) == anio_sel) %>%
      summarise(ultima = max(Fecha, na.rm = TRUE)) %>%
      pull(ultima)
    
    ultimo_dia_mes <- ceiling_date(ultima_fecha_anio, "month") - days(1)
    
    ultimo_mes_completo <- ifelse(
      ultima_fecha_anio == ultimo_dia_mes,
      month(ultima_fecha_anio),
      month(ultima_fecha_anio) - 1
    )
    
    mes_en_curso <- ultimo_mes_completo + 1
    
    temperatura_mensual <- datasetInput() %>%
      mutate(
        Fecha = as.Date(Fecha),
        anio = year(Fecha),
        mes_num = month(Fecha),
        Mes = factor(niveles_meses[mes_num], levels = niveles_meses)
      ) %>%
      filter(
        anio == anio_sel,
        mes_num <= ultimo_mes_completo
      ) %>%
      group_by(Mes) %>%
      summarise(
        Temperatura_Media = round(
          mean(Temperatura_Abrigo_150cm, na.rm = TRUE),
          0
        ),
        mes_num = first(mes_num),
        .groups = "drop"
      ) %>%
      mutate(
        Temperatura_Media = ifelse(
          mes_num >= mes_en_curso,
          NA,
          Temperatura_Media
        )
      )
    
    temperatura_mensual <- tibble(
      Mes = factor(niveles_meses, levels = niveles_meses)
    ) %>%
      left_join(temperatura_mensual, by = "Mes")
    
    historico_temperatura_mensual <- datos_historicos %>%
      mutate(
        Mes = factor(niveles_meses[month(Fecha)], levels = niveles_meses)
      ) %>%
      group_by(Mes) %>%
      summarise(
        Temperatura_Media_Historica = round(
          mean(Temperatura_Abrigo_150cm, na.rm = TRUE),
          0
        ),
        .groups = "drop"
      )
    
    historico_temperatura_mensual <- tibble(
      Mes = factor(niveles_meses, levels = niveles_meses)
    ) %>%
      left_join(historico_temperatura_mensual, by = "Mes")
    
    dataset_completo <- temperatura_mensual %>%
      left_join(historico_temperatura_mensual, by = "Mes")
    
    anio_seleccionado_label <- input$ano_selector
    
    temp_media <- ggplot(dataset_completo, aes(x = Mes)) +
      
      geom_bar(
        aes(
          y = Temperatura_Media_Historica,
          text = paste0(
            "Mes: ", Mes,
            "<br>Histórico: ",
            round(Temperatura_Media_Historica, 0),
            " °C"
          )
        ),
        stat = "identity",
        fill = "#6C757D",
        color = "#495057",
        alpha = 0.4
      ) +
      
      geom_bar(
        aes(
          y = Temperatura_Media,
          text = paste0(
            "Mes: ", Mes,
            "<br>", anio_seleccionado_label, ": ",
            round(Temperatura_Media, 0),
            " °C"
          )
        ),
        stat = "identity",
        fill = "#D00000",
        color = "#8B0000",
        alpha = 0.5
      ) +
      
      labs(
        x = "",
        y = "Temperatura media\nmensual (°C)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          face = "bold",
          color = "black"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black")
      )
    
    ggplotly(temp_media, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.3,
          y = 1.15
        )
      )
  })
  
  
  output$grafico_heladas <- renderPlotly({
    
    niveles_meses <- c(
      "Ene", "Feb", "Mar", "Abr", "May", "Jun",
      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"
    )
    
    anio_sel <- as.numeric(input$ano_selector)
    
    heladas_actual <- datasetInput() %>%
      mutate(
        Fecha = as.Date(Fecha),
        anio = year(Fecha),
        Mes = factor(niveles_meses[month(Fecha)], levels = niveles_meses)
      ) %>%
      filter(
        anio == anio_sel,
        Temperatura_Abrigo_150cm_Minima <= 3
      ) %>%
      group_by(Mes) %>%
      summarise(
        Dias_helada_actual = n(),
        .groups = "drop"
      )
    
    heladas_actual <- tibble(
      Mes = factor(niveles_meses, levels = niveles_meses)
    ) %>%
      left_join(heladas_actual, by = "Mes") %>%
      mutate(
        Dias_helada_actual = replace_na(Dias_helada_actual, 0)
      )
    
    heladas_historicas <- datos_historicos %>%
      mutate(
        Fecha = as.Date(Fecha),
        Mes = factor(niveles_meses[month(Fecha)], levels = niveles_meses)
      ) %>%
      filter(Temperatura_Abrigo_150cm_Minima <= 3) %>%
      group_by(Año, Mes) %>%
      summarise(
        dias_helada = n(),
        .groups = "drop"
      ) %>%
      group_by(Mes) %>%
      summarise(
        Dias_helada_historica = round(mean(dias_helada, na.rm = TRUE), 0),
        .groups = "drop"
      )
    
    heladas_historicas <- tibble(
      Mes = factor(niveles_meses, levels = niveles_meses)
    ) %>%
      left_join(heladas_historicas, by = "Mes") %>%
      mutate(
        Dias_helada_historica = replace_na(Dias_helada_historica, 0)
      )
    
    dataset_completo <- heladas_actual %>%
      left_join(heladas_historicas, by = "Mes")
    
    heladas <- ggplot(dataset_completo, aes(x = Mes)) +
      
      geom_bar(
        aes(
          y = Dias_helada_historica,
          text = paste0(
            "Mes: ", Mes,
            "<br>Histórico: ",
            Dias_helada_historica,
            " días"
          )
        ),
        stat = "identity",
        fill = "#6C757D",
        color = "#495057",
        alpha = 0.4
      ) +
      
      geom_bar(
        aes(
          y = Dias_helada_actual,
          text = paste0(
            "Mes: ", Mes,
            "<br>", anio_sel, ": ",
            Dias_helada_actual
          )
        ),
        stat = "identity",
        fill = "#FFBA08",
        color = "#CC9500",
        alpha = 0.5
      ) +
      
      labs(
        x = "",
        y = "Número de días con\nTemperatura mínima ≤ 3°C"
      ) +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          face = "bold",
          color = "black"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black")
      )
    
    ggplotly(heladas, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })

  ##### CAMBIO CLIMATICO ######

  datasetInput_climatico <- reactive({
    año_actual <- lubridate::year(Sys.Date())

    datos_filtrados_climatico <- datos %>%
      filter(Año < año_actual)

    if (!is.null(input$mes_climatico) && !"Anual" %in% input$mes_climatico) {
      datos_filtrados_climatico <- datos_filtrados_climatico %>%
        filter(Mes == input$mes_climatico)
    }

    return(datos_filtrados_climatico)
  })

  output$grafico_temp_climatico <- renderPlotly({
    datos_filtrados_climatico <- datasetInput_climatico()

    if (input$mes_climatico == "Anual") {
      tt_media <- datos_filtrados_climatico %>%
        group_by(Año) %>%
        summarise(
          Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE)
        )

      etiquetas_10_anios <- tt_media %>%
        filter(Año %% 10 == 0)

      tt_climatico <- ggplot(tt_media, aes(x = Año, y = Temperatura_media)) +
        geom_line(color = "#FFBA08", linewidth = 1) +
        geom_point(size = 1, color = "#FFBA08") +
        geom_smooth(method = "lm", color = "red", linewidth = 0.5, se = TRUE) +
        geom_text(
          data = etiquetas_10_anios,
          aes(label = round(Temperatura_media, 1)),
          vjust = -0.5,
          color = "black",
          size = 3
        ) +
        labs(x = "", y = "ºC", title = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    } else {
      tt_media <- datos_filtrados_climatico %>%
        group_by(Año) %>%
        summarise(
          Temperatura_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE)
        )

      etiquetas_10_anios <- tt_media %>%
        filter(Año %% 10 == 0)

      tt_climatico <- ggplot(tt_media, aes(x = Año, y = Temperatura_media)) +
        geom_line(color = "#FFBA08", linewidth = 1) +
        geom_point(size = 1, color = "#FFBA08") +
        geom_smooth(method = "lm", color = "grey", linewidth = 0.5, se = TRUE) +
        geom_text(
          data = etiquetas_10_anios,
          aes(label = round(Temperatura_media, 1)),
          vjust = -0.5,
          color = "black",
          size = 3
        ) +
        labs(x = "", y = "ºC", title = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    }

    ggplotly(tt_climatico)
  })

  output$grafico_pp_climatico <- renderPlotly({
    datos_filtrados_climatico <- datasetInput_climatico()
    if (input$mes_climatico == "Anual") {
      pp_acum <- datos_filtrados_climatico %>%
        group_by(Año) %>%
        summarise(
          Precipitacion_Acumulada = sum(
            Precipitacion_Pluviometrica,
            na.rm = TRUE
          )
        )

      etiquetas_10_anios <- pp_acum %>%
        filter(Año %% 10 == 0) %>%
        mutate(
          Año_medio = Año + 2,
          Precipitacion_media = round(Precipitacion_Acumulada, 0)
        )
      max_y <- max(pp_acum$Precipitacion_Acumulada)

      pp_climatico <- ggplot(
        pp_acum,
        aes(x = factor(Año), y = Precipitacion_Acumulada)
      ) +
        geom_bar(
          stat = "identity",
          fill = "#007EA7",
          color = "#003459",
          alpha = 0.5
        ) +
        labs(x = "", y = "mm", title = "") +
        scale_x_discrete(breaks = pp_acum$Año[pp_acum$Año %% 10 == 0]) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.caption = element_text(hjust = 0.5, face = "italic")
        ) +
        geom_text(
          data = etiquetas_10_anios,
          aes(x = factor(Año_medio), y = max_y, label = Precipitacion_media),
          vjust = -0.5,
          color = "black",
          size = 3.5
        )
    } else {
      pp_acum <- datos_filtrados_climatico %>%
        group_by(Año) %>%
        summarise(
          Precipitacion_Acumulada = sum(
            Precipitacion_Pluviometrica,
            na.rm = TRUE
          )
        )

      etiquetas_10_anios <- pp_acum %>%
        filter(Año %% 10 == 0) %>%
        mutate(
          Año_medio = Año + 2,
          Precipitacion_media = round(Precipitacion_Acumulada, 0)
        )
      max_y <- max(pp_acum$Precipitacion_Acumulada)

      pp_climatico <- ggplot(
        pp_acum,
        aes(x = Año, y = Precipitacion_Acumulada)
      ) +
        geom_bar(
          stat = "identity",
          fill = "#007EA7",
          color = "#003459",
          alpha = 0.5
        ) +
        labs(x = "", y = "mm", title = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        geom_text(
          data = etiquetas_10_anios,
          aes(x = Año_medio, y = max_y, label = Precipitacion_media),
          vjust = -0.5,
          color = "black",
          size = 3.5
        )
    }

    ggplotly(pp_climatico)
  })

  output$grafico_heladas_climatico <- renderPlotly({
    datos_filtrados_climatico <- datasetInput_climatico()

    dias_extremos <- datos_filtrados_climatico %>%
      mutate(
        dia_bajo = Temperatura_Abrigo_150cm_Minima <= 3,
        dia_alto = Temperatura_Abrigo_150cm_Maxima > 25
      ) %>%
      group_by(Año) %>%
      summarise(
        dias_bajos = sum(dia_bajo, na.rm = TRUE),
        dias_altos = sum(dia_alto, na.rm = TRUE)
      ) %>%
      arrange(Año)

    if (input$mes_climatico == "Anual") {
      # Gráfico para el número de días por año
      dias_extremos_long <- dias_extremos %>%
        pivot_longer(
          cols = c(dias_bajos, dias_altos),
          names_to = "Tipo",
          values_to = "Numero_de_dias"
        ) %>%
        mutate(
          Tipo = factor(
            Tipo,
            levels = c("dias_bajos", "dias_altos"),
            labels = c("Días < 3ºC", "Días > 25ºC")
          )
        )

      tt_dias_extremos <- ggplot(
        dias_extremos_long,
        aes(x = Año, y = Numero_de_dias, color = Tipo, group = Tipo)
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        geom_smooth(
          method = "lm",
          aes(group = Tipo),
          se = TRUE,
          linetype = "dashed",
          size = 0.8
        ) +
        labs(x = "", y = "Días", title = "", color = "") +
        scale_color_manual(
          values = c("Días < 3ºC" = "#A8DADC", "Días > 25ºC" = "#E63946")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    } else {
      # Gráfico para el número de días por mes
      dias_extremos_mes <- datos_filtrados_climatico %>%
        mutate(
          dia_bajo = Temperatura_Abrigo_150cm_Minima < 3,
          dia_alto = Temperatura_Abrigo_150cm_Maxima > 25
        ) %>%
        group_by(Año) %>%
        summarise(
          dias_bajos = sum(dia_bajo, na.rm = TRUE),
          dias_altos = sum(dia_alto, na.rm = TRUE)
        )

      dias_extremos_long_mes <- dias_extremos_mes %>%
        pivot_longer(
          cols = c(dias_bajos, dias_altos),
          names_to = "Tipo",
          values_to = "Numero_de_dias"
        ) %>%
        mutate(
          Tipo = factor(
            Tipo,
            levels = c("dias_bajos", "dias_altos"),
            labels = c("Días < 3ºC", "Días > 25ºC")
          )
        )

      tt_dias_extremos <- ggplot(
        dias_extremos_long_mes,
        aes(x = Año, y = Numero_de_dias, color = Tipo, group = Tipo)
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        geom_smooth(
          method = "lm",
          aes(group = Tipo),
          se = TRUE,
          linetype = "dashed",
          size = 0.8
        ) +
        labs(x = "", y = "Número de Días", title = "", color = "") +
        scale_color_manual(
          values = c("Días < 3ºC" = "#A8DADC", "Días > 25ºC" = "#E63946")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    }

    ggplotly(tt_dias_extremos) %>%
      layout(legend = list(orientation = "h", x = 0.0, y = 1.2))
  })

  output$grafico_primera_helada_agro <- renderPlotly({
    df_primera_agro <- datos %>%
      mutate(
        Fecha = as.Date(Fecha),
        Año = year(Fecha),
        doy = yday(Fecha),
        helada_agro = Temperatura_Abrigo_150cm_Minima <= 3
      ) %>%
      filter(helada_agro) %>%
      group_by(Año) %>%
      slice_min(Fecha, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        doy_primera = yday(Fecha),
        fecha_txt = format(Fecha, "%d/%m/%Y"),
        tooltip = paste0(
          "Año: ",
          Año,
          "<br>Primera helada: ",
          fecha_txt,
          "<br>Tmin: ",
          round(Temperatura_Abrigo_150cm_Minima, 1),
          " °C"
        )
      )

    p <- ggplot(df_primera_agro, aes(x = Año, y = doy_primera)) +

      geom_line(color = "#00AFB9", linewidth = 1) +
      geom_point(aes(text = tooltip), color = "#00AFB9", size = 2) +

      geom_smooth(
        aes(x = Año, y = doy_primera),
        method = "lm",
        se = TRUE,
        color = "grey",
        linewidth = 0.7
      ) +

      scale_y_continuous(
        name = "Fecha de primera helada",
        labels = function(x) {
          format(as.Date(x - 1, origin = "2001-01-01"), "%d/%m")
        }
      ) +

      labs(x = "") +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  output$grafico_ultima_helada_agro <- renderPlotly({
    df_ultima_agro <- datos %>%
      mutate(
        Fecha = as.Date(Fecha),
        Año = lubridate::year(Fecha),
        helada_agro = Temperatura_Abrigo_150cm_Minima <= 3
      ) %>%
      filter(helada_agro) %>%
      group_by(Año) %>%
      slice_max(Fecha, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        doy_ultima = lubridate::yday(Fecha),
        fecha_txt = format(Fecha, "%d/%m/%Y"),
        tooltip = paste0(
          "Año: ",
          Año,
          "<br>Última helada: ",
          fecha_txt,
          "<br>Tmin: ",
          round(Temperatura_Abrigo_150cm_Minima, 1),
          " °C"
        )
      )

    modelo <- lm(doy_ultima ~ Año, data = df_ultima_agro)
    df_ultima_agro$trend <- predict(modelo)

    p <- ggplot(df_ultima_agro, aes(x = Año, y = doy_ultima)) +
      geom_line(color = "#00AFB9", linewidth = 1) +

      geom_point(aes(text = tooltip), color = "#00AFB9", size = 2) +

      geom_smooth(
        aes(x = Año, y = doy_ultima),
        method = "lm",
        se = TRUE,
        color = "grey",
        linewidth = 0.7
      ) +

      scale_y_continuous(
        name = "Fecha de última helada",
        labels = function(x) {
          format(as.Date(x - 1, origin = "2001-01-01"), "%d/%m")
        }
      ) +
      labs(
        x = ""
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1)
      )

    ggplotly(p, tooltip = "text")
  })

  output$grafico_dias_libres_heladas_agro <- renderPlotly({
    df_heladas_agro <- datos %>%
      mutate(
        Fecha = as.Date(Fecha),
        Año = lubridate::year(Fecha),
        helada_agro = Temperatura_Abrigo_150cm_Minima <= 3
      ) %>%
      filter(helada_agro) %>%
      group_by(Año) %>%
      summarise(
        primera = min(Fecha, na.rm = TRUE),
        ultima = max(Fecha, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Año) %>%
      mutate(
        ultima_anio_anterior = lag(ultima),
        dias_libres = as.numeric(primera - ultima_anio_anterior),
        tooltip = paste0(
          "Año: ",
          Año,
          "<br>Días libres de heladas: ",
          round(dias_libres),
          "<br>Última helada año anterior: ",
          format(ultima_anio_anterior, "%d/%m/%Y"),
          "<br>Primera helada: ",
          format(primera, "%d/%m/%Y")
        )
      ) %>%
      filter(!is.na(dias_libres))

    p <- ggplot(df_heladas_agro, aes(x = Año, y = dias_libres)) +
      geom_line(color = "#03045E", linewidth = 1) +
      geom_point(aes(text = tooltip), color = "#03045E", size = 2) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "grey",
        linewidth = 0.7
      ) +
      labs(
        x = "",
        y = "Días libres de heladas"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1)
      )

    ggplotly(p, tooltip = "text")
  })

  ##### HELADAS ######

  heladas_periodo_3 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      mutate(
        fecha = as.Date(Fecha),
        dia_juliano = yday(fecha),
        dia_norm = if_else(
          leap_year(Año) & dia_juliano >= 60,
          dia_juliano + 1,
          dia_juliano
        )
      ) %>%
      filter(Temperatura_Abrigo_150cm_Minima <= 3) %>%
      group_by(Año) %>%
      summarise(
        primera = min(dia_norm),
        ultima = max(dia_norm),
        .groups = "drop"
      ) %>%
      summarise(
        periodo = paste0(start_year, "-", end_year),
        primera_media = mean(primera, na.rm = TRUE),
        ultima_media = mean(ultima, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        fecha_primera_media = as.Date(primera_media, origin = "1800-12-31"),
        fecha_ultima_media = as.Date(ultima_media, origin = "1800-12-31")
      )
  }

  h3 <- heladas_periodo_3(balcarce_EMC, 1991, 2020)

  prob_mes_helada_3 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      group_by(Año, Mes) %>%
      summarise(
        hubo_helada = any(Temperatura_Abrigo_150cm_Minima <= 3),
        .groups = "drop"
      ) %>%
      group_by(Mes) %>%
      summarise(
        meses_con_helada = sum(hubo_helada),
        total_meses = n(),
        .groups = "drop"
      ) %>%
      mutate(
        periodo = paste0(start_year, "-", end_year),
        prob_helada = meses_con_helada / total_meses * 100
      )
  }

  meses_orden <- c(
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo",
    "junio",
    "julio",
    "agosto",
    "septiembre",
    "octubre",
    "noviembre",
    "diciembre"
  )

  dias_medios <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)

  p3 <- prob_mes_helada_3(balcarce_EMC, 1991, 2020) %>%
    mutate(
      Mes = factor(Mes, levels = meses_orden),
      dia_juliano = dias_medios[match(as.character(Mes), meses_orden)]
    ) %>%
    arrange(dia_juliano)

  output$prob_helada_ene_dic_3 <- renderPlotly({
    datos_filtrados <- p3
    fechas_filtradas <- h3

    y_offset <- max(datos_filtrados$prob_helada, na.rm = TRUE) * 0.01

    p_base <- ggplot(
      datos_filtrados,
      aes(x = dia_juliano, y = prob_helada, color = periodo, group = periodo)
    ) +
      geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) +

      geom_vline(
        data = fechas_filtradas,
        aes(xintercept = primera_media, color = periodo),
        linetype = "dashed",
        inherit.aes = FALSE
      ) +
      geom_vline(
        data = fechas_filtradas,
        aes(xintercept = ultima_media, color = periodo),
        linetype = "dashed",
        inherit.aes = FALSE
      ) +

      geom_text(
        data = fechas_filtradas,
        aes(
          x = primera_media,
          y = max(datos_filtrados$prob_helada, na.rm = TRUE) + y_offset,
          label = format(fecha_primera_media, "%d/%m"),
          color = periodo
        ),
        angle = 90,
        vjust = -1.5,
        size = 3,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = fechas_filtradas,
        aes(
          x = ultima_media,
          y = max(datos_filtrados$prob_helada, na.rm = TRUE) + y_offset,
          label = format(fecha_ultima_media, "%d/%m"),
          color = periodo
        ),
        angle = 90,
        vjust = -1.5,
        size = 3,
        inherit.aes = FALSE
      ) +

      scale_x_continuous(
        breaks = dias_medios,
        labels = meses_orden,
        limits = c(0, 365)
      ) +
      scale_color_manual(values = col_agro) +
      scale_y_continuous(
        limits = c(-8, 105),
        labels = scales::percent_format(scale = 1)
      ) +
      labs(x = "Mes", y = "Probabilidad (%)", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )

    # Puntos invisibles para tooltip
    p_base <- p_base +
      geom_point(
        aes(
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Mes: ",
            Mes,
            "<br>Probabilidad: ",
            sprintf("%.2f%%", prob_helada)
          )
        ),
        size = 0,
        alpha = 0
      )

    ggplotly(p_base, tooltip = "text") %>%
      layout(
        annotations = list(
          x = 0.57,
          y = 1.0,
          xref = "paper",
          yref = "paper",
          text = "Fecha media de primera y última helada",
          showarrow = FALSE,
          font = list(size = 10, style = "italic"),
          xanchor = "center",
          yanchor = "bottom"
        )
      )
  })

  heladas_prim_anuales_3 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      mutate(
        fecha = as.Date(Fecha),
        dia_juliano = yday(fecha),
        dia_norm = if_else(
          leap_year(Año) & dia_juliano >= 60,
          dia_juliano + 1,
          dia_juliano
        )
      ) %>%
      filter(Temperatura_Abrigo_150cm_Minima <= 3) %>%
      group_by(Año) %>%
      summarise(
        primera = min(dia_norm),
        .groups = "drop"
      ) %>%
      # convertir día juliano a fecha usando un año ficticio (2000)
      mutate(
        fecha_primera = as.Date(primera, origin = "1999-12-31")
      ) %>%
      arrange(Año)
  }

  h_ano3 <- heladas_prim_anuales_3(balcarce_EMC, 1991, 2020)
  h_ano4 <- heladas_prim_anuales_3(balcarce_EMC, 2016, 2025)

  h_ano3 <- h_ano3 %>% mutate(periodo = "1991-2020")
  h_ano4 <- h_ano4 %>% mutate(periodo = "2016-2025")

  h_todos <- bind_rows(
    h_ano3,
    h_ano4
  )

  h_frec <- h_todos %>%
    mutate(fecha_sin_anio = as.Date(format(fecha_primera, "2000-%m-%d"))) %>%
    group_by(periodo) %>%
    arrange(primera, .by_group = TRUE) %>%
    mutate(frec_acum = row_number() / n()) %>%
    ungroup()

  h_frec <- h_frec %>%
    mutate(dia_juliano = yday(fecha_sin_anio))

  ajustar_logistico <- function(df) {
    fit <- nls(
      frec_acum ~ 1 / (1 + exp(-(a + b * dia_juliano))),
      data = df,
      start = list(a = -5, b = 0.05)
    )

    coefs <- coef(fit)

    pred <- tibble(
      dia_juliano = seq(
        min(df$dia_juliano),
        max(df$dia_juliano),
        length.out = 200
      )
    ) %>%
      mutate(
        frec_acum = predict(fit, newdata = .),
        fecha_sin_anio = as.Date(dia_juliano - 1, origin = "2000-01-01"),
        periodo = unique(df$periodo),
        a = coefs["a"],
        b = coefs["b"]
      )
    pred
  }

  h_pred <- h_frec %>%
    group_by(periodo) %>%
    group_split() %>%
    map_dfr(ajustar_logistico)

  puntos_50 <- h_pred %>%
    group_by(periodo) %>%
    slice_min(abs(frec_acum - 0.5), n = 1) %>%
    ungroup() %>%
    mutate(etiqueta = format(fecha_sin_anio, "%d-%b"))

  puntos_50 <- puntos_50 %>%
    arrange(periodo) %>%
    mutate(
      x_pos = as.Date("2000-03-15"),
      y_pos = seq(0.25, 0.1, length.out = n())
    )

  output$acum_primera_helada_3 <- renderPlotly({
    mes <- as.integer(input$mes_prim_helada_3)
    dia <- as.integer(input$dia_prim_helada_3)
    if (is.na(mes) || is.na(dia)) {
      return(NULL)
    }

    fecha_sel <- as.Date(sprintf("2000-%02d-%02d", mes, dia))
    dia_jul_sel <- yday(fecha_sel)

    params <- h_pred %>%
      filter(periodo == input$periodo_prim_helada_3) %>%
      slice(1)
    if (nrow(params) == 0) {
      return(NULL)
    }

    a <- params$a
    b <- params$b

    frec_sel <- 1 / (1 + exp(-(a + b * dia_jul_sel)))

    punto_sel <- tibble(
      fecha_sin_anio = fecha_sel,
      frec_acum = frec_sel
    )

    colores_periodo <- c(
      "1991-2020" = "darkgrey",
      "2016-2025" = "#0072B2"
    )
    color_sel <- colores_periodo[input$periodo_prim_helada_3]

    plot_prim_helada <- ggplot() +
      geom_point(
        data = h_frec,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 1
      ) +
      geom_line(
        data = h_pred,
        aes(x = fecha_sin_anio, y = frec_acum, color = periodo),
        linewidth = 1
      ) +
      geom_point(
        data = h_pred,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 0,
        alpha = 0
      ) +

      geom_point(
        data = punto_sel,
        aes(x = fecha_sin_anio, y = frec_acum),
        size = 2,
        color = color_sel,
        fill = color_sel,
        shape = 21,
        stroke = 1.5
      ) +

      geom_segment(
        data = punto_sel,
        aes(
          x = min(h_frec$fecha_sin_anio),
          xend = fecha_sin_anio,
          y = frec_acum,
          yend = frec_acum
        ),
        color = color_sel,
        linetype = "dashed",
        linewidth = 1
      ) +
      scale_x_date(date_labels = "%d-%b", date_breaks = "10 day") +
      scale_y_continuous(labels = function(x) x * 100) +
      scale_color_manual(values = colores_periodo) +
      labs(
        x = "Fecha de primera helada",
        y = "Frecuencia acumulada (%)",
        color = NULL
      ) +
      guides(color = guide_legend(nrow = 1)) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.spacing.x = unit(10, "pt"),
        legend.title = element_blank(),
        legend.box.margin = margin(-5, 0, -5, 0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(plot_prim_helada, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.1, y = 1.2)
      )
  })

  heladas_ult_anuales_3 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      mutate(
        fecha = as.Date(Fecha),
        dia_juliano = yday(fecha),
        dia_norm = if_else(
          leap_year(Año) & dia_juliano >= 60,
          dia_juliano + 1,
          dia_juliano
        )
      ) %>%
      filter(Temperatura_Abrigo_150cm_Minima <= 3) %>%
      group_by(Año) %>%
      summarise(
        ultima = max(dia_norm),
        .groups = "drop"
      ) %>%
      mutate(
        fecha_ultima = as.Date(ultima, origin = "1999-12-31")
      ) %>%
      arrange(Año)
  }

  h_ano3_ult <- heladas_ult_anuales_3(balcarce_EMC, 1991, 2020) %>%
    mutate(periodo = "1991-2020")
  h_ano4_ult <- heladas_ult_anuales_3(balcarce_EMC, 2016, 2025) %>%
    mutate(periodo = "2016-2025")

  h_todos_ult <- bind_rows(
    h_ano3_ult,
    h_ano4_ult
  )

  h_frec_ult <- h_todos_ult %>%
    mutate(fecha_sin_anio = as.Date(format(fecha_ultima, "2000-%m-%d"))) %>%
    group_by(periodo) %>%
    arrange(desc(ultima), .by_group = TRUE) %>%
    mutate(frec_acum = row_number() / n()) %>%
    ungroup() %>%
    mutate(dia_juliano = yday(fecha_sin_anio))

  ajustar_logistico_ult <- function(df) {
    start_a <- -log(1 / min(df$frec_acum) - 1)
    start_b <- 0.05

    fit <- tryCatch(
      nlsLM(
        frec_acum ~ 1 / (1 + exp(-(a + b * dia_juliano))),
        data = df,
        start = list(a = start_a, b = start_b),
        control = nls.lm.control(maxiter = 500)
      ),
      error = function(e) return(NULL)
    )
    if (is.null(fit)) {
      return(NULL)
    }

    coefs <- coef(fit)

    tibble(
      dia_juliano = seq(
        min(df$dia_juliano),
        max(df$dia_juliano),
        length.out = 200
      )
    ) %>%
      mutate(
        frec_acum = predict(fit, newdata = .),
        fecha_sin_anio = as.Date(dia_juliano - 1, origin = "2000-01-01"),
        periodo = unique(df$periodo),
        a = coefs["a"],
        b = coefs["b"]
      )
  }

  h_pred_ult <- h_frec_ult %>%
    group_by(periodo) %>%
    group_split() %>%
    map_dfr(ajustar_logistico_ult)

  output$acum_ultima_helada_3 <- renderPlotly({
    mes_ult <- as.integer(input$mes_ult_helada_3)
    dia_ult <- as.integer(input$dia_ult_helada_3)
    if (is.na(mes_ult) || is.na(dia_ult)) {
      return(NULL)
    }

    fecha_sel_ult <- as.Date(sprintf("2000-%02d-%02d", mes_ult, dia_ult))
    dia_jul_sel_ult <- yday(fecha_sel_ult)

    params_ult <- h_pred_ult %>%
      filter(periodo == input$periodo_ult_helada_3) %>%
      slice(1)
    if (nrow(params_ult) == 0) {
      return(NULL)
    }

    a_ult <- params_ult$a
    b_ult <- params_ult$b

    frec_sel_ult <- 1 / (1 + exp(-(a_ult + b_ult * dia_jul_sel_ult)))

    punto_sel_ult <- tibble(
      fecha_sin_anio = fecha_sel_ult,
      frec_acum = frec_sel_ult
    )

    colores_periodo <- c(
      # "1971-2000" = "#1b9e77",
      # "1981-2010" = "#d95f02",
      "1991-2020" = "darkgrey",
      "2016-2025" = "#0072B2"
    )
    color_sel_ult <- colores_periodo[input$periodo_ult_helada_3]

    plot_ult_helada <- ggplot() +

      geom_point(
        data = h_frec_ult,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 1
      ) +

      geom_line(
        data = h_pred_ult,
        aes(x = fecha_sin_anio, y = frec_acum, color = periodo),
        linewidth = 1
      ) +

      geom_point(
        data = h_pred_ult,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 0,
        alpha = 0
      ) +

      geom_point(
        data = punto_sel_ult,
        aes(x = fecha_sin_anio, y = frec_acum),
        size = 2,
        color = color_sel_ult,
        fill = color_sel_ult,
        shape = 21,
        stroke = 1.5
      ) +

      geom_segment(
        data = punto_sel_ult,
        aes(
          x = min(h_frec_ult$fecha_sin_anio),
          xend = fecha_sin_anio,
          y = frec_acum,
          yend = frec_acum
        ),
        color = color_sel_ult,
        linetype = "dashed",
        linewidth = 1
      ) +

      scale_x_date(date_labels = "%d-%b", date_breaks = "10 day") +
      scale_y_continuous(labels = function(x) x * 100) +
      scale_color_manual(values = colores_periodo) +
      labs(
        x = "Fecha de última helada",
        y = "Frecuencia acumulada (%)",
        color = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.box.margin = margin(-5, 0, -5, 0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(plot_ult_helada, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.1, y = 1.2)
      )
  })

  heladas_periodo_0 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      mutate(
        fecha = as.Date(Fecha),
        dia_juliano = yday(fecha),
        dia_norm = if_else(
          leap_year(Año) & dia_juliano >= 60,
          dia_juliano + 1,
          dia_juliano
        )
      ) %>%
      filter(Temperatura_Abrigo_150cm_Minima <= 0) %>%
      group_by(Año) %>%
      summarise(
        primera = min(dia_norm),
        ultima = max(dia_norm),
        .groups = "drop"
      ) %>%
      summarise(
        periodo = paste0(start_year, "-", end_year),
        primera_media = mean(primera, na.rm = TRUE),
        ultima_media = mean(ultima, na.rm = TRUE),
        .groups = "drop"
      ) %>%

      mutate(
        fecha_primera_media = as.Date(primera_media, origin = "1999-12-31"),
        fecha_ultima_media = as.Date(ultima_media, origin = "1999-12-31")
      )
  }

  h0 <- heladas_periodo_0(balcarce_EMC, 1991, 2020)

  prob_mes_helada_0 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      group_by(Año, Mes) %>%
      summarise(
        hubo_helada = any(Temperatura_Abrigo_150cm_Minima <= 0),
        .groups = "drop"
      ) %>%
      group_by(Mes) %>%
      summarise(
        meses_con_helada = sum(hubo_helada),
        total_meses = n(),
        .groups = "drop"
      ) %>%
      mutate(
        periodo = paste0(start_year, "-", end_year),
        prob_helada = meses_con_helada / total_meses * 100
      )
  }

  meses_orden <- c(
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo",
    "junio",
    "julio",
    "agosto",
    "septiembre",
    "octubre",
    "noviembre",
    "diciembre"
  )

  dias_medios <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)

  p0 <- prob_mes_helada_0(balcarce_EMC, 1991, 2020) %>%
    mutate(
      Mes = factor(Mes, levels = meses_orden),
      dia_juliano = dias_medios[match(as.character(Mes), meses_orden)]
    ) %>%
    arrange(dia_juliano)

  output$prob_helada_ene_dic_0 <- renderPlotly({
    datos_filtrados <- p0
    fechas_filtradas <- h0

    y_offset <- max(datos_filtrados$prob_helada, na.rm = TRUE) * 0.01

    p_base <- ggplot(
      datos_filtrados,
      aes(x = dia_juliano, y = prob_helada, color = periodo, group = periodo)
    ) +
      geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) +

      geom_vline(
        data = fechas_filtradas,
        aes(xintercept = primera_media, color = periodo),
        linetype = "dashed",
        inherit.aes = FALSE
      ) +
      geom_vline(
        data = fechas_filtradas,
        aes(xintercept = ultima_media, color = periodo),
        linetype = "dashed",
        inherit.aes = FALSE
      ) +

      geom_text(
        data = fechas_filtradas,
        aes(
          x = primera_media,
          y = max(datos_filtrados$prob_helada, na.rm = TRUE) + y_offset,
          label = format(fecha_primera_media, "%d/%m"),
          color = periodo
        ),
        angle = 90,
        vjust = -1.5,
        size = 3,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = fechas_filtradas,
        aes(
          x = ultima_media,
          y = max(datos_filtrados$prob_helada, na.rm = TRUE) + y_offset,
          label = format(fecha_ultima_media, "%d/%m"),
          color = periodo
        ),
        angle = 90,
        vjust = -1.5,
        size = 3,
        inherit.aes = FALSE
      ) +

      scale_x_continuous(
        breaks = dias_medios,
        labels = meses_orden,
        limits = c(0, 365)
      ) +
      scale_color_manual(values = col_meteo) +
      scale_y_continuous(
        limits = c(-8, 105),
        labels = scales::percent_format(scale = 1)
      ) +
      labs(x = "Mes", y = "Probabilidad (%)", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )

    p_base <- p_base +
      geom_point(
        aes(
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Mes: ",
            Mes,
            "<br>Probabilidad: ",
            sprintf("%.2f%%", prob_helada)
          )
        ),
        size = 0,
        alpha = 0
      )

    ggplotly(p_base, tooltip = "text") %>%
      layout(
        annotations = list(
          x = 0.57,
          y = 1.0,
          xref = "paper",
          yref = "paper",
          text = "Fecha media de primera y última helada",
          showarrow = FALSE,
          font = list(size = 10, style = "italic"),
          xanchor = "center",
          yanchor = "bottom"
        )
      )
  })

  heladas_prim_anuales_0 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      mutate(
        fecha = as.Date(Fecha),
        dia_juliano = yday(fecha),
        dia_norm = if_else(
          leap_year(Año) & dia_juliano >= 60,
          dia_juliano + 1,
          dia_juliano
        )
      ) %>%
      filter(Temperatura_Abrigo_150cm_Minima <= 0) %>%
      group_by(Año) %>%
      summarise(
        primera = min(dia_norm),
        .groups = "drop"
      ) %>%
      # convertir día juliano a fecha usando un año ficticio (2000)
      mutate(
        fecha_primera = as.Date(primera, origin = "1999-12-31")
      ) %>%
      arrange(Año)
  }

  h_ano3_0 <- heladas_prim_anuales_0(balcarce_EMC, 1991, 2020)
  h_ano4_0 <- heladas_prim_anuales_0(balcarce_EMC, 2016, 2025)

  h_ano3_0 <- h_ano3_0 %>% mutate(periodo = "1991-2020")
  h_ano4_0 <- h_ano4_0 %>% mutate(periodo = "2016-2025")

  h_todos_0 <- bind_rows(
    h_ano3_0,
    h_ano4_0
  )

  h_frec_0 <- h_todos_0 %>%
    mutate(fecha_sin_anio = as.Date(format(fecha_primera, "2000-%m-%d"))) %>%
    group_by(periodo) %>%
    arrange(primera, .by_group = TRUE) %>%
    mutate(frec_acum = row_number() / n()) %>%
    ungroup()

  h_frec_0 <- h_frec_0 %>%
    mutate(dia_juliano = yday(fecha_sin_anio))

  ajustar_logistico_0 <- function(df) {
    fit <- nls(
      frec_acum ~ 1 / (1 + exp(-(a + b * dia_juliano))),
      data = df,
      start = list(a = -5, b = 0.05)
    )

    coefs <- coef(fit)

    pred <- tibble(
      dia_juliano = seq(
        min(df$dia_juliano),
        max(df$dia_juliano),
        length.out = 200
      )
    ) %>%
      mutate(
        frec_acum = predict(fit, newdata = .),
        fecha_sin_anio = as.Date(dia_juliano - 1, origin = "2000-01-01"),
        periodo = unique(df$periodo),
        a = coefs["a"],
        b = coefs["b"]
      )
    pred
  }

  h_pred_0 <- h_frec_0 %>%
    group_by(periodo) %>%
    group_split() %>%
    map_dfr(ajustar_logistico_0)

  puntos_50_0 <- h_pred_0 %>%
    group_by(periodo) %>%
    slice_min(abs(frec_acum - 0.5), n = 1) %>%
    ungroup() %>%
    mutate(etiqueta = format(fecha_sin_anio, "%d-%b"))

  puntos_50_0 <- puntos_50_0 %>%
    arrange(periodo) %>%
    mutate(
      x_pos = as.Date("2000-03-15"),
      y_pos = seq(0.25, 0.1, length.out = n())
    )

  output$acum_primera_helada_0 <- renderPlotly({
    mes_0 <- as.integer(input$mes_prim_helada_0)
    dia_0 <- as.integer(input$dia_prim_helada_0)
    if (is.na(mes_0) || is.na(dia_0)) {
      return(NULL)
    }

    fecha_sel_0 <- as.Date(sprintf("2000-%02d-%02d", mes_0, dia_0))
    dia_jul_sel <- yday(fecha_sel_0)

    params <- h_pred_0 %>%
      filter(periodo == input$periodo_prim_helada_0) %>%
      slice(1)
    if (nrow(params) == 0) {
      return(NULL)
    }

    a <- params$a
    b <- params$b

    frec_sel_0 <- 1 / (1 + exp(-(a + b * dia_jul_sel)))

    punto_sel_0 <- tibble(
      fecha_sin_anio = fecha_sel_0,
      frec_acum = frec_sel_0
    )

    colores_periodo <- c(
      # "1971-2000" = "#1b9e77",
      # "1981-2010" = "#d95f02",
      "1991-2020" = "darkgrey",
      "2016-2025" = "#D55E00"
    )
    color_sel <- colores_periodo[input$periodo_prim_helada_0]

    plot_prim_helada_0 <- ggplot() +

      geom_point(
        data = h_frec_0,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 1
      ) +

      geom_line(
        data = h_pred_0,
        aes(x = fecha_sin_anio, y = frec_acum, color = periodo),
        linewidth = 1
      ) +

      geom_point(
        data = h_pred_0,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 0,
        alpha = 0
      ) +

      geom_point(
        data = punto_sel_0,
        aes(x = fecha_sin_anio, y = frec_acum),
        size = 2,
        color = color_sel,
        fill = color_sel,
        shape = 21,
        stroke = 1.5
      ) +

      geom_segment(
        data = punto_sel_0,
        aes(
          x = min(h_frec_0$fecha_sin_anio),
          xend = fecha_sin_anio,
          y = frec_acum,
          yend = frec_acum
        ),
        color = color_sel,
        linetype = "dashed",
        linewidth = 1
      ) +
      scale_x_date(date_labels = "%d-%b", date_breaks = "10 day") +
      scale_y_continuous(labels = function(x) x * 100) +
      scale_color_manual(values = colores_periodo) +
      labs(
        x = "Fecha de primera helada",
        y = "Frecuencia acumulada (%)",
        color = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.box.margin = margin(-5, 0, -5, 0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(plot_prim_helada_0, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.1, y = 1.2)
      )
  })

  heladas_ult_anuales_0 <- function(df, start_year, end_year) {
    df %>%
      filter(
        Año >= start_year,
        Año <= end_year,
        !is.na(Temperatura_Abrigo_150cm_Minima)
      ) %>%
      mutate(
        fecha = as.Date(Fecha),
        dia_juliano = yday(fecha),
        dia_norm = if_else(
          leap_year(Año) & dia_juliano >= 60,
          dia_juliano + 1,
          dia_juliano
        )
      ) %>%
      filter(Temperatura_Abrigo_150cm_Minima <= 0) %>%
      group_by(Año) %>%
      summarise(
        ultima = max(dia_norm),
        .groups = "drop"
      ) %>%
      mutate(
        fecha_ultima = as.Date(ultima, origin = "1999-12-31")
      ) %>%
      arrange(Año)
  }

  h_ano3_ult_0 <- heladas_ult_anuales_0(balcarce_EMC, 1991, 2020) %>%
    mutate(periodo = "1991-2020")
  h_ano4_ult_0 <- heladas_ult_anuales_0(balcarce_EMC, 2016, 2025) %>%
    mutate(periodo = "2016-2025")

  h_todos_ult_0 <- bind_rows(
    h_ano3_ult_0,
    h_ano4_ult_0
  )

  h_frec_ult_0 <- h_todos_ult_0 %>%
    mutate(fecha_sin_anio = as.Date(format(fecha_ultima, "2000-%m-%d"))) %>%
    group_by(periodo) %>%
    arrange(desc(ultima), .by_group = TRUE) %>%
    mutate(frec_acum = row_number() / n()) %>%
    ungroup() %>%
    mutate(dia_juliano = yday(fecha_sin_anio))

  ajustar_logistico_ult_0 <- function(df) {
    start_a <- -log(1 / min(df$frec_acum) - 1)
    start_b <- 0.05

    fit <- tryCatch(
      nlsLM(
        frec_acum ~ 1 / (1 + exp(-(a + b * dia_juliano))),
        data = df,
        start = list(a = start_a, b = start_b),
        control = nls.lm.control(maxiter = 500)
      ),
      error = function(e) return(NULL)
    )
    if (is.null(fit)) {
      return(NULL)
    }

    coefs <- coef(fit)

    tibble(
      dia_juliano = seq(
        min(df$dia_juliano),
        max(df$dia_juliano),
        length.out = 200
      )
    ) %>%
      mutate(
        frec_acum = predict(fit, newdata = .),
        fecha_sin_anio = as.Date(dia_juliano - 1, origin = "2000-01-01"),
        periodo = unique(df$periodo),
        a = coefs["a"],
        b = coefs["b"]
      )
  }

  h_pred_ult_0 <- h_frec_ult_0 %>%
    group_by(periodo) %>%
    group_split() %>%
    map_dfr(ajustar_logistico_ult)

  output$acum_ultima_helada_0 <- renderPlotly({
    mes_ult_0 <- as.integer(input$mes_ult_helada_0)
    dia_ult_0 <- as.integer(input$dia_ult_helada_0)
    if (is.na(mes_ult_0) || is.na(dia_ult_0)) {
      return(NULL)
    }

    fecha_sel_ult <- as.Date(sprintf("2000-%02d-%02d", mes_ult_0, dia_ult_0))
    dia_jul_sel_ult <- yday(fecha_sel_ult)

    params_ult_0 <- h_pred_ult_0 %>%
      filter(periodo == input$periodo_ult_helada_0) %>%
      slice(1)
    if (nrow(params_ult_0) == 0) {
      return(NULL)
    }

    a_ult <- params_ult_0$a
    b_ult <- params_ult_0$b

    frec_sel_ult <- 1 / (1 + exp(-(a_ult + b_ult * dia_jul_sel_ult)))

    punto_sel_ult_0 <- tibble(
      fecha_sin_anio = fecha_sel_ult,
      frec_acum = frec_sel_ult
    )

    colores_periodo <- c(
      "1991-2020" = "darkgrey",
      "2016-2025" = "#D55E00"
    )
    color_sel_ult_0 <- colores_periodo[input$periodo_ult_helada_0]

    plot_ult_helada_0 <- ggplot() +

      geom_point(
        data = h_frec_ult_0,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 1
      ) +

      geom_line(
        data = h_pred_ult_0,
        aes(x = fecha_sin_anio, y = frec_acum, color = periodo),
        linewidth = 1
      ) +

      geom_point(
        data = h_pred_ult_0,
        aes(
          x = fecha_sin_anio,
          y = frec_acum,
          color = periodo,
          text = paste0(
            "Periodo: ",
            periodo,
            "<br>Fecha: ",
            format(fecha_sin_anio, "%d-%b"),
            "<br>Frecuencia: ",
            sprintf("%.2f%%", frec_acum * 100)
          )
        ),
        size = 0,
        alpha = 0
      ) +

      geom_point(
        data = punto_sel_ult_0,
        aes(x = fecha_sin_anio, y = frec_acum),
        size = 2,
        color = color_sel_ult_0,
        fill = color_sel_ult_0,
        shape = 21,
        stroke = 1.5
      ) +

      geom_segment(
        data = punto_sel_ult_0,
        aes(
          x = min(h_frec_ult_0$fecha_sin_anio),
          xend = fecha_sin_anio,
          y = frec_acum,
          yend = frec_acum
        ),
        color = color_sel_ult_0,
        linetype = "dashed",
        linewidth = 1
      ) +

      scale_x_date(date_labels = "%d-%b", date_breaks = "10 day") +
      scale_y_continuous(labels = function(x) x * 100) +
      scale_color_manual(values = colores_periodo) +
      labs(
        x = "Fecha de última helada",
        y = "Frecuencia acumulada (%)",
        color = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.box.margin = margin(-5, 0, -5, 0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggplotly(plot_ult_helada_0, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.1, y = 1.2)
      )
  })

  heladas_anio <- reactive({
    req(input$anio_heladas)

    datos %>%
      filter(Año == input$anio_heladas) %>%
      mutate(
        Fecha = as.Date(Fecha),
        helada_agro = Temperatura_Abrigo_150cm_Minima <= 3,
        helada_meteo = Temperatura_Abrigo_150cm_Minima <= 0
      )
  })

  resumen_heladas <- reactive({
    req(input$anio_heladas)

    df <- heladas_anio()

    anio_actual <- year(Sys.Date())
    es_anio_actual <- input$anio_heladas == anio_actual

    resumen_tipo <- function(variable_helada) {
      df_h <- df %>% filter({{ variable_helada }})

      if (nrow(df_h) == 0) {
        return(tibble(
          primera = NA_Date_,
          ultima = NA_Date_,
          n = 0,
          dias_libres = NA_real_,
          es_anio_actual = es_anio_actual
        ))
      }

      primera <- min(df_h$Fecha, na.rm = TRUE)
      ultima <- max(df_h$Fecha, na.rm = TRUE)

      tibble(
        primera = primera,
        ultima = ultima,
        n = nrow(df_h),
        dias_libres = ifelse(
          es_anio_actual,
          NA_real_,
          as.numeric(primera - ultima) + 365
        ),
        es_anio_actual = es_anio_actual
      )
    }

    list(
      agro = resumen_tipo(helada_agro),
      meteo = resumen_tipo(helada_meteo)
    )
  })

  output$box_heladas_agro <- renderInfoBox({
    r <- resumen_heladas()$agro

    if (r$n == 0) {
      valor <- "Sin registros"
      subtitulo <- "No se registraron heladas agrometeorológicas"
    } else {
      if (r$es_anio_actual) {
        subtitulo <- HTML(paste0(
          "<b>Primera:</b> ",
          format(r$primera, "%d/%m/%Y")
        ))
      } else {
        subtitulo <- HTML(paste0(
          "<b>Primera:</b> ",
          format(r$primera, "%d/%m/%Y"),
          "<br>",
          "<b>Última:</b> ",
          format(r$ultima, "%d/%m/%Y"),
          "<br>",
          "<b>Días libres de heladas:</b> ",
          round(r$dias_libres)
        ))
      }

      valor <- paste0(r$n, " heladas")
    }

    infoBox(
      title = "Heladas agrometeorológicas",
      value = valor,
      subtitle = subtitulo,
      icon = icon("temperature-low"),
      color = "info",
      fill = TRUE
    )
  })

  output$box_heladas_meteo <- renderInfoBox({
    r <- resumen_heladas()$meteo

    if (r$n == 0) {
      valor <- "Sin registros"
      subtitulo <- "No se registraron heladas meteorológicas"
    } else {
      if (r$es_anio_actual) {
        subtitulo <- HTML(paste0(
          "<b>Primera:</b> ",
          format(r$primera, "%d/%m/%Y")
        ))
      } else {
        subtitulo <- HTML(paste0(
          "<b>Primera:</b> ",
          format(r$primera, "%d/%m/%Y"),
          "<br>",
          "<b>Última:</b> ",
          format(r$ultima, "%d/%m/%Y"),
          "<br>",
          "<b>Días libres de heladas:</b> ",
          round(r$dias_libres)
        ))
      }

      valor <- paste0(r$n, " heladas")
    }

    infoBox(
      title = "Heladas meteorológicas",
      value = valor,
      subtitle = subtitulo,
      icon = icon("snowflake"),
      color = "primary",
      fill = TRUE
    )
  })

  ##### AMBIENTE #####

  periodo_critico <- reactive({
    req(input$mes_siembra_ambiente, input$dia_siembra_ambiente)

    dia_siembra <- as.numeric(input$dia_siembra_ambiente)
    mes_siembra <- as.numeric(input$mes_siembra_ambiente)

    fecha_siembra_dia_mes <- as.Date(
      sprintf("%02d-%02d", mes_siembra, dia_siembra),
      format = "%m-%d"
    )

    cultivo <- input$cultivo_ambiente

    dia_juliano <- as.numeric(format(fecha_siembra_dia_mes, "%j"))

    if (dia_juliano < 40) {
      dia_juliano <- dia_juliano + 365
    }

    if (cultivo == "maiz_largo") {
      gd_min <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      gd_max <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )
    } else if (cultivo == "maiz_corto") {
      gd_min <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      gd_max <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )
    } else if (cultivo == "soja") {
      gd_min <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * (dia_juliano)) - 4047.4),
        0
      )
      gd_max <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9),
        0
      )
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
          (format(Fecha, "%Y") == as.character(ano) &
            format(Fecha, "%m-%d") >= format(fecha_siembra_dia_mes, "%m-%d")) |
            (format(Fecha, "%Y") == as.character(ano + 1) &
              format(Fecha, "%m-%d") <= "12-30")
        ) %>%
        arrange(Fecha) %>%
        mutate(
          Dia_Mes = format(Fecha, "%m-%d")
        ) %>%
        mutate(
          TTB = case_when(
            input$cultivo_ambiente %in% c("maiz_largo", "maiz_corto") ~ if_else(
              Temperatura_Abrigo_150cm - 8 < 0,
              0,
              Temperatura_Abrigo_150cm - 8
            ),
            cultivo == "soja" ~ if_else(
              Temperatura_Abrigo_150cm - 11 < 0,
              0,
              Temperatura_Abrigo_150cm - 11
            )
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
        lluvia_acumulada <- sum(
          round(datos_periodo_critico$Precipitacion_Pluviometrica, 0),
          na.rm = TRUE
        )
        etp_acumulada <- sum(
          round(datos_periodo_critico$Evapotranspiracion_Potencial, 0),
          na.rm = TRUE
        )
        radiacion_global_media <- mean(
          round(datos_periodo_critico$Radiacion_Global, 0),
          na.rm = TRUE
        )
        dias_mayores_35 <- sum(
          datos_periodo_critico$Temperatura_Abrigo_150cm_Maxima >= 35,
          na.rm = TRUE
        )
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

    historicos_periodo_critico$decada <- ifelse(
      historicos_periodo_critico$ano >= 2010,
      2010,
      floor(historicos_periodo_critico$ano / 10) * 10
    )
    historicos_periodo_critico <- historicos_periodo_critico %>%
      filter(decada <= 2024)

    mediana_lluvia_decada <- historicos_periodo_critico %>%
      group_by(decada) %>%
      reframe(
        mediana_lluvia = median(lluvia_acumulada, na.rm = TRUE),
        mediana_etp = median(etp_acumulada, na.rm = TRUE),
        decada_fin = ifelse(decada == 2010, 2024, decada + 10)
      )

    pp_acum <- ggplot(historicos_periodo_critico, aes(x = ano)) +
      geom_bar(
        aes(y = etp_acumulada, fill = "ET0 Acumulada"),
        stat = "identity",
        color = "#8C1C13",
        alpha = 0.5
      ) +
      geom_bar(
        aes(y = lluvia_acumulada, fill = "Lluvia Acumulada"),
        stat = "identity",
        color = "#003459",
        alpha = 0.5
      ) +
      geom_segment(
        data = mediana_lluvia_decada,
        aes(
          x = decada,
          xend = decada_fin,
          y = mediana_lluvia + 100,
          yend = mediana_lluvia + 100
        ),
        linetype = "dashed",
        color = "#284B63",
        linewidth = 0.5
      ) +
      geom_text(
        data = mediana_lluvia_decada,
        aes(
          x = (decada + decada_fin) / 2,
          y = mediana_lluvia + 120,
          label = paste0(round(mediana_lluvia, 0), " mm")
        ),
        color = "#284B63",
        size = 3,
        vjust = -0.5
      ) +
      ylim(0, 350) +
      labs(x = "Año", y = "(mm)", fill = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_x_continuous(
        breaks = seq(
          min(historicos_periodo_critico$ano),
          max(historicos_periodo_critico$ano),
          by = 5
        )
      ) +
      scale_fill_manual(
        values = c("ET0 Acumulada" = "#BF4342", "Lluvia Acumulada" = "#007EA7")
      )

    ggplotly(pp_acum) %>%
      layout(legend = list(orientation = "h", x = 0.3, y = 1.2))
  })

  output$rad_temp_PC <- renderPlotly({
    historicos_periodo_critico <- periodo_critico()

    historicos_periodo_critico$decada <- ifelse(
      historicos_periodo_critico$ano >= 2011,
      2011,
      floor(historicos_periodo_critico$ano / 11) * 11
    )
    historicos_periodo_critico <- historicos_periodo_critico %>%
      filter(decada <= 2024)

    mediana_dias_35_decada <- historicos_periodo_critico %>%
      group_by(decada) %>%
      reframe(
        mediana_dias_mayores_35 = median(dias_mayores_35, na.rm = TRUE),
        decada_fin = ifelse(decada == 2011, 2024, decada + 10)
      )

    historicos_long <- historicos_periodo_critico %>%
      pivot_longer(
        cols = c(radiacion_global_media, dias_mayores_35),
        names_to = "variable",
        values_to = "valor"
      )

    promedio_radiacion <- mean(
      historicos_periodo_critico$radiacion_global_media,
      na.rm = TRUE
    )
    promedio_dias_mayores_35 <- median(
      historicos_periodo_critico$dias_mayores_35,
      na.rm = TRUE
    )

    rad_temp <- ggplot(
      historicos_long,
      aes(x = ano, y = valor, fill = variable)
    ) +

      geom_bar(stat = "identity", position = "dodge") +

      annotate(
        "text",
        x = 2016,
        y = 47,
        label = paste0(
          "Radiación\n(1991-2020): ",
          round(promedio_radiacion, 0),
          " Mj / m2 * día"
        ),
        color = "#FFBC42",
        size = 3.5,
        vjust = 2
      ) +

      annotate(
        "text",
        x = 1995,
        y = 47,
        label = paste0(
          "Días temp. máx. > 35ºC\n(1991-2020): ",
          round(promedio_dias_mayores_35, 0)
        ),
        color = "#D00000",
        size = 3.5,
        vjust = 2
      ) +

      labs(
        title = "",
        x = "Año",
        y = "",
        color = "Variables",
        fill = ""
      ) +
      ylim(0, 50) +
      scale_fill_manual(
        name = "",
        values = c(
          "radiacion_global_media" = "#FFBC42",
          "dias_mayores_35" = "#D00000"
        )
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_x_continuous(
        breaks = seq(
          min(historicos_periodo_critico$ano),
          max(historicos_periodo_critico$ano),
          by = 5
        )
      ) +
      guides(fill = "none")

    ggplotly(rad_temp) %>%
      layout(legend = list(orientation = "h", x = 0.05, y = 1.2))
  })

  ####### Comparación fechas de siembra ############

  calcular_periodo_critico <- function(
    mes_siembra,
    dia_siembra,
    cultivo,
    datos,
    datos_historicos_avg
  ) {
    dia_siembra <- as.numeric(dia_siembra)
    mes_siembra <- as.numeric(mes_siembra)

    fecha_siembra_dia_mes <- as.Date(
      sprintf("%02d-%02d", mes_siembra, dia_siembra),
      format = "%m-%d"
    )

    dia_juliano <- as.numeric(format(fecha_siembra_dia_mes, "%j"))

    if (dia_juliano < 40) {
      dia_juliano <- dia_juliano + 365
    }

    if (cultivo == "maiz_largo") {
      gd_min <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      gd_max <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )
    } else if (cultivo == "maiz_corto") {
      gd_min <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      gd_max <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )
    } else if (cultivo == "soja") {
      gd_min <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * (dia_juliano)) - 4047.4),
        0
      )
      gd_max <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9),
        0
      )
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
          (format(Fecha, "%Y") == as.character(ano) &
            format(Fecha, "%m-%d") >= format(fecha_siembra_dia_mes, "%m-%d")) |
            (format(Fecha, "%Y") == as.character(ano + 1) &
              format(Fecha, "%m-%d") < "12-31")
        ) %>%
        arrange(Fecha) %>%
        mutate(
          Dia_Mes = format(Fecha, "%m-%d")
        ) %>%
        mutate(
          TTB = case_when(
            cultivo == "maiz_largo" ~ if_else(
              Temperatura_Abrigo_150cm - 8 < 0,
              0,
              Temperatura_Abrigo_150cm - 8
            ),
            cultivo == "maiz_corto" ~ if_else(
              Temperatura_Abrigo_150cm - 8 < 0,
              0,
              Temperatura_Abrigo_150cm - 8
            ),

            cultivo == "soja" ~ if_else(
              Temperatura_Abrigo_150cm - 11 < 0,
              0,
              Temperatura_Abrigo_150cm - 11
            )
            # ,
            # TRUE ~ if_else(Temperatura_Abrigo_150cm - 9 < 0, 0, Temperatura_Abrigo_150cm - 9)
          ),
          GD_acum = cumsum(TTB)
        )

      datos_periodo_critico <- datos_ano %>%
        filter(GD_acum >= gd_min & GD_acum <= gd_max)

      if (nrow(datos_periodo_critico) > 0) {
        fecha_inicio <- min(datos_periodo_critico$Fecha)
        fecha_fin <- max(datos_periodo_critico$Fecha)
        lluvia_acumulada <- sum(
          round(datos_periodo_critico$Precipitacion_Pluviometrica, 0),
          na.rm = TRUE
        )
        etp_acumulada <- sum(
          round(datos_periodo_critico$Evapotranspiracion_Potencial, 0),
          na.rm = TRUE
        )
        radiacion_global_media <- mean(
          round(datos_periodo_critico$Radiacion_Global, 0),
          na.rm = TRUE
        )
        dias_mayores_35 <- sum(
          datos_periodo_critico$Temperatura_Abrigo_150cm_Maxima >= 35,
          na.rm = TRUE
        )
      } else {
        fecha_inicio <- fecha_fin <- NA
        lluvia_acumulada <- etp_acumulada <- radiacion_global_media <- dias_mayores_35 <- NA
      }

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

    historicos_periodo_critico$inicio_juliano <- yday(
      historicos_periodo_critico$inicio
    )
    historicos_periodo_critico$fin_juliano <- yday(
      historicos_periodo_critico$fin
    )

    historicos_periodo_critico$inicio_juliano <- ifelse(
      historicos_periodo_critico$inicio_juliano < 100,
      historicos_periodo_critico$inicio_juliano + 365,
      historicos_periodo_critico$inicio_juliano
    )

    promedio_inicio_juliano <- mean(
      historicos_periodo_critico$inicio_juliano,
      na.rm = TRUE
    )
    promedio_fin_juliano <- mean(
      historicos_periodo_critico$fin_juliano,
      na.rm = TRUE
    )

    fecha_promedio_inicio <- as.Date(
      promedio_inicio_juliano - 1,
      origin = "2024-01-01"
    )
    fecha_promedio_fin <- as.Date(
      promedio_fin_juliano - 1,
      origin = "2024-01-01"
    )

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
    calcular_periodo_critico(
      input$mes_siembra1,
      input$dia_siembra1,
      input$cultivo_fecha,
      datos,
      datos_historicos_avg
    )
  })

  periodo_critico2 <- reactive({
    req(input$mes_siembra2, input$dia_siembra2, input$cultivo_fecha)
    calcular_periodo_critico(
      input$mes_siembra2,
      input$dia_siembra2,
      input$cultivo_fecha,
      datos,
      datos_historicos_avg
    )
  })

  periodo_critico3 <- reactive({
    req(input$mes_siembra3, input$dia_siembra3, input$cultivo_fecha)
    calcular_periodo_critico(
      input$mes_siembra3,
      input$dia_siembra3,
      input$cultivo_fecha,
      datos,
      datos_historicos_avg
    )
  })

  observeEvent(input$btn_calcular, {
    historicos_periodo_critico1 <- periodo_critico1()
    historicos_periodo_critico2 <- periodo_critico2()
    historicos_periodo_critico3 <- periodo_critico3()

    output$fecha_inicio_pc1 <- renderText({
      paste(
        "Fecha inicio periodo crítico:",
        as.character(periodo_critico1()$fecha_inicio)
      )
    })

    output$fecha_fin_pc1 <- renderText({
      paste(
        "Fecha fin periodo crítico:",
        as.character(periodo_critico1()$fecha_fin)
      )
    })

    output$fecha_inicio_pc2 <- renderText({
      paste(
        "Fecha inicio periodo crítico:",
        as.character(periodo_critico2()$fecha_inicio)
      )
    })

    output$fecha_fin_pc2 <- renderText({
      paste(
        "Fecha fin periodo crítico:",
        as.character(periodo_critico2()$fecha_fin)
      )
    })

    output$fecha_inicio_pc3 <- renderText({
      paste(
        "Fecha inicio periodo crítico:",
        as.character(periodo_critico3()$fecha_inicio)
      )
    })

    output$fecha_fin_pc3 <- renderText({
      paste(
        "Fecha fin periodo crítico:",
        as.character(periodo_critico3()$fecha_fin)
      )
    })

    output$pc_lluvia <- renderPlotly({
      historicos_periodo_critico1 <- periodo_critico1()$historicos
      historicos_periodo_critico2 <- periodo_critico2()$historicos
      historicos_periodo_critico3 <- periodo_critico3()$historicos

      fechas <- c(
        paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
        paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
        paste(input$dia_siembra3, input$mes_siembra3, sep = "-")
      )

      if (length(unique(fechas)) < length(fechas)) {
        plot_blank <- ggplot() +
          theme_void() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "Por favor, elija 3 fechas diferentes.",
            size = 5,
            color = "red"
          ) +
          labs(title = "")

        return(plot_blank)
      }

      mediana_lluvia1 <- median(
        round(historicos_periodo_critico1$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp1 <- median(
        round(historicos_periodo_critico1$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion1 <- mean(
        round(historicos_periodo_critico1$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_1 <- median(
        round(historicos_periodo_critico1$dias_mayores_35, 0),
        na.rm = TRUE
      )

      mediana_lluvia2 <- median(
        round(historicos_periodo_critico2$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp2 <- median(
        round(historicos_periodo_critico2$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion2 <- mean(
        round(historicos_periodo_critico2$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_2 <- median(
        round(historicos_periodo_critico2$dias_mayores_35, 0),
        na.rm = TRUE
      )

      mediana_lluvia3 <- median(
        round(historicos_periodo_critico3$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp3 <- median(
        round(historicos_periodo_critico3$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion3 <- mean(
        round(historicos_periodo_critico3$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_3 <- median(
        round(historicos_periodo_critico3$dias_mayores_35, 0),
        na.rm = TRUE
      )

      estadisticas <- data.frame(
        fecha = fechas,
        lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
        etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
        radiacion_media = c(
          promedio_radiacion1,
          promedio_radiacion2,
          promedio_radiacion3
        ),
        dias_mayores_35 = c(
          mediana_dias_mayores_35_1,
          mediana_dias_mayores_35_2,
          mediana_dias_mayores_35_3
        )
      )

      estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)

      pc_lluvia <- ggplot(
        estadisticas,
        aes(x = fecha, y = lluvia_acumulada, fill = fecha)
      ) +
        geom_bar(
          aes(y = lluvia_acumulada, fill = fecha),
          stat = "identity",
          position = position_dodge(0.9)
        ) +
        geom_bar(
          aes(y = etp_acumulada, fill = fecha),
          stat = "identity",
          alpha = 0.4,
          position = position_dodge(0.9)
        ) +
        geom_text(
          aes(y = lluvia_acumulada, label = lluvia_acumulada),
          position = position_dodge(0.9),
          vjust = -0.5,
          size = 3
        ) +
        geom_text(
          aes(y = etp_acumulada, label = etp_acumulada),
          position = position_dodge(0.9),
          vjust = -0.5,
          size = 3,
          color = "black"
        ) +
        labs(title = "", x = "", y = " mm", fill = NULL) +
        theme_minimal() +
        scale_fill_manual(
          name = "",
          values = c("#E07A5F", "#3D405B", "#AEC3B0")
        ) +
        theme(
          axis.text.x = element_text(size = 10),
          axis.ticks.x = element_blank()
        ) +
        guides(fill = "none")

      ggplotly(pc_lluvia)
    })

    output$pc_radiacion <- renderPlotly({
      historicos_periodo_critico1 <- periodo_critico1()$historicos
      historicos_periodo_critico2 <- periodo_critico2()$historicos
      historicos_periodo_critico3 <- periodo_critico3()$historicos

      fechas <- c(
        paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
        paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
        paste(input$dia_siembra3, input$mes_siembra3, sep = "-")
      )

      if (length(unique(fechas)) < length(fechas)) {
        plot_blank <- ggplot() +
          theme_void() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "Por favor, elija 3 fechas diferentes.",
            size = 5,
            color = "red"
          ) +
          labs(title = "")

        return(plot_blank)
      }

      mediana_lluvia1 <- median(
        round(historicos_periodo_critico1$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp1 <- median(
        round(historicos_periodo_critico1$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion1 <- mean(
        round(historicos_periodo_critico1$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_1 <- median(
        round(historicos_periodo_critico1$dias_mayores_35, 0),
        na.rm = TRUE
      )

      mediana_lluvia2 <- median(
        round(historicos_periodo_critico2$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp2 <- median(
        round(historicos_periodo_critico2$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion2 <- mean(
        round(historicos_periodo_critico2$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_2 <- median(
        round(historicos_periodo_critico2$dias_mayores_35, 0),
        na.rm = TRUE
      )

      mediana_lluvia3 <- median(
        round(historicos_periodo_critico3$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp3 <- median(
        round(historicos_periodo_critico3$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion3 <- mean(
        round(historicos_periodo_critico3$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_3 <- median(
        round(historicos_periodo_critico3$dias_mayores_35, 0),
        na.rm = TRUE
      )

      estadisticas <- data.frame(
        fecha = fechas,
        lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
        etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
        radiacion_media = c(
          promedio_radiacion1,
          promedio_radiacion2,
          promedio_radiacion3
        ),
        dias_mayores_35 = c(
          mediana_dias_mayores_35_1,
          mediana_dias_mayores_35_2,
          mediana_dias_mayores_35_3
        )
      )

      estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)

      pc_radiacion <- ggplot(
        estadisticas,
        aes(x = fecha, y = radiacion_media, fill = fecha)
      ) +
        geom_bar(stat = "identity") +
        geom_text(
          aes(label = round(radiacion_media)),
          position = position_stack(vjust = 1.05),
          size = 3
        ) +
        labs(title = "", x = "", y = " Mj / m2 * día") +
        theme_minimal() +
        scale_fill_manual(
          name = "",
          values = c("#E07A5F", "#3D405B", "#AEC3B0")
        ) +
        theme(
          axis.text.x = element_text(size = 10),
          axis.ticks.x = element_blank()
        ) +
        guides(fill = "none")

      ggplotly(pc_radiacion)
    })

    output$pc_dias_35 <- renderPlotly({
      historicos_periodo_critico1 <- periodo_critico1()$historicos
      historicos_periodo_critico2 <- periodo_critico2()$historicos
      historicos_periodo_critico3 <- periodo_critico3()$historicos

      fechas <- c(
        paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
        paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
        paste(input$dia_siembra3, input$mes_siembra3, sep = "-")
      )

      if (length(unique(fechas)) < length(fechas)) {
        plot_blank <- ggplot() +
          theme_void() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "Por favor, elija 3 fechas diferentes.",
            size = 5,
            color = "red"
          ) +
          labs(title = "")

        return(plot_blank)
      }

      mediana_lluvia1 <- median(
        round(historicos_periodo_critico1$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp1 <- median(
        round(historicos_periodo_critico1$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion1 <- mean(
        round(historicos_periodo_critico1$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_1 <- median(
        round(historicos_periodo_critico1$dias_mayores_35, 0),
        na.rm = TRUE
      )

      mediana_lluvia2 <- median(
        round(historicos_periodo_critico2$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp2 <- median(
        round(historicos_periodo_critico2$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion2 <- mean(
        round(historicos_periodo_critico2$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_2 <- median(
        round(historicos_periodo_critico2$dias_mayores_35, 0),
        na.rm = TRUE
      )

      mediana_lluvia3 <- median(
        round(historicos_periodo_critico3$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp3 <- median(
        round(historicos_periodo_critico3$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion3 <- mean(
        round(historicos_periodo_critico3$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_mayores_35_3 <- median(
        round(historicos_periodo_critico3$dias_mayores_35, 0),
        na.rm = TRUE
      )

      estadisticas <- data.frame(
        fecha = fechas,
        lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
        etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
        radiacion_media = c(
          promedio_radiacion1,
          promedio_radiacion2,
          promedio_radiacion3
        ),
        dias_mayores_35 = c(
          mediana_dias_mayores_35_1,
          mediana_dias_mayores_35_2,
          mediana_dias_mayores_35_3
        )
      )

      estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)

      pc_dias_35 <- ggplot(
        estadisticas,
        aes(x = fecha, y = dias_mayores_35, fill = fecha)
      ) +
        geom_bar(stat = "identity") +
        geom_text(
          aes(label = dias_mayores_35),
          position = position_stack(vjust = 1.05),
          size = 3
        ) +
        labs(title = "", x = "", y = "# días") +
        theme_minimal() +
        scale_fill_manual(
          name = "",
          values = c("#E07A5F", "#3D405B", "#AEC3B0")
        ) +
        theme(
          axis.text.x = element_text(size = 10),
          axis.ticks.x = element_blank()
        ) +
        guides(fill = "none")

      ggplotly(pc_dias_35)
    })
  })

  calcular_llenado_grano <- function(
    mes_siembra,
    dia_siembra,
    cultivo,
    datos,
    datos_historicos_avg
  ) {
    dia_siembra <- as.numeric(dia_siembra)
    mes_siembra <- as.numeric(mes_siembra)

    fecha_siembra_dia_mes <- as.Date(
      sprintf("%02d-%02d", mes_siembra, dia_siembra),
      format = "%m-%d"
    )

    dia_juliano <- as.numeric(format(fecha_siembra_dia_mes, "%j"))

    if (dia_juliano < 40) {
      dia_juliano <- dia_juliano + 365
    }

    if (cultivo == "maiz_largo") {
      gd_min <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )
      gd_max <- round(
        ((-0.0024 * (dia_juliano^2)) - (1.7585 * dia_juliano) + 2469.3),
        0
      )
    } else if (cultivo == "maiz_corto") {
      gd_min <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )
      gd_max <- round(
        ((-0.0212 * (dia_juliano^2)) + (10.045 * dia_juliano) + 420.61),
        0
      )
    } else if (cultivo == "soja") {
      dia_juliano <- as.numeric(format(fecha_siembra_dia_mes, "%j"))

      gd_min <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * (dia_juliano)) - 2764.9),
        0
      )
      gd_max <- round(
        ((-0.0466 * (dia_juliano^2)) + (26.344 * dia_juliano) - 2479.9),
        0
      )
    }

    historicos_llenado_grano <- data.frame(
      ano = integer(),
      lluvia_acumulada = numeric(),
      etp_acumulada = numeric(),
      radiacion_global_media = numeric(),
      dias_minimas_2 = numeric(),
      inicio = as.Date(character()),
      fin = as.Date(character())
    )

    for (ano in 2010:2025) {
      datos_ano <- datos %>%
        filter(
          (format(Fecha, "%Y") == as.character(ano) &
            format(Fecha, "%m-%d") >= format(fecha_siembra_dia_mes, "%m-%d")) |
            (format(Fecha, "%Y") == as.character(ano + 1) &
              format(Fecha, "%m-%d") < "12-31")
        ) %>%
        arrange(Fecha) %>%
        mutate(
          Dia_Mes = format(Fecha, "%m-%d")
        ) %>%
        mutate(
          TTB = case_when(
            cultivo %in% c("maiz_largo", "maiz_corto") ~ if_else(
              Temperatura_Abrigo_150cm - 8 < 0,
              0,
              Temperatura_Abrigo_150cm - 8
            ),

            cultivo == "soja" ~ if_else(
              Temperatura_Abrigo_150cm - 11 < 0,
              0,
              Temperatura_Abrigo_150cm - 11
            )
          ),
          GD_acum = cumsum(TTB)
        )

      datos_llenado_grano <- datos_ano %>%
        filter(GD_acum >= gd_min & GD_acum <= gd_max)

      fecha_inicio <- if (nrow(datos_llenado_grano) > 0) {
        min(datos_llenado_grano$Fecha)
      } else {
        NA
      }
      fecha_fin <- if (nrow(datos_llenado_grano) > 0) {
        max(datos_llenado_grano$Fecha)
      } else {
        NA
      }

      lluvia_acumulada <- if (nrow(datos_llenado_grano) > 0) {
        sum(
          round(datos_llenado_grano$Precipitacion_Pluviometrica, 0),
          na.rm = TRUE
        )
      } else {
        0
      }

      etp_acumulada <- if (nrow(datos_llenado_grano) > 0) {
        sum(
          round(datos_llenado_grano$Evapotranspiracion_Potencial, 0),
          na.rm = TRUE
        )
      } else {
        0
      }

      radiacion_global_media <- if (nrow(datos_llenado_grano) > 0) {
        mean(round(datos_llenado_grano$Radiacion_Global, 0), na.rm = TRUE)
      } else {
        NA
      }

      dias_minimas_2 <- if (nrow(datos_llenado_grano) > 0) {
        sum(
          datos_llenado_grano$Temperatura_Abrigo_150cm_Minima <= 2,
          na.rm = TRUE
        )
      } else {
        0
      }

      historicos_llenado_grano <- rbind(
        historicos_llenado_grano,
        data.frame(
          ano = ano,
          lluvia_acumulada = lluvia_acumulada,
          etp_acumulada = etp_acumulada,
          radiacion_global_media = radiacion_global_media,
          dias_minimas_2 = dias_minimas_2,
          inicio = fecha_inicio,
          fin = fecha_fin
        )
      )
    }

    historicos_llenado_grano$inicio_juliano <- yday(
      historicos_llenado_grano$inicio
    )
    historicos_llenado_grano$fin_juliano <- yday(historicos_llenado_grano$fin)

    historicos_llenado_grano$inicio_juliano <- ifelse(
      historicos_llenado_grano$inicio_juliano < 100,
      historicos_llenado_grano$inicio_juliano + 365,
      historicos_llenado_grano$inicio_juliano
    )

    promedio_inicio_juliano <- mean(
      historicos_llenado_grano$inicio_juliano,
      na.rm = TRUE
    )
    promedio_fin_juliano <- mean(
      historicos_llenado_grano$fin_juliano,
      na.rm = TRUE
    )

    fecha_promedio_inicio <- as.Date(
      promedio_inicio_juliano - 1,
      origin = "2024-01-01"
    )
    fecha_promedio_fin <- as.Date(
      promedio_fin_juliano - 1,
      origin = "2024-01-01"
    )

    fecha_inicio <- format(fecha_promedio_inicio, "%d-%m")
    fecha_fin <- format(fecha_promedio_fin, "%d-%m")

    return(
      list(
        historicos = historicos_llenado_grano,
        fecha_inicio = fecha_inicio,
        fecha_fin = fecha_fin
      )
    )
  }

  llenado_grano1 <- reactive({
    req(input$mes_siembra1, input$dia_siembra1, input$cultivo_fecha)
    calcular_llenado_grano(
      input$mes_siembra1,
      input$dia_siembra1,
      input$cultivo_fecha,
      datos,
      datos_historicos_avg
    )
  })

  llenado_grano2 <- reactive({
    req(input$mes_siembra2, input$dia_siembra2, input$cultivo_fecha)
    calcular_llenado_grano(
      input$mes_siembra2,
      input$dia_siembra2,
      input$cultivo_fecha,
      datos,
      datos_historicos_avg
    )
  })

  llenado_grano3 <- reactive({
    req(input$mes_siembra3, input$dia_siembra3, input$cultivo_fecha)
    calcular_llenado_grano(
      input$mes_siembra3,
      input$dia_siembra3,
      input$cultivo_fecha,
      datos,
      datos_historicos_avg
    )
  })

  observeEvent(input$btn_calcular, {
    historicos_llenado_grano1 <- llenado_grano1()
    historicos_llenado_grano2 <- llenado_grano2()
    historicos_llenado_grano3 <- llenado_grano3()

    output$lg_lluvia <- renderPlotly({
      historicos_llenado_grano1 <- llenado_grano1()$historicos
      historicos_llenado_grano2 <- llenado_grano2()$historicos
      historicos_llenado_grano3 <- llenado_grano3()$historicos

      fechas <- c(
        paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
        paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
        paste(input$dia_siembra3, input$mes_siembra3, sep = "-")
      )

      if (length(unique(fechas)) < length(fechas)) {
        plot_blank <- ggplot() +
          theme_void() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "Por favor, elija 3 fechas diferentes.",
            size = 5,
            color = "red"
          ) +
          labs(title = "")

        return(plot_blank)
      }

      mediana_lluvia1 <- median(
        round(historicos_llenado_grano1$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp1 <- median(
        round(historicos_llenado_grano1$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion1 <- mean(
        round(historicos_llenado_grano1$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_1 <- median(
        round(historicos_llenado_grano1$dias_minimas_2, 0),
        na.rm = TRUE
      )

      mediana_lluvia2 <- median(
        round(historicos_llenado_grano2$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp2 <- median(
        round(historicos_llenado_grano2$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion2 <- mean(
        round(historicos_llenado_grano2$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_2 <- median(
        round(historicos_llenado_grano2$dias_minimas_2, 0),
        na.rm = TRUE
      )

      mediana_lluvia3 <- median(
        round(historicos_llenado_grano3$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp3 <- median(
        round(historicos_llenado_grano3$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion3 <- mean(
        round(historicos_llenado_grano3$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_3 <- median(
        round(historicos_llenado_grano3$dias_minimas_2, 0),
        na.rm = TRUE
      )

      estadisticas <- data.frame(
        fecha = fechas,
        lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
        etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
        radiacion_media = c(
          promedio_radiacion1,
          promedio_radiacion2,
          promedio_radiacion3
        ),
        dias_minimas_2 = c(
          mediana_dias_minimas_2_1,
          mediana_dias_minimas_2_2,
          mediana_dias_minimas_2_3
        )
      )

      estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)

      lg_lluvia <- ggplot(
        estadisticas,
        aes(x = fecha, y = lluvia_acumulada, fill = fecha)
      ) +
        geom_bar(
          aes(y = lluvia_acumulada, fill = fecha),
          stat = "identity",
          position = position_dodge(0.9)
        ) +
        geom_bar(
          aes(y = etp_acumulada, fill = fecha),
          stat = "identity",
          alpha = 0.4,
          position = position_dodge(0.9)
        ) +
        geom_text(
          aes(y = lluvia_acumulada, label = lluvia_acumulada),
          position = position_dodge(0.9),
          vjust = -0.5,
          size = 3
        ) +
        geom_text(
          aes(y = etp_acumulada, label = etp_acumulada),
          position = position_dodge(0.9),
          vjust = -0.5,
          size = 3,
          color = "black"
        ) +
        labs(title = "", x = "", y = " mm", fill = NULL) +
        theme_minimal() +
        scale_fill_manual(
          name = "",
          values = c("#E07A5F", "#3D405B", "#AEC3B0")
        ) +
        theme(
          axis.text.x = element_text(size = 10),
          axis.ticks.x = element_blank()
        ) +
        guides(fill = "none")

      ggplotly(lg_lluvia)
    })

    output$lg_radiacion <- renderPlotly({
      historicos_llenado_grano1 <- llenado_grano1()$historicos
      historicos_llenado_grano2 <- llenado_grano2()$historicos
      historicos_llenado_grano3 <- llenado_grano3()$historicos

      fechas <- c(
        paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
        paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
        paste(input$dia_siembra3, input$mes_siembra3, sep = "-")
      )

      if (length(unique(fechas)) < length(fechas)) {
        plot_blank <- ggplot() +
          theme_void() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "Por favor, elija 3 fechas diferentes.",
            size = 5,
            color = "red"
          ) +
          labs(title = "")

        return(plot_blank)
      }

      mediana_lluvia1 <- median(
        round(historicos_llenado_grano1$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp1 <- median(
        round(historicos_llenado_grano1$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion1 <- mean(
        round(historicos_llenado_grano1$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_1 <- median(
        round(historicos_llenado_grano1$dias_minimas_2, 0),
        na.rm = TRUE
      )

      mediana_lluvia2 <- median(
        round(historicos_llenado_grano2$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp2 <- median(
        round(historicos_llenado_grano2$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion2 <- mean(
        round(historicos_llenado_grano2$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_2 <- median(
        round(historicos_llenado_grano2$dias_minimas_2, 0),
        na.rm = TRUE
      )

      mediana_lluvia3 <- median(
        round(historicos_llenado_grano3$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp3 <- median(
        round(historicos_llenado_grano3$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion3 <- mean(
        round(historicos_llenado_grano3$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_3 <- median(
        round(historicos_llenado_grano3$dias_minimas_2, 0),
        na.rm = TRUE
      )

      estadisticas <- data.frame(
        fecha = fechas,
        lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
        etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
        radiacion_media = c(
          promedio_radiacion1,
          promedio_radiacion2,
          promedio_radiacion3
        ),
        dias_minimas_2 = c(
          mediana_dias_minimas_2_1,
          mediana_dias_minimas_2_2,
          mediana_dias_minimas_2_3
        )
      )

      estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)

      lg_radiacion <- ggplot(
        estadisticas,
        aes(x = fecha, y = radiacion_media, fill = fecha)
      ) +
        geom_bar(stat = "identity") +
        geom_text(
          aes(label = round(radiacion_media)),
          position = position_stack(vjust = 1.05),
          size = 3
        ) +
        labs(title = "", x = "", y = " Mj / m2 * día") +
        theme_minimal() +
        scale_fill_manual(
          name = "",
          values = c("#E07A5F", "#3D405B", "#AEC3B0")
        ) +
        theme(
          axis.text.x = element_text(size = 10),
          axis.ticks.x = element_blank()
        ) +
        guides(fill = "none")

      ggplotly(lg_radiacion)
    })

    output$lg_dias_2 <- renderPlotly({
      historicos_llenado_grano1 <- llenado_grano1()$historicos
      historicos_llenado_grano2 <- llenado_grano2()$historicos
      historicos_llenado_grano3 <- llenado_grano3()$historicos

      fechas <- c(
        paste(input$dia_siembra1, input$mes_siembra1, sep = "-"),
        paste(input$dia_siembra2, input$mes_siembra2, sep = "-"),
        paste(input$dia_siembra3, input$mes_siembra3, sep = "-")
      )

      if (length(unique(fechas)) < length(fechas)) {
        plot_blank <- ggplot() +
          theme_void() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "Por favor, elija 3 fechas diferentes.",
            size = 5,
            color = "red"
          ) +
          labs(title = "")

        return(plot_blank)
      }

      mediana_lluvia1 <- median(
        round(historicos_llenado_grano1$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp1 <- median(
        round(historicos_llenado_grano1$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion1 <- mean(
        round(historicos_llenado_grano1$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_1 <- median(
        round(historicos_llenado_grano1$dias_minimas_2, 0),
        na.rm = TRUE
      )

      mediana_lluvia2 <- median(
        round(historicos_llenado_grano2$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp2 <- median(
        round(historicos_llenado_grano2$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion2 <- mean(
        round(historicos_llenado_grano2$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_2 <- median(
        round(historicos_llenado_grano2$dias_minimas_2, 0),
        na.rm = TRUE
      )

      mediana_lluvia3 <- median(
        round(historicos_llenado_grano3$lluvia_acumulada, 0),
        na.rm = TRUE
      )
      mediana_etp3 <- median(
        round(historicos_llenado_grano3$etp_acumulada, 0),
        na.rm = TRUE
      )
      promedio_radiacion3 <- mean(
        round(historicos_llenado_grano3$radiacion_global_media, 0),
        na.rm = TRUE
      )
      mediana_dias_minimas_2_3 <- median(
        round(historicos_llenado_grano3$dias_minimas_2, 0),
        na.rm = TRUE
      )

      # Crear un nuevo dataframe con las estadísticas
      estadisticas <- data.frame(
        fecha = fechas,
        lluvia_acumulada = c(mediana_lluvia1, mediana_lluvia2, mediana_lluvia3),
        etp_acumulada = c(mediana_etp1, mediana_etp2, mediana_etp3),
        radiacion_media = c(
          promedio_radiacion1,
          promedio_radiacion2,
          promedio_radiacion3
        ),
        dias_minimas_2 = c(
          mediana_dias_minimas_2_1,
          mediana_dias_minimas_2_2,
          mediana_dias_minimas_2_3
        )
      )

      estadisticas$fecha <- factor(estadisticas$fecha, levels = fechas)

      lg_dias_2 <- ggplot(
        estadisticas,
        aes(x = fecha, y = dias_minimas_2, fill = fecha)
      ) +
        geom_bar(stat = "identity") +
        geom_text(
          aes(label = dias_minimas_2),
          position = position_stack(vjust = 1.05),
          size = 3
        ) +
        labs(title = "", x = "", y = "# días") +
        theme_minimal() +
        scale_fill_manual(
          name = "",
          values = c("#E07A5F", "#3D405B", "#AEC3B0")
        ) +
        theme(
          axis.text.x = element_text(size = 10),
          axis.ticks.x = element_blank()
        ) +
        guides(fill = "none")

      ggplotly(lg_dias_2)
    })
  })

  ##### BALANCE DE AGUA ########

  output$descarga_modelo_balcarce <- downloadHandler(
    filename = function() {
      "data_usuario.xlsx"
    },
    content = function(file) {
      modelo <- data.frame(
        Fecha = as.Date(c("2025-09-01", "2026-04-30")),
        Lluvia = c(0, 2),
        Riego = c(10, 0)
      )
      writexl::write_xlsx(modelo, file)
    }
  )
  data_usuario_balcarce <- reactive({
    if (is.null(input$clima_balcarce)) {
      return(NULL)
    }

    ext <- tools::file_ext(input$clima_balcarce$name)

    if (ext == "csv") {
      data <- read.csv(input$clima_balcarce$datapath)
    } else if (ext == "xlsx") {
      data <- readxl::read_xlsx(input$clima_balcarce$datapath)
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
      return(NULL)
    }

    required_columns <- c("Fecha", "Lluvia", "Riego")

    if (all(required_columns %in% colnames(data))) {
      showNotification("Archivo subido correctamente.", type = "message")
    } else {
      showNotification(
        "El archivo no tiene las columnas requeridas: Fecha, Lluvia, Riego.",
        type = "error"
      )
      return(NULL)
    }

    return(data)
  })

  datos_actualizados <- reactive({
    if (is.null(data_usuario_balcarce())) {
      if (!"Riego" %in% colnames(datos)) {
        datos <- datos %>%
          mutate(Riego = 0)
      }
      return(datos)
    }

    data_user <- data_usuario_balcarce() %>%
      mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))

    datos_actualizados <- datos %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      left_join(data_user, by = "Fecha", suffix = c("", "_usuario"))

    datos_actualizados <- datos_actualizados %>%
      mutate(
        Precipitacion_Pluviometrica = coalesce(
          Lluvia,
          Precipitacion_Pluviometrica
        ),
        Riego = coalesce(Riego, 0)
      ) %>%

      select(
        Fecha,
        Precipitacion_Pluviometrica,
        Riego,
        Temperatura_Abrigo_150cm,
        Temperatura_Abrigo_150cm_Minima,
        Evapotranspiracion_Potencial
      )

    return(datos_actualizados)
  })

  observeEvent(input$cultivo_balcarce, {
    if (
      input$cultivo_balcarce == "maiz_largo" ||
        input$cultivo_balcarce == "maiz_corto"
    ) {
      updateNumericInput(session, "umbral_et_balcarce", value = 0.5)
    } else if (input$cultivo_balcarce == "soja") {
      updateNumericInput(session, "umbral_et_balcarce", value = 0.5)
    }
  })

  output$mensaje_cultivo_balcarce1 <- renderUI({
    if (input$cultivo_balcarce == "maiz_largo") {
      tagList(
        p(HTML(
          "<b>Características del híbrido de maíz de ciclo largo simulado</b>:<br>
    Requerimiento de tiempo térmico desde siembra a floración: 950 grados día (Tb 8°C).<br>
    Duración del ciclo: aprox. 160 a 120 días de ciclo de siembra a madurez fisiológica
    (se acorta la duración del ciclo a medida que se retrasa la fecha de siembra).<br>
    Número total de hojas: 21 a 22 hojas."
        ))
      )
    } else if (input$cultivo_balcarce == "maiz_corto") {
      tagList(
        p(HTML(
          "<b>Características del híbrido de maíz de ciclo corto simulado:</b><br>
        Requerimiento de tiempo térmico desde siembra a floración: 750 grados día (Tb 8°C).<br>
        Duración del ciclo: aprox. 130 a 100 días de ciclo de siembra a madurez fisiológica
        (se acorta la duración del ciclo a medida que se retrasa la fecha de siembra).<br>
        Número total de hojas: 17 hojas."
        ))
      )
    } else if (input$cultivo_balcarce == "soja") {
      tagList(
        p(HTML("<b>Variedad de soja de grupo 3</b>."))
      )
    }
  })

  output$mensaje_cultivo_balcarce2 <- renderUI({
    if (input$cultivo_balcarce == "maiz_largo") {
      tagList(
        p(HTML(
          "La línea vertical punteada indica posible corte del ciclo de crecimiento del cultivo
    por temperatura de helada <= 2°C, desde que el maíz tiene 6 hojas desarrolladas."
        )),
        p("Los recuadros en los gráficos indican el período crítico.")
      )
    } else if (input$cultivo_balcarce == "maiz_corto") {
      tagList(
        p(HTML(
          "La línea vertical punteada indica posible corte del ciclo de crecimiento del cultivo
        por temperatura de helada <= 2°C, desde que el maíz tiene 6 hojas desarrolladas."
        )),
        p("Los recuadros en los gráficos indican el período crítico.")
      )
    } else if (input$cultivo_balcarce == "soja") {
      tagList(
        p(
          "La línea vertical punteada indica posible corte del ciclo de crecimiento del cultivo por temperatura de helada <= 2°C, desde que la soja alcanza los 70 GD."
        ),
        p("Los recuadros en los gráficos indican el período crítico.")
      )
    }
  })

  output$mensaje_cultivo_balcarce3 <- renderUI({
    if (input$cultivo_balcarce == "maiz_largo") {
      tagList(
        p(
          tags$sup("1"),
          ": Los balances de agua suponen (i) suelos sin pendiente (es decir que no se considera escurrimiento de agua),
    y (ii) 100% de lluvia efectiva (es decir que toda la lluvia ingresa al suelo, independientemente de su intensidad)."
        ),
        p(
          tags$sup("2"),
          "El umbral de agua disponible por debajo del cual disminuye la ET del cultivo se considera, 
           generalmente, entre 0,7 para plantas de raíces poco profundas creciendo en ambientes de alta 
           demanda evaporativa, hasta 0,30 para plantas de raíces profundas en condiciones de baja demanda 
           evaporativa. Un valor de 0,50 es generalmente utilizado para una amplia variedad de cultivos. 
          "
        ),
        p(
          tags$sup("3"),
          "Valores de referencia de contenidos de agua en el límite mínimo (Lmin), límite máximo (Lmax) y fracción de almacenamiento mínimo 
          respecto del máximo para suelos con diferente textura"
        ),
        img(src = "Tabla.png", height = "200px", width = "400px")
      )
    } else if (input$cultivo_balcarce == "maiz_corto") {
      tagList(
        p(
          tags$sup("1"),
          ": Los balances de agua suponen (i) suelos sin pendiente (es decir que no se considera escurrimiento de agua),
    y (ii) 100% de lluvia efectiva (es decir que toda la lluvia ingresa al suelo, independientemente de su intensidad)."
        ),
        p(
          tags$sup("2"),
          "El umbral de agua disponible por debajo del cual disminuye la ET del cultivo se considera, 
           generalmente, entre 0,7 para plantas de raíces poco profundas creciendo en ambientes de alta 
           demanda evaporativa, hasta 0,30 para plantas de raíces profundas en condiciones de baja demanda 
           evaporativa. Un valor de 0,50 es generalmente utilizado para una amplia variedad de cultivos. 
          "
        ),
        p(
          tags$sup("3"),
          "Valores de referencia de contenidos de agua en el límite mínimo (Lmin), límite máximo (Lmax) y fracción de almacenamiento mínimo 
          respecto del máximo para suelos con diferente textura"
        ),
        img(src = "Tabla.png", height = "200px", width = "400px")
      )
    } else if (input$cultivo_balcarce == "soja") {
      tagList(
        p(
          tags$sup("1"),
          ": Los balances de agua suponen (i) suelos sin pendiente (es decir que no se considera escurrimiento de agua),
    y (ii) 100% de lluvia efectiva (es decir que toda la lluvia ingresa al suelo, independientemente de su intensidad)."
        ),
        p(
          tags$sup("2"),
          "El umbral de agua disponible por debajo del cual disminuye la ET del cultivo se considera, 
           generalmente, entre 0,7 para plantas de raíces poco profundas creciendo en ambientes de alta 
           demanda evaporativa, hasta 0,30 para plantas de raíces profundas en condiciones de baja demanda 
           evaporativa. Un valor de 0,50 es generalmente utilizado para una amplia variedad de cultivos. 
           "
        ),
        p(
          tags$sup("3"),
          "Valores de referencia de contenidos de agua en el límite mínimo (Lmin), límite máximo (Lmax) y fracción de almacenamiento mínimo 
          respecto del máximo para suelos con diferente textura"
        ),
        img(src = "Tabla.png", height = "200px", width = "400px")
      )
    }
  })

  almacenamiento_maximo_balcarce <- reactive({
    input$capacidad_campo_balcarce * input$profundidad_balcarce
  })

  almacenamiento_minimo_balcarce <- reactive({
    almacenamiento_maximo_balcarce() * input$fraccion_min_balcarce
  })

  agua_util_total_balcarce <- reactive({
    almacenamiento_maximo_balcarce() - almacenamiento_minimo_balcarce()
  })

  disminucion_et_balcarce <- reactive({
    1 / input$umbral_et_balcarce
  })

  GD_balcarce <- reactive({
    dia_juliano <- yday(input$fecha_siembra_balcarce)

    if (dia_juliano < 60) {
      dia_juliano <- dia_juliano + 365
    }

    if (input$cultivo_balcarce == "maiz_largo") {
      GD_maiz_largo <- round(
        ((-0.0024 * (dia_juliano^2)) - (1.7585 * dia_juliano) + 2469.3),
        0
      )
      return(GD_maiz_largo)
    } else if (input$cultivo_balcarce == "maiz_corto") {
      GD_maiz_corto <- round(
        ((-0.0212 * (dia_juliano^2)) + (10.045 * dia_juliano) + 420.61),
        0
      )
      return(GD_maiz_corto)
    } else if (input$cultivo_balcarce == "soja") {
      GD_soja <- round(
        ((-0.0466 * (dia_juliano^2)) + (26.344 * dia_juliano) - 2479.9),
        0
      )
      return(GD_soja)
    }
  })

  output$almacenamiento_maximo_balcarce <- renderText({
    paste(
      "Almacenamiento máximo de agua (mm):",
      round(almacenamiento_maximo_balcarce(), 2)
    )
  })

  output$almacenamiento_minimo_balcarce <- renderText({
    paste(
      "Almacenamiento mínimo de agua (mm):",
      round(almacenamiento_minimo_balcarce(), 2)
    )
  })

  output$agua_util_total_balcarce <- renderText({
    paste("Agua útil total (mm):", round(agua_util_total_balcarce(), 2))
  })

  output$disminucion_et_balcarce <- renderText({
    paste(
      "Disminución de evapotranspiración:",
      round(disminucion_et_balcarce(), 2)
    )
  })

  output$GD_balcarce <- renderText({
    paste("Grados-Días:", GD_balcarce())
  })

  observe({
    req(input$cultivo_balcarce)

    valor_default <- case_when(
      input$cultivo_balcarce == "maiz_largo" &
        input$densidad_siembra == 8 ~ 0.55,
      input$cultivo_balcarce %in% c("maiz_largo", "maiz_corto", "soja") ~ 0.50,
      TRUE ~ 0.50
    )

    updateNumericInput(
      session,
      "fraccion_inicial_balcarce",
      value = valor_default
    )
  })

  fc_cache <- reactiveVal(NULL)
  fc_cache_time <- reactiveVal(as.POSIXct(NA))

  get_fc_cached <- function() {
    message(">>> ENTER get_fc_cached() at ", as.character(Sys.time()))

    t <- fc_cache_time()
    message("cache_time = ", as.character(t))

    if (!is.na(t) && difftime(Sys.time(), t, units = "mins") < 60) {
      message(">>> RETURN cached forecast (still fresh)")
      return(fc_cache())
    }

    message(">>> Calling Meteored forecast...")
    fc_raw <- tryCatch(
      get_forecast_daily5_full(hash = HASH_BALCARCE),
      error = function(e) {
        message(">>> Meteored ERROR: ", conditionMessage(e))
        return(NULL)
      }
    )

    if (is.null(fc_raw)) {
      message(">>> fc_raw is NULL, returning NULL")
      return(NULL)
    }

    fc <- add_et0_hargreaves_fc(fc_raw, lat_deg = LAT_BALCARCE) |>
      dplyr::mutate(
        Riego = 0,
        Precipitacion_Pluviometrica = dplyr::coalesce(
          as.numeric(Precipitacion_Pluviometrica),
          0
        )
      )

    fc_cache(fc)
    fc_cache_time(Sys.time())

    fc
  }

  serie_balance_balcarce <- reactive({
    obs_raw <- datos_actualizados()
    shiny::validate(shiny::need(
      is.data.frame(obs_raw),
      "datos_actualizados() no devolvió un data.frame/tibble"
    ))

    obs <- tibble::as_tibble(obs_raw) %>%
      mutate(Fecha = as.Date(Fecha), fuente = "Observado") %>%
      select(
        Fecha,
        Temperatura_Abrigo_150cm,
        Temperatura_Abrigo_150cm_Minima,
        Riego,
        Precipitacion_Pluviometrica,
        Evapotranspiracion_Potencial,
        fuente
      ) %>%
      arrange(Fecha)

    fc_raw <- get_fc_cached()
    if (is.null(fc_raw) || !is.data.frame(fc_raw)) {
      return(obs)
    }

    fc <- tibble::as_tibble(fc_raw) %>%
      mutate(
        Fecha = as.Date(Fecha),
        Riego = 0,
        Precipitacion_Pluviometrica = dplyr::coalesce(
          as.numeric(Precipitacion_Pluviometrica),
          0
        ),
        fuente = "Pronóstico"
      )

    corte <- max(obs$Fecha, na.rm = TRUE)

    bind_rows(
      obs,
      fc %>% filter(Fecha > corte)
    ) %>%
      arrange(Fecha) %>%
      distinct(Fecha, .keep_all = TRUE)
  })

  observe({
    get_fc_cached()
  })

  balance_agua_balcarce <- reactive({
    if (is.null(input$fecha_siembra_balcarce)) {
      showNotification(
        "Debe ingresar una fecha de siembra válida.",
        type = "error"
      )
      return(NULL)
    }

    fecha_siembra <- as.Date(input$fecha_siembra_balcarce)

    if (is.null(serie_balance_balcarce())) {
      showNotification("Los datos no están disponibles.", type = "error")
      return(NULL)
    }

    datos_filtrados <- serie_balance_balcarce() %>%
      filter(Fecha >= input$fecha_siembra_balcarce) %>%
      select(
        Fecha,
        Temperatura_Abrigo_150cm,
        Temperatura_Abrigo_150cm_Minima,
        Riego,
        Precipitacion_Pluviometrica,
        Evapotranspiracion_Potencial,
        fuente
      )

    datos_filtrados <- datos_filtrados %>%
      mutate(Dia_Mes = format(Fecha, "%m-%d"))

    fraccion_inicial_balcarce <- input$fraccion_inicial_balcarce
    agua_util_total_val_balcarce <- agua_util_total_balcarce()
    disminucion_et_val_balcarce <- disminucion_et_balcarce()
    GD_balcarce <- GD_balcarce()

    datos_filtrados <- datos_filtrados %>%
      arrange(Fecha) %>%
      mutate(
        TTB_balcarce = case_when(
          input$cultivo_balcarce %in% c("maiz_largo", "maiz_corto") ~ if_else(
            Temperatura_Abrigo_150cm - 8 < 0,
            0,
            Temperatura_Abrigo_150cm - 8
          ),
          input$cultivo_balcarce == "soja" ~ if_else(
            Temperatura_Abrigo_150cm - 11 < 0, # Umbral para soja
            0,
            Temperatura_Abrigo_150cm - 11
          )
        ),
        GD_acum_balcarce = cumsum(TTB_balcarce)
      ) %>%

      filter(GD_acum_balcarce >= 70) %>%
      mutate(
        Ttrelativo_balcarce = GD_acum_balcarce / GD_balcarce,
        Kc_balcarce = case_when(
          input$cultivo_balcarce == "maiz_largo" & input$densidad_siembra == 4 ~
            if_else(
              Ttrelativo_balcarce < 0.2,
              (2 / (7^0.49)) *
                1^((-0.02 - (0.04 * log(7))) * Evapotranspiracion_Potencial),
              -5.0431 *
                Ttrelativo_balcarce^2 +
                6.5687 * Ttrelativo_balcarce -
                0.8352
            ),

          input$cultivo_balcarce == "maiz_largo" & input$densidad_siembra == 8 ~
            if_else(
              Ttrelativo_balcarce < 0.2,
              (2 / (7^0.49)) *
                1^((-0.02 - (0.04 * log(7))) * Evapotranspiracion_Potencial),
              -4.8222 *
                Ttrelativo_balcarce^2 +
                5.9944 * Ttrelativo_balcarce -
                0.5498
            ),

          input$cultivo_balcarce == "maiz_corto" ~
            if_else(
              Ttrelativo_balcarce < 0.2,
              (2 / (7^0.49)) *
                1^((-0.02 - (0.04 * log(7))) * Evapotranspiracion_Potencial),
              -4.8222 *
                Ttrelativo_balcarce^2 +
                5.9944 * Ttrelativo_balcarce -
                0.5498
            ),

          input$cultivo_balcarce == "soja" ~
            if_else(
              Ttrelativo_balcarce > 0.16,
              2.988041 *
                Ttrelativo_balcarce^4 -
                4.052411 * Ttrelativo_balcarce^3 -
                3.999317 * Ttrelativo_balcarce^2 +
                6.015032 * Ttrelativo_balcarce -
                0.390632,
              0.4
            )
        ),
        ETM_balcarce = Kc_balcarce * Evapotranspiracion_Potencial,
        ETM_acum_balcarce = cumsum(ETM_balcarce),

        Fr_agua_util_balcarce = NA_real_,
        agua_util_balcarce = NA_real_,
        ETR_balcarce = NA_real_,
        deficiencia_balcarce = NA_real_
      )

    datos_filtrados$Fr_agua_util_balcarce[1] <- fraccion_inicial_balcarce
    datos_filtrados$agua_util_balcarce[1] <- fraccion_inicial_balcarce *
      agua_util_total_val_balcarce

    for (i in 2:nrow(datos_filtrados)) {
      if (datos_filtrados$GD_acum_balcarce[i - 1] > GD_balcarce) {
        break
      }

      datos_filtrados$ETR_balcarce[i] <- if_else(
        is.na(datos_filtrados$Fr_agua_util_balcarce[i - 1]) |
          is.na(datos_filtrados$ETM_balcarce[i]),
        NA_real_,
        if_else(
          datos_filtrados$Fr_agua_util_balcarce[i - 1] >=
            input$umbral_et_balcarce,
          datos_filtrados$ETM_balcarce[i],
          disminucion_et_val_balcarce *
            datos_filtrados$Fr_agua_util_balcarce[i - 1] *
            datos_filtrados$ETM_balcarce[i]
        )
      )

      datos_filtrados$agua_util_balcarce[i] <- if_else(
        is.na(datos_filtrados$agua_util_balcarce[i - 1]) |
          is.na(datos_filtrados$Precipitacion_Pluviometrica[i]) |
          is.na(datos_filtrados$Riego[i]) |
          is.na(datos_filtrados$ETR_balcarce[i]),
        NA_real_,
        if_else(
          datos_filtrados$agua_util_balcarce[i - 1] +
            datos_filtrados$Precipitacion_Pluviometrica[i] +
            datos_filtrados$Riego[i] -
            datos_filtrados$ETR_balcarce[i] >
            agua_util_total_val_balcarce,
          agua_util_total_val_balcarce,
          datos_filtrados$agua_util_balcarce[i - 1] +
            datos_filtrados$Precipitacion_Pluviometrica[i] +
            datos_filtrados$Riego[i] -
            datos_filtrados$ETR_balcarce[i]
        )
      )

      datos_filtrados$Fr_agua_util_balcarce[i] <- if_else(
        is.na(datos_filtrados$agua_util_balcarce[i]) |
          is.na(agua_util_total_val_balcarce),
        NA_real_,
        datos_filtrados$agua_util_balcarce[i] / agua_util_total_val_balcarce
      )

      datos_filtrados$deficiencia_balcarce[i] <- if_else(
        is.na(datos_filtrados$ETR_balcarce[i]) |
          is.na(datos_filtrados$ETM_balcarce[i]),
        NA_real_,
        datos_filtrados$ETR_balcarce[i] - datos_filtrados$ETM_balcarce[i]
      )
    }

    return(datos_filtrados)
  })

  etm_etr_acum_balcarce <- reactive({
    GD_balcarce <- GD_balcarce()
    shiny::validate(shiny::need(
      is.finite(GD_balcarce),
      "GD del cultivo no disponible (GD_balcarce es NA)."
    ))
    df_siembra_balcarce <- balance_agua_balcarce()
    df_siembra_balcarce <- df_siembra_balcarce %>%
      filter(GD_acum_balcarce <= GD_balcarce)

    ultimo_etm_acum_balcarce <- tail(df_siembra_balcarce$ETM_acum_balcarce, 1)

    ETR_acum_balcarce <- cumsum(ifelse(
      is.na(df_siembra_balcarce$ETR_balcarce),
      0,
      df_siembra_balcarce$ETR_balcarce
    ))
    ultimo_etr_acum_balcarce <- tail(ETR_acum_balcarce, 1)

    huella_hidrica_balcarce <- ultimo_etr_acum_balcarce *
      10000 /
      input$rendimiento_balcarce

    list(
      df_siembra_balcarce = df_siembra_balcarce,
      ultimo_etm_acum_balcarce = ultimo_etm_acum_balcarce,
      ultimo_etr_acum_balcarce = ultimo_etr_acum_balcarce,
      huella_hidrica_balcarce = huella_hidrica_balcarce
    )
  })

  output$ETMacum_balcarce <- renderInfoBox({
    acumulados <- etm_etr_acum_balcarce()
    infoBox(
      title = "",
      subtitle = div(
        p(
          "ETM Acumulado",
          style = "text-align: center; font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 2px;"
      ),
      value = div(
        paste(round(acumulados$ultimo_etm_acum_balcarce, 0), "mm"),
        style = "text-align: center; font-size: 24px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-circle-up",
        style = "font-size: 60px; opacity: 0.6;"
      ),
      color = "danger",
      fill = TRUE
    )
  })

  output$ETRacum_balcarce <- renderInfoBox({
    acumulados <- etm_etr_acum_balcarce()
    infoBox(
      title = "",
      subtitle = div(
        p(
          "ETR Acumulado",
          style = "text-align: center; font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 2px;"
      ),
      value = div(
        paste(round(acumulados$ultimo_etr_acum_balcarce, 0), "mm"),
        style = "text-align: center; font-size: 24px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-retweet",
        style = "font-size: 60px; opacity: 0.6;"
      ),
      color = "olive",
      fill = TRUE
    )
  })

  output$huella_hidrica_balcarce <- renderInfoBox({
    acumulados <- etm_etr_acum_balcarce()

    valor_huella <- if (
      is.na(acumulados$huella_hidrica_balcarce) ||
        !is.finite(acumulados$huella_hidrica_balcarce)
    ) {
      "–"
    } else {
      paste0(round(acumulados$huella_hidrica_balcarce, 0), " l / kg")
    }

    infoBox(
      title = "",
      subtitle = div(
        HTML(
          "<div style='text-align: center; font-size: 24px; font-weight: bold;'>Huella hídrica</div>
         <div style='text-align: center; font-size: 12px; margin-top: 0px;'>l / kg = m<sup>3</sup> / tn</div>
         <div style='text-align: center; font-size: 11px; margin-top: 4px; opacity: 0.7;'>
           Agua necesaria para producir 1 kg de cultivo
         </div>"
        ),
        style = "margin-bottom: 0px;"
      ),
      value = div(
        valor_huella,
        style = "text-align: center; font-size: 24px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-water",
        style = "font-size: 50px; opacity: 0.6;"
      ),
      color = "lightblue",
      fill = TRUE
    )
  })

  output$agua_util_balcarce <- renderPlotly({
    GD_balcarce <- GD_balcarce()
    df_siembra <- balance_agua_balcarce()
    df_siembra <- df_siembra %>% filter(GD_acum_balcarce <= GD_balcarce)

    req(input$fecha_siembra_balcarce)

    dia_juliano <- yday(input$fecha_siembra_balcarce)

    if (dia_juliano < 90) {
      dia_juliano <- dia_juliano + 365
    }

    if (input$cultivo_balcarce == "maiz_largo") {
      GD_ipc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      GD_fpc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo_balcarce == "maiz_corto") {
      GD_ipc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      GD_fpc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo_balcarce == "soja") {
      GD_R3 <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * dia_juliano) - 4047.4),
        0
      )
      GD_R6 <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * dia_juliano) - 2764.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_R3
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_R6
        ],
        na.rm = TRUE
      )

      GD_umbral <- 70

      color_rect <- "darkgreen"
    } else {
      fecha_min <- NA
      fecha_max <- NA
    }

    if (month(input$fecha_siembra_balcarce) >= 5) {
      fecha_inicio_abril <- as.Date(paste0(
        year(input$fecha_siembra_balcarce) + 1,
        "-04-01"
      ))
    } else {
      fecha_inicio_abril <- as.Date(paste0(
        year(input$fecha_siembra_balcarce),
        "-04-01"
      ))
    }

    fecha_vertical_roja <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$GD_acum_balcarce >= GD_umbral &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    fecha_vertical_azul <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$Fecha >= fecha_inicio_abril &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    has_obs <- any(df_siembra$fuente == "Observado")
    has_fc <- any(df_siembra$fuente == "Pronóstico")

    agua_util_balcarce <- ggplot(df_siembra, aes(x = Fecha)) +
      labs(
        title = "",
        x = "",
        y = "Fracción de Agua Útil (0 – 1)"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Fr_agua_util_balcarce" = "#E9C46A")) +
      guides(color = "none")

    if (!is.na(fecha_min) && !is.na(fecha_max) && fecha_max >= fecha_min) {
      agua_util_balcarce <- agua_util_balcarce +
        geom_rect(
          aes(xmin = fecha_min, xmax = fecha_max, ymin = 0, ymax = 1),
          fill = color_rect,
          alpha = 0.2,
          color = NA
        )
    }

    if (has_obs) {
      agua_util_balcarce <- agua_util_balcarce +
        geom_line(
          data = df_siembra %>% dplyr::filter(fuente == "Observado"),
          aes(y = Fr_agua_util_balcarce, color = "Fr_agua_util_balcarce"),
          linewidth = 0.9,
          linetype = "solid"
        )
    }

    if (has_fc) {
      agua_util_balcarce <- agua_util_balcarce +
        geom_line(
          data = df_siembra %>% dplyr::filter(fuente == "Pronóstico"),
          aes(y = Fr_agua_util_balcarce, color = "Fr_agua_util_balcarce"),
          linewidth = 0.8,
          linetype = "dotted"
        )
    }

    if (is.finite(fecha_vertical_roja)) {
      agua_util_balcarce <- agua_util_balcarce +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_roja),
          color = "red",
          linetype = "dashed"
        )
    }

    if (is.finite(fecha_vertical_azul)) {
      agua_util_balcarce <- agua_util_balcarce +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_azul),
          color = "blue",
          linetype = "dashed"
        )
    }

    ggplotly(agua_util_balcarce) %>%
      plotly::style(name = "Periodo crítico", traces = 1) %>%
      plotly::style(name = "Fracción de Agua Útil", traces = 2)
  })

  output$consumo_agua_balcarce <- renderPlotly({
    GD_balcarce <- GD_balcarce()
    df_siembra <- balance_agua_balcarce()
    df_siembra <- df_siembra %>% filter(GD_acum_balcarce <= GD_balcarce)

    shiny::validate(
      shiny::need(
        is.data.frame(df_siembra),
        "balance_agua_balcarce() no devolvió un data.frame."
      ),
      shiny::need(
        nrow(df_siembra) > 1,
        "No hay suficientes filas para graficar."
      ),
      shiny::need(is.finite(GD_balcarce), "GD del cultivo no disponible.")
    )

    shiny::validate(shiny::need(
      nrow(df_siembra) > 1,
      "No se alcanzó el GD necesario para graficar."
    ))

    req(input$fecha_siembra_balcarce)

    dia_juliano <- yday(input$fecha_siembra_balcarce)

    if (dia_juliano < 90) {
      dia_juliano <- dia_juliano + 365
    }

    fecha_actual <- Sys.Date()

    if (input$cultivo_balcarce == "maiz_largo") {
      GD_ipc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      GD_fpc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo_balcarce == "maiz_corto") {
      GD_ipc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      GD_fpc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo_balcarce == "soja") {
      GD_R3 <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * dia_juliano) - 4047.4),
        0
      )
      GD_R6 <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * dia_juliano) - 2764.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_R3
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_R6
        ],
        na.rm = TRUE
      )

      GD_umbral <- 70

      color_rect <- "darkgreen"
    } else {
      fecha_min <- NA
      fecha_max <- NA
    }

    ymax_ETM_balcarce <- max_safe_num(df_siembra$ETM_balcarce)

    if (lubridate::month(input$fecha_siembra_balcarce) >= 5) {
      fecha_inicio_abril <- as.Date(paste0(
        lubridate::year(input$fecha_siembra_balcarce) + 1,
        "-04-01"
      ))
    } else {
      fecha_inicio_abril <- as.Date(paste0(
        lubridate::year(input$fecha_siembra_balcarce),
        "-04-01"
      ))
    }

    fecha_vertical_roja <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$GD_acum_balcarce >= GD_umbral &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )
    fecha_vertical_azul <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$Fecha >= fecha_inicio_abril &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    cons_agua_balcarce <- ggplot(df_siembra, aes(x = Fecha)) +
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_color_manual(values = c("ETM" = "#2A9D8F", "ETR" = "#E76F51")) +
      guides(color = guide_legend(title = NULL))

    if (
      !is.na(fecha_min) &&
        !is.na(fecha_max) &&
        fecha_max >= fecha_min &&
        !is.na(ymax_ETM_balcarce) &&
        ymax_ETM_balcarce > 0
    ) {
      cons_agua_balcarce <- cons_agua_balcarce +
        geom_rect(
          aes(
            xmin = fecha_min,
            xmax = fecha_max,
            ymin = 0,
            ymax = ymax_ETM_balcarce
          ),
          fill = color_rect,
          alpha = 0.2,
          color = NA
        )
    }

    has_obs <- any(df_siembra$fuente == "Observado")
    has_fc <- any(df_siembra$fuente == "Pronóstico")

    if (has_obs) {
      cons_agua_balcarce <- cons_agua_balcarce +
        geom_line(
          data = df_siembra %>% dplyr::filter(fuente == "Observado"),
          aes(y = ETM_balcarce, color = "ETM"),
          linewidth = 0.9
        ) +
        geom_line(
          data = df_siembra %>% dplyr::filter(fuente == "Observado"),
          aes(y = ETR_balcarce, color = "ETR"),
          linewidth = 0.9
        )
    }

    if (has_fc) {
      cons_agua_balcarce <- cons_agua_balcarce +
        geom_line(
          data = df_siembra %>% dplyr::filter(fuente == "Pronóstico"),
          aes(y = ETM_balcarce),
          color = "#2A9D8F80",
          linewidth = 0.7,
          linetype = "dotted"
        ) +
        geom_line(
          data = df_siembra %>% dplyr::filter(fuente == "Pronóstico"),
          aes(y = ETR_balcarce),
          color = "#E76F5180",
          linewidth = 0.7,
          linetype = "dotted"
        )
    }

    if (!is.na(fecha_vertical_roja)) {
      cons_agua_balcarce <- cons_agua_balcarce +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_roja),
          color = "red",
          linetype = "dashed"
        )
    }
    if (!is.na(fecha_vertical_azul)) {
      cons_agua_balcarce <- cons_agua_balcarce +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_azul),
          color = "blue",
          linetype = "dashed"
        )
    }

    ggplotly(cons_agua_balcarce) %>%
      layout(legend = list(orientation = "h", x = 0.3, y = 1.1))
  })

  output$deficit_agua_balcarce <- renderPlotly({
    GD_balcarce <- GD_balcarce()
    df_siembra <- balance_agua_balcarce()
    df_siembra <- df_siembra %>% filter(GD_acum_balcarce <= GD_balcarce)

    obs_raw <- datos_actualizados()
    req(obs_raw)
    corte_obs <- max(as.Date(obs_raw$Fecha), na.rm = TRUE)

    df_siembra <- df_siembra %>%
      mutate(
        fuente_barra = if_else(Fecha <= corte_obs, "Observado", "Pronóstico")
      )

    dia_juliano <- yday(input$fecha_siembra_balcarce)

    if (dia_juliano < 60) {
      dia_juliano <- dia_juliano + 365
    }

    fecha_actual <- Sys.Date()

    if (input$cultivo_balcarce == "maiz_largo") {
      GD_ipc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      GD_fpc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo_balcarce == "maiz_corto") {
      GD_ipc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      GD_fpc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo_balcarce == "soja") {
      GD_R3 <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * dia_juliano) - 4047.4),
        0
      )
      GD_R6 <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * dia_juliano) - 2764.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce >= GD_R3
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum_balcarce) &
            df_siembra$GD_acum_balcarce <= GD_R6
        ],
        na.rm = TRUE
      )

      GD_umbral <- 70

      color_rect <- "darkgreen"
    } else {
      fecha_min <- NA
      fecha_max <- NA
    }

    if (month(input$fecha_siembra_balcarce) >= 5) {
      fecha_inicio_abril <- as.Date(paste0(
        year(input$fecha_siembra_balcarce) + 1,
        "-04-01"
      ))
    } else {
      fecha_inicio_abril <- as.Date(paste0(
        year(input$fecha_siembra_balcarce),
        "-04-01"
      ))
    }

    fecha_vertical_roja <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$GD_acum_balcarce >= GD_umbral &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    fecha_vertical_azul <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$Fecha >= fecha_inicio_abril &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    ymax_pp <- max(df_siembra$Precipitacion_Pluviometrica, na.rm = TRUE)

    has_obs <- any(df_siembra$fuente_barra == "Observado")
    has_fc <- any(df_siembra$fuente_barra == "Pronóstico")

    def_agua_balcarce <- ggplot(df_siembra, aes(x = Fecha)) +
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_fill_manual(
        values = c(
          "Déficit hídrico" = "#C51E3A",
          "Precipitaciones" = "#0047AB",
          "Riego" = "#43B3AE"
        )
      ) +
      guides(fill = guide_legend(title = NULL))

    if (
      !is.na(fecha_min) &&
        !is.na(fecha_max) &&
        fecha_max >= fecha_min &&
        is.finite(ymax_pp) &&
        ymax_pp > 0
    ) {
      def_agua_balcarce <- def_agua_balcarce +
        geom_rect(
          aes(xmin = fecha_min, xmax = fecha_max, ymin = 0, ymax = ymax_pp),
          fill = color_rect,
          alpha = 0.2,
          color = NA
        )
    }

    if (has_obs) {
      def_agua_balcarce <- def_agua_balcarce +
        geom_col(
          data = df_siembra %>% dplyr::filter(fuente_barra == "Observado"),
          aes(y = Precipitacion_Pluviometrica, fill = "Precipitaciones"),
          alpha = 1,
          width = 0.9
        ) +
        geom_col(
          data = df_siembra %>% dplyr::filter(fuente_barra == "Observado"),
          aes(y = deficiencia_balcarce, fill = "Déficit hídrico"),
          alpha = 1,
          width = 0.9
        ) +
        geom_col(
          data = df_siembra %>% dplyr::filter(fuente_barra == "Observado"),
          aes(y = Riego, fill = "Riego"),
          alpha = 1,
          width = 0.9
        )
    }

    if (has_fc) {
      def_agua_balcarce <- def_agua_balcarce +
        geom_col(
          data = df_siembra %>% dplyr::filter(fuente_barra == "Pronóstico"),
          aes(y = Precipitacion_Pluviometrica, fill = "Precipitaciones"),
          alpha = 0.5,
          width = 0.9,
          linewidth = 0.2
        ) +
        geom_col(
          data = df_siembra %>% dplyr::filter(fuente_barra == "Pronóstico"),
          aes(y = deficiencia_balcarce, fill = "Déficit hídrico"),
          alpha = 0.5,
          width = 0.9,
          linewidth = 0.2
        ) +
        geom_col(
          data = df_siembra %>% dplyr::filter(fuente_barra == "Pronóstico"),
          aes(y = Riego, fill = "Riego"),
          alpha = 0.5,
          width = 0.9,
          linewidth = 0.2
        )
    }

    if (!is.na(fecha_vertical_roja)) {
      def_agua_balcarce <- def_agua_balcarce +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_roja),
          color = "red",
          linetype = "dashed"
        )
    }
    if (!is.na(fecha_vertical_azul)) {
      def_agua_balcarce <- def_agua_balcarce +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_azul),
          color = "blue",
          linetype = "dashed"
        )
    }

    ggplotly(def_agua_balcarce) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = 1.2))
  })

  ###### Otros sitios fuera de Balcarce ######

  output$descarga_modelo <- downloadHandler(
    filename = function() {
      "data_usuario.xlsx"
    },
    content = function(file) {
      # Crear un dataframe modelo
      modelo <- data.frame(
        Fecha = as.Date(c("2024-01-01", "2024-02-01")),
        Lluvia = c(0, 2),
        Riego = c(10, 0),
        Temperatura_Media = c(15, 25.6),
        Temperatura_Minima = c(3.2, 5.6),
        ET0 = c(2.1, 3.5)
      )

      writexl::write_xlsx(modelo, file)
    }
  )

  data_usuario <- reactive({
    if (is.null(input$otros_clima)) {
      return(NULL) # Si no hay archivo subido, devuelve NULL
    }

    ext <- tools::file_ext(input$otros_clima$name)

    if (ext == "csv") {
      data <- read.csv(input$otros_clima$datapath)
    } else if (ext == "xlsx") {
      data <- readxl::read_xlsx(input$otros_clima$datapath)
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
      return(NULL)
    }

    required_columns <- c(
      "Fecha",
      "Temperatura_Media",
      "Temperatura_Minima",
      "Riego",
      "Lluvia",
      "ET0"
    )
    if (all(required_columns %in% colnames(data))) {
      showNotification("Archivo subido correctamente.", type = "message")
    } else {
      showNotification(
        "El archivo no tiene las columnas requeridas: Fecha, Lluvia, Riego, Temperatura_Media, Temperatura_Minima, ET0.",
        type = "error"
      )
      return(NULL) # Si no tiene las columnas requeridas, devolver NULL
    }

    return(data)
  })

  output$fileUploaded <- reactive({
    !is.null(input$otros_clima)
  })

  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  datos_validos <- reactive({
    datos <- data_usuario()

    columnas_requeridas <- c(
      "Fecha",
      "Temperatura_Media",
      "Temperatura_Minima",
      "Riego",
      "Lluvia",
      "ET0"
    )

    if (!all(columnas_requeridas %in% colnames(datos))) {
      showNotification(
        "El archivo no contiene todas las columnas requeridas.",
        type = "error"
      )

      return(NULL)
    }
    datos
  })

  observeEvent(input$cultivo, {
    if (input$cultivo == "maiz_largo" || input$cultivo == "maiz_corto") {
      updateNumericInput(session, "umbral_et", value = 0.5)
    } else if (input$cultivo == "soja") {
      updateNumericInput(session, "umbral_et", value = 0.5)
    }
  })

  output$mensaje_cultivo1 <- renderUI({
    if (input$cultivo == "maiz_largo") {
      tagList(
        p(HTML(
          "<b>Características del híbrido de maíz de ciclo largo simulado</b>:<br>
    Requerimiento de tiempo térmico desde siembra a floración: 950 grados día (Tb 8°C).<br>
    Duración del ciclo: aprox. 160 a 120 días de ciclo de siembra a madurez fisiológica
    (se acorta la duración del ciclo a medida que se retrasa la fecha de siembra).<br>
    Número total de hojas: 21 a 22 hojas."
        ))
      )
    } else if (input$cultivo == "maiz_corto") {
      tagList(
        p(HTML(
          "<b>Características del híbrido de maíz de ciclo corto simulado:</b><br>
        Requerimiento de tiempo térmico desde siembra a floración: 750 grados día (Tb 8°C).<br>
        Duración del ciclo: aprox. 130 a 100 días de ciclo de siembra a madurez fisiológica
        (se acorta la duración del ciclo a medida que se retrasa la fecha de siembra).<br>
        Número total de hojas: 17 hojas."
        ))
      )
    } else if (input$cultivo == "soja") {
      tagList(
        p(HTML("<b>Variedad de soja de grupo 3</b>.")),
      )
    }
  })

  output$mensaje_cultivo2 <- renderUI({
    if (input$cultivo == "maiz_largo") {
      tagList(
        p(HTML(
          "La línea vertical punteada indica posible corte del ciclo de crecimiento del cultivo
    por temperatura de helada <= 2°C, desde que el maíz tiene 6 hojas desarrolladas."
        )),
        p("Los recuadros en los gráficos indican el período crítico."),
      )
    } else if (input$cultivo == "maiz_corto") {
      tagList(
        p(HTML(
          "La línea vertical punteada indica posible corte del ciclo de crecimiento del cultivo
        por temperatura de helada <= 2°C, desde que el maíz tiene 6 hojas desarrolladas."
        )),
        p("Los recuadros en los gráficos indican el período crítico."),
      )
    } else if (input$cultivo == "soja") {
      tagList(
        p(
          "La línea vertical punteada indica posible corte del ciclo de crecimiento del cultivo por temperatura de helada <= 2°C, desde que la soja alcanza los 70 GD."
        ),
        p("Los recuadros en los gráficos indican el período crítico."),
      )
    }
  })

  output$mensaje_cultivo3 <- renderUI({
    if (input$cultivo == "maiz_largo") {
      tagList(
        p(
          tags$sup("1"),
          ": Los balances de agua suponen (i) suelos sin pendiente (es decir que no se considera escurrimiento de agua),
    y (ii) 100% de lluvia efectiva (es decir que toda la lluvia ingresa al suelo, independientemente de su intensidad)."
        ),
        p(
          tags$sup("2"),
          "El umbral de agua disponible por debajo del cual disminuye la ET del cultivo se considera, 
           generalmente, entre 0,7 para plantas de raíces poco profundas creciendo en ambientes de alta 
           demanda evaporativa, hasta 0,30 para plantas de raíces profundas en condiciones de baja demanda 
           evaporativa. Un valor de 0,50 es generalmente utilizado para una amplia variedad de cultivos."
        ),
        p(
          tags$sup("3"),
          "Valores de referencia de contenidos de agua en el límite mínimo (Lmin), límite máximo (Lmax) y fracción de almacenamiento mínimo 
          respecto del máximo para suelos con diferente textura"
        ),
        img(src = "Tabla.png", height = "200px", width = "400px")
      )
    } else if (input$cultivo == "maiz_corto") {
      tagList(
        p(
          tags$sup("1"),
          ": Los balances de agua suponen (i) suelos sin pendiente (es decir que no se considera escurrimiento de agua),
    y (ii) 100% de lluvia efectiva (es decir que toda la lluvia ingresa al suelo, independientemente de su intensidad)."
        ),
        p(
          tags$sup("2"),
          "El umbral de agua disponible por debajo del cual disminuye la ET del cultivo se considera, 
           generalmente, entre 0,7 para plantas de raíces poco profundas creciendo en ambientes de alta 
           demanda evaporativa, hasta 0,30 para plantas de raíces profundas en condiciones de baja demanda 
           evaporativa. Un valor de 0,50 es generalmente utilizado para una amplia variedad de cultivos."
        ),
        p(
          tags$sup("3"),
          "Valores de referencia de contenidos de agua en el límite mínimo (Lmin), límite máximo (Lmax) y fracción de almacenamiento mínimo 
          respecto del máximo para suelos con diferente textura"
        ),
        img(src = "Tabla.png", height = "200px", width = "400px")
      )
    } else if (input$cultivo == "soja") {
      tagList(
        p(
          tags$sup("1"),
          ": Los balances de agua suponen (i) suelos sin pendiente (es decir que no se considera escurrimiento de agua),
    y (ii) 100% de lluvia efectiva (es decir que toda la lluvia ingresa al suelo, independientemente de su intensidad)."
        ),
        p(
          tags$sup("2"),
          "El umbral de agua disponible por debajo del cual disminuye la ET del cultivo se considera, 
           generalmente, entre 0,7 para plantas de raíces poco profundas creciendo en ambientes de alta 
           demanda evaporativa, hasta 0,30 para plantas de raíces profundas en condiciones de baja demanda 
           evaporativa. Un valor de 0,50 es generalmente utilizado para una amplia variedad de cultivos."
        ),
        p(
          tags$sup("3"),
          "Valores de referencia de contenidos de agua en el límite mínimo (Lmin), límite máximo (Lmax) y fracción de almacenamiento mínimo 
          respecto del máximo para suelos con diferente textura"
        ),
        img(src = "Tabla.png", height = "200px", width = "400px")
      )
    }
  })

  almacenamiento_maximo <- reactive({
    input$capacidad_campo * input$profundidad
  })

  almacenamiento_minimo <- reactive({
    almacenamiento_maximo() * input$fraccion_min
  })

  agua_util_total <- reactive({
    almacenamiento_maximo() - almacenamiento_minimo()
  })

  disminucion_et <- reactive({
    1 / input$umbral_et
  })

  GD <- reactive({
    dia_juliano <- yday(input$fecha_siembra)

    if (dia_juliano < 60) {
      dia_juliano <- dia_juliano + 365
    }

    if (input$cultivo == "maiz_largo") {
      GD_maiz_largo <- round(
        ((-0.0024 * (dia_juliano^2)) - (1.7585 * dia_juliano) + 2469.3),
        0
      )
      return(GD_maiz_largo)
    } else if (input$cultivo == "maiz_corto") {
      GD_maiz_corto <- round(
        ((-0.0212 * (dia_juliano^2)) + (10.045 * dia_juliano) + 420.61),
        0
      )
      return(GD_maiz_corto)
    } else if (input$cultivo == "soja") {
      GD_soja <- round(
        ((-0.0466 * (dia_juliano^2)) + (26.344 * dia_juliano) - 2479.9),
        0
      )
      return(GD_soja)
    }
  })

  output$almacenamiento_maximo <- renderText({
    paste(
      "Almacenamiento máximo de agua (mm):",
      round(almacenamiento_maximo(), 2)
    )
  })

  output$almacenamiento_minimo <- renderText({
    paste(
      "Almacenamiento mínimo de agua (mm):",
      round(almacenamiento_minimo(), 2)
    )
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
    datos_sitios <- datos_validos()

    req(datos_sitios, input$fecha_siembra)

    fraccion_inicial <- input$fraccion_inicial
    agua_util_total_val <- agua_util_total()
    disminucion_et_val <- disminucion_et()
    GD <- GD()

    datos_sitios <- datos_sitios %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      filter(Fecha >= input$fecha_siembra) %>%
      arrange(Fecha) %>%
      mutate(
        TTB = pmax(
          Temperatura_Media - ifelse(input$cultivo == "soja", 11, 8),
          0
        ),
        GD_acum = cumsum(TTB),
        Ttrelativo = GD_acum / GD(),
        Kc = if_else(
          Ttrelativo > 0.16,
          2.988041 *
            Ttrelativo^4 -
            4.052411 * Ttrelativo^3 -
            3.999317 * Ttrelativo^2 +
            6.015032 * Ttrelativo -
            0.390632,
          0.4
        ),
        ETM = Kc * ET0,
        ETM_acum = cumsum(ETM),
        Fr_agua_util = NA_real_,
        agua_util = NA_real_,
        ETR = NA_real_,
        deficiencia = NA_real_
      )

    datos_sitios$Fr_agua_util[1] <- fraccion_inicial
    datos_sitios$agua_util[1] <- fraccion_inicial * agua_util_total_val

    for (i in 2:nrow(datos_sitios)) {
      if (datos_sitios$GD_acum[i - 1] > GD) {
        break
      }

      datos_sitios$ETR[i] <- if_else(
        is.na(datos_sitios$Fr_agua_util[i - 1]) | is.na(datos_sitios$ETM[i]),
        NA_real_,
        if_else(
          datos_sitios$Fr_agua_util[i - 1] >= input$umbral_et,
          datos_sitios$ETM[i],
          disminucion_et_val *
            datos_sitios$Fr_agua_util[i - 1] *
            datos_sitios$ETM[i]
        )
      )

      datos_sitios$agua_util[i] <- if_else(
        is.na(datos_sitios$agua_util[i - 1]) |
          is.na(datos_sitios$Lluvia[i]) |
          is.na(datos_sitios$Riego[i]) |
          is.na(datos_sitios$ETR[i]),
        NA_real_,
        if_else(
          datos_sitios$agua_util[i - 1] +
            datos_sitios$Lluvia[i] +
            datos_sitios$Riego[i] -
            datos_sitios$ETR[i] >
            agua_util_total_val,
          agua_util_total_val,
          datos_sitios$agua_util[i - 1] +
            datos_sitios$Lluvia[i] +
            datos_sitios$Riego[i] -
            datos_sitios$ETR[i]
        )
      )

      datos_sitios$Fr_agua_util[i] <- if_else(
        is.na(datos_sitios$agua_util[i]) | is.na(agua_util_total_val),
        NA_real_,
        datos_sitios$agua_util[i] / agua_util_total_val
      )

      datos_sitios$deficiencia[i] <- if_else(
        is.na(datos_sitios$ETR[i]) | is.na(datos_sitios$ETM[i]),
        NA_real_,
        datos_sitios$ETR[i] - datos_sitios$ETM[i]
      )
    }

    return(datos_sitios)
  })

  etm_etr_acum <- reactive({
    GD <- GD()
    df_siembra <- balance_agua()
    df_siembra <- df_siembra %>% filter(GD_acum <= GD)

    ultimo_etm_acum <- tail(df_siembra$ETM_acum, 1)

    ETR_acum <- cumsum(ifelse(is.na(df_siembra$ETR), 0, df_siembra$ETR))
    ultimo_etr_acum <- tail(ETR_acum, 1)

    rend <- suppressWarnings(as.numeric(input$rendimiento))

    huella_hidrica <- if (
      is.null(input$rendimiento) || is.na(rend) || rend <= 0
    ) {
      NA_real_
    } else {
      ultimo_etr_acum * 10000 / rend
    }

    list(
      df_siembra = df_siembra,
      ultimo_etm_acum = ultimo_etm_acum,
      ultimo_etr_acum = ultimo_etr_acum,
      huella_hidrica = huella_hidrica
    )
  })

  output$ETMacum <- renderInfoBox({
    acumulados <- etm_etr_acum()
    infoBox(
      title = "",
      subtitle = div(
        p(
          "ETM Acumulado",
          style = "text-align: center; font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 2px;"
      ),
      value = div(
        paste(round(acumulados$ultimo_etm_acum, 1), "mm"),
        style = "text-align: center; font-size: 24px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-circle-up",
        style = "font-size: 60px; opacity: 0.6;"
      ),
      color = "danger",
      fill = TRUE
    )
  })

  output$ETRacum <- renderInfoBox({
    acumulados <- etm_etr_acum()
    infoBox(
      title = "",
      subtitle = div(
        p(
          "ETR Acumulado",
          style = "text-align: center; font-size: 20px; font-weight: bold;"
        ),
        style = "margin-bottom: 2px;"
      ),
      value = div(
        paste(round(acumulados$ultimo_etr_acum, 1), "mm"),
        style = "text-align: center; font-size: 24px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-retweet",
        style = "font-size: 50px; opacity: 0.6;"
      ),
      color = "olive",
      fill = TRUE
    )
  })

  output$huella_hidrica <- renderInfoBox({
    acumulados <- etm_etr_acum()

    valor_huella <- if (
      is.na(acumulados$huella_hidrica) || !is.finite(acumulados$huella_hidrica)
    ) {
      "–"
    } else {
      paste0(round(acumulados$huella_hidrica, 1), " l / kg")
    }

    infoBox(
      title = "",
      subtitle = div(
        HTML(
          "<div style='text-align: center; font-size: 20px; font-weight: bold;'>Huella hídrica</div>
         <div style='text-align: center; font-size: 12px; margin-top: 0px;'>l / kg = m<sup>3</sup> / tn</div>
         <div style='text-align: center; font-size: 11px; margin-top: 4px; opacity: 0.7;'>
           Agua necesaria para producir 1 kg de cultivo
         </div>"
        ),
        style = "margin-bottom: 0px;"
      ),
      value = div(
        valor_huella,
        style = "text-align: center; font-size: 24px; font-weight: bold;"
      ),
      icon = tags$i(
        class = "fa fa-water",
        style = "font-size: 50px; opacity: 0.6;"
      ),
      color = "lightblue",
      fill = TRUE
    )
  })

  output$agua_util <- renderPlotly({
    GD <- GD()
    df_siembra <- balance_agua()
    df_siembra <- df_siembra %>% filter(GD_acum <= GD)

    req(input$fecha_siembra)

    dia_juliano <- yday(input$fecha_siembra)

    if (dia_juliano < 90) {
      dia_juliano <- dia_juliano + 365
    }

    if (input$cultivo == "maiz_largo") {
      GD_ipc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      GD_fpc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo == "maiz_corto") {
      GD_ipc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      GD_fpc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo == "soja") {
      GD_R3 <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * dia_juliano) - 4047.4),
        0
      )
      GD_R6 <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * dia_juliano) - 2764.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_R3
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_R6
        ],
        na.rm = TRUE
      )

      GD_umbral <- 70

      color_rect <- "darkgreen"
    } else {
      fecha_min <- NA
      fecha_max <- NA
    }

    if (month(input$fecha_siembra) >= 5) {
      fecha_inicio_abril <- as.Date(paste0(
        year(input$fecha_siembra) + 1,
        "-04-01"
      ))
    } else {
      fecha_inicio_abril <- as.Date(paste0(year(input$fecha_siembra), "-04-01"))
    }

    fecha_vertical_roja <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$GD_acum_balcarce >= GD_umbral &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    fecha_vertical_azul <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$Fecha >= fecha_inicio_abril &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    agua_util <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_rect(
        aes(xmin = fecha_min, xmax = fecha_max, ymin = 0, ymax = 1),
        fill = color_rect,
        alpha = 0.2,
        color = NA
      ) +
      geom_line(aes(y = Fr_agua_util, color = "Fr_agua_util")) +
      labs(title = "", x = "", y = "Fracción de Agua Útil (0 - 1)") +
      theme_minimal() +
      scale_color_manual(values = c("#E9C46A")) +
      guides(color = "none")

    if (is.finite(fecha_vertical_roja)) {
      agua_util <- agua_util +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_roja),
          color = "red",
          linetype = "dashed"
        )
    }

    if (is.finite(fecha_vertical_azul)) {
      agua_util <- agua_util +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_azul),
          color = "blue",
          linetype = "dashed"
        )
    }

    ggplotly(agua_util) %>%
      plotly::style(name = "Fracción de Agua Útil", traces = 1)
  })

  output$consumo_agua <- renderPlotly({
    GD <- GD()
    df_siembra <- balance_agua()
    df_siembra <- df_siembra %>% filter(GD_acum <= GD)

    dia_juliano <- yday(input$fecha_siembra)

    if (dia_juliano < 90) {
      dia_juliano <- dia_juliano + 365
    }

    fecha_actual <- Sys.Date()

    if (input$cultivo == "maiz_largo") {
      GD_ipc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      GD_fpc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo == "maiz_corto") {
      GD_ipc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      GD_fpc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo == "soja") {
      GD_R3 <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * dia_juliano) - 4047.4),
        0
      )
      GD_R6 <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * dia_juliano) - 2764.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_R3
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_R6
        ],
        na.rm = TRUE
      )

      GD_umbral <- 70

      color_rect <- "darkgreen"
    } else {
      fecha_min <- NA
      fecha_max <- NA
    }

    ymax_ETM <- max(df_siembra$ETM, na.rm = TRUE)

    if (month(input$fecha_siembra) >= 5) {
      fecha_inicio_abril <- as.Date(paste0(
        year(input$fecha_siembra) + 1,
        "-04-01"
      ))
    } else {
      fecha_inicio_abril <- as.Date(paste0(year(input$fecha_siembra), "-04-01"))
    }

    fecha_vertical_roja <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$GD_acum_balcarce >= GD_umbral &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    fecha_vertical_azul <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$Fecha >= fecha_inicio_abril &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    cons_agua <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_line(aes(y = ETM, color = "ETM")) +
      geom_line(aes(y = ETR, color = "ETR")) +
      geom_rect(
        aes(xmin = fecha_min, xmax = fecha_max, ymin = 0, ymax = ymax_ETM),
        fill = color_rect,
        alpha = 0.2,
        color = NA
      ) +
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_color_manual(values = c("#E76F51", "#2A9D8F")) +
      guides(color = guide_legend(title = NULL))

    if (is.finite(fecha_vertical_roja)) {
      cons_agua <- cons_agua +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_roja),
          color = "red",
          linetype = "dashed"
        )
    }

    if (is.finite(fecha_vertical_azul)) {
      cons_agua <- cons_agua +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_azul),
          color = "blue",
          linetype = "dashed"
        )
    }

    ggplotly(cons_agua) %>%
      layout(legend = list(orientation = "h", x = 0.3, y = 1.1)) %>%
      plotly::style(name = "ETM", traces = 1) %>%
      plotly::style(name = "ETR", traces = 2)
  })

  output$deficit_agua <- renderPlotly({
    GD <- GD()
    df_siembra <- balance_agua()
    df_siembra <- df_siembra %>% filter(GD_acum <= GD)

    dia_juliano <- yday(input$fecha_siembra)

    if (dia_juliano < 60) {
      dia_juliano <- dia_juliano + 365
    }

    fecha_actual <- Sys.Date()

    if (input$cultivo == "maiz_largo") {
      GD_ipc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3295.9),
        0
      )
      GD_fpc <- round(
        ((0.0217 * (dia_juliano^2)) - (14.967 * dia_juliano) + 3745.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo == "maiz_corto") {
      GD_ipc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2339.9),
        0
      )
      GD_fpc <- round(
        ((0.0134 * (dia_juliano^2)) - (9.8499 * dia_juliano) + 2789.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_ipc
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_fpc
        ],
        na.rm = TRUE
      )

      GD_umbral <- 340

      color_rect <- "darkgreen"
    } else if (input$cultivo == "soja") {
      GD_R3 <- round(
        ((-0.0476 * (dia_juliano^2)) + (30.212 * dia_juliano) - 4047.4),
        0
      )
      GD_R6 <- round(
        ((-0.0447 * (dia_juliano^2)) + (26.268 * dia_juliano) - 2764.9),
        0
      )

      fecha_min <- min_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum >= GD_R3
        ],
        na.rm = TRUE
      )
      fecha_max <- max_safe_date(
        df_siembra$Fecha[
          !is.na(df_siembra$GD_acum) & df_siembra$GD_acum <= GD_R6
        ],
        na.rm = TRUE
      )

      GD_umbral <- 70

      color_rect <- "darkgreen"
    } else {
      fecha_min <- NA
      fecha_max <- NA
    }

    if (month(input$fecha_siembra) >= 5) {
      fecha_inicio_abril <- as.Date(paste0(
        year(input$fecha_siembra) + 1,
        "-04-01"
      ))
    } else {
      fecha_inicio_abril <- as.Date(paste0(year(input$fecha_siembra), "-04-01"))
    }

    fecha_vertical_roja <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$GD_acum_balcarce >= GD_umbral &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    fecha_vertical_azul <- min_safe_date(
      df_siembra$Fecha[
        df_siembra$Fecha >= fecha_inicio_abril &
          df_siembra$Temperatura_Abrigo_150cm_Minima <= 2
      ]
    )

    ymax_pp <- max(df_siembra$Precipitacion_Pluviometrica, na.rm = TRUE)

    def_agua <- ggplot(df_siembra, aes(x = Fecha)) +
      geom_bar(
        aes(y = Lluvia, fill = "Precipitacion_Pluviometrica"),
        stat = "identity",
        position = "dodge"
      ) +
      geom_bar(
        aes(y = deficiencia, fill = "deficiencia"),
        stat = "identity",
        position = "dodge"
      ) +
      geom_bar(
        aes(y = Riego, fill = "Riego"),
        stat = "identity",
        position = "dodge"
      ) +
      geom_rect(
        aes(xmin = fecha_min, xmax = fecha_max, ymin = 0, ymax = ymax_pp),
        fill = color_rect,
        alpha = 0.2,
        color = NA
      ) +
      labs(title = "", x = "", y = "mm") +
      theme_minimal() +
      scale_fill_manual(values = c("#BC4749", "#007EA7", "#BDE0FE")) +
      guides(fill = guide_legend(title = NULL))

    if (is.finite(fecha_vertical_roja)) {
      def_agua <- def_agua +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_roja),
          color = "red",
          linetype = "dashed"
        )
    }

    if (is.finite(fecha_vertical_azul)) {
      def_agua <- def_agua +
        geom_vline(
          xintercept = as.numeric(fecha_vertical_azul),
          color = "blue",
          linetype = "dashed"
        )
    }

    ggplotly(def_agua) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = 1.2)) %>%
      plotly::style(name = "Precipitación", traces = 1) %>%
      plotly::style(name = "Déficit hídrico", traces = 2) %>%
      plotly::style(name = "Riego", traces = 3)
  })

  ##### DATOS #####
  output$datos <- renderDT(
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
      paste("Datos_metereologicos_balcarce_", Sys.Date(), ".xlsx", sep = ",")
    },
    content = function(file) {
      datos_filtrados3 <- datos

      if (!is.null(input$fecha_inicio) && !is.null(input$fecha_fin)) {
        datos_filtrados3 <- subset(
          datos_filtrados3,
          Fecha >= input$fecha_inicio & Fecha <= input$fecha_fin
        )
      } else if (!is.null(input$fecha_inicio)) {
        datos_filtrados3 <- subset(
          datos_filtrados3,
          Fecha >= input$fecha_inicio
        )
      } else if (!is.null(input$fecha_fin)) {
        datos_filtrados3 <- subset(datos_filtrados3, Fecha <= input$fecha_fin)
      }

      datos_filtrados3 <- datos_filtrados3[,
        c("Fecha", input$variables),
        drop = FALSE
      ]

      write_xlsx(datos_filtrados3, file)
    }
  )
}

message("METEORED_API_KEY length: ", nchar(Sys.getenv("METEORED_API_KEY")))

# Run the app ----
shinyApp(ui = ui, server = server)

# renv::snapshot() #para capturar todas las dependencias
# renv::status() #para ver si hay paquetes no instalados

# rsconnect::forgetDeployment(appPath = "I:/TRABAJO/CERBAS/GrupoAgrometeorologia/App_Meteo")

# rsconnect::setAccountInfo(name='intabalcarce',
#                           token='77F02260172FAF69FBFBEA5CCA574B99',
#                           secret='u8vt3UUrp8R9AfAnasDLAwmQilGTd4LctZy9ebnj')
# rsconnect::deployApp(appDir = "I:/TRABAJO/CERBAS/GrupoAgrometeorologia/App_Meteo", appPrimaryDoc = "app.R",
#                      appName = "Agromet", account = 'intabalcarce', server = 'shinyapps.io')

# Conectar a la base de datos
# con <- dbConnect(SQLite(), "datos_diarios.sqlite")
#
# # Listar tablas existentes
# tablas <- dbListTables(con)
# print(tablas)
#
# # Leer la tabla `datos_siga` y visualizar los datos
# datos_siga <- dbReadTable(con, "datos_siga")
# print(head(datos_siga))  # Muestra las primeras filas
#
# # También puedes ejecutar una consulta SQL
# query <- "SELECT * FROM datos_siga LIMIT 10"
# resultados <- dbGetQuery(con, query)
# print(resultados)
#
# # Desconectar
# dbDisconnect(con)
