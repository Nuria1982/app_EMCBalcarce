# SIGA - INTA Balcarce (NH0400)
# Funciones para descarga y resumen de datos diarios

ESTACION_ID <- "NH0400"
DIR_CACHE <- "cache"

# -----------------------------------------------------------------------------
# 1. Verificar dependencias
# -----------------------------------------------------------------------------

if (!requireNamespace("siga", quietly = TRUE)) {
  stop("Falta instalar el paquete siga.")
}

# -----------------------------------------------------------------------------
# 2. Obtener la estación
# -----------------------------------------------------------------------------

siga_obtener_estacion <- function(id = "NH0400") {
  estaciones <- as.data.frame(
    siga::siga_estaciones(forzar = TRUE)
  )

  estaciones$id <- trimws(as.character(estaciones$id))

  estacion <- estaciones[
    !is.na(estaciones$id) & estaciones$id == id,
    ,
    drop = FALSE
  ]

  if (nrow(estacion) != 1L) {
    stop(
      "Se encontraron ",
      nrow(estacion),
      " estaciones con ID ",
      id
    )
  }

  message("Estación seleccionada: ", estacion$nombre)

  estacion
}

# -----------------------------------------------------------------------------
# 3. Descargar y organizar los datos diarios
# -----------------------------------------------------------------------------

siga_diario <- function(id = ESTACION_ID, desde, hasta, forzar = TRUE) {
  desde <- as.Date(desde)
  hasta <- as.Date(hasta)

  if (
    length(desde) != 1L ||
      length(hasta) != 1L ||
      is.na(desde) ||
      is.na(hasta) ||
      desde > hasta
  ) {
    stop("Intervalo de fechas inválido.")
  }

  estacion <- siga_obtener_estacion(id)

  message("Descargando datos SIGA de ", id, "...")

  datos <- siga::siga_datos(
    estacion,
    forzar = forzar
  )

  datos <- as.data.frame(datos)
  datos$fecha <- as.Date(datos$fecha)

  # Seleccionar intervalo solicitado
  datos <- datos[
    !is.na(datos$fecha) &
      datos$fecha >= desde &
      datos$fecha <= hasta,
    ,
    drop = FALSE
  ]

  # Nombres utilizados en el proyecto
  campos <- c(
    tmax = "temperatura_abrigo_150cm_maxima",
    tmin = "temperatura_abrigo_150cm_minima",
    tmed = "temperatura_abrigo_150cm",
    pp = "precipitacion_pluviometrica",
    et0 = "evapotranspiracion_potencial",
    rad = "radiacion_global",
    hr = "humedad_media_8_14_20",
    helio = "heliofania_efectiva",
    viento = "velocidad_viento_200cm_media",
    pres = "presion_media"
  )

  salida <- data.frame(fecha = datos$fecha)

  for (nombre in names(campos)) {
    campo <- campos[[nombre]]

    if (campo %in% names(datos)) {
      salida[[nombre]] <- suppressWarnings(
        as.numeric(datos[[campo]])
      )
    } else {
      salida[[nombre]] <- rep(NA_real_, nrow(datos))
    }
  }

  # Eliminar duplicados por fecha
  salida <- salida[
    !duplicated(salida$fecha),
    ,
    drop = FALSE
  ]

  # Calendario completo
  calendario <- data.frame(
    fecha = seq(desde, hasta, by = "day")
  )

  # Mantener días sin registros como NA
  salida <- merge(
    calendario,
    salida,
    by = "fecha",
    all.x = TRUE,
    sort = TRUE
  )

  message(
    "Período solicitado: ",
    desde,
    " a ",
    hasta,
    " | Registros con Tmax: ",
    sum(!is.na(salida$tmax)),
    " | Registros con lluvia: ",
    sum(!is.na(salida$pp))
  )

  salida
}

# -----------------------------------------------------------------------------
# 4. Seleccionar última semana completa
# -----------------------------------------------------------------------------

siga_ultima_semana <- function(id = ESTACION_ID) {
  estacion <- siga_obtener_estacion(id)

  datos <- siga::siga_datos(estacion, forzar = TRUE)
  datos <- as.data.frame(datos)

  datos$fecha <- as.Date(datos$fecha)

  # Última fecha con Tmax, Tmin y precipitación disponibles
  validos <- !is.na(datos$fecha) &
    is.finite(datos$temperatura_abrigo_150cm_maxima) &
    is.finite(datos$temperatura_abrigo_150cm_minima) &
    is.finite(datos$precipitacion_pluviometrica)

  if (!any(validos)) {
    stop("No se encontraron registros meteorológicos válidos.")
  }

  ultima_fecha <- max(datos$fecha[validos])

  # Último domingo anterior o igual a la fecha disponible
  fecha_fin <- ultima_fecha -
    (as.integer(format(ultima_fecha, "%u")) %% 7)

  fecha_inicio <- fecha_fin - 6

  semana <- siga_diario(
    id = id,
    desde = fecha_inicio,
    hasta = fecha_fin,
    forzar = FALSE
  )

  # Verificar que los 7 días estén completos
  if (
    nrow(semana) != 7 ||
      anyNA(semana[, c("tmax", "tmin", "pp")])
  ) {
    stop("La última semana no tiene datos completos.")
  }

  semana
}

# -----------------------------------------------------------------------------
# 5. Resumen meteorológico semanal
# -----------------------------------------------------------------------------

siga_resumen_semanal <- function(semana) {
  if (
    nrow(semana) != 7 ||
      anyNA(semana[, c("tmax", "tmin", "pp")])
  ) {
    stop("Se necesitan 7 días con Tmax, Tmin y lluvia.")
  }

  et_completa <- all(is.finite(semana$et0))

  pp_total <- sum(semana$pp)

  et_total <- if (et_completa) {
    sum(semana$et0)
  } else {
    NA_real_
  }

  data.frame(
    inicio = min(semana$fecha),
    fin = max(semana$fecha),
    lluviaTotal = pp_total,
    diasLluvia = sum(semana$pp > 0),
    tmaxAbsoluta = max(semana$tmax),
    tminAbsoluta = min(semana$tmin),
    tmaxMedia = mean(semana$tmax),
    tminMedia = mean(semana$tmin),
    et0Total = et_total,
    balanceAtmosferico = if (et_completa) {
      pp_total - et_total
    } else {
      NA_real_
    }
  )
}


# -----------------------------------------------------------------------------
# 6. Cache de la serie histórica completa
# -----------------------------------------------------------------------------

siga_diario_cache <- function(
  id = ESTACION_ID,
  desde,
  hasta,
  dir = DIR_CACHE,
  actualizar = FALSE
) {
  desde <- as.Date(desde)
  hasta <- as.Date(hasta)

  if (
    length(desde) != 1L ||
      length(hasta) != 1L ||
      is.na(desde) ||
      is.na(hasta) ||
      desde > hasta
  ) {
    stop("Intervalo de fechas inválido.")
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  archivo <- file.path(
    dir,
    paste0("siga_", id, "_historico.rds")
  )

  # Actualizar la serie o crear el archivo si no existe
  if (actualizar || !file.exists(archivo)) {
    message("Actualizando serie histórica SIGA...")

    datos <- siga_diario(
      id = id,
      desde = as.Date("1961-01-01"),
      hasta = max(hasta, Sys.Date()),
      forzar = TRUE
    )

    saveRDS(datos, archivo)
  } else {
    message("Leyendo serie histórica desde cache...")

    datos <- readRDS(archivo)
  }

  # Seleccionar el intervalo solicitado
  datos <- datos[
    datos$fecha >= desde & datos$fecha <= hasta,
    ,
    drop = FALSE
  ]

  # Mantener todas las fechas solicitadas
  calendario <- data.frame(
    fecha = seq(desde, hasta, by = "day")
  )

  merge(
    calendario,
    datos,
    by = "fecha",
    all.x = TRUE,
    sort = TRUE
  )
}

