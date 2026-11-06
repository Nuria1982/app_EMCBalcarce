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
