# Actualización automática del Excel de Agromet
# Estación INTA Balcarce EMC - NH0400

library(readxl)
library(openxlsx)
library(dplyr)

source("scripts/siga_api.R")

archivo <- "balcarce_EMC.xlsx"

# Leer histórico conservando tipos
tipos <- c(
  "date",
  "text",
  rep("numeric", 22),
  "text",
  "numeric",
  "text",
  rep("numeric", 9)
)
tipos[32] <- "text"

historico <- readxl::read_excel(
  archivo,
  col_types = tipos
)

historico$Fecha <- as.Date(historico$Fecha)

stopifnot(
  ncol(historico) == 36,
  !anyNA(historico$Fecha),
  !anyDuplicated(historico$Fecha)
)

# Consultar últimos 30 días y fechas pendientes
hasta <- Sys.Date() - 1L
desde <- min(
  max(historico$Fecha) - 29L,
  hasta - 29L
)

cat("Consultando SIGA:", desde, "a", hasta, "\n")

nuevos <- siga_diario(
  id = "NH0400",
  desde = desde,
  hasta = hasta,
  forzar = TRUE
)

# No modificar Excel si SIGA falla o no devuelve datos
if (is.null(nuevos) || nrow(nuevos) == 0) {
  stop("SIGA no devolvió registros. Excel sin modificar.")
}

campos <- c(
  tmax = "Temperatura_Abrigo_150cm_Maxima",
  tmin = "Temperatura_Abrigo_150cm_Minima",
  tmed = "Temperatura_Abrigo_150cm",
  pp = "Precipitacion_Pluviometrica",
  et0 = "Evapotranspiracion_Potencial",
  rad = "Radiacion_Global",
  hr = "Humedad_Media_8_14_20",
  helio = "Heliofania_Efectiva",
  viento = "Velocidad_Viento_200cm_Media",
  pres = "Presion_Media"
)

stopifnot(
  "fecha" %in% names(nuevos),
  all(names(campos) %in% names(nuevos))
)

nuevos$fecha <- as.Date(nuevos$fecha)

stopifnot(
  !anyNA(nuevos$fecha),
  !anyDuplicated(nuevos$fecha)
)

# Conservar solo días con observaciones
validos <- rowSums(
  !is.na(nuevos[, names(campos), drop = FALSE])
) >
  0

nuevos <- nuevos[validos, , drop = FALSE]

if (nrow(nuevos) == 0) {
  stop("No hay observaciones válidas. Excel sin modificar.")
}

# Abrir Excel original sin reconstruirlo
wb <- openxlsx::loadWorkbook(archivo)
hoja <- names(wb)[1]

meses <- c(
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

fechas_excel <- historico$Fecha
modificaciones <- 0L

for (i in seq_len(nrow(nuevos))) {
  fecha <- nuevos$fecha[i]
  fila <- match(fecha, fechas_excel)

  # Agregar fecha nueva
  if (is.na(fila)) {
    fila <- length(fechas_excel) + 1L
    fechas_excel <- c(fechas_excel, fecha)

    fila_excel <- fila + 1L

    openxlsx::writeData(
      wb,
      hoja,
      x = as.Date(fecha),
      startCol = 1,
      startRow = fila_excel,
      colNames = FALSE
    )

    openxlsx::writeData(
      wb,
      hoja,
      x = meses[as.integer(format(fecha, "%m"))],
      startCol = 2,
      startRow = fila_excel,
      colNames = FALSE
    )

    openxlsx::writeData(
      wb,
      hoja,
      x = as.numeric(format(fecha, "%Y")),
      startCol = 3,
      startRow = fila_excel,
      colNames = FALSE
    )

    historico <- dplyr::bind_rows(
      historico,
      historico[NA_integer_, , drop = FALSE]
    )

    historico$Fecha[fila] <- fecha
    modificaciones <- modificaciones + 1L
  }

  fila_excel <- fila + 1L

  # Completar solo valores faltantes
  for (campo in names(campos)) {
    destino <- campos[[campo]]
    valor <- nuevos[[campo]][i]

    if (!is.finite(valor)) {
      next
    }

    if (is.na(historico[[destino]][fila])) {
      columna <- match(destino, names(historico))

      openxlsx::writeData(
        wb,
        hoja,
        x = valor,
        startCol = columna,
        startRow = fila_excel,
        colNames = FALSE
      )

      historico[[destino]][fila] <- valor
      modificaciones <- modificaciones + 1L
    }
  }
}

# Guardar solo si hay cambios
if (modificaciones > 0L) {
  temporal <- tempfile(fileext = ".xlsx")

  openxlsx::saveWorkbook(
    wb,
    temporal,
    overwrite = TRUE
  )

  # Verificar el archivo generado
  verificacion <- readxl::read_excel(
    temporal,
    col_types = tipos
  )

  stopifnot(
    ncol(verificacion) == 36,
    nrow(verificacion) == length(fechas_excel),
    !anyNA(as.Date(verificacion$Fecha)),
    !anyDuplicated(as.Date(verificacion$Fecha))
  )

  # Sustituir el Excel después de validar
  if (
    !file.copy(
      temporal,
      archivo,
      overwrite = TRUE
    )
  ) {
    stop("No se pudo guardar el Excel actualizado.")
  }

  unlink(temporal)
}

cat("Modificaciones realizadas:", modificaciones, "\n")
cat("Última fecha:", as.character(max(fechas_excel)), "\n")
cat("Registros totales:", length(fechas_excel), "\n")
