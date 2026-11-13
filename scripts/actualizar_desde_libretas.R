# Completa balcarce_EMC.xlsx con las libretas meteorológicas mensuales de Dropbox.
# Estación INTA Balcarce EMC - NH0400
#
# Requiere la variable de entorno DROPBOX_URL con el link compartido de la
# CARPETA "BALCARCE" de Dropbox (en GitHub se carga como secret). Con dl=1,
# Dropbox descarga la carpeta entera como .zip.
#
# Toma las libretas del mes actual y del mes anterior (archivos que empiezan
# con "AAAA-MM_", por ejemplo "2026-11_ BALCARCE INTA_nov_rad.xlsx"), lee la
# hoja "Resumen Diario" y:
#   - agrega los días que todavía no están en balcarce_EMC.xlsx
#   - actualiza los valores que cambiaron en la libreta (correcciones)
#   - nunca borra un dato que la libreta tenga vacío

library(readxl)
library(openxlsx)
options(timeout = 600)

archivo <- "balcarce_EMC.xlsx"

# ---------------------------------------------------------------------------
# 1. Descargar la carpeta de Dropbox
# ---------------------------------------------------------------------------

url <- Sys.getenv("DROPBOX_URL")
if (!nzchar(url)) stop("Falta la variable DROPBOX_URL (secret de GitHub).")

if (grepl("dl=0", url, fixed = TRUE)) {
  url <- sub("dl=0", "dl=1", url, fixed = TRUE)
} else if (!grepl("dl=1", url, fixed = TRUE)) {
  url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", "dl=1")
}

zip_tmp <- tempfile(fileext = ".zip")
download.file(url, zip_tmp, mode = "wb", quiet = TRUE)

if (!identical(readBin(zip_tmp, "raw", n = 2), charToRaw("PK"))) {
  stop("Lo que bajó de Dropbox no es un zip. DROPBOX_URL tiene que ser el link de la carpeta BALCARCE.")
}

carpeta <- tempfile("dropbox_")
contenido <- unzip(zip_tmp, list = TRUE)$Name

# ---------------------------------------------------------------------------
# 2. Elegir las libretas del mes actual y del anterior
# ---------------------------------------------------------------------------

hoy <- Sys.Date()
mes_actual <- format(hoy, "%Y-%m")
mes_anterior <- format(as.Date(format(hoy, "%Y-%m-01")) - 1, "%Y-%m")

nombres <- basename(contenido)
es_libreta <- grepl("^\\d{4}-\\d{2}_.*\\.xlsx$", nombres) & !grepl("^~\\$", nombres)
meses_archivo <- substr(nombres, 1, 7)
elegidas <- contenido[es_libreta & meses_archivo %in% c(mes_anterior, mes_actual)]

if (length(elegidas) == 0) {
  stop("No se encontró la libreta de ", mes_actual, " ni de ", mes_anterior, " en la carpeta de Dropbox.")
}

unzip(zip_tmp, files = elegidas, exdir = carpeta, junkpaths = TRUE)
cat("Libretas encontradas:", paste(basename(elegidas), collapse = " | "), "\n")

# ---------------------------------------------------------------------------
# 3. Leer la hoja "Resumen Diario" de cada libreta
# ---------------------------------------------------------------------------

# Números: las celdas vacías, con espacios o con errores de Excel (#VALUE!) quedan NA
a_numero <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

# Redondeo como Excel (0,05 -> 0,1), que es como se venía cargando a mano
redondear <- function(x, dec) sign(x) * floor(abs(x) * 10^dec + 0.5 + 1e-9) / 10^dec

# columna de la libreta -> columna de balcarce_EMC.xlsx, con sus decimales
mapa <- list(
  list("Temperatura Media (°C)",                 "Temperatura_Abrigo_150cm",          1),
  list("Temperatura Maxima (°C)",                "Temperatura_Abrigo_150cm_Maxima",   1),
  list("Temperatura Minima (°C)",                "Temperatura_Abrigo_150cm_Minima",   1),
  list("Temperatura Minima 5cm s/s (°C)",        "Temperatura_Intemperie_5cm_Minima", 1),
  list("Precipitacion (mm)",                     "Precipitacion_Pluviometrica",       1),
  list("Granizo",                                "Granizo",                           0),
  list("Nieve",                                  "Nieve",                             0),
  list("Heliofania Efectiva (hs)",               "Heliofania_Efectiva",               1),
  list("Heliofania Relativa (%)",                "Heliofania_Relativa",               0),
  list("Tension del Vapor media 9-15-21 (hPa)",  "Tesion_Vapor_Media",                1),
  list("HR media 24hs (%)",                      "Humedad_Media",                     0),
  list("HR media 9-15-21 (%)",                   "Humedad_Media_8_14_20",             0),
  list("Punto de Rocío medio 9-15-21 (°C)",      "Rocio_Medio",                       1),
  list("Viento Medio diario 2 m (km/h)",         "Velocidad_Viento_200cm_Media",      1),
  list("Viento Medio diario10 m (km/h)",         "Velocidad_Viento_1000cm_Media",     1),
  list("Presión Estac media 9-15-21 (hPa)",      "Presion_Media",                     1),
  list("Rad Global (Mj/m2.día)",                 "Radiacion_Global",                  1),
  list("Rad Neta (Mj/m2.día)",                   "Radiacion_Neta",                    1),
  list("Evaporación (mm)",                       "Evaporacion_Tanque",                1),
  list("ETP Pennman (mm)",                       "Evapotranspiracion_Potencial",      1)
)

# En la libreta, lluvia / granizo / nieve en blanco significan 0 (si el día tiene datos)
cero_si_vacio <- c("Precipitacion_Pluviometrica", "Granizo", "Nieve")

leer_libreta <- function(ruta) {
  x <- readxl::read_excel(ruta, sheet = "Resumen Diario", col_types = "text")
  faltan <- setdiff(c("Año", "Mes", "DIA", vapply(mapa, `[[`, "", 1)), names(x))
  if (length(faltan)) {
    stop(basename(ruta), ": faltan columnas en 'Resumen Diario': ", paste(faltan, collapse = ", "))
  }
  fecha <- suppressWarnings(as.Date(sprintf(
    "%04d-%02d-%02d",
    as.integer(a_numero(x$Año)), as.integer(a_numero(x$Mes)), as.integer(a_numero(x$DIA))
  )))
  out <- data.frame(Fecha = fecha)
  for (m in mapa) out[[m[[2]]]] <- redondear(a_numero(x[[m[[1]]]]), m[[3]])

  # Un día cuenta como observado si tiene temperatura máxima o mínima
  observado <- !is.na(fecha) & (!is.na(out$Temperatura_Abrigo_150cm_Maxima) |
                                !is.na(out$Temperatura_Abrigo_150cm_Minima))
  out <- out[observado, , drop = FALSE]
  for (v in cero_si_vacio) out[[v]][is.na(out[[v]])] <- 0
  out
}

libretas <- do.call(rbind, lapply(list.files(carpeta, full.names = TRUE), leer_libreta))
libretas <- libretas[order(libretas$Fecha), , drop = FALSE]
libretas <- libretas[!duplicated(libretas$Fecha, fromLast = TRUE), , drop = FALSE]

if (nrow(libretas) == 0) {
  cat("Las libretas todavía no tienen días cargados. Nada que actualizar.\n")
  quit(save = "no", status = 0)
}
if (any(libretas$Fecha > hoy)) stop("Las libretas tienen fechas futuras: revisar año/mes/día.")

cat("Días con datos en las libretas:", format(min(libretas$Fecha)), "a", format(max(libretas$Fecha)), "\n")

# ---------------------------------------------------------------------------
# 4. Volcar los datos en balcarce_EMC.xlsx
# ---------------------------------------------------------------------------

historico <- suppressWarnings(readxl::read_excel(archivo, sheet = 1, col_types = "text"))
fechas_hist <- suppressWarnings(as.Date(as.numeric(historico$Fecha), origin = "1899-12-30"))
sin_num <- is.na(fechas_hist) & !is.na(historico$Fecha)
fechas_hist[sin_num] <- suppressWarnings(as.Date(substr(historico$Fecha[sin_num], 1, 10)))
n_filas <- max(which(!is.na(fechas_hist)), 0)
fechas_hist <- fechas_hist[seq_len(n_filas)]

stopifnot(
  ncol(historico) == 36,
  !anyNA(fechas_hist),
  !anyDuplicated(fechas_hist),
  all(vapply(mapa, `[[`, "", 2) %in% names(historico))
)

wb <- openxlsx::loadWorkbook(archivo)
hoja <- names(wb)[1]
estilo_fecha <- openxlsx::createStyle(numFmt = "dd/mm/yyyy")

meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
           "agosto", "septiembre", "octubre", "noviembre", "diciembre")

agregados <- 0L
cambios <- 0L

for (i in seq_len(nrow(libretas))) {
  fecha <- libretas$Fecha[i]
  fila <- match(fecha, fechas_hist)

  if (is.na(fila)) {
    # Día nuevo: solo se agregan días posteriores al último, para no desordenar
    if (fecha < max(fechas_hist)) {
      warning("Falta el ", format(fecha), " en medio de la serie: no se agrega.")
      next
    }
    fechas_hist <- c(fechas_hist, fecha)
    fila <- length(fechas_hist)
    historico[fila, ] <- NA
    openxlsx::writeData(wb, hoja, fecha, startCol = 1, startRow = fila + 1)
    openxlsx::addStyle(wb, hoja, estilo_fecha, rows = fila + 1, cols = 1)
    openxlsx::writeData(wb, hoja, meses[as.integer(format(fecha, "%m"))], startCol = 2, startRow = fila + 1)
    openxlsx::writeData(wb, hoja, as.numeric(format(fecha, "%Y")), startCol = 3, startRow = fila + 1)
    agregados <- agregados + 1L
  }

  for (m in mapa) {
    destino <- m[[2]]
    nuevo <- libretas[[destino]][i]
    if (is.na(nuevo)) next                       # nunca se borra un dato existente

    actual <- a_numero(historico[[destino]][fila])
    if (!is.na(actual) && abs(actual - nuevo) < 1e-9) next

    openxlsx::writeData(wb, hoja, nuevo, startCol = match(destino, names(historico)), startRow = fila + 1)
    historico[[destino]][fila] <- as.character(nuevo)
    cambios <- cambios + 1L
  }
}

if (agregados + cambios == 0L) {
  cat("balcarce_EMC.xlsx ya estaba al día.\n")
} else {
  temporal <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, temporal, overwrite = TRUE)

  # Verificar antes de reemplazar
  control <- suppressWarnings(readxl::read_excel(temporal, sheet = 1))
  fechas_control <- as.Date(control$Fecha)
  fechas_control <- fechas_control[!is.na(fechas_control)]
  stopifnot(
    ncol(control) == 36,
    length(fechas_control) == length(fechas_hist),
    !anyDuplicated(fechas_control),
    max(fechas_control) == max(fechas_hist)
  )
  if (!file.copy(temporal, archivo, overwrite = TRUE)) stop("No se pudo guardar balcarce_EMC.xlsx.")
  cat("Días agregados:", agregados, "| Valores actualizados:", cambios, "\n")
}

cat("Última fecha en balcarce_EMC.xlsx:", format(max(fechas_hist)), "\n")
