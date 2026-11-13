# Actualiza balcarce_EMC.xlsx con la versión que se carga en Dropbox.
# Estación INTA Balcarce EMC - NH0400
#
# Requiere la variable de entorno DROPBOX_URL con el link compartido del Excel
# (en GitHub se carga como secret). Solo reemplaza el archivo del repo si el de
# Dropbox pasa todos los controles, así un archivo roto o a medio cargar nunca
# llega a la app.

library(readxl)
options(timeout = 600)

archivo <- "balcarce_EMC.xlsx"

url <- Sys.getenv("DROPBOX_URL")
if (!nzchar(url)) stop("Falta la variable DROPBOX_URL (secret de GitHub).")

# Un link compartido de Dropbox abre la vista previa (dl=0); con dl=1 descarga
# el archivo directamente.
if (grepl("dl=0", url, fixed = TRUE)) {
  url <- sub("dl=0", "dl=1", url, fixed = TRUE)
} else if (!grepl("dl=1", url, fixed = TRUE)) {
  url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", "dl=1")
}

descargado <- tempfile(fileext = ".xlsx")
download.file(url, descargado, mode = "wb", quiet = TRUE)

# Si el link está mal, Dropbox devuelve una página HTML en vez del Excel.
# Un .xlsx es un zip y siempre empieza con "PK".
firma <- readBin(descargado, "raw", n = 2)
if (!identical(firma, charToRaw("PK"))) {
  stop("Lo que bajó de Dropbox no es un Excel. Revisar el link en DROPBOX_URL.")
}

leer_serie <- function(ruta) {
  x <- suppressWarnings(readxl::read_excel(ruta, sheet = 1))
  x$Fecha <- as.Date(x$Fecha)
  x[!is.na(x$Fecha), , drop = FALSE]
}

nuevo <- leer_serie(descargado)
actual <- leer_serie(archivo)

# Controles antes de reemplazar el Excel del repo
problemas <- c(
  if (!identical(names(nuevo), names(actual)))
    "las columnas no coinciden con las del Excel actual",
  if (anyDuplicated(nuevo$Fecha))
    "hay fechas duplicadas",
  if (nrow(nuevo) < nrow(actual))
    "tiene menos días que el Excel actual",
  if (max(nuevo$Fecha) < max(actual$Fecha))
    "termina antes que el Excel actual",
  if (max(nuevo$Fecha) > Sys.Date())
    "tiene fechas futuras"
)

if (length(problemas) > 0) {
  stop("El Excel de Dropbox no se usó porque ", paste(problemas, collapse = "; "), ".")
}

if (identical(tools::md5sum(descargado)[[1]], tools::md5sum(archivo)[[1]])) {
  cat("El Excel de Dropbox es igual al del repo. Nada que actualizar.\n")
} else {
  if (!file.copy(descargado, archivo, overwrite = TRUE)) {
    stop("No se pudo guardar el Excel actualizado.")
  }
  cat("Excel actualizado.\n")
}

cat("Última fecha:", format(max(nuevo$Fecha)), "\n")
cat("Registros totales:", nrow(nuevo), "\n")
