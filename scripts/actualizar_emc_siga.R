# Actualizar datos de Agromet desde SIGA
# Requiere cargar previamente scripts/siga_api.R

actualizar_emc_siga <- function(
  archivo_historico = "balcarce_EMC.xlsx",
  estacion = "NH0400",
  dias_revision = 30L,
  hasta = Sys.Date() - 1L
) {
  tipos <- c(
    "date", # 1 Fecha
    "text", # 2 Mes
    rep("numeric", 22), # 3 a 24
    "text", # 25 Dirección viento 200 cm
    "numeric", # 26 Velocidad viento 1000 cm
    "text", # 27 Dirección viento 1000 cm
    rep("numeric", 9) # 28 a 36
  )

  # Leer evaporación del tanque como texto
  tipos[32] <- "text"

  historico <- readxl::read_excel(
    archivo_historico,
    col_types = tipos
  )

  # Convertir únicamente los valores numéricos
  historico$Evaporacion_Tanque <- suppressWarnings(
    as.numeric(historico$Evaporacion_Tanque)
  )
  historico$Fecha <- as.Date(historico$Fecha)

  if (
    anyNA(historico$Fecha) ||
      anyDuplicated(historico$Fecha)
  ) {
    stop("El Excel contiene fechas inválidas o duplicadas.")
  }

  columnas <- names(historico)
  hasta <- as.Date(hasta)

  desde <- max(
    min(historico$Fecha),
    min(
      max(historico$Fecha) - dias_revision + 1L,
      hasta - dias_revision + 1L
    )
  )

  if (desde > hasta) {
    return(historico)
  }

  # Correspondencia SIGA - Excel
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

  if (!all(unname(campos) %in% columnas)) {
    stop("Faltan columnas esperadas en el Excel.")
  }

  # Consultar SIGA
  nuevos <- tryCatch(
    siga_diario(
      id = estacion,
      desde = desde,
      hasta = hasta,
      forzar = TRUE
    ),
    error = function(e) {
      warning(
        "SIGA no disponible. Se conserva el Excel: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(nuevos)) {
    return(historico)
  }

  # Excluir fechas sin observaciones
  disponibles <- rowSums(
    !is.na(nuevos[, names(campos), drop = FALSE])
  ) >
    0

  nuevos <- nuevos[disponibles, , drop = FALSE]

  if (nrow(nuevos) == 0) {
    return(historico)
  }

  # Agregar fechas que no existen en el Excel
  faltantes <- setdiff(nuevos$fecha, historico$Fecha)

  if (length(faltantes) > 0) {
    plantilla <- historico[
      rep(NA_integer_, length(faltantes)),
      ,
      drop = FALSE
    ]

    plantilla$Fecha <- as.Date(
      faltantes,
      origin = "1970-01-01"
    )

    historico <- dplyr::bind_rows(
      historico,
      plantilla
    )
  }

  # Completar únicamente valores faltantes
  indices <- match(nuevos$fecha, historico$Fecha)

  for (campo in names(campos)) {
    destino <- campos[[campo]]
    origen <- nuevos[[campo]]
    actual <- historico[[destino]][indices]

    completar <- is.na(actual) & is.finite(origen)

    historico[[destino]][indices[completar]] <-
      origen[completar]
  }

  # Completar Mes y Año
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

  sin_mes <- is.na(historico$Mes)

  historico$Mes[sin_mes] <- meses[
    as.integer(
      format(historico$Fecha[sin_mes], "%m")
    )
  ]

  sin_anio <- is.na(historico$Año)

  historico$Año[sin_anio] <- as.numeric(
    format(historico$Fecha[sin_anio], "%Y")
  )

  historico <- historico[
    order(historico$Fecha),
    columnas,
    drop = FALSE
  ]

  return(historico)
}
