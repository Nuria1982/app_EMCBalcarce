# =============================================================================
# ANÁLISIS CLIMÁTICO — AÑOS NIÑO — MÚLTIPLES LOCALIDADES
# Datos de entrada : Datos_completos_procesados.xlsx  ("Datos_completos_zona")
# Variables        : Tmax, Tmin, Tmedia, Precipitación
# Período evento   : JJA(año base) → DEF(año base / año+1)
# Análisis         : por trimestre (JJA, SON, DEF) y mes a mes (Jun–Feb)
# Referencia hist. : mediana de la serie 1971–2000 (eventos ≤1990)
#                            mediana de la serie 1991–2020 (eventos ≥1991)
# Prueba estadíst. : Wilcoxon signed-rank bilateral (α = 0.05)
#                    aplicado sobre diferencias (valor Niño − mediana histórica)
# Salida           : Analisis_Anios_Nino_Localidades.xlsx (una hoja/localidad)
# =============================================================================

library(readxl)
library(openxlsx)
library(dplyr)
library(lubridate)

# ── Ajustar si el archivo no está en el directorio de trabajo ──────────────
INPUT_FILE  <- "Datos_completos_procesados.xlsx"
OUTPUT_FILE <- "Analisis_Anios_Nino_Localidades.xlsx"

# ─────────────────────────────────────────────────────────────────────────────
# 0. PARÁMETROS GLOBALES
# ─────────────────────────────────────────────────────────────────────────────
NINO_YEARS <- c(1972, 1982, 1987, 1991, 1994, 1997, 2002, 2004, 2015, 2023)
SHEETS     <- c("Balcarce", "Mar del Plata", "Tandil", "Olavarria", "Azul", "BenitoJuarez")
REF_PRE    <- c(1971, 2000)   # para eventos ≤ 1990
REF_POST   <- c(1991, 2020)   # para eventos ≥ 1991
ALPHA      <- 0.05

VARS   <- c("Tmax", "Tmin", "Tmedia", "Prec")
LABELS <- c("Tmax media (°C)", "Tmin media (°C)", "Tmedia media (°C)", "Prec acum (mm)")

# Definición de trimestres: mes + offset respecto al año base del evento
TRIMESTRES <- list(
  JJA = list(
    meses   = c(6, 7, 8),
    offsets = c(0, 0, 0)
  ),
  SON = list(
    meses   = c(9, 10, 11),
    offsets = c(0, 0, 0)
  ),
  DEF = list(
    meses   = c(12, 1, 2),
    offsets = c(0,  1, 1)   # Dic año base, Ene y Feb año+1
  )
)

# Meses individuales Jun(año) → Feb(año+1)
MESES_SEQ <- list(
  list(mes=6,  off=0, lab="Jun"),
  list(mes=7,  off=0, lab="Jul"),
  list(mes=8,  off=0, lab="Ago"),
  list(mes=9,  off=0, lab="Sep"),
  list(mes=10, off=0, lab="Oct"),
  list(mes=11, off=0, lab="Nov"),
  list(mes=12, off=0, lab="Dic"),
  list(mes=1,  off=1, lab="Ene"),
  list(mes=2,  off=1, lab="Feb")
)

# Paleta de colores (hexadecimal sin #)
C_HDR1   <- "1F4E79"   # azul muy oscuro — título de tabla
C_HDR2   <- "2E75B6"   # azul oscuro — nombre de ventana
C_HDR3   <- "9DC3E6"   # azul claro — sub-grupos y variables
C_MED    <- "DDEBF7"   # celeste muy claro — filas de mediana
C_ODD    <- "FFFFFF"   # blanco
C_EVEN   <- "F2F2F2"   # gris muy claro
C_PROM   <- "FFF2CC"   # amarillo claro — fila promedio Niño
C_SIG_P  <- "C6EFCE"   # verde — significativamente mayor
C_SIG_N  <- "FFC7CE"   # rojo — significativamente menor
C_ND     <- "E7E6E6"   # gris — sin datos

# ─────────────────────────────────────────────────────────────────────────────
# 1. FUNCIONES AUXILIARES
# ─────────────────────────────────────────────────────────────────────────────

# Agrega datos diarios para una ventana de (meses, offsets) dado un año base
agg_window <- function(df, yr_base, meses, offsets, var) {
  dias <- do.call(rbind, mapply(function(m, off) {
    df[year(df$Fecha) == yr_base + off & month(df$Fecha) == m, ]
  }, meses, offsets, SIMPLIFY = FALSE))

  if (is.null(dias) || nrow(dias) == 0) return(NA_real_)

  if (var == "Prec") sum(dias$Prec, na.rm = TRUE)
  else               mean(dias[[var]], na.rm = TRUE)
}

# Calcula la mediana histórica de una ventana para todos los años del período
mediana_historica <- function(df, meses, offsets, periodo, var) {
  anios_base <- unique(year(df$Fecha))
  anios_base <- anios_base[anios_base >= periodo[1] & anios_base <= periodo[2]]

  vals <- sapply(anios_base, function(yr) {
    # Verificar que el año base + offsets tengan datos
    tiene_datos <- all(mapply(function(m, off) {
      nrow(df[year(df$Fecha) == yr + off & month(df$Fecha) == m, ]) > 0
    }, meses, offsets))
    if (!tiene_datos) return(NA_real_)
    agg_window(df, yr, meses, offsets, var)
  })

  median(vals, na.rm = TRUE)
}

# Período de referencia según año del evento
ref_period <- function(yr) if (yr <= 1990) REF_PRE else REF_POST

# Diferencia porcentual
pct_diff <- function(valor, med) {
  if (is.na(med) || med == 0) return(NA_real_)
  round((valor - med) / abs(med) * 100, 1)
}

# Wilcoxon signed-rank bilateral: diferencias valor_niño − mediana_hist
# Devuelve lista(p, dir)
wilcox_test_nino <- function(valores, medianas) {
  diffs <- valores - medianas
  diffs <- diffs[!is.na(diffs)]
  diffs <- diffs[diffs != 0]   # eliminar empates exactos con µ₀
  if (length(diffs) < 4) return(list(p = NA_real_, dir = NA_character_))
  tryCatch({
    res <- wilcox.test(diffs, mu = 0, alternative = "two.sided", exact = TRUE)
    list(p = res$p.value,
         dir = if (median(diffs) > 0) "mayor" else "menor")
  }, error = function(e) list(p = NA_real_, dir = NA_character_))
}

# Escribe una celda con estilo
wc <- function(wb, ws, row, col, val,
               bg = NULL, bold = FALSE, italic = FALSE,
               fc = "000000", fsz = 10,
               ha = "center", wrap = FALSE, border = TRUE) {
  sty <- createStyle(
    fontName       = "Arial",
    fontSize       = fsz,
    fontColour     = paste0("#", fc),
    fgFill         = if (!is.null(bg)) paste0("#", bg) else NA,
    halign         = ha,
    valign         = "center",
    textDecoration = c(if (bold) "bold", if (italic) "italic"),
    border         = if (border) "TopBottomLeftRight" else NULL,
    borderStyle    = if (border) "thin" else NULL,
    wrapText       = wrap
  )
  writeData(wb, ws, val, startRow = row, startCol = col)
  addStyle(wb, ws, sty, rows = row, cols = col)
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. CALCULAR TABLA COMPLETA PARA UNA LOCALIDAD
# ─────────────────────────────────────────────────────────────────────────────
calcular_tabla <- function(df, ventanas_list, vent_labels, nino_disp) {
  # ventanas_list: list de list(meses, offsets)
  # Devuelve list por ventana, por variable:
  #   nino_vals, med_pre, med_post, wtest

  n_vent <- length(ventanas_list)
  resultado <- vector("list", n_vent)
  names(resultado) <- vent_labels

  for (vi in seq_along(ventanas_list)) {
    vent   <- ventanas_list[[vi]]
    vname  <- vent_labels[vi]

    res_var <- vector("list", length(VARS))
    names(res_var) <- VARS

    for (var in VARS) {
      # Medianas históricas (pre y post)
      med_pre  <- mediana_historica(df, vent$meses, vent$offsets, REF_PRE,  var)
      med_post <- mediana_historica(df, vent$meses, vent$offsets, REF_POST, var)

      # Valores para cada año Niño disponible
      vals_nino <- sapply(nino_disp, function(yr) {
        agg_window(df, yr, vent$meses, vent$offsets, var)
      })
      names(vals_nino) <- as.character(nino_disp)

      # Mediana correcta para cada año Niño
      meds_correc <- ifelse(nino_disp <= 1990, med_pre, med_post)

      # Test Wilcoxon
      wt <- wilcox_test_nino(vals_nino, meds_correc)

      res_var[[var]] <- list(
        vals_nino   = vals_nino,
        med_pre     = med_pre,
        med_post    = med_post,
        meds_correc = meds_correc,
        wtest       = wt
      )
    }
    resultado[[vname]] <- res_var
  }
  resultado
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. ESCRIBIR BLOQUE DE TABLA EN EXCEL
# ─────────────────────────────────────────────────────────────────────────────
# Estructura de columnas por ventana:
#   [n_vars cols Mediana hist.] | [n_vars cols Valor Niño] | [n_vars cols Δ%]
# Total por ventana: 3 * n_vars = 12 cols
# Col 1: Año / etiqueta fila

escribir_bloque <- function(wb, ws, r0, c0, titulo, tbl, nino_disp, vent_labels) {
  NV   <- length(VARS)
  CPV  <- 3 * NV      # columnas por ventana
  NW   <- length(vent_labels)
  TCOL <- 1 + NW * CPV  # total columnas

  r <- r0

  # ── Fila 1: título de bloque ──────────────────────────────────────────────
  mergeCells(wb, ws, rows = r, cols = c0:(c0 + TCOL - 1))
  wc(wb, ws, r, c0, titulo, bg = C_HDR1, bold = TRUE, fc = "FFFFFF",
     fsz = 11, ha = "center")
  r <- r + 1

  # ── Fila 2: "Año" + nombre de ventana (merged sobre CPV cols) ────────────
  mergeCells(wb, ws, rows = r, cols = c0:c0)
  wc(wb, ws, r, c0, "Año", bg = C_HDR2, bold = TRUE, fc = "FFFFFF")
  for (vi in seq_len(NW)) {
    cs <- c0 + 1 + (vi - 1) * CPV
    ce <- cs + CPV - 1
    mergeCells(wb, ws, rows = r, cols = cs:ce)
    wc(wb, ws, r, cs, vent_labels[vi], bg = C_HDR2, bold = TRUE, fc = "FFFFFF")
  }
  r <- r + 1

  # ── Fila 3: sub-grupos (Mediana | Valor Niño | Δ%) ───────────────────────
  wc(wb, ws, r, c0, "", bg = C_HDR3)
  for (vi in seq_len(NW)) {
    cs <- c0 + 1 + (vi - 1) * CPV
    # Mediana histórica
    mergeCells(wb, ws, rows = r, cols = cs:(cs + NV - 1))
    wc(wb, ws, r, cs, "Mediana histórica", bg = C_HDR3, bold = TRUE, fsz = 9)
    # Valor Niño
    mergeCells(wb, ws, rows = r, cols = (cs + NV):(cs + 2*NV - 1))
    wc(wb, ws, r, cs + NV, "Valor año Niño", bg = C_HDR3, bold = TRUE, fsz = 9)
    # Δ%
    mergeCells(wb, ws, rows = r, cols = (cs + 2*NV):(cs + 3*NV - 1))
    wc(wb, ws, r, cs + 2*NV, "Δ% vs mediana", bg = C_HDR3, bold = TRUE, fsz = 9)
  }
  r <- r + 1

  # ── Fila 4: nombres de variables ─────────────────────────────────────────
  wc(wb, ws, r, c0, "", bg = C_MED)
  for (vi in seq_len(NW)) {
    cs <- c0 + 1 + (vi - 1) * CPV
    for (k in seq_along(VARS)) {
      lbl <- LABELS[k]
      wc(wb, ws, r, cs + k - 1,       lbl, bg = C_MED, bold = TRUE, fsz = 9, wrap = TRUE)
      wc(wb, ws, r, cs + NV + k - 1,  lbl, bg = C_MED, bold = TRUE, fsz = 9, wrap = TRUE)
      wc(wb, ws, r, cs + 2*NV + k - 1,
         sub("media \\(°C\\)|acum \\(mm\\)", "%", lbl),
         bg = C_MED, bold = TRUE, fsz = 9, wrap = TRUE)
    }
  }
  r <- r + 1

  # ── Filas de datos: un año Niño por fila ─────────────────────────────────
  for (ri in seq_along(nino_disp)) {
    yr   <- nino_disp[ri]
    bg_r <- if (ri %% 2 == 1) C_ODD else C_EVEN
    wc(wb, ws, r, c0, yr, bg = bg_r, bold = FALSE)

    for (vi in seq_len(NW)) {
      vname <- vent_labels[vi]
      cs    <- c0 + 1 + (vi - 1) * CPV

      for (k in seq_along(VARS)) {
        var  <- VARS[k]
        vd   <- tbl[[vname]][[var]]
        val  <- vd$vals_nino[as.character(yr)]
        med  <- vd$meds_correc[ri]
        pdif <- pct_diff(val, med)
        is_p <- (var == "Prec")

        fmt_val <- if (is.na(val))  "SD"  else if (is_p) round(val, 0) else round(val, 1)
        fmt_med <- if (is.na(med))  "SD"  else if (is_p) round(med, 0) else round(med, 1)
        fmt_pct <- if (is.na(pdif)) "SD"  else paste0(ifelse(pdif >= 0, "+", ""), pdif, "%")

        wc(wb, ws, r, cs + k - 1,       fmt_med, bg = C_MED)
        wc(wb, ws, r, cs + NV + k - 1,  fmt_val, bg = bg_r)
        wc(wb, ws, r, cs + 2*NV + k - 1, fmt_pct, bg = bg_r)
      }
    }
    r <- r + 1
  }

  # ── Fila promedio Niño ────────────────────────────────────────────────────
  wc(wb, ws, r, c0, "Prom. Niño", bg = C_PROM, bold = TRUE)

  for (vi in seq_len(NW)) {
    vname <- vent_labels[vi]
    cs    <- c0 + 1 + (vi - 1) * CPV

    for (k in seq_along(VARS)) {
      var  <- VARS[k]
      vd   <- tbl[[vname]][[var]]
      is_p <- (var == "Prec")

      vals_v <- as.numeric(vd$vals_nino)
      meds_v <- vd$meds_correc
      prom_v <- mean(vals_v, na.rm = TRUE)
      prom_m <- mean(meds_v, na.rm = TRUE)
      pdif_p <- pct_diff(prom_v, prom_m)

      # Wilcoxon
      wt    <- vd$wtest
      p_val <- wt$p
      dir_s <- wt$dir
      is_sig <- !is.na(p_val) && p_val < ALPHA

      sig_str <- if (is.na(p_val))     "n/d"
                 else if (!is_sig)      paste0("ns (p=", round(p_val, 3), ")")
                 else if (dir_s == "mayor") paste0("↑ sig. (p=", round(p_val, 3), ")")
                 else                   paste0("↓ sig. (p=", round(p_val, 3), ")")

      bg_sig <- if (!is_sig) C_PROM
                else if (dir_s == "mayor") C_SIG_P
                else C_SIG_N

      fmt_pv  <- if (is.na(prom_v)) "SD" else if (is_p) round(prom_v, 0) else round(prom_v, 1)
      fmt_pm  <- if (is.na(prom_m)) "SD" else if (is_p) round(prom_m, 0) else round(prom_m, 1)
      fmt_pp  <- if (is.na(pdif_p)) "SD" else paste0(ifelse(pdif_p >= 0, "+", ""), pdif_p, "%")

      # Mediana promedio
      wc(wb, ws, r, cs + k - 1, fmt_pm, bg = C_MED, bold = TRUE)
      # Promedio Niño + test
      wc(wb, ws, r, cs + NV + k - 1,
         paste0(fmt_pv, "\n[", sig_str, "]"),
         bg = bg_sig, bold = TRUE, wrap = TRUE)
      # Δ% + test
      wc(wb, ws, r, cs + 2*NV + k - 1,
         paste0(fmt_pp, "\n[", sig_str, "]"),
         bg = bg_sig, bold = TRUE, wrap = TRUE)
    }
  }
  r <- r + 1

  return(r)  # fila siguiente disponible
}

# ─────────────────────────────────────────────────────────────────────────────
# 4. PROCESAR UNA LOCALIDAD COMPLETA
# ─────────────────────────────────────────────────────────────────────────────
procesar_localidad <- function(df_raw, sheet_name, wb) {
  message("  >> ", sheet_name)

  df <- df_raw %>%
    rename(
      Tmax   = `Tmax (°C)`,
      Tmin   = `Tmin (°C)`,
      Tmedia = `Tmedia (°C)`,
      Prec   = `Precipitacion (mm)`
    ) %>%
    mutate(Fecha = as.Date(Fecha))

  # Años Niño con datos suficientes (al menos 30 días en la ventana Jun–Feb)
  nino_disp <- NINO_YEARS[sapply(NINO_YEARS, function(yr) {
    n <- nrow(df[df$Fecha >= as.Date(paste0(yr, "-06-01")) &
                 df$Fecha <= as.Date(paste0(yr + 1, "-02-28")), ])
    n >= 30
  })]

  if (length(nino_disp) == 0) {
    message("    Sin datos. Saltando.")
    return(invisible(NULL))
  }
  message("    Años disponibles: ", paste(nino_disp, collapse = ", "))

  # ── Preparar ventanas ────────────────────────────────────────────────────
  vent_trim  <- TRIMESTRES
  vlabs_trim <- names(TRIMESTRES)

  vent_mes <- lapply(MESES_SEQ, function(m)
    list(meses = m$mes, offsets = m$off))
  vlabs_mes <- sapply(MESES_SEQ, `[[`, "lab")

  # ── Calcular tablas ───────────────────────────────────────────────────────
  tbl_trim <- calcular_tabla(df, vent_trim, vlabs_trim, nino_disp)
  tbl_mes  <- calcular_tabla(df, vent_mes,  vlabs_mes,  nino_disp)

  # ── Hoja Excel ────────────────────────────────────────────────────────────
  addWorksheet(wb, sheet_name)

  titulo_trim <- paste0(
    "ANÁLISIS TRIMESTRAL — ", toupper(sheet_name),
    " | JJA / SON / DEF | n Niño = ", length(nino_disp),
    " | Ref.: 1971–2000 (≤1990) / 1991–2020 (≥1991)"
  )
  titulo_mes <- paste0(
    "ANÁLISIS MENSUAL — ", toupper(sheet_name),
    " | Jun(año) → Feb(año+1) | n Niño = ", length(nino_disp)
  )

  next_r <- escribir_bloque(wb, sheet_name,
                             r0 = 2, c0 = 1,
                             titulo = titulo_trim,
                             tbl    = tbl_trim,
                             nino_disp = nino_disp,
                             vent_labels = vlabs_trim)

  escribir_bloque(wb, sheet_name,
                  r0 = next_r + 2, c0 = 1,
                  titulo = titulo_mes,
                  tbl    = tbl_mes,
                  nino_disp = nino_disp,
                  vent_labels = vlabs_mes)

  # Anchos de columna: col 1 (año) ancha, resto uniformes
  NV   <- length(VARS)
  CPV  <- 3 * NV
  TCOL <- 1 + max(length(vlabs_trim), length(vlabs_mes)) * CPV
  setColWidths(wb, sheet_name, cols = 1,      widths = 14)
  setColWidths(wb, sheet_name, cols = 2:TCOL, widths = 13)

  # Altura de filas de encabezado (wrap)
  setRowHeights(wb, sheet_name, rows = c(5, next_r + 6), heights = 42)

  # Congelar encabezados
  freezePane(wb, sheet_name, firstActiveRow = 7, firstActiveCol = 2)
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. HOJA DE NOTAS METODOLÓGICAS
# ─────────────────────────────────────────────────────────────────────────────
escribir_notas <- function(wb) {
  addWorksheet(wb, "Notas_metodologicas")
  notas <- c(
    "NOTAS METODOLÓGICAS — ANÁLISIS AÑOS NIÑO",
    "",
    "Años Niño analizados: 1972, 1982, 1987, 1991, 1994, 1997, 2002, 2004, 2015, 2023",
    "  Criterio: anomalía RONI ≥ 0.50 en todas las ventanas estacionales JJA → DEF.",
    "  El año 1972 está disponible únicamente en Balcarce (serie desde 1971).",
    "  El resto de localidades inicia en 1980 o posterior. Olavarría sin datos para 1982.",
    "",
    "Período de evaluación por evento: Junio(año base) → Febrero(año base + 1)",
    "  JJA : Junio, Julio, Agosto — año base",
    "  SON : Septiembre, Octubre, Noviembre — año base",
    "  DEF : Diciembre (año base), Enero y Febrero (año base + 1)",
    "",
    "Serie de referencia para el cálculo de la mediana histórica:",
    "  Eventos ≤ 1990 → serie 1971–2000  (normas climatológicas WMO 1961–1990 extendida)",
    "  Eventos ≥ 1991 → serie 1991–2020  (normas climatológicas WMO 1991–2020)",
    "  Para localidades con datos desde 1980, la serie pre-1991 usa los años disponibles",
    "  dentro de 1980–2000 (21 años en vez de 30); esto puede subestimar la variabilidad.",
    "",
    "Variables:",
    "  Tmax media  : promedio de temperaturas máximas diarias del período (°C)",
    "  Tmin media  : promedio de temperaturas mínimas diarias del período (°C)",
    "  Tmedia media: promedio de temperaturas medias diarias del período (°C)",
    "  Prec acum   : precipitación acumulada del período (mm)",
    "",
    "Prueba de significancia estadística:",
    "  Wilcoxon signed-rank test bilateral (función wilcox.test de R, exact = TRUE)",
    "  Hipótesis nula: mediana de las diferencias (valor Niño − mediana histórica) = 0",
    "  Nivel de significancia: α = 0.05",
    "  n mínimo requerido: 4 pares con diferencia ≠ 0 (empates exactos excluidos)",
    "  Resultados en fila 'Prom. Niño':",
    "    ↑ sig.  = significativamente MAYOR que la mediana histórica (p < 0.05)",
    "    ↓ sig.  = significativamente MENOR que la mediana histórica (p < 0.05)",
    "    ns      = no significativo (p ≥ 0.05)",
    "    n/d     = no calculable (n efectivo < 4)",
    "",
    "Código de colores en fila 'Prom. Niño':",
    "  Verde claro (#C6EFCE) : resultado promedio Niño significativamente MAYOR que mediana",
    "  Rojo claro  (#FFC7CE) : resultado promedio Niño significativamente MENOR que mediana",
    "  Amarillo    (#FFF2CC) : resultado no significativo o no disponible",
    "",
    "Advertencias por localidad:",
    "  Olavarría   : sin datos para 1972 y 1982; evento 1987 parcialmente cubierto.",
    "  BenitoJuárez: alta proporción de datos completados con normales históricas (1984–2002).",
    "                Esto puede reducir la variabilidad real y sesgar las pruebas estadísticas.",
    "                Interpretar resultados con precaución para el período 1982–2002.",
    "  Nota general: n Niño efectivo ≤ 9 en la mayoría de las localidades;",
    "                la potencia estadística es limitada para efectos de magnitud moderada."
  )

  sty_title <- createStyle(fontName = "Arial", fontSize = 12, textDecoration = "bold",
                            halign = "left")
  sty_body  <- createStyle(fontName = "Arial", fontSize = 10, halign = "left")

  for (i in seq_along(notas)) {
    writeData(wb, "Notas_metodologicas", notas[i], startRow = i, startCol = 1)
    if (i == 1) addStyle(wb, "Notas_metodologicas", sty_title, rows = i, cols = 1)
    else        addStyle(wb, "Notas_metodologicas", sty_body,  rows = i, cols = 1)
  }
  setColWidths(wb, "Notas_metodologicas", cols = 1, widths = 100)
}

# ─────────────────────────────────────────────────────────────────────────────
# 6. EJECUCIÓN
# ─────────────────────────────────────────────────────────────────────────────
message("=== Inicio del análisis ===")
wb <- createWorkbook()

for (sname in SHEETS) {
  df_raw <- tryCatch(
    read_excel(INPUT_FILE, sheet = sname),
    error = function(e) {
      message("ERROR al leer hoja '", sname, "': ", e$message)
      NULL
    }
  )
  if (!is.null(df_raw)) procesar_localidad(df_raw, sname, wb)
}

escribir_notas(wb)

message("Guardando: ", OUTPUT_FILE)
saveWorkbook(wb, OUTPUT_FILE, overwrite = TRUE)
message("=== Completado ===")
