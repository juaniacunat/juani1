library(rvest)
library(httr)
library(openxlsx)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

PERSONAS <- list(
  list(
    nombre     = "Abelardo de la Espriella",
    busqueda   = "Abelardo de la Espriella"
  ),
  list(
    nombre     = "Iván Cepeda",
    busqueda   = "Iván Cepeda"
  )
)

ARCHIVO_SALIDA = "frases_politicos.xlsx"

# User-agent para evitar bloqueos básicos
UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

# ── Función auxiliar: GET con reintentos ────────────────────

safe_get <- function(url, n_intentos = 3, pausa = 2) {
  for (i in seq_len(n_intentos)) {
    resp <- tryCatch(
      GET(url,
          add_headers(`User-Agent` = UA,
                      `Accept-Language` = "es-CO,es;q=0.9"),
          timeout(20)),
      error = function(e) NULL
    )
    if (!is.null(resp) && status_code(resp) == 200) return(resp)
    Sys.sleep(pausa)
  }
  message("  ⚠ No se pudo acceder: ", url)
  NULL
}

# ── Función: extraer párrafos que mencionan a la persona ────

extraer_frases <- function(html_nodo, nombre_buscado) {
  # Párrafos del cuerpo del artículo (CORREGIDO: sin comillas)
  parrafos <- html_nodo |>
    html_elements("p, blockquote") |>
    html_text2()
  
  # Filtrar solo los que contienen el nombre
  apellido <- word(nombre_buscado, -1)          # último apellido
  patron   <- str_c(apellido, collapse = "|")   # CORREGIDO: sin comillas
  
  parrafos |>
    keep(~ str_detect(.x, regex(patron, ignore_case = TRUE))) |>
    keep(~ nchar(.x) > 60)                       # descartar fragmentos muy cortos
}

# ── 1. El Espectador ────────────────────────────────────────

scrape_espectador <- function(termino_busqueda) {
  message("\n── El Espectador: '", termino_busqueda, "'")
  query <- URLencode(termino_busqueda, reserved = TRUE)
  url   <- paste0("https://www.elespectador.com/buscar/?q=", query)
  
  resp <- safe_get(url)
  if (is.null(resp)) return(tibble())
  
  pagina <- read_html(content(resp, "text", encoding = "UTF-8"))
  
  # Extraer links de resultados
  links <- pagina |>
    html_elements("a[href]") |>
    html_attr("href") |>
    keep(~ str_detect(.x, "/colombia/")) |>
    unique() |>
    head(10)
  
  links <- if_else(
    str_starts(links, "http"),
    links,
    paste0("https://www.elespectador.com", links)
  )
  
  resultados <- map_dfr(links, function(url_art) {
    Sys.sleep(runif(1, 0.8, 1.5))
    resp2 <- safe_get(url_art)
    if (is.null(resp2)) return(tibble())
    
    art <- read_html(content(resp2, "text", encoding = "UTF-8"))
    
    titulo <- art |> html_element("h1") |> html_text2()
    fecha  <- art |>
      html_element("time, [class*='date'], [class*='fecha']") |>
      html_attr("datetime") %||%
      art |> html_element("time") |> html_text2()
    
    frases <- extraer_frases(art, termino_busqueda)
    
    if (length(frases) == 0) return(tibble())
    
    tibble(
      medio    = "El Espectador",
      url      = url_art,
      titulo   = titulo,
      fecha    = as.character(fecha),
      fragmento = frases
    )
  })
  
  resultados
}

# ── 2. Semana ───────────────────────────────────────────────

scrape_semana <- function(termino_busqueda) {
  message("\n── Semana: '", termino_busqueda, "'")
  query <- URLencode(termino_busqueda, reserved = TRUE)
  url   <- paste0("https://www.semana.com/buscador/?query=", query)
  
  resp <- safe_get(url)
  if (is.null(resp)) return(tibble())
  
  pagina <- read_html(content(resp, "text", encoding = "UTF-8"))
  
  links <- pagina |>
    html_elements("a[href]") |>
    html_attr("href") |>
    keep(~ str_detect(.x, "/nacion/|/politica/|/colombia/")) |>
    unique() |>
    head(10)
  
  links <- if_else(
    str_starts(links, "http"),
    links,
    paste0("https://www.semana.com", links)
  )
  
  resultados <- map_dfr(links, function(url_art) {
    Sys.sleep(runif(1, 0.8, 1.5))
    resp2 <- safe_get(url_art)
    if (is.null(resp2)) return(tibble())
    
    art <- read_html(content(resp2, "text", encoding = "UTF-8"))
    titulo <- art |> html_element("h1") |> html_text2()
    fecha  <- art |>
      html_element("time") |>
      html_attr("datetime") %||% ""
    
    frases <- extraer_frases(art, termino_busqueda)
    if (length(frases) == 0) return(tibble())
    
    tibble(
      medio     = "Semana",
      url       = url_art,
      titulo    = titulo,
      fecha     = as.character(fecha),
      fragmento = frases
    )
  })
  
  resultados
}

# ── 3. Infobae Colombia ─────────────────────────────────────

scrape_infobae <- function(termino_busqueda) {
  message("\n── Infobae Colombia: '", termino_busqueda, "'")
  query <- URLencode(termino_busqueda, reserved = TRUE)
  url   <- paste0(
    "https://www.infobae.com/colombia/",
    "?q=", query
  )
  
  resp <- safe_get(url)
  if (is.null(resp)) return(tibble())
  
  pagina <- read_html(content(resp, "text", encoding = "UTF-8"))
  
  links <- pagina |>
    html_elements("a[href]") |>
    html_attr("href") |>
    keep(~ str_detect(.x, "/colombia/20")) |>
    unique() |>
    head(10)
  
  links <- if_else(
    str_starts(links, "http"),
    links,
    paste0("https://www.infobae.com", links)
  )
  
  resultados <- map_dfr(links, function(url_art) {
    Sys.sleep(runif(1, 0.8, 1.5))
    resp2 <- safe_get(url_art)
    if (is.null(resp2)) return(tibble())
    
    art    <- read_html(content(resp2, "text", encoding = "UTF-8"))
    titulo <- art |> html_element("h1") |> html_text2()
    fecha  <- art |> html_element("time") |> html_attr("datetime") %||% ""
    frases <- extraer_frases(art, termino_busqueda)
    
    if (length(frases) == 0) return(tibble())
    
    tibble(
      medio     = "Infobae Colombia",
      url       = url_art,
      titulo    = titulo,
      fecha     = as.character(fecha),
      fragmento = frases
    )
  })
  
  resultados
}

# ── 4. El Tiempo ────────────────────────────────────────────

scrape_eltiempo <- function(termino_busqueda) {
  message("\n── El Tiempo: '", termino_busqueda, "'")
  query <- URLencode(termino_busqueda, reserved = TRUE)
  url   <- paste0("https://www.eltiempo.com/buscar/?q=", query)
  
  resp <- safe_get(url)
  if (is.null(resp)) return(tibble())
  
  pagina <- read_html(content(resp, "text", encoding = "UTF-8"))
  
  links <- pagina |>
    html_elements("a[href]") |>
    html_attr("href") |>
    keep(~ str_detect(.x, "/politica/|/colombia/|/justicia/")) |>
    unique() |>
    head(10)
  
  links <- if_else(
    str_starts(links, "http"),
    links,
    paste0("https://www.eltiempo.com", links)
  )
  
  resultados <- map_dfr(links, function(url_art) {
    Sys.sleep(runif(1, 0.8, 1.5))
    resp2 <- safe_get(url_art)
    if (is.null(resp2)) return(tibble())
    
    art    <- read_html(content(resp2, "text", encoding = "UTF-8"))
    titulo <- art |> html_element("h1") |> html_text2()
    fecha  <- art |> html_element("time") |> html_attr("datetime") %||% ""
    frases <- extraer_frases(art, termino_busqueda)
    
    if (length(frases) == 0) return(tibble())
    
    tibble(
      medio     = "El Tiempo",
      url       = url_art,
      titulo    = titulo,
      fecha     = as.character(fecha),
      fragmento = frases
    )
  })
  
  resultados
}

# ══════════════════════════════════════════════════════════════
#  LOOP PRINCIPAL
# ══════════════════════════════════════════════════════════════

scrapers <- list(
  scrape_espectador,
  scrape_semana,
  scrape_infobae,
  scrape_eltiempo
)

todos_los_resultados <- map_dfr(PERSONAS, function(persona) {
  message("\n\n========================================")
  message("Buscando: ", persona$nombre)
  message("========================================")
  
  resultados_persona <- map_dfr(scrapers, function(fn) {
    tryCatch(
      fn(persona$busqueda),
      error = function(e) {
        message("  Error en scraper: ", conditionMessage(e))
        tibble()
      }
    )
  })
  
  if (nrow(resultados_persona) == 0) {
    message("  Sin resultados para ", persona$nombre)
    return(tibble())
  }
  
  resultados_persona |>
    mutate(persona = persona$nombre, .before = 1)
})

# ── Limpieza final con Validación de Seguridad ──────────────────────────

if (nrow(todos_los_resultados) > 0 && "fragmento" %in% colnames(todos_los_resultados)) {
  todos_los_resultados <- todos_los_resultados |>
    distinct(fragmento, .keep_all = TRUE) |>   # Eliminar duplicados exactos
    mutate(
      across(where(is.character), str_squish),  # quitar espacios extra
      fragmento = str_replace_all(fragmento,    # limpiar saltos de línea
                                  "[\r\n\t]+", " ")
    ) |>
    arrange(persona, medio, fecha)
} else {
  stop("❌ No se encontraron fragmentos ni noticias válidas para ninguna persona. Revisa el código o la conexión.")
}

# ── Reporte en consola ──────────────────────────────────────

message("\n\n=== RESUMEN ===")
todos_los_resultados |>
  count(persona, medio) |>
  print()

# ══════════════════════════════════════════════════════════════
#  EXPORTAR A EXCEL
# ══════════════════════════════════════════════════════════════

wb <- createWorkbook()

# Estilos
estilo_encabezado <- createStyle(
  fontName      = "Arial",
  fontSize      = 11,
  fontColour    = "#FFFFFF",
  fgFill        = "#1F3864",
  halign        = "CENTER",
  valign        = "CENTER",
  textDecoration = "bold",
  border        = "Bottom",
  borderColour  = "#FFFFFF",
  wrapText      = TRUE
)

estilo_persona <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  fgFill   = "#D9E1F2",
  fontColour = "#1F3864",
  textDecoration = "bold"
)

estilo_normal <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  valign   = "TOP",
  wrapText = TRUE
)

# CORREGIDO: textDecoration = "underline" en lugar de underline = TRUE
estilo_link <- createStyle(
  fontName   = "Arial",
  fontSize   = 10,
  fontColour = "#0563C1",
  textDecoration = "underline"  
)

colores_personas <- c(
  "Abelardo de la Espriella" = "#E2EFDA",
  "Iván Cepeda"              = "#FCE4D6"
)

# ── Hoja 1: Todos los resultados ────────────────────────────

addWorksheet(wb, "Todos los resultados")

encabezados <- c("Persona", "Medio", "Fecha", "Titular",
                 "Fragmento con declaración", "URL")

writeData(wb, "Todos los resultados",
          as.data.frame(t(encabezados)),
          startRow = 1, colNames = FALSE)

addStyle(wb, "Todos los resultados",
         estilo_encabezado,
         rows = 1, cols = 1:6, gridExpand = TRUE)

# Datos
df_export <- todos_los_resultados |>
  select(persona, medio, fecha, titulo, fragmento, url)

writeData(wb, "Todos los resultados",
          df_export,
          startRow = 2, colNames = FALSE)

# Colorear filas por persona
for (i in seq_len(nrow(df_export))) {
  persona_i <- df_export$persona[i]
  color_i   <- colores_personas[persona_i]
  fila_excel <- i + 1
  
  addStyle(wb, "Todos los resultados",
           createStyle(fontName = "Arial", fontSize = 10,
                       valign = "TOP", wrapText = TRUE,
                       fgFill = color_i),
           rows = fila_excel, cols = 1:5, gridExpand = TRUE, stack = FALSE)
  
  # CORREGIDO: textDecoration = "underline" en el bucle
  addStyle(wb, "Todos los resultados",
           createStyle(fontName = "Arial", fontSize = 10,
                       fontColour = "#0563C1", textDecoration = "underline", 
                       valign = "TOP", fgFill = color_i),
           rows = fila_excel, cols = 6, gridExpand = TRUE)
}

# Anchos de columna
setColWidths(wb, "Todos los resultados",
             cols = 1:6,
             widths = c(25, 18, 15, 35, 60, 50))

freezePane(wb, "Todos los resultados", firstRow = TRUE)

# Filtros
addFilter(wb, "Todos los resultados", row = 1, cols = 1:6)

# ── Hojas individuales por persona ──────────────────────────

for (persona in unique(df_export$persona)) {
  nombre_hoja <- str_trunc(persona, 31)  # Excel límita a 31 chars
  addWorksheet(wb, nombre_hoja)
  
  df_p <- df_export |> filter(persona == !!persona)
  
  writeData(wb, nombre_hoja,
            as.data.frame(t(encabezados[-1])),   # sin columna "Persona"
            startRow = 1, colNames = FALSE)
  
  addStyle(wb, nombre_hoja,
           estilo_encabezado,
           rows = 1, cols = 1:5, gridExpand = TRUE)
  
  df_p_sin_persona <- df_p |> select(-persona)
  
  writeData(wb, nombre_hoja,
            df_p_sin_persona,
            startRow = 2, colNames = FALSE)
  
  n_filas <- nrow(df_p_sin_persona)
  
  addStyle(wb, nombre_hoja, estilo_normal,
           rows = 2:(n_filas + 1), cols = 1:4,
           gridExpand = TRUE, stack = FALSE)
  
  addStyle(wb, nombre_hoja, estilo_link,
           rows = 2:(n_filas + 1), cols = 5,
           gridExpand = TRUE, stack = FALSE)
  
  setColWidths(wb, nombre_hoja,
               cols = 1:5,
               widths = c(18, 15, 35, 65, 55))
  
  freezePane(wb, nombre_hoja, firstRow = TRUE)
  addFilter(wb, nombre_hoja, row = 1, cols = 1:5)
}

# ── Hoja de metadata ────────────────────────────────────────

addWorksheet(wb, "Metadata")

meta <- data.frame(
  Campo = c("Fecha de extracción", "Personas buscadas",
            "Fuentes consultadas", "Total fragmentos",
            "Script"),
  Valor = c(
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    paste(sapply(PERSONAS, `[[`, "nombre"), collapse = " | "),
    "El Espectador, Semana, Infobae Colombia, El Tiempo",
    nrow(todos_los_resultados),
    "buscar_frases_politicos.R"
  )
)

writeData(wb, "Metadata", meta, startRow = 1, colNames = TRUE)

addStyle(wb, "Metadata",
         estilo_encabezado,
         rows = 1, cols = 1:2, gridExpand = TRUE)

setColWidths(wb, "Metadata", cols = 1:2, widths = c(22, 60))

# ── Guardar ─────────────────────────────────────────────────

saveWorkbook(wb, ARCHIVO_SALIDA, overwrite = TRUE)
message("\n✅ Archivo guardado: ", ARCHIVO_SALIDA)
message("   Filas totales  : ", nrow(todos_los_resultados))