# --- Librerías ---
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)

sf::sf_use_s2(FALSE)

# --- Cargar shapefile ---
mapa <- st_read("geodir_ubigeo_inei.shp", quiet = TRUE)
mapa <- st_make_valid(mapa)
mapa$ubigeo <- as.character(mapa$ubigeo)

# --- Cargar Excel de conflictos ---
conflictos <- read_excel("defensoria 08.xlsx")
conflictos$Ubigeo <- as.character(conflictos$Ubigeo)

# --- Unir shapefile con base de conflictos ---
mapa_data <- mapa %>%
  left_join(conflictos, by = c("ubigeo" = "Ubigeo"))

# --- Calcular centroides solo donde hay conflictos ---
puntos <- st_centroid(mapa_data) %>%
  filter(!is.na(`Denominación del caso`))

coords <- st_coordinates(puntos)
puntos$long <- coords[, 1]
puntos$lat <- coords[, 2]

# --- Colores por Estado ---
colores_estado <- c(
  "Activo" = "red",
  "Latente" = "orange",
  "Resuelto" = "green",
  "Retirado" = "gray"
)

# --- Cargar base IDH ---
idh <- read_excel("idh_limpio.xlsx") %>%
  rename(ubigeo = Ubigeo) %>%
  mutate(ubigeo = as.character(ubigeo))

# --- Unir shapefile con IDH ---
mapa_idh <- mapa_data %>%
  left_join(idh, by = "ubigeo")

# --- INTERFAZ (UI) ---
ui <- fluidPage(
  titlePanel("🗺️ Visualizador de Conflicto Sociales Perú"),
  
  fluidRow(
    column(
      width = 12,
      div(
        style = "background-color:#f8f9fa; border-radius:10px; padding:15px; margin-bottom:15px; text-align:center; box-shadow:0 1px 3px rgba(0,0,0,0.1);",
        h4("Total de conflictos visualizados"),
        h2(textOutput("total_conflictos"), style = "color:#2c3e50; font-weight:bold;")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros de Conflictos"),
      selectInput("departamento", "Departamento:",
                  choices = c("Todos", sort(unique(puntos$Dpto.)))),
      selectInput("provincia", "Provincia:",
                  choices = c("Todos", sort(unique(puntos$Provincia)))),
      selectInput("distrito", "Distrito:",
                  choices = c("Todos", sort(unique(puntos$Distrito)))),
      selectInput("tipo", "Tipo:",
                  choices = c("Todos", sort(unique(puntos$Tipo)))),
      selectInput("actividad", "Actividad:",
                  choices = c("Todos", sort(unique(puntos$Actividad)))),
      selectInput("estado", "Estado:",
                  choices = c("Todos", sort(unique(puntos$Estado)))),
      hr(),
      sliderInput("anio", "Año del IDH:", min = 2017, max = 2024, value = 2024, sep = ""),
      hr(),
      h5("🟢 Leyenda de colores (conflictos):"),
      tags$ul(
        tags$li(tags$span(style="color:red;", "●"), " Activo"),
        tags$li(tags$span(style="color:orange;", "●"), " Latente"),
        tags$li(tags$span(style="color:green;", "●"), " Resuelto"),
        tags$li(tags$span(style="color:gray;", "●"), " Retirado")
      )
    ),
    
    mainPanel(
      leafletOutput("mapa", height = 650)
    )
  )
)

# --- SERVIDOR (Server) ---
server <- function(input, output, session) {
  
  # Filtrar conflictos según selección del usuario
  data_filtrada <- reactive({
    puntos %>%
      filter(
        (Dpto. %in% input$departamento | input$departamento == "Todos"),
        (Provincia %in% input$provincia | input$provincia == "Todos"),
        (Distrito %in% input$distrito | input$distrito == "Todos"),
        (Tipo %in% input$tipo | input$tipo == "Todos"),
        (Actividad %in% input$actividad | input$actividad == "Todos"),
        (Estado %in% input$estado | input$estado == "Todos")
      )
  })
  
  # Contador total
  output$total_conflictos <- renderText({
    nrow(data_filtrada())
  })
  
  # Reactivo para IDH según año
  idh_anio <- reactive({
    mapa_idh %>%
      mutate(IDH = .data[[paste0("IDH ", input$anio)]])
  })
  
  # Render inicial del mapa
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -75, lat = -9, zoom = 5)
  })
  
  # Actualizar mapa según IDH y conflictos
  observe({
    df_idh <- idh_anio()
    df_conf <- data_filtrada()
    
    # --- Escala de colores para el IDH ---
    pal <- colorNumeric(palette = "YlGnBu", domain = df_idh$IDH)
    
    # --- Asignar color según estado ---
    df_conf <- df_conf %>%
      mutate(color_estado = case_when(
        Estado == "Activo" ~ "red",
        Estado == "Latente" ~ "orange",
        Estado == "Resuelto" ~ "green",
        Estado == "Retirado" ~ "gray",
        TRUE ~ "black"
      ))
    
    leafletProxy("mapa", data = df_idh) %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%   # 🔥 Esta línea limpia las leyendas anteriores
      addPolygons(
        fillColor = ~pal(IDH),
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>", distrito, ", ", provincia, "</b><br>",
          "Departamento: ", departamen, "<br>",
          "IDH ", input$anio, ": ", round(IDH, 3)
        )
      ) %>%
      addCircleMarkers(
        data = df_conf,
        lng = ~long,
        lat = ~lat,
        color = ~color_estado,
        fillColor = ~color_estado,
        fillOpacity = 0.9,
        radius = 7,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<b>📍 Denominación del caso:</b> ", `Denominación del caso`, "<br>",
          "<b>Departamento:</b> ", Dpto., "<br>",
          "<b>Provincia:</b> ", Provincia, "<br>",
          "<b>Distrito:</b> ", Distrito, "<br>",
          "<b>Empresa involucrada:</b> ", `Empresa involucrada`, "<br>",
          "<b>Tipo:</b> ", Tipo, "<br>",
          "<b>Actividad:</b> ", Actividad, "<br>",
          "<b>Estado:</b> ", Estado, "<br>",
          "<b>Momento del diálogo:</b> ", `Momento del diálogo`, "<br>",
          "<b>Presencia de la Defensoría:</b> ", `Presencia de la Defensoria del Pueblo`
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~IDH,
                title = paste0("IDH ", input$anio),
                opacity = 1)
  })
}

# --- Lanzar la app ---
shinyApp(ui = ui, server = server)
