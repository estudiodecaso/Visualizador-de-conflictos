library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)

sf::sf_use_s2(FALSE)

# --- Cargar shapefile ---
mapa <- st_read("geodir_ubigeo_inei.shp", quiet = TRUE)
mapa <- st_make_valid(mapa)

# --- Cargar Excel ---
conflictos <- read_excel("defensoria 08.xlsx")

# --- Unir shapefile con base ---
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

# --- UI ---
ui <- fluidPage(
  titlePanel("🗺️ Visualizador de Conflictos Sociales - Defensoría del Pueblo"),
  
  # --- Cuadro de resumen al inicio ---
  fluidRow(
    column(
      width = 12,
      div(
        style = "background-color:#f8f9fa; border-radius:10px; padding:15px; margin-bottom:15px; text-align:center; box-shadow:0 1px 3px rgba(0,0,0,0.1);",
        h4("🔢 Total de conflictos visualizados"),
        h2(textOutput("total_conflictos"), style = "color:#2c3e50; font-weight:bold;")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros"),
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
      h5("🟢 Leyenda de colores:"),
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

# --- Server ---
server <- function(input, output, session) {
  
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
  
  output$total_conflictos <- renderText({
    nrow(data_filtrada())
  })
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -75, lat = -9, zoom = 5)
  })
  
  observe({
    # obtener datos filtrados
    df <- data_filtrada()
    
    # si no hay datos, limpiamos y salimos
    if (nrow(df) == 0) {
      leafletProxy("mapa") %>% clearMarkers()
      return()
    }
    
    # crear columna 'color' basada en el estado (y manejar NA)
    df$color <- colores_estado[ as.character(df$Estado) ]
    df$color[is.na(df$color)] <- "black"  # color por defecto si falta el estado
    
    # actualizar mapa con los colores ya asignados
    leafletProxy("mapa", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        color = ~color,       # contorno
        fillColor = ~color,   # relleno
        fillOpacity = 0.85,
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
      )
  })
  
}

shinyApp(ui, server)
