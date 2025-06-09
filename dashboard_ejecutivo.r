# Instalamos Las librerias Necesarias
install.packages("shinydashboard")
install.packages("data.table")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("plotly")
install.packages("dplyr")
install.packages("shiny")
install.packages("fmsb")

# Cargamos las librerias necesarias
library(shinydashboard)
library(data.table)
library(lubridate)
library(ggplot2)
library(leaflet)
library(plotly)
library(dplyr)
library(shiny)
library(fmsb)

# Cargar y preparar datos
dataset_path <- file.choose()
data <- fread(dataset_path)
data[, Date := ymd(Date)]
data[, Total_Calculated := Quantity * `Price per Unit`]

# Simulación de regiones
data$Region <- sample(c("Norte", "Sur", "Este", "Oeste", "Centro"), nrow(data), replace = TRUE)

# Coordenadas de cada región
regiones_coords <- data.frame(
  Region = c("Norte", "Sur", "Este", "Oeste", "Centro"),
  Lat = c(21.5, 17.5, 19.0, 19.0, 18.5),
  Lon = c(-71.0, -70.7, -68.5, -72.2, -69.9)
)

# Agregar totales por región
ventas_region <- data %>%
  group_by(Region) %>%
  summarise(Ventas = sum(`Total Amount`, na.rm = TRUE)) %>%
  left_join(regiones_coords, by = "Region")


# Set Paleta de colores
ggplot2::theme_set(
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "#f9f9f9")
    )
)

# Paleta global
paleta_color <- c("#36CE95", "#184BD3")

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
     title = tags$div(
    style = "display: flex; align-items: center; justify-content: center; height: 100%; width: 100%;",
    tags$img(
      src = "https://www.uapa.edu.do/wp-content/uploads/2022/06/Logo-azul-UAPA.png", 
      height = "30px",
      style = "margin-right: 10px;"
    )
  )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen Ejecutivo", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Ventas", tabName = "ventas", icon = icon("shopping-cart")),
      menuItem("Clientes", tabName = "clientes", icon = icon("users")),
      menuItem("Productos", tabName = "productos", icon = icon("box")),
      menuItem("Geografía", tabName = "mapas", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@300;400;500;600;700&display=swap", rel = "stylesheet"),
      tags$style(HTML("
        /* Aplicar Montserrat a todo el dashboard */
        body, .wrapper, .main-header .logo, .content-wrapper, 
        .box-title, .sidebar-menu li a, .info-box-text, 
        .info-box-number, h1, h2, h3, h4, h5, h6 {
          font-family: 'Montserrat', sans-serif !important;
        }

        /* Fondo blanco para todo el dashboard */
        body, .wrapper {
          background-color: white !important;
        }
        
        /* Fondo blanco para el content-wrapper */
        .content-wrapper, .right-side {
          background-color: white !important;
        }
        
        /* Fondo blanco para las boxes */
        .box {
          background-color: white !important;
          border-top: 3px solid #DAE0F0;
        }
        
        /* Estilo para el header */
        .main-header .logo {
          background-color: white !important;
          color: #333 !important;
          border-bottom: 1px solid #DAE0F0;
        }
        
        /* Estilo para el navbar */
        .main-header .navbar {
          background-color: white !important;
          color: #333 !important;
          border-bottom: 1px solid #DAE0F0;
        }
        
        /* Estilo para el sidebar */
        .main-sidebar {
          background-color: #ffffff !important;
          border-right: 1px solid #DAE0F0;
        }
        
        /* Estilo para los items del sidebar */
        .sidebar-menu li a {
        border-radius: 24px !important;
          color: #ACB2C4 !important;
        }
        
        .sidebar-menu li:hover a {
          font-weight: 500 !important;
          background-color: #F4F7FD !important;
        }

        .sidebar-menu li.active a {
        font-weight: 900 !important;
          background-color: #F4F7FD !important;
          color: #282B34 !important;
        }
        
        /* Centrar el título con imagen en el header */
        .main-header .logo {
          display: flex;
          align-items: center;
          justify-content: center;
        }
        
        .main-header .logo img {
          margin-right: 10px;
        }

       /* Eliminar todos los estilos por defecto del box */
        .box {
          border: none !important;
          box-shadow: none !important;
          background-color: transparent !important;
          margin-bottom: 32px !important;
          padding: 32px !important;
          border-radius: 16px !important;
          border: 1px solid #DAE0F0 !important;
        }
        
        /* Eliminar estilos del header del box */
        .box-header {
          border: none !important;
          background-color: transparent !important;
          padding: 10px 0 0 0 !important;
        }
        
        /* Estilo para el título del box */
        .box-title {
          font-family: 'Montserrat', sans-serif;
          font-weight: 500;
          color: #333;
          font-size: 18px;
          padding-left: 5px;
        }
        
        /* Eliminar bordes del body del box */
        .box-body {
          border: none !important;
          padding: 15px 0 0 0 !important;
        }

       /* Estilo para los valueBox */
        .small-box {
          border-radius: 16px;
          box-shadow: none !important;
        }
        
        /* Tamaño del texto del valor */
        .small-box .inner h3 {
          font-size: 28px !important;
          font-weight: 600;
          color:#282B34;
        }
        
        /* Tamaño del texto del subtítulo */
        .small-box .inner p {
          font-size: 14px !important;
          color: #ACB2C4;
        }
        

        .small-box .icon-large{
            border-radius: 8px;
            padding: 10px;
            font-size: 10px;
            color: #ACB2C4;
            position: absolute;
            bottom: inherit !important;
            top: 0;
            background-color: #F4F7FD !important;
        }

        .col-sm-2, .col-sm-4{
         border-right: 1px solid #DAE0F0;
        }
        .col-sm-2:last-child, .col-sm-4:last-child{
         border-right: 0px solid #DAE0F0;
        }


        /* Colores personalizados */
        .bg-aqua {
          background-color: #ffffff !important;
        }
        .bg-green {
        background-color: #ffffff !important;
        }
        .bg-yellow {
          background-color: #ffffff !important;
        }
        .bg-red {
          background-color: #ffffff !important;
        }
        .bg-light-blue {
          background-color: #ffffff !important;
        }
        .bg-blue {
          background-color: #ffffff !important;
        }

      "))
    ),
    tabItems(
      tabItem(
      tabName = "resumen", 
      h1("Hola, Smerlyn Javier"),
      p(style = "font-size: 16px; color:#ACB2C4;", "Hoy es", format(Sys.Date(), "%d %B %Y")),
      h3("Resumen Ejecutivo"),
    fluidRow(
        box(
status = "primary",
          solidHeader = FALSE,
          width = 12,
        valueBox(
          value = "89,935",
          subtitle = "Clientes Nuevos",
          icon = icon("user-plus"),
          color = "green",
          width = 2
        ),
        valueBox(
          value = "23,283",
          subtitle = "Total Conversiones",
          icon = icon("exchange-alt"),
          color = "aqua",
          width = 2
        ),
        valueBox(
          value = "124,854",
          subtitle = "Total Ventas",
          icon = icon("shopping-basket"),
          color = "green",
          width = 2
        ),
        
        valueBox(
          value = "$18.20",
          subtitle = "Ticket Promedio",
          icon = icon("receipt"),
          color = "light-blue",
          width = 2
        ),
        valueBox(
          value = "$124,854",
          subtitle = "CPC Total",
          icon = icon("money-bill-wave"),
          color = "red",
          width = 2
        ),
        valueBox(
          value = "46,827",
          subtitle = "Objetivos Alcanzados",
          icon = icon("bullseye"),
          color = "yellow",
          width = 2
        )
        ),
        box(
          title = "Evolución de Ventas en el Tiempo",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          plotlyOutput("ventas_series_plot", height = "400px")
        )
    ),
    fluidRow(
        box(
      title = "Dispersión: Edad vs Monto Total por Género",
      status = "primary",
      solidHeader = FALSE,
      width = 6,
      plotlyOutput("dispersión_edad_monto", height = "400px")
         ),
        box(
      title = "Dispersión con Regresión Lineal",
      status = "info",
      solidHeader = FALSE,
      width = 6,
      plotlyOutput("dispersión_regresión", height = "400px")
    )

      )),
      tabItem(
  tabName = "ventas",
  h2("Análisis de Ventas"),
  fluidRow(
    box(title = "Ventas Totales por Mes", width = 12, status = "primary", solidHeader = FALSE,
        plotlyOutput("ventas_por_mes", height = "300px"))
  ),
  fluidRow(
    box(title = "Top 10 Productos Más Vendidos", width = 6, status = "info", solidHeader = FALSE,
        plotlyOutput("top_productos", height = "300px")),
    box(title = "Distribución de Ventas por Categoría", width = 6, status = "warning", solidHeader = FALSE,
        plotlyOutput("pie_categorias", height = "300px"))
  ),
  fluidRow(
       box(
      title = "Campana de Gauss - Quantity",
      status = "success",
      solidHeader = FALSE,
      width = 12,
      plotlyOutput("campana_gauss_quantity", height = "400px")
     )
  )
),

      tabItem(tabName = "clientes", h2("Análisis de Clientes"),
              fluidRow(
                box(title = "Pirámide de Audiencia por Edad y Género", width = 8, plotlyOutput("audiencia_piramide")),
                box(title = "Perfil Promedio por Género", width = 4, plotOutput("audiencia_radar"))
              ),
              fluidRow(
                box(title = "Distribución de Clientes por Categoría y Género", width = 12, plotlyOutput("audiencia_barras")),
              ),
              fluidRow(
                         box(
      title = "Campana de Gauss - Distribución de Edad",
      status = "warning",
      solidHeader = FALSE,
      width = 12,
      plotlyOutput("campana_gauss", height = "400px")
    )
              )


              ),
      tabItem(
        tabName = "productos",
       h2("Productos Destacados"),
      fluidRow(
        box(
            width = 12,
    valueBoxOutput("total_productos"),
    valueBoxOutput("producto_top"),
    valueBoxOutput("producto_menos")
        )
  ),
  fluidRow(
    box(title = "Top Productos Más Vendidos", width = 6, plotlyOutput("top_productos_plot")),
    box(title = "Distribución por Categoría", width = 6, plotlyOutput("pie_productos"))
  ),
  fluidRow(
    box(title = "Distribución de Ventas por Categoría (Boxplot)", width = 12, plotlyOutput("box_productos"))
  )
      ),
      tabItem(
        tabName = "mapas", 
      h2("Visualización Geográfica"),
      fluidRow(
    box(title = "Mapa de Ventas por Región", width = 12, status = "primary", solidHeader = FALSE,
        leafletOutput("mapa_ventas", height = "400px"))
  ),
  fluidRow(
    box(title = "Mapa de Calor: Categorías vs Regiones", width = 6, status = "danger", solidHeader = FALSE,
        plotlyOutput("heatmap_region_categoria", height = "300px")),
    box(title = "Barras Apiladas por Región y Categoría", width = 6, status = "info", solidHeader = FALSE,
        plotlyOutput("barras_region_categoria", height = "300px"))
  ))
    )
  )
)

# Server
server <- function(input, output, session) {
# Series Temporales
    output$ventas_series_plot <- renderPlotly({
  ventas_por_dia <- data %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date) %>%
    summarise(ventas = sum(`Total Amount`, na.rm = TRUE))  # Usa acento grave para nombres con espacios

  p <- ggplot(ventas_por_dia, aes(x = Date, y = ventas)) +
    geom_line(color = "#2C3E50", size = 1.2) +
    geom_point(color = "#18BC9C") +
    labs(
      title = "Ventas Diarias",
      x = "Fecha",
      y = "Ventas (Total Amount)"
    ) +
    theme_minimal()

  ggplotly(p)
})
#   Gráfico de barras
  output$audiencia_barras <- renderPlotly({
    resumen <- data %>%
      group_by(`Product Category`, Gender) %>%
      summarise(clientes = n(), .groups = "drop")
    
    p <- ggplot(resumen, aes(x = `Product Category`, y = clientes, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Distribución de Clientes por Categoría y Género",
        x = "Categoría de Producto",
        y = "Cantidad de Clientes"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p)
  })
#   Gráfico de pirámide
  output$audiencia_piramide <- renderPlotly({
    data_piramide <- data %>%
      mutate(AgeGroup = cut(Age, breaks = seq(10, 70, by = 10), right = FALSE)) %>%
      group_by(AgeGroup, Gender) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Count = ifelse(Gender == "Male", -Count, Count))  # Valores negativos para hombres

    p <- ggplot(data_piramide, aes(x = AgeGroup, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Pirámide de Audiencia por Edad y Género",
        x = "Grupo de Edad",
        y = "Cantidad de Clientes"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E63"))
    
    ggplotly(p)
  })
#   Gráfico de radar
  output$audiencia_radar <- renderPlot({
    radar_data <- data %>%
      group_by(Gender) %>%
      summarise(
        avg_age = mean(Age, na.rm = TRUE),
        avg_amount = mean(`Total Amount`, na.rm = TRUE),
        avg_quantity = mean(Quantity, na.rm = TRUE)
      )

    radar_df <- as.data.frame(radar_data[, -1])
    rownames(radar_df) <- radar_data$Gender

    # Añadir límites para normalización
    radar_df <- rbind(
      max = c(70, 500, 10),
      min = c(10, 0, 1),
      radar_df
    )

    radarchart(radar_df,
      axistype = 1,
      pcol = c("#3498DB", "#E91E63"),
      pfcol = c("#3498DB80", "#E91E6380"),
      plwd = 2,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 100, 20),
      title = "Perfil Promedio por Género"
    )

    legend("topright", legend = rownames(radar_df)[-c(1, 2)], col = c("#3498DB", "#E91E63"), lty = 1, bty = "n")
  })
#  Gráfico de Dispersión (Scatter Plot)
  output$dispersión_edad_monto <- renderPlotly({
  p <- ggplot(data, aes(x = Age, y = `Total Amount`, color = Gender)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "Dispersión: Edad vs Monto Total de Compra",
      x = "Edad",
      y = "Total Amount"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Male" = "#2980B9", "Female" = "#E91E63"))

  ggplotly(p)
})
# Gráfico de Dispersión con Regresión Lineal
output$dispersión_regresión <- renderPlotly({
  p <- ggplot(data, aes(x = Age, y = `Total Amount`, color = Gender)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1) +
    labs(
      title = "Dispersión con Regresión Lineal: Edad vs Monto Total",
      x = "Edad",
      y = "Total Amount"
    ) +
    theme_minimal()

  ggplotly(p)
})
# Gráfico de Campana de Gauss
output$campana_gauss <- renderPlotly({
  gg <- ggplot(data, aes(x = Age)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "#74b9ff", alpha = 0.6, color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean(data$Age, na.rm = TRUE), 
                                           sd = sd(data$Age, na.rm = TRUE)),
                  color = "#d63031", size = 1.2, linetype = "dashed") +
    labs(
      title = "Distribución Normal de Edad (Campana de Gauss)",
      x = "Edad",
      y = "Densidad"
    ) +
    theme_minimal()

  ggplotly(gg)
})
# Gráfico de Campana de Gauss para Cantidad Comprada
output$campana_gauss_quantity <- renderPlotly({
  gg <- ggplot(data, aes(x = Quantity)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "#36CE95", alpha = 0.6, color = "black") +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean(data$Quantity, na.rm = TRUE),
        sd = sd(data$Quantity, na.rm = TRUE)
      ),
      color = "#00b894",
      size = 1.2,
      linetype = "dashed"
    ) +
    labs(
      title = "Campana de Gauss - Distribución de Cantidad Comprada",
      x = "Cantidad",
      y = "Densidad"
    ) +
    theme_minimal()

  ggplotly(gg)
})
#  Productos más vendidos (Top 10) — Gráfico de Barras Horizontal
output$top_productos <- renderPlotly({
  data %>%
    group_by(`Product Category`) %>%
    summarise(Total_Quantity = sum(Quantity, na.rm = TRUE)) %>%
    arrange(desc(Total_Quantity)) %>%
    top_n(10, Total_Quantity) %>%
    ggplot(aes(x = reorder(`Product Category`, Total_Quantity), y = Total_Quantity)) +
    geom_bar(stat = "identity", fill = "#184BD3") +
    coord_flip() +
    labs(title = "Top 10 Productos Más Vendidos", x = "Categoría", y = "Cantidad Vendida") +
    theme_minimal() -> p

  ggplotly(p)
})
# Ventas Totales por Mes — Serie Temporal
 output$ventas_por_mes <- renderPlotly({
  data %>%
    mutate(Mes = lubridate::floor_date(as.Date(Date), "month")) %>%
    group_by(Mes) %>%
    summarise(Ventas = sum(`Total Amount`, na.rm = TRUE)) %>%
    ggplot(aes(x = Mes, y = Ventas)) +
    geom_line(color = "#0984e3", size = 1.2) +
    geom_point(color = "#74b9ff", size = 2) +
    labs(title = "Ventas Totales por Mes", x = "Mes", y = "Ventas") +
    theme_minimal() -> p

  ggplotly(p)
})

# Participación de cada categoría de producto — Gráfico de Pastel
output$pie_categorias <- renderPlotly({
  data %>%
    group_by(`Product Category`) %>%
    summarise(Total_Ventas = sum(`Total Amount`, na.rm = TRUE)) %>%
    plot_ly(labels = ~`Product Category`, values = ~Total_Ventas, type = "pie") %>%
    layout(title = "Distribución de Ventas por Categoría de Producto")
})
# Distribución de Ventas por Región y Categoría
output$mapa_ventas <- renderLeaflet({
  # Crear paleta de colores basada en las ventas
  pal <- colorNumeric(palette = "Blues", domain = ventas_region$Ventas)

  leaflet(ventas_region) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~Lon,
      lat = ~Lat,
      radius = ~sqrt(Ventas) / 20,
      color = ~pal(Ventas),
      fillOpacity = 0.7,
      stroke = FALSE,
      label = ~paste0(Region, ": $", round(Ventas, 2))
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = ~Ventas,
      title = "Ventas por Región",
      opacity = 1
    )
})

# Gráfico de Calor (Heatmap de ventas por región y categoría)
output$heatmap_region_categoria <- renderPlotly({
  data %>%
    group_by(Region, `Product Category`) %>%
    summarise(Ventas = sum(`Total Amount`, na.rm = TRUE)) %>%
    ggplot(aes(x = Region, y = `Product Category`, fill = Ventas)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#ffeaa7", high = "#d63031") +
    labs(title = "Mapa de Calor: Ventas por Región y Categoría", x = "Región", y = "Categoría") +
    theme_minimal() -> p

  ggplotly(p)
})

# Gráfico de Barras Apiladas — Ventas por Categoría en cada Región
output$barras_region_categoria <- renderPlotly({
  data %>%
    group_by(Region, `Product Category`) %>%
    summarise(Ventas = sum(`Total Amount`, na.rm = TRUE)) %>%
    ggplot(aes(x = Region, y = Ventas, fill = `Product Category`)) +
    geom_bar(stat = "identity") +
    labs(title = "Ventas por Región y Categoría", x = "Región", y = "Ventas") +
    theme_minimal() -> p

  ggplotly(p)
})

# Métricas clave
output$total_productos <- renderValueBox({
  valueBox(length(unique(data$`Product Category`)), "Categorías de Productos", icon = icon("th-large"), color = "blue")
})

output$producto_top <- renderValueBox({
  top_product <- data %>%
    group_by(`Product Category`) %>%
    summarise(Ventas = sum(`Total Amount`)) %>%
    arrange(desc(Ventas)) %>%
    slice(1)
  
  valueBox(top_product$`Product Category`, "Producto Más Vendido", icon = icon("trophy"), color = "green")
})

output$producto_menos <- renderValueBox({
  least_product <- data %>%
    group_by(`Product Category`) %>%
    summarise(Ventas = sum(`Total Amount`)) %>%
    arrange(Ventas) %>%
    slice(1)
  
  valueBox(least_product$`Product Category`, "Producto Menos Vendido", icon = icon("thumbs-down"), color = "red")
})
# Barras de productos más vendidos
output$top_productos_plot <- renderPlotly({
  top_products <- data %>%
    group_by(`Product Category`) %>%
    summarise(Ventas = sum(`Total Amount`)) %>%
    arrange(desc(Ventas)) %>%
    head(10)

  ggplot(top_products, aes(x = reorder(`Product Category`, Ventas), y = Ventas)) +
    geom_col(fill = "#0073C2FF") +
    coord_flip() +
    labs(title = "Top 10 Productos Más Vendidos", x = "Categoría", y = "Ventas") +
    theme_minimal()
})
# Gráfico de pastel por % de ventas por categoría
output$pie_productos <- renderPlotly({
  ventas_cat <- data %>%
    group_by(`Product Category`) %>%
    summarise(Ventas = sum(`Total Amount`))

  plot_ly(ventas_cat, labels = ~`Product Category`, values = ~Ventas, type = 'pie') %>%
    layout(title = "Distribución de Ventas por Categoría")
})

# Boxplot por categoría vs. monto total de ventas por orden
output$box_productos <- renderPlotly({
  ggplot(data, aes(x = `Product Category`, y = `Total Amount`, fill = `Product Category`)) +
    geom_boxplot() +
    labs(title = "Distribución de Ventas por Categoría", x = "Categoría", y = "Monto de Venta") +
    theme_minimal()
})


}

# Run App
shinyApp(ui, server)
