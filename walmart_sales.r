# Cargar librerías y datos:
# Librerías
library(tidyverse)
library(cluster)
library(arules)
library(arulesViz)
library(factoextra)
library(data.table)
library(lubridate)

# Cargar el dataset
dataset_path <- file.choose()
data <- fread(dataset_path)
head(data)
str(data)


# Preprocesamiento:``
# Convertir fecha
data$Date <- dmy(data$Date)

# Agrupar por tienda para análisis de ventas promedio y tendencias
sales_summary <- data %>%
  group_by(Store) %>%
  summarise(
    avg_sales = mean(Weekly_Sales),
    avg_temp = mean(Temperature),
    avg_fuel = mean(Fuel_Price),
    avg_cpi = mean(CPI),
    avg_unemp = mean(Unemployment)
  )

# Clustering con K-Means
# Escalar los datos
scaled_data <- scale(sales_summary[, -1])  # Excluye la columna 'Store'

# Determinar número óptimo de clusters (opcional)
fviz_nbclust(scaled_data, kmeans, method = "wss")

# Aplicar K-means (puedes ajustar k según el gráfico anterior)
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

# Añadir los clusters al resumen
sales_summary$cluster <- kmeans_result$cluster

# Visualizar clusters
fviz_cluster(list(data = scaled_data, cluster = kmeans_result$cluster), 
             geom = "point", ellipse.type = "norm", main = "Cluster por tienda")
