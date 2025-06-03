
# Cargar librerías y datos:
# Librerías
library(tidyverse)
library(cluster)
library(arules)
library(arulesViz)
library(factoextra)
library(data.table)
library(lubridate)


# Cargar el Dataset
dataset_path <- file.choose()
df <- fread(dataset_path)
selected_df <- df[, c("InvoiceNo", "StockCode", "Description", "Quantity", "InvoiceDate", "UnitPrice", "CustomerID", "Country")]



# Eliminar NA y devoluciones
df_clean <- na.omit(selected_df)
df_clean <- df_clean[df_clean$Quantity > 0 & df_clean$UnitPrice > 0, ]

# Convertir la columna de fecha
df_clean$InvoiceDate <- as.POSIXct(df_clean$InvoiceDate, format="%Y-%m-%d %H:%M:%S")
df_clean$Month <- format(df_clean$InvoiceDate, "%m")


# Transformación
df_clean$TotalPrice <- df_clean$Quantity * df_clean$UnitPrice
library(dplyr)
customer_data <- df_clean %>%
  group_by(CustomerID) %>%
  summarise(TotalSpent = sum(TotalPrice),
            NumPurchases = n_distinct(InvoiceNo),
            AvgPurchase = mean(TotalPrice))


# Minería de datos
customer_scaled <- scale(customer_data[, -1])
set.seed(123)
kmeans_model <- kmeans(customer_scaled, centers = 4)
customer_data$Segment <- kmeans_model$cluster

# Visualización
library(ggplot2)
ggplot(customer_data, aes(x=TotalSpent, y=NumPurchases, color=factor(Segment))) +
  geom_point() +
  labs(title="Segmentación de Clientes por K-Means")
