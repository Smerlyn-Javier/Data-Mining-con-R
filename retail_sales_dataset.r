# =====================================
# PROYECTO RETAIL SALES DATA MINING
# Minería de Datos para Optimización de Estrategias Comerciales
# Dataset: Retail Sales Dataset (Kaggle - mohammadtalib786)
# =====================================

# Limpiar ambiente de trabajo
rm(list = ls())
gc()

# =====================================
# 1. CONFIGURACIÓN INICIAL Y LIBRERÍAS
# =====================================

# Instalar librerías si no están instaladas
required_packages <- c(
  "tidyverse", "ggplot2", "plotly", "corrplot", "lubridate",
  "cluster", "factoextra", "randomForest", "caret", "e1071",
  "arules", "arulesViz", "Rtsne", "umap", "pROC", "VIM",
  "scales", "gridExtra", "knitr", "DT", "fmsb"
)

install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
}

install_if_missing(required_packages)

# Cargar librerías
library(tidyverse)      # Manipulación de datos
library(ggplot2)        # Visualización
library(plotly)         # Gráficos interactivos
library(corrplot)       # Matriz de correlación
library(lubridate)      # Manejo de fechas
library(cluster)        # Clustering
library(factoextra)     # Visualización de clustering
library(randomForest)   # Random Forest
library(caret)          # Machine Learning
library(e1071)          # SVM
library(arules)         # Reglas de asociación
library(arulesViz)      # Visualización de reglas
library(Rtsne)          # t-SNE
library(umap)           # UMAP
library(pROC)           # Curvas ROC
library(VIM)            # Visualización de missing values
library(scales)         # Formateo de escalas
library(gridExtra)      # Organización de gráficos
library(fmsb)           # Gráficos radar

# Configuración de tema para gráficos
theme_set(theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom"))

# Configurar opciones
options(scipen = 999)  # Evitar notación científica
set.seed(123)          # Reproducibilidad

# =====================================
# 2. CARGA Y EXPLORACIÓN INICIAL DE DATOS
# =====================================

# Cargar dataset
# NOTA: Asegúrate de tener el archivo en tu directorio de trabajo
dataset_path <- file.choose()
retail_data <- read.csv(dataset_path, stringsAsFactors = FALSE)

# Exploración inicial del dataset
cat("=== INFORMACIÓN BÁSICA DEL DATASET ===\n")
print(paste("Dimensiones:", nrow(retail_data), "filas x", ncol(retail_data), "columnas"))
print("Estructura del dataset:")
str(retail_data)

print("\nPrimeras 6 filas:")
head(retail_data)

print("\nResumen estadístico:")
summary(retail_data)

# Verificar nombres de columnas
print("\nNombres de columnas:")
colnames(retail_data)

# Verificar valores faltantes
cat("\n=== ANÁLISIS DE VALORES FALTANTES ===\n")
missing_values <- sapply(retail_data, function(x) sum(is.na(x)))
print(missing_values)

# Visualizar missing values si existen
if(sum(missing_values) > 0) {
  VIM::aggr(retail_data, col = c('navyblue','red'), 
            numbers = TRUE, sortVars = TRUE)
}

# =====================================
# 3. LIMPIEZA Y PREPARACIÓN DE DATOS
# =====================================

# Crear copia para trabajar
data_clean <- retail_data

# Convertir tipos de datos apropiados
data_clean <- data_clean %>%
  mutate(
    # Convertir fecha si existe columna Date
    Date = if("Date" %in% colnames(data_clean)) as.Date(Date) else Sys.Date(),
    
    # Asegurar tipos numéricos
    Age = as.numeric(Age),
    Quantity = as.numeric(Quantity),
    Price_per_Unit = if("Price_per_Unit" %in% colnames(data_clean)) as.numeric(Price_per_Unit) else 0,
    Total_Amount = as.numeric(Total.Amount),
    
    # Factorizar variables categóricas
    Gender = as.factor(Gender),
    Product_Category = as.factor(Product.Category)
  )

# Crear variables derivadas
data_clean <- data_clean %>%
  mutate(
    # Grupos etarios
    Age_Group = case_when(
      Age < 25 ~ "Joven (18-24)",
      Age < 35 ~ "Adulto Joven (25-34)",
      Age < 50 ~ "Adulto (35-49)",
      Age < 65 ~ "Adulto Mayor (50-64)",
      TRUE ~ "Senior (65+)"
    ),
    
    # Categorías de valor de compra
    Purchase_Value_Tier = case_when(
      Total_Amount < quantile(Total_Amount, 0.33, na.rm = TRUE) ~ "Bajo",
      Total_Amount < quantile(Total_Amount, 0.67, na.rm = TRUE) ~ "Medio",
      TRUE ~ "Alto"
    ),
    
    # Variables temporales
    Month = month(Date),
    Weekday = wday(Date),
    Quarter = quarter(Date),
    
    # Revenue per item
    Revenue_per_Item = Total_Amount / pmax(Quantity, 1),
    
    # Factorizar nuevas variables categóricas
    Age_Group = factor(Age_Group, levels = c("Joven (18-24)", "Adulto Joven (25-34)", 
                                           "Adulto (35-49)", "Adulto Mayor (50-64)", "Senior (65+)")),
    Purchase_Value_Tier = factor(Purchase_Value_Tier, levels = c("Bajo", "Medio", "Alto"))
  )

# Tratamiento de outliers (usando IQR)
treat_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm)
  caps <- quantile(x, probs = c(.05, .95), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

# Aplicar tratamiento de outliers a variables numéricas
data_clean <- data_clean %>%
  mutate(
    Total_Amount = treat_outliers(Total_Amount),
    Age = treat_outliers(Age),
    Quantity = treat_outliers(Quantity)
  )

print("Dataset limpio y preparado:")
glimpse(data_clean)

# =====================================
# 4. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# =====================================

cat("\n=== ANÁLISIS EXPLORATORIO DE DATOS ===\n")

# 4.1 Análisis univariado
print("Distribución de variables categóricas:")

# Distribución por género
gender_dist <- data_clean %>%
  count(Gender) %>%
  mutate(Percentage = round(n/sum(n)*100, 1))
print(gender_dist)

# Distribución por categoría de producto
category_dist <- data_clean %>%
  count(Product_Category) %>%
  mutate(Percentage = round(n/sum(n)*100, 1)) %>%
  arrange(desc(n))
print(category_dist)

# 4.2 Visualizaciones univariadas

# Gráfico de distribución de edad
p1 <- ggplot(data_clean, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribución de Edad de Clientes",
       x = "Edad", y = "Frecuencia") +
  theme_minimal()

# Distribución de montos de venta
p2 <- ggplot(data_clean, aes(x = Total_Amount)) +
  geom_histogram(bins = 30, fill = "coral", alpha = 0.7) +
  labs(title = "Distribución de Montos de Venta",
       x = "Monto Total ($)", y = "Frecuencia") +
  scale_x_continuous(labels = dollar_format())

# Distribución por género
p3 <- ggplot(data_clean, aes(x = Gender, fill = Gender)) +
  geom_bar(alpha = 0.8) +
  labs(title = "Distribución por Género",
       x = "Género", y = "Frecuencia") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  guides(fill = FALSE)

# Distribución por categoría de producto
p4 <- ggplot(data_clean, aes(x = reorder(Product_Category, -table(Product_Category)[Product_Category]))) +
  geom_bar(fill = "lightgreen", alpha = 0.8) +
  labs(title = "Distribución por Categoría de Producto",
       x = "Categoría", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar gráficos
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# 4.3 Análisis bivariado

# Ventas por género
p5 <- ggplot(data_clean, aes(x = Gender, y = Total_Amount, fill = Gender)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Distribución de Ventas por Género",
       x = "Género", y = "Monto Total ($)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  guides(fill = FALSE)

# Ventas por grupo etario
p6 <- ggplot(data_clean, aes(x = Age_Group, y = Total_Amount, fill = Age_Group)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Distribución de Ventas por Grupo Etario",
       x = "Grupo Etario", y = "Monto Total ($)") +
  scale_y_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

# Ventas por categoría
p7 <- ggplot(data_clean, aes(x = reorder(Product_Category, -Total_Amount, FUN = median), 
                            y = Total_Amount, fill = Product_Category)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Distribución de Ventas por Categoría",
       x = "Categoría", y = "Monto Total ($)") +
  scale_y_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

# Relación Edad vs Monto
p8 <- ggplot(data_clean, aes(x = Age, y = Total_Amount, color = Gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre Edad y Monto de Compra",
       x = "Edad", y = "Monto Total ($)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_brewer(type = "qual", palette = "Set1")

grid.arrange(p5, p6, p7, p8, ncol = 2, nrow = 2)

# 4.4 Análisis de correlación
numeric_vars <- data_clean %>%
  select_if(is.numeric) %>%
  select(-matches("ID|Transaction"))

correlation_matrix <- cor(numeric_vars, use = "complete.obs")

# Visualizar matriz de correlación
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")
title("Matriz de Correlación - Variables Numéricas")

# =====================================
# 5. ANÁLISIS RFM Y SEGMENTACIÓN DE CLIENTES
# =====================================

cat("\n=== ANÁLISIS RFM Y SEGMENTACIÓN ===\n")

# Calcular métricas RFM por cliente
rfm_data <- data_clean %>%
  group_by(Customer_ID) %>%
  summarise(
    # Recency: días desde la última compra
    Recency = as.numeric(max(Date, na.rm = TRUE) - max(Date, na.rm = TRUE)),
    
    # Frequency: número de transacciones
    Frequency = n(),
    
    # Monetary: valor total gastado
    Monetary = sum(Total_Amount, na.rm = TRUE),
    
    # Variables adicionales
    Avg_Purchase = mean(Total_Amount, na.rm = TRUE),
    Avg_Quantity = mean(Quantity, na.rm = TRUE),
    Favorite_Category = names(sort(table(Product_Category), decreasing = TRUE))[1],
    Gender = first(Gender),
    Avg_Age = mean(Age, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Crear scores RFM (1-5, donde 5 es mejor)
  mutate(
    R_Score = ntile(-Recency, 5),  # Negativo porque menor recency es mejor
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5),
    RFM_Score = paste0(R_Score, F_Score, M_Score),
    
    # Valor total del cliente
    Total_Score = R_Score + F_Score + M_Score
  )

# Estadísticas RFM
print("Estadísticas RFM:")
summary(rfm_data[, c("Recency", "Frequency", "Monetary")])

# Segmentación de clientes basada en RFM
rfm_data <- rfm_data %>%
  mutate(
    Customer_Segment = case_when(
      Total_Score >= 12 ~ "Campeones",
      Total_Score >= 9 & F_Score >= 3 ~ "Clientes Leales",
      Total_Score >= 8 & R_Score >= 3 ~ "Potenciales",
      Total_Score >= 6 ~ "Nuevos Clientes",
      R_Score <= 2 ~ "En Riesgo",
      TRUE ~ "Requiere Atención"
    )
  )

# Distribución de segmentos
segment_dist <- rfm_data %>%
  count(Customer_Segment) %>%
  mutate(Percentage = round(n/sum(n)*100, 1)) %>%
  arrange(desc(n))

print("Distribución de segmentos de clientes:")
print(segment_dist)

# Visualización de segmentos RFM
p_rfm1 <- ggplot(segment_dist, aes(x = reorder(Customer_Segment, n), y = n, fill = Customer_Segment)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(title = "Distribución de Segmentos de Clientes",
       x = "Segmento", y = "Número de Clientes") +
  guides(fill = FALSE)

# Análisis de valor por segmento
segment_value <- rfm_data %>%
  group_by(Customer_Segment) %>%
  summarise(
    Count = n(),
    Avg_Monetary = mean(Monetary),
    Avg_Frequency = mean(Frequency),
    Total_Value = sum(Monetary),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Value))

print("Valor por segmento:")
print(segment_value)

# Gráfico de valor por segmento
p_rfm2 <- ggplot(segment_value, aes(x = reorder(Customer_Segment, Total_Value), 
                                   y = Total_Value, fill = Customer_Segment)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(title = "Valor Total por Segmento",
       x = "Segmento", y = "Valor Total ($)") +
  scale_y_continuous(labels = dollar_format()) +
  guides(fill = FALSE)

grid.arrange(p_rfm1, p_rfm2, ncol = 2)

# =====================================
# 6. CLUSTERING K-MEANS
# =====================================

cat("\n=== CLUSTERING K-MEANS ===\n")

# Preparar datos para clustering
clustering_data <- rfm_data %>%
  select(Recency, Frequency, Monetary) %>%
  scale() %>%
  as.data.frame()

# Determinar número óptimo de clusters
# Método del codo
wss <- sapply(1:10, function(k) {
  kmeans(clustering_data, k, nstart = 25)$tot.withinss
})

# Gráfico del método del codo
elbow_data <- data.frame(k = 1:10, wss = wss)
p_elbow <- ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Método del Codo para Determinar K Óptimo",
       x = "Número de Clusters (k)", y = "Within-cluster Sum of Squares") +
  scale_x_continuous(breaks = 1:10)

print(p_elbow)

# Aplicar K-means con k=4
set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 4, nstart = 25)

# Añadir clusters a los datos
rfm_data$Cluster <- as.factor(kmeans_result$cluster)

# Análisis de clusters
cluster_analysis <- rfm_data %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Avg_Age = mean(Avg_Age, na.rm = TRUE),
    .groups = 'drop'
  )

print("Análisis de clusters:")
print(cluster_analysis)

# Visualización de clusters
p_cluster1 <- ggplot(rfm_data, aes(x = Frequency, y = Monetary, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clusters de Clientes: Frecuencia vs Valor Monetario",
       x = "Frecuencia de Compras", y = "Valor Monetario ($)") +
  scale_y_continuous(labels = dollar_format()) +
  theme(legend.position = "bottom")

print(p_cluster1)

# =====================================
# 7. REDUCCIÓN DE DIMENSIONALIDAD
# =====================================

cat("\n=== REDUCCIÓN DE DIMENSIONALIDAD ===\n")

# 7.1 PCA (Principal Component Analysis)
pca_data <- clustering_data
pca_result <- prcomp(pca_data, scale = TRUE)

# Resumen del PCA
print("Resumen del PCA:")
summary(pca_result)

# Visualización del PCA
fviz_pca_biplot(pca_result, 
                geom.ind = "point",
                col.ind = rfm_data$Cluster,
                addEllipses = TRUE,
                title = "PCA - Segmentación de Clientes")

# Gráfico de varianza explicada por componente
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 80),
         title = "Varianza Explicada por Componente Principal")

# 7.2 t-SNE
set.seed(123)
tsne_result <- Rtsne(pca_data, dims = 2, perplexity = 30, verbose = FALSE)

tsne_df <- data.frame(
  X = tsne_result$Y[,1],
  Y = tsne_result$Y[,2],
  Cluster = rfm_data$Cluster
)

p_tsne <- ggplot(tsne_df, aes(x = X, y = Y, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "t-SNE: Visualización de Clusters de Clientes",
       x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
  theme(legend.position = "bottom")

print(p_tsne)

# 7.3 UMAP
set.seed(123)
umap_result <- umap(pca_data)

umap_df <- data.frame(
  X = umap_result$layout[,1],
  Y = umap_result$layout[,2],
  Cluster = rfm_data$Cluster
)

p_umap <- ggplot(umap_df, aes(x = X, y = Y, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "UMAP: Visualización de Clusters de Clientes",
       x = "UMAP Dimension 1", y = "UMAP Dimension 2") +
  theme(legend.position = "bottom")

print(p_umap)

# Comparación de las tres técnicas
grid.arrange(
  fviz_pca_ind(pca_result, col.ind = rfm_data$Cluster, addEllipses = TRUE, 
               title = "PCA", legend.title = "Cluster") + theme(legend.position = "none"),
  p_tsne + theme(legend.position = "none"),
  p_umap + theme(legend.position = "none"),
  ncol = 3
)

# =====================================
# 8. MODELADO PREDICTIVO
# =====================================

cat("\n=== MODELADO PREDICTIVO ===\n")

# 8.1 Preparación de datos para modelado
# Crear variable objetivo: Cliente de alto valor (top 25% en gastos)
modeling_data <- data_clean %>%
  group_by(Customer_ID) %>%
  summarise(
    Total_Spent = sum(Total_Amount),
    Avg_Purchase = mean(Total_Amount),
    Purchase_Frequency = n(),
    Avg_Age = mean(Age, na.rm = TRUE),
    Gender = first(Gender),
    Favorite_Category = names(sort(table(Product_Category), decreasing = TRUE))[1],
    Avg_Quantity = mean(Quantity),
    .groups = 'drop'
  ) %>%
  mutate(
    High_Value_Customer = ifelse(Total_Spent > quantile(Total_Spent, 0.75), 1, 0),
    High_Value_Customer = as.factor(High_Value_Customer)
  ) %>%
  filter(!is.na(Avg_Age), !is.na(Gender))

# Codificar variables categóricas
modeling_data$Gender_Numeric <- as.numeric(as.factor(modeling_data$Gender))
modeling_data$Category_Numeric <- as.numeric(as.factor(modeling_data$Favorite_Category))

# Seleccionar variables para el modelo
model_vars <- modeling_data %>%
  select(High_Value_Customer, Avg_Purchase, Purchase_Frequency, Avg_Age, 
         Gender_Numeric, Category_Numeric, Avg_Quantity)

# División entrenamiento/prueba
set.seed(123)
train_index <- createDataPartition(model_vars$High_Value_Customer, p = 0.8, list = FALSE)
train_data <- model_vars[train_index, ]
test_data <- model_vars[-train_index, ]

print(paste("Datos de entrenamiento:", nrow(train_data)))
print(paste("Datos de prueba:", nrow(test_data)))

# 8.2 Random Forest
cat("\n--- Random Forest ---\n")
set.seed(123)
rf_model <- randomForest(High_Value_Customer ~ ., 
                        data = train_data, 
                        ntree = 500,
                        importance = TRUE)

# Predicciones
rf_pred <- predict(rf_model, test_data)
rf_prob <- predict(rf_model, test_data, type = "prob")[,2]

# Evaluación Random Forest
rf_confusion <- confusionMatrix(rf_pred, test_data$High_Value_Customer)
print("Matriz de Confusión - Random Forest:")
print(rf_confusion)

# Importancia de variables
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  Importance = importance(rf_model)[,1]
) %>%
  arrange(desc(Importance))

p_importance <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Importancia de Variables - Random Forest",
       x = "Variable", y = "Importancia")

print(p_importance)

# 8.3 SVM
cat("\n--- Support Vector Machine ---\n")
set.seed(123)
svm_model <- svm(High_Value_Customer ~ ., 
                data = train_data, 
                kernel = "radial",
                probability = TRUE)

# Predicciones SVM
svm_pred <- predict(svm_model, test_data)
svm_prob <- attr(predict(svm_model, test_data, probability = TRUE), "probabilities")[,2]

# Evaluación SVM
svm_confusion <- confusionMatrix(svm_pred, test_data$High_Value_Customer)
print("Matriz de Confusión - SVM:")
print(svm_confusion)

# 8.4 Regresión Logística
cat("\n--- Regresión Logística ---\n")
glm_model <- glm(High_Value_Customer ~ ., 
                 data = train_data, 
                 family = binomial)

# Predicciones Regresión Logística
glm_prob <- predict(glm_model, test_data, type = "response")
glm_pred <- as.factor(ifelse(glm_prob > 0.5, 1, 0))

# Evaluación Regresión Logística
glm_confusion <- confusionMatrix(glm_pred, test_data$High_Value_Customer)
print("Matriz de Confusión - Regresión Logística:")
print(glm_confusion)

# 8.5 Comparación de modelos
model_comparison <- data.frame(
  Model = c("Random Forest", "SVM", "Regresión Logística"),
  Accuracy = c(rf_confusion$overall['Accuracy'],
               svm_confusion$overall['Accuracy'],
               glm_confusion$overall['Accuracy']),
  Sensitivity = c(rf_confusion$byClass['Sensitivity'],
                  svm_confusion$byClass['Sensitivity'],
                  glm_confusion$byClass['Sensitivity']),
  Specificity = c(rf_confusion$byClass['Specificity'],
                  svm_confusion$byClass['Specificity'],
                  glm_confusion$byClass['Specificity'])
)

print("Comparación de Modelos:")
print(model_comparison)

# Curvas ROC
roc_rf <- roc(test_data$High_Value_Customer, rf_prob)
roc_svm <- roc(test_data$High_Value_Customer, svm_prob)
roc_glm <- roc(test_data$High_Value_Customer, glm_prob)

# Gráfico de curvas ROC
plot(roc_rf, col = "red", main = "Comparación de Curvas ROC")
plot(roc_svm, col = "blue", add = TRUE)
plot(roc_glm, col = "green", add = TRUE)
legend("bottomright", legend = c(paste("RF (AUC =", round(auc(roc_rf), 3), ")"),
                                paste("SVM (AUC =", round(auc(roc_svm), 3), ")"),
                                paste("GLM (AUC =", round(auc(roc_glm), 3), ")")),
       col = c("red", "blue", "green"), lwd = 2)

