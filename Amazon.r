# Cargar librerías
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)

# Leer datos
dataset_path <- file.choose()
df <- fread(dataset_path)

# Convertir calificación a variable categórica (buena = >=4)
df <- df %>% mutate(
  rating_label = ifelse(rating >= 4, "bueno", "malo"),
  rating_label = as.factor(rating_label),
  category = as.factor(category)
)

# Dividir en entrenamiento y prueba
set.seed(123)
index <- createDataPartition(df$rating_label, p = 0.7, list = FALSE)
train <- df[index, ]
test <- df[-index, ]

# Árbol de decisión
modelo_arbol <- rpart(rating_label ~ discounted_price + actual_price + discount_percentage + category, data = train, method = "class")
rpart.plot(modelo_arbol)

# Predicción
pred_arbol <- predict(modelo_arbol, test, type = "class")
confusionMatrix(pred_arbol, test$rating_label)

# Random Forest
modelo_rf <- randomForest(rating_label ~ discounted_price + actual_price + discount_percentage + category, data = train, importance = TRUE)
print(modelo_rf)
varImpPlot(modelo_rf)

# Predicción RF
pred_rf <- predict(modelo_rf, test)
confusionMatrix(pred_rf, test$rating_label)
