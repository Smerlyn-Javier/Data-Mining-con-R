install.packages(c("tidyverse", "readr", "ggplot2", "leaflet", "dplyr"))

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
dhl_data <- fread(dataset_path)

# Mostrar los primeros registros
head(dhl_data)


# Ver resumen de columnas
str(dhl_data)

# Ver nombres de columnas
colnames(dhl_data)


library(dplyr)

# Renombramos columnas clave para facilitar el análisis
dhl_clean <- dhl_data %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE), !is.na(NAME), !is.na(STATE)) %>%
  rename(
    Facility_Name = NAME,
    Latitude = LATITUDE,
    Longitude = LONGITUDE,
    State = STATE,
    City = CITY,
    Address = ADDRESS,
    Location_Type = LOCATION_TY
  )

# Eliminar duplicados
dhl_clean <- dhl_clean %>%
  distinct()

# Revisar estructura limpia
summary(dhl_clean)

library(dplyr)

top_states <- dhl_clean %>%
  group_by(State) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  head(10)

print(top_states)


library(ggplot2)

ggplot(top_states, aes(x = reorder(State, -Total), y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Estados con más instalaciones DHL",
       x = "Estado",
       y = "Cantidad de instalaciones") +
  theme_minimal()



library(leaflet)

leaflet(data = dhl_clean) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,
    popup = ~paste0("<b>", Facility_Name, "</b><br>",
                    City, ", ", State, "<br>",
                    "Tipo: ", Location_Type),
    radius = 3,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7
  ) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -95, lat = 37, zoom = 4)


library(dplyr)

facility_types <- dhl_clean %>%
  group_by(Location_Type) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

library(ggplot2)

ggplot(facility_types, aes(x = reorder(Location_Type, Total), y = Total)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Distribución por tipo de instalación DHL",
    x = "Tipo de instalación",
    y = "Cantidad"
  ) +
  theme_minimal()

