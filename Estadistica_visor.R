
library(readxl)
library(writexl)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# Leer base de datos
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", sheet = "Estadistica")

# Convertir a sf y agregar coordenadas
Registros_SF_C <- Registros %>%
  st_as_sf(coords = c("Latitud", "Longitud"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  cbind(st_coordinates(st_centroid(.$geometry)))

# Lista de códigos
codigos <- c("CS-01-1", "CS-02-1", "CS-03-1", "CS-04-1", "CS-05-1", "CM-06", "CS-07-2", "CS-08-6",
             "CS-09-1", "CS-10-6", "CS-11-6", "CM-12", "CS-13-8", "CS-14-06", "CS-15-8", "CM-16",
             "CS-17-6", "CM-18", "CS-19-11", "CS-20-6", "CS-21-10", "CS-22-1", "CS-23-10", "CM-24",
             "CS-25-10", "CS-26-11", "CS-27-11", "CM-28", "CS-29-11", "CM-30", "CS-31-9", "CS-32-10",
             "CM-33", "CM-34", "CS-35-10", "CM-36", "CM-37", "CS-38-6", "CM-39", "CS-40-1", "CS-41-2",
             "CM-42", "CS-43-18", "CS-44-3", "CS-45-4", "CS-46-4", "CS-47-4", "CS-48-4", "CS-49_5",
             "CS-50-6", "CS-51-6", "CS-52-7", "CS-53-7", "CS-54-8", "CM-55", "CS-56-9", "CS-57-10",
             "CM-58", "CS-59-10", "CS-60-11", "CM-61", "CS-62-12", "CS-63-12", "CS-64-13", "CS-65-13",
             "CS-66-13", "CM-67", "CS-68-14", "CS-69-15", "CS-70-16", "CS-71-16", "CS-72-16", "CS-73-17",
             "CS-74-18", "CS-75-18")

# Automatizar la generación de gráficos

# for (codigo in codigos) { plan01 <- subset(Registros_SF_C, Codigo.de.campo == codigo) source("01_Base estadistico mensual.R") nombre <- paste0("plan", sprintf("%02d", which(codigos == codigo)), "_plot.png") ggsave(plot = plan01_plot, filename = file.path("PNG", nombre), units = "cm", width = 28, height = 21, dpi = 300


















