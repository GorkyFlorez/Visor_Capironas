

###  DATA BASE ÑUÑA: D:\INIA_PROAGROBIO_2025 
ata <- read.delim("clipboard")


# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

# Reorganizar tus datos en formato largo
ata_long <- ata %>%
  pivot_longer(cols = c(Feb, Mar, Abr), names_to = "Mes", values_to = "Longitud")

# Agrupar por especie y mes para calcular promedio y desviación estándar
ata_summary <- ata_long %>%
  group_by(Especie, Mes) %>%
  summarise(
    media = mean(Longitud, na.rm = TRUE),
    sd = sd(Longitud, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n)
  )

# Crear el gráfico
ggplot(ata_summary, aes(x = Mes, y = media, color = Especie, group = Especie)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Crecimiento del tallo de dos especies de capirona",
    x = "Mes",
    y = "Diámetro del tallo a ras de suelo (mm)",
    color = "Especie de capirona",
    fill = "Especie"
  ) +
  theme_minimal()





# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

# Reorganizar los datos a formato largo
ata_long <- ata %>%
  pivot_longer(cols = c(Feb, Mar, Abr), names_to = "Mes", values_to = "Longitud")

# Agrupar por Accesion y Mes para obtener resumen estadístico
ata_summary_acc <- ata_long %>%
  group_by(Accesion, Mes) %>%
  summarise(
    media = mean(Longitud, na.rm = TRUE),
    sd = sd(Longitud, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Crear el gráfico
ggplot(ata_summary_acc, aes(x = Mes, y = media, color = Accesion, group = Accesion)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Crecimiento del tallo por accesion en meses",
    x = "Mes",
    y = "Diámetro del tallo a ras de suelo (mm)",
    color = "Codigo de accesion",
    fill = "Accesión"
  ) +
  theme_minimal()


