

library(readxl)
library(writexl)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_DTB = as.numeric(plan01$FEB_DTB),
  MAR_DTB = as.numeric(plan01$MAR_DTB),
  ABR_DTB = as.numeric(plan01$ABR_DTB)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_DTB, MAR_DTB, ABR_DTB),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_DTB" = "Febrero",
                 "MAR_DTB" = "Marzo",
                 "ABR_DTB" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_DTB = ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Diámetro del tallo en la base (mm)",
       tag = "A") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)
plan01_DTB 




# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_DTMA = as.numeric(plan01$FEB_DTMA),
  MAR_DTMA = as.numeric(plan01$MAR_DTMA),
  ABR_DTMA = as.numeric(plan01$ABR_DTMA)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_DTMA, MAR_DTMA, ABR_DTMA),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_DTMA" = "Febrero",
                 "MAR_DTMA" = "Marzo",
                 "ABR_DTMA" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_DTMA= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Diámetro del tallo a media altura (mm)",
       tag = "B") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_DTMA



# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_ALT = as.numeric(plan01$FEB_ALT),
  MAR_ALT = as.numeric(plan01$MAR_ALT),
  ABR_ALT = as.numeric(plan01$ABR_ALT)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_ALT, MAR_ALT, ABR_ALT),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_ALT" = "Febrero",
                 "MAR_ALT" = "Marzo",
                 "ABR_ALT" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_ALT= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Altura total (m)",
       tag = "C") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_ALT




# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_ALT_In = as.numeric(plan01$FEB_ALT_In),
  MAR_ALT_In = as.numeric(plan01$MAR_ALT_In),
  ABR_ALT_In = as.numeric(plan01$ABR_ALT_In)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_ALT_In, MAR_ALT_In, ABR_ALT_In),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_ALT_In" = "Febrero",
                 "MAR_ALT_In" = "Marzo",
                 "ABR_ALT_In" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_ALT_In= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Altura inserción de la primera rama (m)",
       tag = "D") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_ALT_In



# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_NH = as.numeric(plan01$FEB_NH),
  MAR_NH = as.numeric(plan01$MAR_NH),
  ABR_NH = as.numeric(plan01$ABR_NH)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_NH, MAR_NH, ABR_NH),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_NH" = "Febrero",
                 "MAR_NH" = "Marzo",
                 "ABR_NH" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_EF= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Número de ramas",
       tag = "E") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_EF


# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_DC = as.numeric(plan01$FEB_DC),
  MAR_DC = as.numeric(plan01$MAR_DC),
  ABR_DC = as.numeric(plan01$ABR_DC)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_DC, MAR_DC, ABR_DC),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_DC" = "Febrero",
                 "MAR_DC" = "Marzo",
                 "ABR_DC" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_DC= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Diametro Copa (cm)",
       tag = "F") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_DC





# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_NHN = as.numeric(plan01$FEB_NHN),
  MAR_NHN = as.numeric(plan01$MAR_NHN),
  ABR_NHN = as.numeric(plan01$ABR_NHN)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_NHN, MAR_NHN, ABR_NHN),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_NHN" = "Febrero",
                 "MAR_NHN" = "Marzo",
                 "ABR_NHN" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_NHN= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Número de hojas nuevas (rebrote)",
       tag = "G") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_NHN


# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_LE = as.numeric(plan01$FEB_LE),
  MAR_LE = as.numeric(plan01$MAR_LE),
  ABR_LE = as.numeric(plan01$ABR_LE)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_LE, MAR_LE, ABR_LE),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_LE" = "Febrero",
                 "MAR_LE" = "Marzo",
                 "ABR_LE" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_LE= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Longitud de entrenudos (cm)",
       tag = "H") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_LE




# Crear un dataframe desde tu objeto plan01
df <- data.frame(
  Accesion = plan01$Accesion,
  FEB_NTS = as.numeric(plan01$FEB_NTS),
  MAR_NTS = as.numeric(plan01$MAR_NTS),
  ABR_NTS = as.numeric(plan01$ABR_NTS)
)

# Convertir a formato largo y ordenar los meses
df_long <- df %>%
  pivot_longer(cols = c(FEB_NTS, MAR_NTS, ABR_NTS),
               names_to = "Mes",
               values_to = "Valor") %>%
  mutate(
    Mes = recode(Mes,
                 "FEB_NTS" = "Febrero",
                 "MAR_NTS" = "Marzo",
                 "ABR_NTS" = "Abril"),
    Mes = factor(Mes, levels = c("Febrero", "Marzo", "Abril")) # ordenar meses
  )

# Crear gráfico de barras
plan01_NTS= ggplot(df_long, aes(x = Mes, y = Valor, group = 1)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +  # barras más anchas (más juntas)
  geom_line(color = "black", size = 1) +  # línea de crecimiento encima
  geom_point(color = "black", size = 2) +  # puntos sobre la línea
  geom_text(aes(label = sprintf("%.2f", Valor)), 
            vjust = -0.5,
            size = 3) +
  labs(x = "Mes",
       y = "Número de tallos secundarios (brotes basales)",
       tag = "I") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 8)),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    plot.tag = element_text(size =9, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )+
  ylim(0, max(df_long$Valor, na.rm = TRUE) * 1.2)

plan01_NTS



plan01_DTB
plan01_DTMA
plan01_ALT
plan01_ALT_In
plan01_EF
plan01_DC
plan01_NHN
plan01_LE
plan01_NTS


library(grid)
library(png)
library(ggimage)
# Leer la imagen PNG
Capirona_img <- readPNG("PNG/Planta.png", FALSE)

# Convertir a objeto raster para cowplot
Capirona_img <- rasterGrob(Capirona_img, 
                           x = unit(0.5, "npc"),  # centrado horizontalmente
                           y = unit(0.5, "npc"),  # centrado verticalmente
                           width = unit(1, "npc"),
                           height = unit(1, "npc"))

library(cowplot)
# Crear la figura
plan01_plot = ggdraw() +
  coord_equal(xlim = c(0, 28), ylim = c(0, 21), expand = FALSE) +
  
  draw_plot(plan01_DTB    , width = 7, height = 7,  x = 7,  y = 14) +
  draw_plot(plan01_DTMA   , width = 7, height = 7,  x = 14, y = 14) +
  draw_plot(plan01_ALT    , width = 7, height = 7,  x = 21, y = 14) +
  draw_plot(plan01_ALT_In , width = 7, height = 7,  x = 7,  y = 7) +
  draw_plot(plan01_EF     , width = 7, height = 7,  x = 14, y = 7) +
  draw_plot(plan01_DC     , width = 7, height = 7,  x = 21, y = 7) +
  draw_plot(plan01_NHN    , width = 7, height = 7,  x = 7,  y = 0) +
  draw_plot(plan01_LE     , width = 7, height = 7,  x = 14, y = 0) +
  draw_plot(plan01_NTS    , width = 7, height = 7,  x = 21, y = 0) +
  
  # Colocar la imagen en 7 de ancho y 21 de alto, desde el borde izquierdo
  draw_plot(Capirona_img, width = 7, height = 21, x = 0, y = 0) +
  
  theme(panel.background = element_rect(fill = "white"))
