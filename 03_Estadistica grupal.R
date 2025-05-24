# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

library(readxl)
library(writexl)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")

datos = Registros
# Convertir nombres de columnas si es necesario
colnames(datos)[colnames(datos) %in% c("FEB_DTB", "MAR_DTB", "ABR_DTB")] <- c("Feb", "Mar", "Abr")

# Convertir columnas de diámetro a numérico y redondear
datos <- datos %>%
  mutate(
    Feb = round(as.numeric(Feb), 2),
    Mar = round(as.numeric(Mar), 2),
    Abr = round(as.numeric(Abr), 2)
  )

# Reorganizar a formato largo
datos_long <- datos %>%
  pivot_longer(cols = c(Feb, Mar, Abr), 
               names_to = "Mes", 
               values_to = "Diametro")

# Calcular promedio y error estándar
resumen <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(
    media = mean(Diametro, na.rm = TRUE),
    sd = sd(Diametro, na.rm = TRUE),
    n = sum(!is.na(Diametro)),
    se = sd / sqrt(n)
  )

# Reordenar los niveles del factor Mes
resumen$Mes <- factor(resumen$Mes, levels = c("Feb", "Mar", "Abr"))



library(RColorBrewer)

# Paleta de colores extendida
colores <- colorRampPalette(brewer.pal(8, "Set3"))(length(unique(datos_long$Accesion)))


# Graficar
AA = ggplot(resumen, aes(x = Mes, y = media, color = Especie, group = Especie)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
  labs(
    title = "",
    x = "Mes",
    y = "Diámetro del tallo en la base (mm)",
    color = "Especie de capirona"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),     # quitar grillas mayores
    panel.grid.minor = element_blank(),     # quitar grillas menores
    
    axis.line = element_line(color = "black"), # eje en negro
    axis.title = element_text(size = 12, face = "bold"), # títulos en negrita
    axis.text = element_text(size = 10, color = "black"), # textos del eje
    legend.position = "bottom",                # leyenda arriba
    legend.title = element_blank(),         # sin título de leyenda
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # título centrado
    strip.text = element_text(size = 12, face = "bold") # texto de facetas
  )
AA



# Asegurar el orden cronológico de los meses
datos_long$Mes <- factor(datos_long$Mes, levels = c("Feb", "Mar", "Abr"))

# Calcular medias por especie y mes para la línea
medias <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(media = mean(Diametro, na.rm = TRUE), .groups = "drop")

# Crear boxplot con línea de medias
A <- ggplot(datos_long, aes(x = Mes, y = Diametro, fill = Especie)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.7, width = 0.6) +
  geom_line(data = medias, aes(x = Mes, y = media, group = Especie, color = Especie),
            size = 1.2, position = position_dodge(width = 0.6)) +
  geom_point(data = medias, aes(x = Mes, y = media, color = Especie),
             size = 3, position = position_dodge(width = 0.6)) +
  labs(
    title = "Tendencia del diámetro del tallo en la base",
    x = "Mes",
    y = "Diámetro del tallo en la base (mm)",
    fill = "Especie de capirona",
    color = "Especie de capirona",
    tag = "A"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

A


Registros
CM = subset(Registros, Especie== "Capirona macrophylla" )
CS = subset(Registros, Especie== "Calycophyllum spruceanum" )



library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CM %>%
  mutate(
    FEB_DTB = as.numeric(FEB_DTB),
    MAR_DTB = as.numeric(MAR_DTB),
    ABR_DTB = as.numeric(ABR_DTB)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_DTB, MAR_DTB, ABR_DTB),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_DTB", "MAR_DTB", "ABR_DTB"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
B <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Capirona macrophylla",
    x = "Accesión",
    y = "Diámetro del tallo en la base (mm)",
    fill = "Accesión",
    tag = "B"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

B









library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CS %>%
  mutate(
    FEB_DTB = as.numeric(FEB_DTB),
    MAR_DTB = as.numeric(MAR_DTB),
    ABR_DTB = as.numeric(ABR_DTB)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_DTB, MAR_DTB, ABR_DTB),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_DTB", "MAR_DTB", "ABR_DTB"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
C <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x", ncol=1) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Diámetro del tallo en la base (mm)",
    fill = "Accesión",
    tag = "C"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

C




# Agregar columna 'Tipo_propagacion' al dataframe largo
CS$Tipo_propagacion <- factor(CS$`Tipo de propagacion`)  # Asegurar que es factor

# Convertir a formato largo incluyendo tipo de propagación
df_long <- CS %>%
  mutate(
    FEB_DTB = as.numeric(FEB_DTB),
    MAR_DTB = as.numeric(MAR_DTB),
    ABR_DTB = as.numeric(ABR_DTB)
  ) %>%
  pivot_longer(
    cols = c(FEB_DTB, MAR_DTB, ABR_DTB),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro)) %>%
  mutate(Mes = factor(Mes,
                      levels = c("FEB_DTB", "MAR_DTB", "ABR_DTB"),
                      labels = c("Febrero", "Marzo", "Abril")))

# Calcular promedios por accesión, mes y tipo de propagación
df_promedios <- df_long %>%
  group_by(Accesion, Mes, Tipo_propagacion) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes, Tipo_propagacion) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%
  ungroup()

# Paleta de colores extendida
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
D <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.3, fontface = "bold") +
  facet_grid(Mes ~ Tipo_propagacion, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Diámetro del tallo en la base (mm)",
    fill = "Accesión",
    tag = "D"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.3)


D


library(cowplot)
# Crear la figura
DTB = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 20), expand = FALSE) +
  
  draw_plot(C  , width = 14, height = 12,  x = 0,  y = 8) +
  draw_plot(B  , width = 14, height = 8,  x = 0, y = 0) +
  
  draw_plot(A  , width = 16, height = 8,  x = 14, y = 12) +
  draw_plot(D  , width = 16, height = 12,  x = 14,  y = 0) +
  
  
  # Colocar la imagen en 7 de ancho y 21 de alto, desde el borde izquierdo
  
  
  theme(panel.background = element_rect(fill = "white"))

DTB


ggsave(plot=DTB ,"PNG/Abril/DTB.png",units = "cm",width = 30, #alto
       height = 20, #ancho
       dpi=600) 









# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

library(readxl)
library(writexl)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")

datos = Registros
# Convertir nombres de columnas si es necesario
colnames(datos)[colnames(datos) %in% c("FEB_DTMA", "MAR_DTMA", "ABR_DTMA")] <- c("Feb", "Mar", "Abr")

# Convertir columnas de diámetro a numérico y redondear
datos <- datos %>%
  mutate(
    Feb = round(as.numeric(Feb), 2),
    Mar = round(as.numeric(Mar), 2),
    Abr = round(as.numeric(Abr), 2)
  )

# Reorganizar a formato largo
datos_long <- datos %>%
  pivot_longer(cols = c(Feb, Mar, Abr), 
               names_to = "Mes", 
               values_to = "Diametro")

# Calcular promedio y error estándar
resumen <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(
    media = mean(Diametro, na.rm = TRUE),
    sd = sd(Diametro, na.rm = TRUE),
    n = sum(!is.na(Diametro)),
    se = sd / sqrt(n)
  )

# Reordenar los niveles del factor Mes
resumen$Mes <- factor(resumen$Mes, levels = c("Feb", "Mar", "Abr"))



library(RColorBrewer)

# Paleta de colores extendida
colores <- colorRampPalette(brewer.pal(8, "Set3"))(length(unique(datos_long$Accesion)))

# Graficar
AA = ggplot(resumen, aes(x = Mes, y = media, color = Especie, group = Especie)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
  labs(
    title = "",
    x = "Mes",
    y = "Diámetro del tallo en la base (mm)",
    color = "Especie de capirona"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),     # quitar grillas mayores
    panel.grid.minor = element_blank(),     # quitar grillas menores
    
    axis.line = element_line(color = "black"), # eje en negro
    axis.title = element_text(size = 12, face = "bold"), # títulos en negrita
    axis.text = element_text(size = 10, color = "black"), # textos del eje
    legend.position = "bottom",                # leyenda arriba
    legend.title = element_blank(),         # sin título de leyenda
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # título centrado
    strip.text = element_text(size = 12, face = "bold") # texto de facetas
  )
AA


# Asegurar el orden cronológico de los meses
datos_long$Mes <- factor(datos_long$Mes, levels = c("Feb", "Mar", "Abr"))

# Calcular medias por especie y mes para la línea
medias <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(media = mean(Diametro, na.rm = TRUE), .groups = "drop")

# Crear boxplot con línea de medias
A <- ggplot(datos_long, aes(x = Mes, y = Diametro, fill = Especie)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.7, width = 0.6) +
  geom_line(data = medias, aes(x = Mes, y = media, group = Especie, color = Especie),
            size = 1.2, position = position_dodge(width = 0.6)) +
  geom_point(data = medias, aes(x = Mes, y = media, color = Especie),
             size = 3, position = position_dodge(width = 0.6)) +
  labs(
    title = "Tendencia del Diámetro del tallo a media altura",
    x = "Mes",
    y = "Diámetro del tallo a media altura (mm)",
    fill = "Especie de capirona",
    color = "Especie de capirona",
    tag = "A"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

A


Registros
CM = subset(Registros, Especie== "Capirona macrophylla" )
CS = subset(Registros, Especie== "Calycophyllum spruceanum" )


library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CM %>%
  mutate(
    FEB_DTMA = as.numeric(FEB_DTMA),
    MAR_DTMA = as.numeric(MAR_DTMA),
    ABR_DTMA = as.numeric(ABR_DTMA)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_DTMA, MAR_DTMA, ABR_DTMA),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_DTMA", "MAR_DTMA", "ABR_DTMA"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
B <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Capirona macrophylla",
    x = "Accesión",
    y = "Diámetro del tallo a media altura (mm)",
    fill = "Accesión",
    tag = "B"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

B




library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CS %>%
  mutate(
    FEB_DTMA = as.numeric(FEB_DTMA),
    MAR_DTMA = as.numeric(MAR_DTMA),
    ABR_DTMA = as.numeric(ABR_DTMA)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_DTMA, MAR_DTMA, ABR_DTMA),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_DTMA", "MAR_DTMA", "ABR_DTMA"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
C <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x", ncol=1) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Diámetro del tallo a media altura (mm)",
    fill = "Accesión",
    tag = "C"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

C




# Agregar columna 'Tipo_propagacion' al dataframe largo
CS$Tipo_propagacion <- factor(CS$`Tipo de propagacion`)  # Asegurar que es factor

# Convertir a formato largo incluyendo tipo de propagación
df_long <- CS %>%
  mutate(
    FEB_DTMA = as.numeric(FEB_DTMA),
    MAR_DTMA = as.numeric(MAR_DTMA),
    ABR_DTMA = as.numeric(ABR_DTMA)
  ) %>%
  pivot_longer(
    cols = c(FEB_DTMA, MAR_DTMA, ABR_DTMA),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro)) %>%
  mutate(Mes = factor(Mes,
                      levels = c("FEB_DTMA", "MAR_DTMA", "ABR_DTMA"),
                      labels = c("Febrero", "Marzo", "Abril")))

# Calcular promedios por accesión, mes y tipo de propagación
df_promedios <- df_long %>%
  group_by(Accesion, Mes, Tipo_propagacion) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes, Tipo_propagacion) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%
  ungroup()

# Paleta de colores extendida
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
D <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = 0.5, size = 2.3, fontface = "bold", angle= 90) +
  facet_grid(Mes ~ Tipo_propagacion, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Diámetro del tallo a media altura (mm)",
    fill = "Accesión",
    tag = "D"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.3)


D


library(cowplot)
# Crear la figura
DTMA = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 20), expand = FALSE) +
  
  draw_plot(C  , width = 14, height = 12,  x = 0,  y = 8) +
  draw_plot(B  , width = 14, height = 8,  x = 0, y = 0) +
  
  draw_plot(A  , width = 16, height = 8,  x = 14, y = 12) +
  draw_plot(D  , width = 16, height = 12,  x = 14,  y = 0) +
  
  
  # Colocar la imagen en 7 de ancho y 21 de alto, desde el borde izquierdo
  
  
  theme(panel.background = element_rect(fill = "white"))

DTMA


ggsave(plot=DTMA ,"PNG/Abril/DTMA.png",units = "cm",width = 30, #alto
       height = 20, #ancho
       dpi=600) 











# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

library(readxl)
library(writexl)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")

datos = Registros
# Convertir nombres de columnas si es necesario
colnames(datos)[colnames(datos) %in% c("FEB_ALT", "MAR_ALT", "ABR_ALT")] <- c("Feb", "Mar", "Abr")

# Convertir columnas de diámetro a numérico y redondear
datos <- datos %>%
  mutate(
    Feb = round(as.numeric(Feb), 2),
    Mar = round(as.numeric(Mar), 2),
    Abr = round(as.numeric(Abr), 2)
  )

# Reorganizar a formato largo
datos_long <- datos %>%
  pivot_longer(cols = c(Feb, Mar, Abr), 
               names_to = "Mes", 
               values_to = "Diametro")

# Calcular promedio y error estándar
resumen <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(
    media = mean(Diametro, na.rm = TRUE),
    sd = sd(Diametro, na.rm = TRUE),
    n = sum(!is.na(Diametro)),
    se = sd / sqrt(n)
  )

# Reordenar los niveles del factor Mes
resumen$Mes <- factor(resumen$Mes, levels = c("Feb", "Mar", "Abr"))



library(RColorBrewer)

# Paleta de colores extendida
colores <- colorRampPalette(brewer.pal(8, "Set3"))(length(unique(datos_long$Accesion)))

# Graficar
AA = ggplot(resumen, aes(x = Mes, y = media, color = Especie, group = Especie)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
  labs(
    title = "",
    x = "Mes",
    y = "Altura total (m)",
    color = "Especie de capirona"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),     # quitar grillas mayores
    panel.grid.minor = element_blank(),     # quitar grillas menores
    
    axis.line = element_line(color = "black"), # eje en negro
    axis.title = element_text(size = 12, face = "bold"), # títulos en negrita
    axis.text = element_text(size = 10, color = "black"), # textos del eje
    legend.position = "bottom",                # leyenda arriba
    legend.title = element_blank(),         # sin título de leyenda
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # título centrado
    strip.text = element_text(size = 12, face = "bold") # texto de facetas
  )
AA


# Asegurar el orden cronológico de los meses
datos_long$Mes <- factor(datos_long$Mes, levels = c("Feb", "Mar", "Abr"))

# Calcular medias por especie y mes para la línea
medias <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(media = mean(Diametro, na.rm = TRUE), .groups = "drop")

# Crear boxplot con línea de medias
A <- ggplot(datos_long, aes(x = Mes, y = Diametro, fill = Especie)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.7, width = 0.6) +
  geom_line(data = medias, aes(x = Mes, y = media, group = Especie, color = Especie),
            size = 1.2, position = position_dodge(width = 0.6)) +
  geom_point(data = medias, aes(x = Mes, y = media, color = Especie),
             size = 3, position = position_dodge(width = 0.6)) +
  labs(
    title = "Tendencia de la Altura total (m)",
    x = "Mes",
    y = "Altura total (m)",
    fill = "Especie de capirona",
    color = "Especie de capirona",
    tag = "A"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

A


Registros
CM = subset(Registros, Especie== "Capirona macrophylla" )
CS = subset(Registros, Especie== "Calycophyllum spruceanum" )


library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CM %>%
  mutate(
    FEB_ALT = as.numeric(FEB_ALT),
    MAR_ALT = as.numeric(MAR_ALT),
    ABR_ALT = as.numeric(ABR_ALT)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_ALT, MAR_ALT, ABR_ALT),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_ALT", "MAR_ALT", "ABR_ALT"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
B <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Capirona macrophylla",
    x = "Accesión",
    y = "Altura total (m)",
    fill = "Accesión",
    tag = "B"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

B




library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CS %>%
  mutate(
    FEB_ALT = as.numeric(FEB_ALT),
    MAR_ALT = as.numeric(MAR_ALT),
    ABR_ALT = as.numeric(ABR_ALT)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_ALT, MAR_ALT, ABR_ALT),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_ALT", "MAR_ALT", "ABR_ALT"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
C <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x", ncol=1) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Altura total (m)",
    fill = "Accesión",
    tag = "C"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

C




# Agregar columna 'Tipo_propagacion' al dataframe largo
CS$Tipo_propagacion <- factor(CS$`Tipo de propagacion`)  # Asegurar que es factor

# Convertir a formato largo incluyendo tipo de propagación
df_long <- CS %>%
  mutate(
    FEB_ALT = as.numeric(FEB_ALT),
    MAR_ALT = as.numeric(MAR_ALT),
    ABR_ALT = as.numeric(ABR_ALT)
  ) %>%
  pivot_longer(
    cols = c(FEB_ALT, MAR_ALT, ABR_ALT),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro)) %>%
  mutate(Mes = factor(Mes,
                      levels = c("FEB_ALT", "MAR_ALT", "ABR_ALT"),
                      labels = c("Febrero", "Marzo", "Abril")))

# Calcular promedios por accesión, mes y tipo de propagación
df_promedios <- df_long %>%
  group_by(Accesion, Mes, Tipo_propagacion) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes, Tipo_propagacion) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%
  ungroup()

# Paleta de colores extendida
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
D <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.3, fontface = "bold") +
  facet_grid(Mes ~ Tipo_propagacion, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Altura total (m)",
    fill = "Accesión",
    tag = "D"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.3)


D


library(cowplot)
# Crear la figura
ALT = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 20), expand = FALSE) +
  
  draw_plot(C  , width = 14, height = 12,  x = 0,  y = 8) +
  draw_plot(B  , width = 14, height = 8,  x = 0, y = 0) +
  
  draw_plot(A  , width = 16, height = 8,  x = 14, y = 12) +
  draw_plot(D  , width = 16, height = 12,  x = 14,  y = 0) +
  
  
  # Colocar la imagen en 7 de ancho y 21 de alto, desde el borde izquierdo
  
  
  theme(panel.background = element_rect(fill = "white"))

ALT


ggsave(plot=ALT ,"PNG/Abril/ALT.png",units = "cm",width = 30, #alto
       height = 20, #ancho
       dpi=600) 







# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

library(readxl)
library(writexl)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")

datos = Registros
# Convertir nombres de columnas si es necesario
colnames(datos)[colnames(datos) %in% c("FEB_ALT_In", "MAR_ALT_In", "ABR_ALT_In")] <- c("Feb", "Mar", "Abr")

# Convertir columnas de diámetro a numérico y redondear
datos <- datos %>%
  mutate(
    Feb = round(as.numeric(Feb), 2),
    Mar = round(as.numeric(Mar), 2),
    Abr = round(as.numeric(Abr), 2)
  )

# Reorganizar a formato largo
datos_long <- datos %>%
  pivot_longer(cols = c(Feb, Mar, Abr), 
               names_to = "Mes", 
               values_to = "Diametro")

# Calcular promedio y error estándar
resumen <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(
    media = mean(Diametro, na.rm = TRUE),
    sd = sd(Diametro, na.rm = TRUE),
    n = sum(!is.na(Diametro)),
    se = sd / sqrt(n)
  )

# Reordenar los niveles del factor Mes
resumen$Mes <- factor(resumen$Mes, levels = c("Feb", "Mar", "Abr"))



library(RColorBrewer)

# Paleta de colores extendida
colores <- colorRampPalette(brewer.pal(8, "Set3"))(length(unique(datos_long$Accesion)))

# Graficar
AA = ggplot(resumen, aes(x = Mes, y = media, color = Especie, group = Especie)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
  labs(
    title = "",
    x = "Mes",
    y = "Altura inserción de la primra rama (m)",
    color = "Especie de capirona"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),     # quitar grillas mayores
    panel.grid.minor = element_blank(),     # quitar grillas menores
    
    axis.line = element_line(color = "black"), # eje en negro
    axis.title = element_text(size = 12, face = "bold"), # títulos en negrita
    axis.text = element_text(size = 10, color = "black"), # textos del eje
    legend.position = "bottom",                # leyenda arriba
    legend.title = element_blank(),         # sin título de leyenda
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # título centrado
    strip.text = element_text(size = 12, face = "bold") # texto de facetas
  )
AA


# Asegurar el orden cronológico de los meses
datos_long$Mes <- factor(datos_long$Mes, levels = c("Feb", "Mar", "Abr"))

# Calcular medias por especie y mes para la línea
medias <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(media = mean(Diametro, na.rm = TRUE), .groups = "drop")

# Crear boxplot con línea de medias
A <- ggplot(datos_long, aes(x = Mes, y = Diametro, fill = Especie)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.7, width = 0.6) +
  geom_line(data = medias, aes(x = Mes, y = media, group = Especie, color = Especie),
            size = 1.2, position = position_dodge(width = 0.6)) +
  geom_point(data = medias, aes(x = Mes, y = media, color = Especie),
             size = 3, position = position_dodge(width = 0.6)) +
  labs(
    title = "Altura inserción de la primra rama (m)",
    x = "Mes",
    y = "Altura total (m)",
    fill = "Especie de capirona",
    color = "Especie de capirona",
    tag = "A"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

A


Registros
CM = subset(Registros, Especie== "Capirona macrophylla" )
CS = subset(Registros, Especie== "Calycophyllum spruceanum" )


library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CM %>%
  mutate(
    FEB_ALT_In = as.numeric(FEB_ALT_In),
    MAR_ALT_In = as.numeric(MAR_ALT_In),
    ABR_ALT_In = as.numeric(ABR_ALT_In)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_ALT_In, MAR_ALT_In, ABR_ALT_In),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_ALT_In", "MAR_ALT_In", "ABR_ALT_In"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
B <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Capirona macrophylla",
    x = "Accesión",
    y = "Altura inserción de la primra rama (m)",
    fill = "Accesión",
    tag = "B"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

B




library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CS %>%
  mutate(
    FEB_ALT_In = as.numeric(FEB_ALT_In),
    MAR_ALT_In = as.numeric(MAR_ALT_In),
    ABR_ALT_In = as.numeric(ABR_ALT_In)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_ALT_In, MAR_ALT_In, ABR_ALT_In),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_ALT_In", "MAR_ALT_In", "ABR_ALT_In"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
C <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x", ncol=1) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Altura inserción de la primra rama (m)",
    fill = "Accesión",
    tag = "C"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

C




# Agregar columna 'Tipo_propagacion' al dataframe largo
CS$Tipo_propagacion <- factor(CS$`Tipo de propagacion`)  # Asegurar que es factor

# Convertir a formato largo incluyendo tipo de propagación
df_long <- CS %>%
  mutate(
    FEB_ALT_In = as.numeric(FEB_ALT_In),
    MAR_ALT_In = as.numeric(MAR_ALT_In),
    ABR_ALT_In = as.numeric(ABR_ALT_In)
  ) %>%
  pivot_longer(
    cols = c(FEB_ALT_In, MAR_ALT_In, ABR_ALT_In),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro)) %>%
  mutate(Mes = factor(Mes,
                      levels = c("FEB_ALT_In", "MAR_ALT_In", "ABR_ALT_In"),
                      labels = c("Febrero", "Marzo", "Abril")))

# Calcular promedios por accesión, mes y tipo de propagación
df_promedios <- df_long %>%
  group_by(Accesion, Mes, Tipo_propagacion) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes, Tipo_propagacion) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%
  ungroup()

# Paleta de colores extendida
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
D <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.3, fontface = "bold") +
  facet_grid(Mes ~ Tipo_propagacion, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Altura inserción de la primra rama (m)",
    fill = "Accesión",
    tag = "D"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.3)


D


library(cowplot)
# Crear la figura
ALT_In = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 20), expand = FALSE) +
  
  draw_plot(C  , width = 14, height = 12,  x = 0,  y = 8) +
  draw_plot(B  , width = 14, height = 8,  x = 0, y = 0) +
  
  draw_plot(A  , width = 16, height = 8,  x = 14, y = 12) +
  draw_plot(D  , width = 16, height = 12,  x = 14,  y = 0) +
  
  
  # Colocar la imagen en 7 de ancho y 21 de alto, desde el borde izquierdo
  
  
  theme(panel.background = element_rect(fill = "white"))

ALT_In


ggsave(plot=ALT_In ,"PNG/Abril/ALT_In.png",units = "cm",width = 30, #alto
       height = 20, #ancho
       dpi=600) 









# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

library(readxl)
library(writexl)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")

datos = Registros
# Convertir nombres de columnas si es necesario
colnames(datos)[colnames(datos) %in% c("FEB_NH", "MAR_NH", "ABR_NH")] <- c("Feb", "Mar", "Abr")

# Convertir columnas de diámetro a numérico y redondear
datos <- datos %>%
  mutate(
    Feb = round(as.numeric(Feb), 2),
    Mar = round(as.numeric(Mar), 2),
    Abr = round(as.numeric(Abr), 2)
  )

# Reorganizar a formato largo
datos_long <- datos %>%
  pivot_longer(cols = c(Feb, Mar, Abr), 
               names_to = "Mes", 
               values_to = "Diametro")

# Calcular promedio y error estándar
resumen <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(
    media = mean(Diametro, na.rm = TRUE),
    sd = sd(Diametro, na.rm = TRUE),
    n = sum(!is.na(Diametro)),
    se = sd / sqrt(n)
  )

# Reordenar los niveles del factor Mes
resumen$Mes <- factor(resumen$Mes, levels = c("Feb", "Mar", "Abr"))



library(RColorBrewer)

# Paleta de colores extendida
colores <- colorRampPalette(brewer.pal(8, "Set3"))(length(unique(datos_long$Accesion)))

# Graficar
AA = ggplot(resumen, aes(x = Mes, y = media, color = Especie, group = Especie)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
  labs(
    title = "",
    x = "Mes",
    y = "Número de hojas",
    color = "Especie de capirona"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),     # quitar grillas mayores
    panel.grid.minor = element_blank(),     # quitar grillas menores
    
    axis.line = element_line(color = "black"), # eje en negro
    axis.title = element_text(size = 12, face = "bold"), # títulos en negrita
    axis.text = element_text(size = 10, color = "black"), # textos del eje
    legend.position = "bottom",                # leyenda arriba
    legend.title = element_blank(),         # sin título de leyenda
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # título centrado
    strip.text = element_text(size = 12, face = "bold") # texto de facetas
  )
AA


# Asegurar el orden cronológico de los meses
datos_long$Mes <- factor(datos_long$Mes, levels = c("Feb", "Mar", "Abr"))

# Calcular medias por especie y mes para la línea
medias <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(media = mean(Diametro, na.rm = TRUE), .groups = "drop")

# Crear boxplot con línea de medias
A <- ggplot(datos_long, aes(x = Mes, y = Diametro, fill = Especie)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.7, width = 0.6) +
  geom_line(data = medias, aes(x = Mes, y = media, group = Especie, color = Especie),
            size = 1.2, position = position_dodge(width = 0.6)) +
  geom_point(data = medias, aes(x = Mes, y = media, color = Especie),
             size = 3, position = position_dodge(width = 0.6)) +
  labs(
    title = "Tentencia del Número de hojas",
    x = "Mes",
    y = "Número de hojas",
    fill = "Especie de capirona",
    color = "Especie de capirona",
    tag = "A"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

A


Registros
CM = subset(Registros, Especie== "Capirona macrophylla" )
CS = subset(Registros, Especie== "Calycophyllum spruceanum" )


library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CM %>%
  mutate(
    FEB_NH = as.numeric(FEB_NH),
    MAR_NH = as.numeric(MAR_NH),
    ABR_NH = as.numeric(ABR_NH)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_NH, MAR_NH, ABR_NH),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_NH", "MAR_NH", "ABR_NH"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
B <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Capirona macrophylla",
    x = "Accesión",
    y = "Número de hojas",
    fill = "Accesión",
    tag = "B"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

B




library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CS %>%
  mutate(
    FEB_NH = as.numeric(FEB_NH),
    MAR_NH = as.numeric(MAR_NH),
    ABR_NH = as.numeric(ABR_NH)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_NH, MAR_NH, ABR_NH),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_NH", "MAR_NH", "ABR_NH"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
C <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x", ncol=1) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Número de hojas",
    fill = "Accesión",
    tag = "C"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

C




# Agregar columna 'Tipo_propagacion' al dataframe largo
CS$Tipo_propagacion <- factor(CS$`Tipo de propagacion`)  # Asegurar que es factor

# Convertir a formato largo incluyendo tipo de propagación
df_long <- CS %>%
  mutate(
    FEB_NH = as.numeric(FEB_NH),
    MAR_NH = as.numeric(MAR_NH),
    ABR_NH = as.numeric(ABR_NH)
  ) %>%
  pivot_longer(
    cols = c(FEB_NH, MAR_NH, ABR_NH),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro)) %>%
  mutate(Mes = factor(Mes,
                      levels = c("FEB_NH", "MAR_NH", "ABR_NH"),
                      labels = c("Febrero", "Marzo", "Abril")))

# Calcular promedios por accesión, mes y tipo de propagación
df_promedios <- df_long %>%
  group_by(Accesion, Mes, Tipo_propagacion) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes, Tipo_propagacion) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%
  ungroup()

# Paleta de colores extendida
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
D <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.3, fontface = "bold") +
  facet_grid(Mes ~ Tipo_propagacion, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Número de hojas",
    fill = "Accesión",
    tag = "D"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.3)


D


library(cowplot)
# Crear la figura
NH = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 20), expand = FALSE) +
  
  draw_plot(C  , width = 14, height = 12,  x = 0,  y = 8) +
  draw_plot(B  , width = 14, height = 8,  x = 0, y = 0) +
  
  draw_plot(A  , width = 16, height = 8,  x = 14, y = 12) +
  draw_plot(D  , width = 16, height = 12,  x = 14,  y = 0) +
  
  
  # Colocar la imagen en 7 de ancho y 21 de alto, desde el borde izquierdo
  
  
  theme(panel.background = element_rect(fill = "white"))

NH


ggsave(plot=NH ,"PNG/Abril/NH.png",units = "cm",width = 30, #alto
       height = 20, #ancho
       dpi=600) 






# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)

library(readxl)
library(writexl)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")

datos = Registros
# Convertir nombres de columnas si es necesario
colnames(datos)[colnames(datos) %in% c("FEB_DC", "MAR_DC", "ABR_DC")] <- c("Feb", "Mar", "Abr")

# Convertir columnas de diámetro a numérico y redondear
datos <- datos %>%
  mutate(
    Feb = round(as.numeric(Feb), 2),
    Mar = round(as.numeric(Mar), 2),
    Abr = round(as.numeric(Abr), 2)
  )

# Reorganizar a formato largo
datos_long <- datos %>%
  pivot_longer(cols = c(Feb, Mar, Abr), 
               names_to = "Mes", 
               values_to = "Diametro")

# Calcular promedio y error estándar
resumen <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(
    media = mean(Diametro, na.rm = TRUE),
    sd = sd(Diametro, na.rm = TRUE),
    n = sum(!is.na(Diametro)),
    se = sd / sqrt(n)
  )

# Reordenar los niveles del factor Mes
resumen$Mes <- factor(resumen$Mes, levels = c("Feb", "Mar", "Abr"))



library(RColorBrewer)

# Paleta de colores extendida
colores <- colorRampPalette(brewer.pal(8, "Set3"))(length(unique(datos_long$Accesion)))

# Graficar
AA = ggplot(resumen, aes(x = Mes, y = media, color = Especie, group = Especie)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
  labs(
    title = "",
    x = "Mes",
    y = "Diametro de copa (cm)",
    color = "Especie de capirona"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),     # quitar grillas mayores
    panel.grid.minor = element_blank(),     # quitar grillas menores
    
    axis.line = element_line(color = "black"), # eje en negro
    axis.title = element_text(size = 12, face = "bold"), # títulos en negrita
    axis.text = element_text(size = 10, color = "black"), # textos del eje
    legend.position = "bottom",                # leyenda arriba
    legend.title = element_blank(),         # sin título de leyenda
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # título centrado
    strip.text = element_text(size = 12, face = "bold") # texto de facetas
  )
AA


# Asegurar el orden cronológico de los meses
datos_long$Mes <- factor(datos_long$Mes, levels = c("Feb", "Mar", "Abr"))

# Calcular medias por especie y mes para la línea
medias <- datos_long %>%
  group_by(Especie, Mes) %>%
  summarise(media = mean(Diametro, na.rm = TRUE), .groups = "drop")

# Crear boxplot con línea de medias
A <- ggplot(datos_long, aes(x = Mes, y = Diametro, fill = Especie)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.7, width = 0.6) +
  geom_line(data = medias, aes(x = Mes, y = media, group = Especie, color = Especie),
            size = 1.2, position = position_dodge(width = 0.6)) +
  geom_point(data = medias, aes(x = Mes, y = media, color = Especie),
             size = 3, position = position_dodge(width = 0.6)) +
  labs(
    title = "Tendencia del Diametro de copa",
    x = "Mes",
    y = "Diametro de copa (cm)",
    fill = "Especie de capirona",
    color = "Especie de capirona",
    tag = "A"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

A


Registros
CM = subset(Registros, Especie== "Capirona macrophylla" )
CS = subset(Registros, Especie== "Calycophyllum spruceanum" )


library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CM %>%
  mutate(
    FEB_DC = as.numeric(FEB_DC),
    MAR_DC = as.numeric(MAR_DC),
    ABR_DC = as.numeric(ABR_DC)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_DC, MAR_DC, ABR_DC),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_DC", "MAR_DC", "ABR_DC"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
B <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Capirona macrophylla",
    x = "Accesión",
    y = "Diametro de copa (cm)",
    fill = "Accesión",
    tag = "B"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

B




library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

# Convertir columnas a numéricas
df <- CS %>%
  mutate(
    FEB_DC = as.numeric(FEB_DC),
    MAR_DC = as.numeric(MAR_DC),
    ABR_DC = as.numeric(ABR_DC)
  )

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(
    cols = c(FEB_DC, MAR_DC, ABR_DC),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro))

# Reordenar nombres de meses
df_long$Mes <- factor(df_long$Mes,
                      levels = c("FEB_DC", "MAR_DC", "ABR_DC"),
                      labels = c("Febrero", "Marzo", "Abril"))

# Calcular promedio por accesión y mes
df_promedios <- df_long %>%
  group_by(Accesion, Mes) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%  # Ordenar descendente
  ungroup()

# Paleta de colores
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
C <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~Mes, scales = "free_x", ncol=1) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Diametro de copa (cm)",
    fill = "Accesión",
    tag = "C"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.5)

C




# Agregar columna 'Tipo_propagacion' al dataframe largo
CS$Tipo_propagacion <- factor(CS$`Tipo de propagacion`)  # Asegurar que es factor

# Convertir a formato largo incluyendo tipo de propagación
df_long <- CS %>%
  mutate(
    FEB_DC = as.numeric(FEB_DC),
    MAR_DC = as.numeric(MAR_DC),
    ABR_DC = as.numeric(ABR_DC)
  ) %>%
  pivot_longer(
    cols = c(FEB_DC, MAR_DC, ABR_DC),
    names_to = "Mes",
    values_to = "Diametro"
  ) %>%
  filter(!is.na(Diametro)) %>%
  mutate(Mes = factor(Mes,
                      levels = c("FEB_DC", "MAR_DC", "ABR_DC"),
                      labels = c("Febrero", "Marzo", "Abril")))

# Calcular promedios por accesión, mes y tipo de propagación
df_promedios <- df_long %>%
  group_by(Accesion, Mes, Tipo_propagacion) %>%
  summarise(Diametro = mean(Diametro, na.rm = TRUE), .groups = "drop") %>%
  group_by(Mes, Tipo_propagacion) %>%
  mutate(Accesion = fct_reorder(Accesion, -Diametro)) %>%
  ungroup()

# Paleta de colores extendida
base_colors <- brewer.pal(12, "Set3")
colores <- colorRampPalette(base_colors)(length(unique(df_promedios$Accesion)))

# Graficar promedio con etiquetas encima
D <- ggplot(df_promedios, aes(x = Accesion, y = Diametro, fill = Accesion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Diametro)),
            vjust = 0.5, size = 2.3, fontface = "bold", angle =90) +
  facet_grid(Mes ~ Tipo_propagacion, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = colores) +
  labs(
    title = "Calycophyllum spruceanum",
    x = "Accesión",
    y = "Diametro de copa (cm)",
    fill = "Accesión",
    tag = "D"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, "mm"),
    keywidth = unit(2, "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.position = "bottom",
    nrow = 1
  ))+
  ylim(0, max(df_promedios$Diametro, na.rm = TRUE) * 1.3)


D


library(cowplot)
# Crear la figura
DC = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 20), expand = FALSE) +
  
  draw_plot(C  , width = 14, height = 12,  x = 0,  y = 8) +
  draw_plot(B  , width = 14, height = 8,  x = 0, y = 0) +
  
  draw_plot(A  , width = 16, height = 8,  x = 14, y = 12) +
  draw_plot(D  , width = 16, height = 12,  x = 14,  y = 0) +
  
  
  # Colocar la imagen en 7 de ancho y 21 de alto, desde el borde izquierdo
  
  
  theme(panel.background = element_rect(fill = "white"))


ggsave(plot=DC ,"PNG/Abril/DC.png",units = "cm",width = 30, #alto
       height = 20, #ancho
       dpi=600) 

