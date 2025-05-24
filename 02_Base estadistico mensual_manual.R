
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
for (codigo in codigos) {
  plan01 <- subset(Registros_SF_C, Codigo.de.campo == codigo)
  source("Ets_indiv.R")
  nombre <- paste0("plan", sprintf("%02d", which(codigos == codigo)), "_plot.png")
  ggsave(plot = plan01_plot, filename = file.path("PNG", nombre),
         units = "cm", width = 28, height = 21, dpi = 300)
}








library(readxl)
library(writexl)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")



library(sf)
Registros_SF <- Registros %>% 
  st_as_sf(coords = c("Latitud", "Longitud"), crs = "+proj=longlat +datum=WGS84 +no_defs")

Registros_SF_C  <- cbind(Registros_SF, st_coordinates(st_centroid(Registros_SF$geometry)))



plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-01-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan01_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)



plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-02-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan02_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)



plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-03-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan03_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-04-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan04_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-05-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan05_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-06")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan06_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-07-2")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan07_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-08-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan08_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-09-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan09_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-10-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan10_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-11-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan11_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-12")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan12_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-13-8")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan13_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-14-06")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan14_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-15-8")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan15_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-16")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan16_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-17-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan17_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-18")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan18_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-19-11")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan19_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-20-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan20_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-21-10")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan21_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-22-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan22_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-23-10")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan23_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-24")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan24_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-25-10")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan25_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-26-11")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan26_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-27-11")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan27_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-28")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan28_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-29-11")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan29_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-30")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan30_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-31-9")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan31_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-32-10")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan32_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-33")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan33_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-34")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan34_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-35-10")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan35_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-36")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan36_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-37")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan37_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-38-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan38_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-39")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan39_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-40-1")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan40_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-41-2")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan41_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-42")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan42_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-43-18")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan43_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-44-3")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan44_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-45-4")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan45_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-46-4")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan46_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-47-4")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan47_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-48-4")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan48_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-49_5")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan49_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-50-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan50_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)



plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-51-6")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan51_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-52-7")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan52_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-53-7")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan53_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-54-8")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan54_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-55")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan55_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-56-9")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan56_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-57-10")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan57_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-58")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan58_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)



plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-59-10")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan59_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-60-11")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan60_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)    


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-61")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan61_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)    


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-62-12")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan62_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)    




plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-63-12")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan63_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300)    


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-64-13")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan64_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-65-13")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan65_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-66-13")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan66_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CM-67")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan67_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 



plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-68-14")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan68_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-69-15")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan69_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-70-16")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan70_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-71-16")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan71_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-72-16")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan72_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-73-17")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan73_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 


plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-74-18")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan74_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 

plan01 = subset(Registros_SF_C , Codigo.de.campo == "CS-75-18")

source("Ets_indiv.R")

ggsave(plot=plan01_plot ,"PNG/plan75_plot.png",units = "cm",width = 28, #alto
       height = 21, #ancho
       dpi=300) 