

library(readxl)
library(writexl)
library(leaflet.extras)
library(leaflet.providers)
library(htmlwidgets)
library(leafem)
Registros <- read_excel("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/BD_Plantacion.xlsx", 
                        sheet = "Estadistica")



library(sf)
Registros_SF <- Registros %>% 
  st_as_sf(coords = c("Latitud", "Longitud"), crs = "+proj=longlat +datum=WGS84 +no_defs")

Registros_SF_C  <- cbind(Registros_SF, st_coordinates(st_centroid(Registros_SF$geometry)))



CM = subset(Registros_SF_C, Especie== "Capirona macrophylla" )
CS = subset(Registros_SF_C, Especie== "Calycophyllum spruceanum" )


#Definiendo el Logo
m="https://imagizer.imageshack.com/img922/3715/mUPmX3.png"



icons <- makeAwesomeIcon(icon='leaf', 
                         library='fa', 
                         markerColor = 'green', 
                         iconColor = 'white')

icons2 <- makeAwesomeIcon(icon='leaf', 
                          library='fa', 
                          markerColor = 'red', 
                          iconColor = 'white')


Buscador <- data.frame(Codigo.de.Campo = Registros_SF_C$Codigo.de.campo,
                       Accesion = Registros_SF_C$Accesion, 
                       Propagacion =  Registros_SF_C$Tipo.de.propagacion ,
                       Diametro.Feb = Registros_SF_C$FEB_DTB, 
                       Diametro.Mar =  Registros_SF_C$MAR_DTB,
                       Diametro.Abri = Registros_SF_C$ABR_DTB,
                       Este = Registros_SF_C$X,
                       Norte = Registros_SF_C$Y)


Colores <- c(
  "red", "blue","green" )
Spp_Names <- Registros_SF_C$Especie %>% unique()

library(leaflet)
pal <- colorFactor(Colores, domain = Spp_Names)

Poligono<- st_read("C:/Users/INIA/OneDrive/03_INIA_2025/16_Establecimiento del bancoi de germoplasma/Plantacion/02_SHP/Poligono.shp")
Poligono <- st_transform(Poligono, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))


library(leaflet.extras)
library(leaflet.providers)
library(htmlwidgets)
library(leafem)

Mapa = leaflet(Buscador) %>%
  addControl(html = "<p><strong><em>Plantación de dos especies de Capirona mediante dos formas de propagación</em></strong></p>",
             position = "topright")%>%
  addLogo(position = "topleft",
          offset.x = 40,
          offset.y = -90,
          width = 250,
          height = 250, m)%>%
  addMarkers(
    data = Buscador, lng = ~Norte, lat = ~Este, label = Buscador$Codigo.de.Campo,
    group = 'Numero', 
    
    icon = makeIcon( 
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
      iconWidth = 1, iconHeight = 1
    )
  )%>% 
  addSearchFeatures(
    targetGroups = 'Numero',
    
    options = searchFeaturesOptions(
      zoom=20, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE
    )
  )%>%
  addPolygons(data= Poligono , color = "black", fillOpacity = 0.01,weight = 1,
              group = "Poligono")%>%
  
  
  addAwesomeMarkers(data = CM, lng = ~Y, lat = ~X, icon = icons,
                    group = "Capirona macrophylla", 
                    
                    popup = paste0(
                      "<h1 style='color:#006400'> Capirona macrophylla</h1>",
                      "<b style='color:#38b000'> Codigo de Campo: </b>",CM$Codigo.de.campo, "<br>",
                      "<b style='color:#38b000'> Accesion: </b>",CM$Accesion, "<br>",
                      "<b style='color:#38b000'> Tipo de Propagacion: </b>",CM$Tipo.de.propagacion, "<br>",
                      
                      "<b style='color:#38b000'> Diámetro del tallo en la base: </b>",round(CM$Prom_DTB, 2), "<br>",
                      "<b style='color:#38b000'> Diámetro del tallo a media altura: </b>",round(CM$Prom_DTMA, 2), "<br>",
                      "<b style='color:#38b000'> Altura total: </b>",round(CM$Prom_Alt, 2), "<br>",
                      "<b style='color:#38b000'> Altura inserción: </b>",round(CM$Prom_Altins, 2), "<br>",
                      "<b style='color:#38b000'> Estado del Fitosanitario: </b>",CM$Prom_EF, 2, "<br>",
                      "<b style='color:#38b000'> Numero de Ramas: </b>",round(CM$Prom_NH, 2), "<br>",
                      "<b style='color:#38b000'> Diametro Copa: </b>",round(CM$Prom_DC, 2), "<br>",
                      "<b style='color:#38b000'> Número de hojas nuevas: </b>",round(CM$Prom_NHN, 2), "<br>",
                      "<b style='color:#38b000'> Longitud de entrenudos: </b>",round(CM$Prom_LE, 2), "<br>",
                      "<b style='color:#38b000'> Número de tallos secundarios: </b>",round(CM$Prom_NTS, 2), "<br>",
                      
                      "<div style='display: flex; justify-content: center; gap: 10px;'>
                      <img src='", CM$Abril, "' style='height: 150px; width: auto;'>
                      <img src='", CM$Estadis_Abril, "' style='height: 150px; width: auto;'>
                      </div>",
                      "<a href='", CM$Abril, "' target='_blank'>Foto Abril</a><br>",
                      "<a href='", CM$Estadis_Abril, "' target='_blank'>Estadistica Abril</a><br>"
                    ))%>%
  
  addAwesomeMarkers(data = CS, lng = ~Y, lat = ~X, icon = icons2,
                    group = "Calycophyllum spruceanum", 
                    
                    popup = paste0(
                      "<h1 style='color:#c1121f'> Calycophyllum spruceanum</h1>",
                      
                      "<b style='color:#c1121f'> Codigo de Campo: </b>",Registros_SF_C$Codigo.de.campo, "<br>",
                      "<b style='color:#c1121f'> Accesion: </b>",Registros_SF_C$Accesion, "<br>",
                      "<b style='color:#c1121f'> Tipo de Propagacion: </b>",Registros_SF_C$Tipo.de.propagacion, "<br>",
                      
                      "<b style='color:#c1121f'> Diámetro del tallo en la base: </b>",round(CS$Prom_DTB, 2), "<br>",
                      "<b style='color:#c1121f'> Diámetro del tallo a media altura: </b>",round(CS$Prom_DTMA, 2), "<br>",
                      "<b style='color:#c1121f'> Altura total: </b>",round(CS$Prom_Alt, 2), "<br>",
                      "<b style='color:#c1121f'> Altura inserción: </b>",round(CS$Prom_Altins, 2), "<br>",
                      "<b style='color:#c1121f'> Estado del Fitosanitario: </b>",CS$Prom_EF, "<br>",
                      "<b style='color:#c1121f'> Numero de Ramas: </b>",round(CS$Prom_NH, 2), "<br>",
                      "<b style='color:#c1121f'> Diametro Copa: </b>",round(CM$Prom_DC, 2), "<br>",
                      "<b style='color:#c1121f'> Número de hojas nuevas: </b>",round(CS$Prom_NHN, 2), "<br>",
                      "<b style='color:#c1121f'> Longitud de entrenudos: </b>",round(CS$Prom_LE, 2), "<br>",
                      "<b style='color:#c1121f'> Número de tallos secundarios: </b>",round(CS$Prom_NTS, 2), "<br>",
                      
                      "<div style='display: flex; justify-content: center; gap: 10px;'>
                      <img src='", CS$Abril, "' style='height: 150px; width: auto;'>
                      <img src='", CS$Estadis_Abril, "' style='height: 150px; width: auto;'>
                      </div>",
                      "<a href='", CS$Abril, "' target='_blank'>Foto Abril</a><br>",
                      "<a href='", CS$Estadis_Abril, "' target='_blank'>Estadistica Abril</a><br>"
                    ))%>%
  
  
  addLayersControl(overlayGroups = c("Calycophyllum spruceanum", "Capirona macrophylla", "Poligono"),
                   position = "topright",
                   options = layersControlOptions(collapsed = T))%>%
  addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 100,
                                                                 metric = TRUE,
                                                                 imperial = TRUE,
                                                                 updateWhenIdle = TRUE)) %>%
  addDrawToolbar(targetGroup = "Graficos",
                 editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
  
  addMeasure(position = "topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479")%>% 
  addSearchOSM() %>% 
  addControlGPS() %>% 
  addResetMapButton()%>% 
  addControl("<P><B>PI: PROAGROBIO!</B> Caracterizacion morfologica<br/><ul><li>Ing. Gorky Florez Castillo</li> 
     <li>Ing. Yicelia M. Mamani Mariaca</li>
    </ul></P>",
             position='bottomright') %>% 
  
  onRender("
           function(map) {
             $('.custom-link').on('click', function(e) {
               e.preventDefault();
               var url = $(this).attr('href');
               window.open(url, '_blank');
             });
           }
           ")%>%
  
  addLegend(data = Registros_SF_C , "bottomright", pal = pal, 
            values = ~Especie, title = "Especies de Capirona", 
            opacity = 1, group = "Leyenda")%>% 
  addMiniMap(
    tiles = providers$Esri.WorldImagery,
    toggleDisplay = TRUE,
    position = "bottomleft",
    width = 150,
    height = 150,
    zoomLevelOffset = -5
  )
#%>% addLegend(pal = pale, values = values(Poligo_alt), title = "elevation (m)")

Mapa

#library(mapview)
#mapview(Poligono )






