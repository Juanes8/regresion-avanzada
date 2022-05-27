# librerías 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, skimr, janitor, ozmaps, sf)

# La base original del clima en Australia (está almacenada en Google Drive para que cualquiera la pueda leer)
id_clima_australia <- "1ld71eMURJBKh9oUsnbWFKrF27xeG5CJ6"
clima_australia <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_clima_australia))

# La base que sacó Gori después de hacer el profile report (está almacenada en Google Drive para que cualquiera la pueda leer)
id_base_modificada <- "1lolxl-E-DH8nmOAHSXQImtbz2V6BgzzW"
base_modificada <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_base_modificada))

# La base tiene la localidad donde está el sensor pero no dice en qué estado está ni cuales son suscoordenadas, armé una base con eso 
# (está almacenada en Google Drive para que cualquiera la pueda leer)
id_estados <- "1luuRfPmJCVwAmcG8Ac3W85C9_ZfWqm7H"
localidad_estados <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_estados))

localidad_estados <- localidad_estados |> mutate(Lat = substr(Coordinates, 1,9),
                                                 Lon = substr(Coordinates, 14,22)) |>
  select(-Coordinates)

clima_australia <- clima_australia |> left_join(localidad_estados |> rename(Location = Station)) |> relocate(State:Lon, .after = Location)

# Después de análizar las variables con las que se quedó Gori, decidí usar las mismas con las que se quedó él, quitando el id
# y volviendo a construir las variables de año, día y mes (para hacer un groupby por el valor de las variable en la quincena)
vector_usar <- c(names(base_modificada)[2:8],"Temp3pm","RainToday") # el nombre de las variables que usaré

clima_australia <- clima_australia |> mutate(Year = substr(clima_australia$Date, 1,4), # contruyendo de nuevo el año
                                             Month = substr(clima_australia$Date, 6,7), # construyendo de nuevo el mes
                                             Day = substr(clima_australia$Date, 9,10), # constriyendo el día
                                             Fortnight = if_else(Day <= 15, "1W", "2W"), # estableciendo la quincena que corresponde del mes
                                             WMY = paste0(Fortnight,"-", Month,"-", Year), .after = Date) |>
  select(Date:Lon, all_of(vector_usar)) |> relocate(RainTomorrow, .after = RainToday)

# Dando un vistazo a las variables numéricas (n_missing y complete_rate)
clima_australia |> skim() |> yank("numeric")

# Voy a modificar ligeramente la imputación de los NA haciéndolos mucho más estrictos. En caso de NA, voy a imputar el valor de la localidad
# para la quincena donde falló el sensor en los casos donde hubo la misma ocurrencia de lluvia (llovió o no llovió).
imputados_clima <- clima_australia |> 
  group_by(Location, WMY, RainToday) |> 
  mutate(across(where(is.numeric), \(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))) |>
  ungroup() |>
  mutate(Year = as.integer(Year),
         Month = as.integer(Month))

# La primera forma de hacer los mapas es con el paquete choroplethr, pero decidimos no usarla.

library(choroplethr)
library(choroplethrAdmin1)

# valores_hum <- valores_hum[c(1,3,2,4,5,6,7,8)]
# 
# australia_regions <- get_admin1_regions("australia")
# australia_regions$value <- valores_hum
# 
# admin1_choropleth("australia", 
#                   australia_regions,
#                   num_colors = 1,
#                   legend="Humedad promedio",
#                   title = "Humedad en los Estados de Australia")


# Pintaremos los mapas con los paquetes ozmaps y ggplot2
# Extraemos los polígonos espaciales de Australia
# Aplicamos un head para quitar la zona de "Otras Zonas" que son Islas sin importancia en nuestra base
sf_oz <- ozmap_data("states") |> head(8)

plot_valores <- function(indicador, year, mes){
  mes_interno <- paste0(paste("Humedad promedio en los diferentes estados del país en el mes"), " ", "0", as.character(mes))
  vector_indicador <- imputados_clima |>
    filter(Year == year) |>
    filter(Month == mes) |>
    group_by(State) |>
    summarise(indicador_promedio = mean({{indicador}}, na.rm=T)) |> pull(indicador_promedio)
  vector_indicador <- vector_indicador[c(2,7,4,5,8,6,3,1)]
  sf_oz <- sf_oz |> mutate(indicador_promedio = as.numeric(vector_indicador))
  pl <- ggplot(data = sf_oz, aes(fill = indicador_promedio)) + geom_sf()
  pl <- pl + scale_fill_gradient(high = "#132B43", low = "#56B1F7") # para cambiar el color
  pl <- pl + labs(title ="Indicadores climatológicos de Australia")
  pl <- pl + labs(subtitle= mes_interno)
  pl <- pl + theme_void()
  pl
}

imputados_clima |> pull(unique(Month))

plot_valores(Humidity3pm, 2011, 6)



# valores_hum <- imputados_clima |>
#   filter(Year == 2011) |>
#   group_by(State) |>
#   summarize(humedad_promedio = mean(Humidity3pm, na.rm=T)) |> pull(humedad_promedio)
#  
# 
# valores_hum_ggplot_plus <- valores_hum[c(2,7,4,5,8,6,3,1)]
# 
# 
# sf_oz_plus$Humedad <- as.numeric(valores_hum_ggplot_plus)
# 
# sf_oz$Humedad <- as.numeric(valores_hum_ggplot)
# 
# pl <- ggplot(data = sf_oz_plus, aes(fill = Humedad)) + geom_sf()
# pl <- pl + scale_fill_gradient(high = "#132B43", low = "#56B1F7") # para cambiar el color
# pl <- pl + labs(title ="Indicadores climatológicos de Australia")
# pl <- pl + labs(subtitle ="Humedad promedio a las 3 de la tarde")
# pl <- pl + theme_void()
# pl
# 
# 
# years_apply <- c(2008:2013)
# 
# lapply(years_apply, function(x){tabyl(clima_australia |> filter(Year==x), Month)})
