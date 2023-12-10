library(sf) # working with spatial vector data
library(terra) 
#install.packages("tmap")
library(tmap) # plotting maps

list.pa.xy <- readRDS("data/cv/list.pa.xy.rds") %>% 
  lapply(function(df) {
  df$layer <- "0"
  return(df)
})

library(readxl)
pres.xy <- read_excel("data/cv/pres.xy.xlsx") 
pres.xy$layer <- 1

agregar_filas_sin_duplicados <- function(df) {
  resultado <- anti_join(df, pres.xy, by = c("x", "y"))
  return(resultado)
}

agregar_filas <- function(df) {
  resultado <- rbind(df, pres.xy)
  return(resultado)
}

# Aplicar la función a cada dataframe en la lista
#total.xy <- lapply(list.pa.xy, agregar_filas_sin_duplicados)
total.xy <- lapply(list.pa.xy, agregar_filas)

convertir_a_sf <- function(df) {
  sf_objeto <- st_as_sf(df, coords = c("x", "y"), crs= 23030)
  return(sf_objeto)
}

# Aplicar la función a cada dataframe en la lista
total.xy.sf <- lapply(total.xy, convertir_a_sf)

library(blockCV)

aplicar_cv_spatial_autocor <- function(sf_objeto) {
  nombre_elemento <- deparse(substitute(sf_objeto))  # Obtener el nombre del elemento
  plot_nombre <- paste("plot.", nombre_elemento, sep = "")
  resultado <- cv_spatial_autocor(x= sf_objeto, column = "layer", plot = TRUE)
  plot_PA[[nombre_elemento]] <- resultado$plot
  return(resultado)
}

PA1<- total.xy.sf$PA1
plot_PA <- list()
spatial_autocor_PA1 <- aplicar_cv_spatial_autocor(PA1)

spatial_autocor_total <- lapply(total.xy.sf, aplicar_cv_spatial_autocor)
saveRDS(spatial_autocor_total, "data/cv/spatial_autocor_total.rds")

range_total <- lapply(spatial_autocor_total, function(elemento) elemento$range)
range_total$PA51 <- NULL
saveRDS(range_total, "data/cv/range_total.rds")


rasters <- rast(list.files(pattern='\\.tiff$', full=TRUE, path=here::here("data/variablesexpl")))

aplicar_cv_spatial <- function(df, size) {
  resultado <- cv_spatial(df, 
                          size = size, 
                          column = "layer", # the response column (binary or multi-class)
                          r = rasters,
                          k = 5,
                          hexagon = FALSE,
                          selection = "random", # stystematic blocks-to-fold
                          iteration = 50, # find evenly dispersed folds
                          progress = TRUE, # turn on progress bar
                          biomod2 = TRUE, # also create folds for biomod2
                          raster_colors = terrain.colors(10, rev = TRUE))
  return(resultado)
}

spatialblock_random_1_20 <- mapply(aplicar_cv_spatial, total.xy.sf[1:20], size = range_total[1:20], SIMPLIFY = FALSE)
spatialblock_random_21_22 <- mapply(aplicar_cv_spatial, total.xy.sf[21:22], size = range_total[21:22], SIMPLIFY = FALSE)
spatialblock_random_23 <- cv_spatial(total.xy.sf$PA23, 
          size = 6161.932, 
          column = "layer", # the response column (binary or multi-class)
          r = rasters,
          k = 5,
          hexagon = FALSE,
          selection = "random", # stystematic blocks-to-fold
          iteration = 50, # find evenly dispersed folds
          progress = TRUE, # turn on progress bar
          biomod2 = TRUE, # also create folds for biomod2
          raster_colors = terrain.colors(10, rev = TRUE))
spblock_23 <- list(spatialblock_random_23) 
names(spblock_23) = "PA23"

spatialblock_random_24_35 <- mapply(aplicar_cv_spatial, total.xy.sf[24:35], size = range_total[24:35], SIMPLIFY = FALSE)
spatialblock_random_36 <- cv_spatial(total.xy.sf$PA36, 
                                  size = 6280.127, 
                                  column = "layer", # the response column (binary or multi-class)
                                  r = rasters,
                                  k = 5,
                                  hexagon = FALSE,
                                  selection = "random", # stystematic blocks-to-fold
                                  iteration = 50, # find evenly dispersed folds
                                  progress = TRUE, # turn on progress bar
                                  biomod2 = TRUE, # also create folds for biomod2
                                  raster_colors = terrain.colors(10, rev = TRUE))
spblock_36 <- list(spatialblock_random_36) 
names(spblock_36) = "PA36"

spatialblock_random_37 <- cv_spatial(total.xy.sf$PA37, 
                                     size = range_total$PA37, 
                                     column = "layer", # the response column (binary or multi-class)
                                     r = rasters,
                                     k = 5,
                                     hexagon = FALSE,
                                     selection = "random", # stystematic blocks-to-fold
                                     iteration = 50, # find evenly dispersed folds
                                     progress = TRUE, # turn on progress bar
                                     biomod2 = TRUE, # also create folds for biomod2
                                     raster_colors = terrain.colors(10, rev = TRUE))
spblock_37 <- list(spatialblock_random_37) 
names(spblock_37) = "PA37"

spatialblock_random_38 <- cv_spatial(total.xy.sf$PA38, 
                                     size = 6369.026, 
                                     column = "layer", # the response column (binary or multi-class)
                                     r = rasters,
                                     k = 5,
                                     hexagon = FALSE,
                                     selection = "random", # stystematic blocks-to-fold
                                     iteration = 50, # find evenly dispersed folds
                                     progress = TRUE, # turn on progress bar
                                     biomod2 = TRUE, # also create folds for biomod2
                                     raster_colors = terrain.colors(10, rev = TRUE))
spblock_38 <- list(spatialblock_random_38) 
names(spblock_38) = "PA38"

spatialblock_random_39 <- cv_spatial(total.xy.sf$PA39, 
                                     size = 4697.979, 
                                     column = "layer", # the response column (binary or multi-class)
                                     r = rasters,
                                     k = 5,
                                     hexagon = FALSE,
                                     selection = "random", # stystematic blocks-to-fold
                                     iteration = 50, # find evenly dispersed folds
                                     progress = TRUE, # turn on progress bar
                                     biomod2 = TRUE, # also create folds for biomod2
                                     raster_colors = terrain.colors(10, rev = TRUE))
spblock_39 <- list(spatialblock_random_39) 
names(spblock_39) = "PA39"

spatialblock_random_40 <- cv_spatial(total.xy.sf$PA40, 
                                     size = 6630.596, 
                                     column = "layer", # the response column (binary or multi-class)
                                     r = rasters,
                                     k = 5,
                                     hexagon = FALSE,
                                     selection = "random", # stystematic blocks-to-fold
                                     iteration = 50, # find evenly dispersed folds
                                     progress = TRUE, # turn on progress bar
                                     biomod2 = TRUE, # also create folds for biomod2
                                     raster_colors = terrain.colors(10, rev = TRUE))
spblock_40 <- list(spatialblock_random_40) 
names(spblock_40) = "PA40"

spatialblock_random_41_50 <- mapply(aplicar_cv_spatial, total.xy.sf[41:50], size = range_total[41:50], SIMPLIFY = FALSE)

#bien: 41, 42, 43, 44, 45, 46, 47, 48, 49, 50

obj <- ls()
obj_spatialblocks <- obj[grep("^spatialblock", obj)]

# Crear una lista con los objetos filtrados
lista_spatialblocks <- lapply(obj_spatialblocks[1:10], get)

lista_spatialblocks <- c(spatialblock_random_1_20, 
                         spatialblock_random_21_22, 
                         spblock_23, 
                         spatialblock_random_24_35, 
                         spblock_36,
                         spblock_37,
                         spblock_38,
                         spblock_39, 
                         spblock_40, 
                         spatialblock_random_41_50)

saveRDS(lista_spatialblocks, "data/cv/lista_spatialblocks.rds")
