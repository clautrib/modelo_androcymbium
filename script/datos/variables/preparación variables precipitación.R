
library(terra)
library(dplyr)

aoi <- vect("rawdata/AOI_puntos.shp") %>% 
  project("epsg:23030")
aoi


# prueba por pasos en vez de la función 
prueba <- rast("data/variablesexpl/prec_anual_avg.tif") %>% 
  project("epsg:23030") 
ext(prueba) <- ext(aoi)
#prueba[is.na(prueba)] <- 0

extract_prueba <- terra::extract(prueba, aoi, xy=TRUE, bind=TRUE)

rast_prueba <-  rasterize(extract_prueba, esqueleto, extract_prueba$mean)
rast_prueba

#función para importar, extraer y rasterizar

molde <- rast("data/variablesexpl/FR_MATDE.tiff")

esqueleto <- rast(nrow=875, ncol=1575 , nlyr=1) 
ext(esqueleto) <- ext(molde)
res(esqueleto) <- res(molde)
crs(esqueleto) <- crs(molde)

precip_extract <- function(x, ...) {
  r <- terra::rast(x)
  r_project <- project(r, "epsg:23030")
  ext(r_project) <- ext(aoi)
  r_extract <- terra::extract(r_project, aoi, xy=TRUE, bind=TRUE) 
  rast <- rasterize(r_extract, esqueleto, r_extract$mean)
  return(rast)
}

PREC_ANUAL <- precip_extract("data/variablesexpl/prec_anual_avg.tif")
names(PREC_ANUAL) <- "PREC_ANUAL"
writeRaster(PREC_ANUAL, "data/variablesexpl/PREC_ANUAL.tiff", overwrite=TRUE)

PREC_INV <- precip_extract("data/variablesexpl/prec_inv_avg.tif")
names(PREC_INV) <- "PREC_INV"
writeRaster(PREC_INV, "data/variablesexpl/PREC_INV.tiff", overwrite=TRUE)

PREC_OTO <- precip_extract("data/variablesexpl/prec_oto_avg.tif")
names(PREC_OTO) <- "PREC_OTO"
writeRaster(PREC_OTO, "data/variablesexpl/PREC_OTO.tiff", overwrite=TRUE)
