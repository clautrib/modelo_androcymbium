


library(mapSpain)
almeria <- esp_get_prov(prov = "Almeria")
almeria
almeria <- st_transform(almeria, 23030)
plot(st_geometry(almeria))


library(terra)

presencia_shp <- vect("data/presencia_ausencia/presencia.shp")
dfpresencia <- terra::as.data.frame(presencia, xy=TRUE, na.rm=TRUE)

#RASTERIZADO DE PRESENCIA

#tif del que sacamos extensión para presencia y esqueleto, y resolución para esqueleto
molde <- rast("data/variablesexpl/FR_MATDE.tiff")
molde

esqueleto <- rast(ncol=875, nrow=1575, xmin=525129.8, xmax=588129.9, ymin=4060209, ymax=4095209)
ext(esqueleto) <- ext(molde)
res(esqueleto) <- res(molde)
crs(esqueleto) <- crs(molde)

esqueleto
molde

esqueleto$valor <- 1
presencia_shp$valor <-1
presencia_rast <- rasterize(presencia_shp, esqueleto, presencia_shp$valor)

df_presencia <- terra::as.data.frame(presencia_rast, xy=TRUE, na.rm=TRUE)
df_presencia
rownames(df_presencia) <- seq(1, 8654)

library(writexl)
write_xlsx(df_presencia, "data/presencia_ausencia/df_presencia.xlsx")

