library(sf) # working with spatial vector data
library(terra) 
#install.packages("tmap")
library(tmap) # plotting maps

# load raster data
rasters <- rast(list.files(pattern='\\.tiff$', full=TRUE, path=here::here("data/variablesexpl")))

# load species presence-absence data and convert to sf
library(readxl)

points <- read_excel("data/presencia_ausencia/df_presencia_eval.xlsx")
head(points)

pres_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 23030)
plot(pres_data)


#install.packages("blockCV")
library(blockCV)


# exploring the effective range of spatial autocorrelation in raster covariates or sample data
spatial_autocor <- cv_spatial_autocor(r = rasters, # a SpatRaster object or path to files
                   num_sample = 50000, # number of cells to be used
                   plot = TRUE)
spatial_autocor$range

#RESULTS-rasters: 7930.596

spatial_autocor_pres <- cv_spatial_autocor(x = pres_data, # a SpatRaster object or path to files
                                     column = "layer", # number of cells to be used
                                      plot = TRUE)
spatial_autocor_pres$range
#RESULTS-presencia: 7016.081

# spatial blocking by specified range and systematic assignment
spatialblock_systematic <- cv_spatial(
  x = pres_data,
  column = "layer", # the response column (binary or multi-class)
  r = rasters,
  k = 5,
  hexagon = TRUE, # number of folds
  size = 7016.081, # size of the blocks in metres
  selection = "systematic", # stystematic blocks-to-fold
  iteration = 50, # find evenly dispersed folds
  progress = TRUE, # turn on progress bar
  biomod2 = TRUE, # also create folds for biomod2
  raster_colors = terrain.colors(10, rev = TRUE) # options from cv_plot for a better colour contrast
) 


spatialblock_systematic_biomod <- spatialblock_systematic$biomod_table
library(openxlsx)
write.xlsx(spatialblock_systematic_biomod,"data/cv/cv_table_blockCV_systematic.xlsx" )

# spatial blocking by specified range and random assignment
spatialblock_random <- cv_spatial(
  x = pres_data,
  column = "layer", # the response column (binary or multi-class)
  r = rasters,
  k = 5,
  hexagon = FALSE, # number of folds
  size = 7016.081, # size of the blocks in metres
  selection = "random", # stystematic blocks-to-fold
  iteration = 50, # find evenly dispersed folds
  progress = TRUE, # turn on progress bar
  biomod2 = TRUE, # also create folds for biomod2
  raster_colors = terrain.colors(10, rev = TRUE) # options from cv_plot for a better colour contrast
) 

spatialblock_random_biomod <- spatialblock_random$biomod_table
write.xlsx(spatialblock_random_biomod,"data/cv/cv_table_blockCV_random.xlsx")

# visualizing testing and training folds 

cv_plot(
  cv = spatialblock_random, # cv object
  x = pres_data, 
  num_plots = 1:5) 
