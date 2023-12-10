library(biomod2)
library(terra)

# Load species occurrences 
library(readxl)
presencia_filtered<- read_excel("data/presencia_ausencia/presencia_filtered.xlsx")
presencia_filtered$Androcymbium_europaeum <- 1

# Select the name of the studied species
myRespName <- 'Androcymbium_europaeum'

# Get corresponding presence/absence data
myResp <- (presencia_filtered[,myRespName])

# Get corresponding XY coordinates
myRespXY <- presencia_filtered[, c('x','y')]

# Get explanatory variables
myExpl <- rast(list.files(pattern='\\.tiff$', full=TRUE, path=here::here("data/variablesexpl")))
names(myExpl)

# Format data and get pseudoausences 
myResp.PA <- ifelse(myResp == 1, 1, NA)

myBiomodData.r <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = myExpl,
                                       resp.xy = myRespXY,
                                       resp.name = myRespName,
                                       filter.raster = TRUE,
                                       PA.nb.rep = 50,
                                       PA.nb.absences =6000,
                                       PA.strategy = 'random')

########################### METHOD 1
print(myBiomodData.r)
PA.table <- myBiomodData.r@PA.table 
PA.table <- PA.table[2130:259993,]
table(PA.table[1])
table(PA.table[10])
replace_values <- function(x) {
  ifelse(x, 0, NA)
}
PA.table[] <- lapply(PA.table, replace_values)
PA.table$ID <- rownames(xy_Pa.table)
PA.table <- data.frame(t(PA.table))

xy_Pa.table <- myBiomodData.r@coord
xy_Pa.table <- xy_Pa.table[2130:259993,]
xy_Pa.table$ID <- c(1:257864)

PA.table <- merge(PA.table, xy_Pa.table, by="ID")
PA.table$x
library(openxlsx)
write.xlsx(PA.table, "data/cv/PA.table.xlsx")

######################### METHOD 2
https://rstudio-pubs-static.s3.amazonaws.com/416446_3ef37751ae1e4e569964dabc09a75b56.html

library(dplyr)

## function to get PA dataset
get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA.table
  )
}

## function to get background mask
get_mask <- function(bfd){
  bfd@data.mask
}

## get the coordiantes of presences
(pres.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(status == 1) %>%
    select(x, y))
saveRDS(pres.xy, "data/cv/pres.xy.rds")
library(openxlsx)
write.xlsx(pres.xy, "data/cv/pres.xy.xlsx")

## get the coordiantes of pseudo - absences
## all repetition of pseudo absences sampling merged 
(pa.all.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()

(pa.1.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA1 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.2.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA2 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.3.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA3 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.4.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA4 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.5.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA5 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.6.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA6 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.7.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA8 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.8.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA8 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.9.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA9 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.10.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA10 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.11.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA11 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.12.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA12 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.13.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA13 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.14.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA14 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.15.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA15 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.16.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA16 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.17.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA17 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.18.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA18 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.19.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA1 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.20.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA20 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.21.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA21 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.22.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA22 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.23.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA23 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.24.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA24 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.25.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA25 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.26.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA26 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.27.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA27 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.28.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA28 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.29.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA29 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.30.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA30 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.31.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA31 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.32.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA32 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.33.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA33 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.34.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA34 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.35.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA35 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.36.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA36 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.37.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA37 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.38.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA38 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.39.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA39 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.40.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA40 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.41.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA41 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.42.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA42 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.43.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA43 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.44.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA44 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.45.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA45 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.46.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA46 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.47.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA47 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.48.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA48 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.49.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA49 == TRUE) %>%
    select(x, y)) %>%
  distinct()
(pa.50.xy <- get_PAtab(myBiomodData.r) %>% 
    filter(is.na(status) & PA50 == TRUE) %>%
    select(x, y)) %>%
  distinct()

obj <- ls()
obj_pa <- obj[grep("^pa", obj)]
list.pa.xy  <- lapply(seq_along(obj_pa), function(i) {
  nombre_elemento <- paste("PA", i, sep = "")
  setNames(list(get(obj_pa[i])), nombre_elemento)
})


list.pa.xy <- do.call(c, list.pa.xy)
saveRDS(list.pa.xy, file = "data/cv/list.pa.xy.rds")
