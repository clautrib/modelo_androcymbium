library(biomod2)
library(terra)

# Load species occurrences (6 species available)
library(readxl)

data <- read_excel("data/presencia_ausencia/presencia_filtered.xlsx")
head(data)
data$A_europaeum <- 1

# Select the name of the studied species
myRespName <- "A_europaeum"

# Get corresponding presence/absence data
myResp <- as.numeric(data$A_europaeum)

# Get corresponding XY coordinates
myRespXY <- data[, c('x', 'y')]

# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
# myExpl <- rast(list.files("data/variablesexpl/", pattern = ".tiff"))

myExpl <- rast(list.files(pattern='\\.tiff$', full=TRUE, path=here::here("data/variablesexpl")))


# Transform true absences into potential pseudo-absences
myResp.PA <- ifelse(myResp == 1, 1, NA)

# Format Data with pseudo-absences : random method
myBiomodData.r <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                       expl.var = myExpl,
                                       resp.xy = myRespXY,
                                       resp.name = myRespName,
                                       PA.nb.rep = 50,
                                       PA.nb.absences = 6000,
                                       PA.strategy = 'random')

# Cross Validation 
install.packages("ENMeval")
library(ENMeval)
cv.b <- bm_CrossValidation(bm.format = myBiomodData.r,
                           strategy = 'block')


bm_DefaultModelingOptions()

modelling_options <-BIOMOD_ModelingOptions(
  GLM = list( type = 'quadratic',
              interaction.level = 0,
              myFormula = NULL,
              test = 'AIC',
              family = binomial(link = 'logit'),
              mustart = 0.5,
              control = glm.control(epsilon = 1e-08, maxit = 50, trace = FALSE) ),
  RF = list( do.classif = TRUE,
             ntree = 500,
             mtry = 'default',
             sampsize = NULL,
             nodesize = 5,
             maxnodes = NULL),
  MAXENT = list( path_to_maxent.jar = 'C:/Users/triba/Desktop/UGR/TFM/modelo_androcymbium', 
                 memory_allocated = 512,
                 initial_heap_size = NULL,
                 maximum_heap_size = NULL,
                 background_data_dir = 'default',
                 maximumbackground = 'default',
                 maximumiterations = 200,
                 visible = FALSE,
                 linear = TRUE,
                 quadratic = TRUE,
                 product = TRUE,
                 threshold = TRUE,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = -1,
                 beta_categorical = -1,
                 beta_lqp = -1,
                 beta_hinge = -1,
                 betamultiplier = 1,
                 defaultprevalence = 0.5),
  
)


myBiomodModelOut1 <- BIOMOD_Modeling(bm.format = myBiomodData.r,
                                     models.pa = list("PA1"),
                                     modeling.id = 'AllModels',
                                     CV.strategy = 'user.defined',
                                     CV.user.table= cv_biomod_block_tables[["PA1"]],
                                     var.import = 3,
                                     metric.eval = c('TSS','ROC'))