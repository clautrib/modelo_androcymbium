library(biomod2)
library(terra)

# DATA ############################################

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

# install.packages("here")
library(here)
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

# CROSS VALIDATION ############################################
# install.packages("ENMeval")
library(ENMeval)
cv.b <- bm_CrossValidation(bm.format = myBiomodData.r,
                           strategy = 'block')

# MODELLING ############################################

load("A.europaeum/A.europaeum.maxent.models.out")

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
  MAXENT = list( path_to_maxent.jar = 'maxent/maxent.jar', 
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


myBiomodModelOut_maxent <- BIOMOD_Modeling(bm.format = myBiomodData.r,
                                     models = "MAXENT",
                                     modeling.id = 'maxent',
                                     bm.options = modelling_options,
                                     CV.strategy = 'user.defined',
                                     CV.user.table= cv.b,
                                     var.import = 1,
                                     metric.eval = c('TSS','ROC'))

# EVALUATIONS ############################################

evaluations_maxent <- get_evaluations(A.europaeum.maxent.models.out)

library(dplyr)

# evaluations_maxent_filter <- evaluations_maxent %>% 
#   filter(validation >= 0.8) 54 MODELS

# evaluations_maxent_filter2 <- evaluations_maxent %>% 
#   filter(validation >= 0.85) 18 MODELS
  
load("A.europaeum/A.europaeum.AllModels.models.out")

evaluations_glm_rf<- get_evaluations(A.europaeum.AllModels.models.out)

# evaluations_glm_rf_filter <- evaluations_glm_rf %>% 
#   filter(validation >= 0.8) 296 MODELS

# evaluations_glm_rf_filter2 <- evaluations_glm_rf %>% 
  # filter(validation >= 0.85) 202 MODELS 

evaluations_glm_rf_filter <- evaluations_glm_rf %>% 
    filter(validation >= 0.85, sensitivity >= 90, specificity  >= 90) 

models_glm_rf <- as.character(evaluations_glm_rf_filter[,1])

evaluations_maxent_filter <- evaluations_maxent %>%
  filter(validation >= 0.85, sensitivity >= 90, specificity  >= 90)

models_maxent <- as.character(evaluations_maxent_filter[,1])
model_names <- c(models_glm_rf, models_maxent)

ensamblaje_all <- BIOMOD_EnsembleModeling(
  A.europaeum.AllModels.models.out,
  models.chosen = models_glm_rf,
  em.by = "all",
  em.algo = "EMca",
  metric.select = "ROC",
  metric.select.thresh = 0.8,
  metric.eval =  "ROC",
  var.import = 1,
  EMci.alpha = 0.05,
  nb.cpu = 4,
  seed.val = NULL,
  do.progress = TRUE,
)

ensamblaje_maxent <- BIOMOD_EnsembleModeling(
  A.europaeum.maxent.models.out,
  models.chosen = models_maxent,
  em.by = "all",
  em.algo = "EMca",
  metric.select = "ROC",
  metric.select.thresh = 0.8,
  metric.eval =  "ROC",
  var.import = 1,
  EMci.alpha = 0.05,
  nb.cpu = 4,
  seed.val = NULL,
  do.progress = TRUE,
)
