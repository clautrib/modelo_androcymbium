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

# MODELLING OPTIONS ###########################################

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

myBiomodModelOut_DEF <- BIOMOD_Modeling(bm.format = myBiomodData.r,
                                     models = c("GLM", "MAXENT", "RF"),
                                     modeling.id = '3_algorithms',
                                     bm.options = modelling_options,
                                     CV.strategy = 'user.defined',
                                     CV.user.table= cv.b,
                                     var.import = 3,
                                     metric.eval = c('TSS','ROC'))

# CHOOSING BEST MODELS ###############################################################

library(dplyr)

evaluations <- get_evaluations(myBiomodModelOut_DEF)

evaluations_glm <- evaluations %>% 
  filter(algo == "GLM") 
best_glm_name <- "A.europaeum_PA44_RUN4_GLM"
best_glm <- evaluations_glm %>% 
  filter(full.name == "A.europaeum_PA44_RUN4_GLM")

evaluations_rf <- evaluations %>% 
  filter(algo == "RF")
best_rf_name <- "A.europaeum_PA17_RUN2_RF"
best_rf <- evaluations_rf %>% 
  filter(full.name == "A.europaeum_PA17_RUN2_RF")

evaluations_maxent <- evaluations %>% 
  filter(algo == "MAXENT")
best_maxent_name <- "A.europaeum_PA29_RUN1_MAXENT"
best_maxent <- evaluations_maxent %>% 
  filter(full.name == "A.europaeum_PA29_RUN1_MAXENT")

# ENSEMBLE ###############################################################

ensamblaje <- BIOMOD_EnsembleModeling(
  myBiomodModelOut_DEF, 
  models.chosen = c("A.europaeum_PA29_RUN1_MAXENT",
                    "A.europaeum_PA17_RUN2_RF", 
                    "A.europaeum_PA44_RUN4_GLM"), 
  em.by = "all",
  em.algo = "EMca",
  metric.select = "ROC",
  metric.select.thresh = 0.8,
  metric.eval =  "ROC",
  var.import = 1,
  EMci.alpha = 0.05,
  seed.val = NULL,
  do.progress = TRUE,
)

var.imp <- get_variables_importance(ensamblaje)

bm_PlotVarImpBoxplot(
  ensamblaje,
  group.by = c("expl.var", "expl.var", "algo"),
  do.plot = TRUE
)

predictions <- get_predictions(ensamblaje)

# ENSEMBLE ###############################################################

ensamblaje_proj <- BIOMOD_EnsembleForecasting(bm.em = ensamblaje,
                                             proj.name = "3_algorithms",
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')
plot(ensamblaje_proj)
