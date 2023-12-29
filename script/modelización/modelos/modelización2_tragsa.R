
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
                                     models = c("GLM", "MAXENT", "RF"),
                                     modeling.id = 'AllModels',
                                     CV.strategy = 'user.defined',
                                     CV.user.table= cv.b,
                                     var.import = 3,
                                     metric.eval = c('TSS','ROC'))



load("A.europaeum/A.europaeum.AllModels.models.out")

# EVALUATION AND RESPONSE CURVES ###############################################

# nombre de todos los modelos construidos 
built_models <-
  get_built_models(
    A.europaeum.AllModels.models.out,
    full.name = NULL,
    PA = NULL,
    run = NULL,
    algo = NULL
  )

# Get evaluation scores & variables importance
evaluations_glm_rf <- get_evaluations(A.europaeum.AllModels.models.out)
var_importance_glm_rf <- get_variables_importance(A.europaeum.AllModels.models.out)



# valor de importancia de cada variable por cada modelo construido 
data_var_importance_glm_rf <- get_variables_importance(
  myBiomodModelOut1,
  full.name = NULL,
  PA = NULL,
  run = NULL,
  algo = NULL,
  expl.var = NULL
)

# métricas de evaluación por cada modelo construido 
data_evaluations_glm_rf <- get_evaluations(
  A.europaeum.AllModels.models.out,
  full.name = NULL,
  PA = NULL,
  run = NULL,
  algo = NULL,
  metric.eval = NULL
)

library(dplyr)

models_ensamblaje <- data_evaluations_glm_rf %>% 
  filter(validation > 0.8) 

names_models_ensamblaje <- as.character(models_ensamblaje[,1]) 




# Represent evaluation scores & variables importance
plot_eval_mean_calib <- bm_PlotEvalMean(
  bm.out = A.europaeum.AllModels.models.out,
  group.by = c("algo"),
  metric.eval = c("ROC", "TSS"), 
  dataset = "calibration",
  main = "Evaluation metrics: mean (by algorithm). Calibration dataset"
)

plot_eval_mean_valid <- bm_PlotEvalMean(
  bm.out = A.europaeum.AllModels.models.out,
  group.by = c("algo"),
  metric.eval = c("ROC", "TSS"), 
  dataset = "validation",
  main = "Evaluation metrics: mean (by algorithm). Validation dataset"
)

plot_eval_box_calib <-
  bm_PlotEvalBoxplot(
    bm.out = A.europaeum.AllModels.models.out,
    group.by = c("PA", "algo"),
    dataset = "calibration", 
    main = "Evaluation metrics: boxplot (by algorithm, and PA). Calibration dataset",
  )
plot_eval_box_calib_2 <-
  bm_PlotEvalBoxplot(
    bm.out = A.europaeum.AllModels.models.out,
    group.by = c("algo", "algo"),
    dataset = "calibration", 
    main = "Evaluation metrics: boxplot (by algorithm). Calibration dataset",
  )


plot_eval_box_valid <-
  bm_PlotEvalBoxplot(
    bm.out = A.europaeum.AllModels.models.out,
    group.by = c("PA", "algo"),
    dataset = "validation", 
    main = "Evaluation metrics: boxplot (by algorithm, and PA). Validation dataset",
  )
plot_eval_box_valid_2 <-
  bm_PlotEvalBoxplot(
    bm.out = A.europaeum.AllModels.models.out,
    group.by = c("algo", "algo"),
    dataset = "validation", 
    main = "Evaluation metrics: boxplot (by algorithm, and PA). Validation dataset",
  )
# bm_PlotEvalBoxplot(bm.out = myBiomodModelOutRF_kfold, group.by = c("run", "algo"), dataset= "calibration") 
# bm_PlotEvalBoxplot(bm.out = myBiomodModelOutRF_kfold, group.by = c("PA", "algo"), dataset= "evaluation") 
# bm_PlotEvalBoxplot(bm.out = myBiomodModelOutRF_kfold, group.by = c('full.name', 'run'))


var_importance_PA <-
  bm_PlotVarImpBoxplot(bm.out = A.europaeum.AllModels.models.out,
                       group.by = c('expl.var', "algo", 'PA')) 

var_importance_algo <-
  bm_PlotVarImpBoxplot(bm.out = A.europaeum.AllModels.models.out,
                       group.by = c('expl.var', "algo", 'algo'), 
                       main= "Variable importance (by algorithm)") 
var_importance_run <-
  bm_PlotVarImpBoxplot(bm.out = A.europaeum.AllModels.models.out,
                       group.by = c('expl.var', "algo", 'run')) 


# Represent response curves
bm_PlotResponseCurves(
  bm.out = A.europaeum.AllModels.models.out,
  models.chosen = get_built_models(A.europaeum.AllModels.models.out)[c(1, 2, 75, 76, 157, 158, 239, 240, 313, 314, 394, 396)],
  fixed.var = 'mean'
)

# ENSAMBLAJE ###################################################################

ensamblaje <- BIOMOD_EnsembleModeling(
  A.europaeum.AllModels.models.out,
  models.chosen = "all",
  em.by = "PA+run",
  em.algo = c("EMci", "EMca"),
  metric.select = "all",
  metric.select.thresh = c(0.8, 0.8),
  metric.eval = c("TSS", "ROC"),
  var.import = 1,
  EMci.alpha = 0.05,
  nb.cpu = 4,
  seed.val = NULL,
  do.progress = TRUE,
)

models_ensamblaje <- ensamblaje@em.models_kept

ensamblaje_all <- BIOMOD_EnsembleModeling(
  A.europaeum.AllModels.models.out,
  models.chosen = "all",
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

load("A.europaeum/.BIOMOD_DATA/AllModels/ensemble.models/models.prediction")

prediction <- objValue %>% 
  filter(algo == "EMca") %>% 
  filter(pred == "1000")

ensamblaje_select <- BIOMOD_EnsembleModeling(
  A.europaeum.AllModels.models.out,
  models.chosen = names_models_ensamblaje,
  em.by = "all",
  em.algo = "EMca",
  metric.eval =  "ROC",
  var.import = 1,
  EMci.alpha = 0.05,
  nb.cpu = 4,
  seed.val = NULL,
  do.progress = TRUE,
)

# Get evaluation scores & variables importance
eval_ensamblaje <- get_evaluations(ensamblaje)
varimp_ensamblaje <- get_variables_importance(ensamblaje)


bm_PlotEvalMean(bm.out = ensamblaje, dataset = 'calibration')
bm_PlotEvalBoxplot(bm.out = ensamblaje, group.by = c('algo', 'algo'))


bm_PlotEvalMean(
  bm.out = ensamblaje,
  group.by = c("algo"),
  metric.eval = c("ROC", "TSS"), 
  dataset = "calibration",
  main = "Evaluation metrics: mean (by algorithm). Calibration dataset"
)

var_importance_algo <-
  bm_PlotVarImpBoxplot(bm.out = ensamblaje,
                       group.by = c('expl.var', "algo", 'algo'), 
                       main= "Ensemble: Variable importance (by algorithm)") 

  # PROJECTION ###################################################################
proj_ensamblaje <- BIOMOD_EnsembleForecasting(
  ensamblaje,
  bm.proj = NULL,
  proj.name = "ensamblaje",
  new.env = myExpl,
  new.env.xy = NULL,
  models.chosen = "all",
  metric.binary = "all",
  metric.filter = "all",
  compress = TRUE,
  nb.cpu = 4,
  na.rm = TRUE
)
