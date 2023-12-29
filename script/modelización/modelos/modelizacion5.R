load("A.europaeum/A.europaeum.AllModels.models.out")

load("A.europaeum/A.europaeum.maxent.models.out")

library(biomod2)

# CHOOSING BEST MODELS ###############################################################


evaluations_maxent <- get_evaluations(A.europaeum.maxent.models.out)
evaluations_glm_rf<- get_evaluations(A.europaeum.AllModels.models.out)

library(dplyr)

evaluations_glm <- evaluations_glm_rf %>% 
  filter(algo == "GLM") 
best_glm <- evaluations_glm %>% 
  filter(full.name == "A.europaeum_PA11_RUN4_GLM")

evaluations_rf <- evaluations_glm_rf %>% 
  filter(algo == "RF") 
best_rf <- evaluations_rf %>% 
  filter(full.name == "A.europaeum_PA10_RUN2_RF")

best_maxent <- evaluations_maxent %>% 
  filter(full.name == "A.europaeum_PA23_RUN1_MAXENT")

# LOADING BEST MODELS ###############################################################

BIOMOD_LoadModels(A.europaeum.AllModels.models.out, 
                  full.name = c("A.europaeum_PA11_RUN4_GLM","A.europaeum_PA10_RUN2_RF"))

BIOMOD_LoadModels(A.europaeum.maxent.models.out, 
                  full.name = "A.europaeum_PA23_RUN1_MAXENT")

# ENSEMBLE TRY ###############################################################

ensamblaje_1 <- BIOMOD_EnsembleModeling(
  A.europaeum.AllModels.models.out, 
  models.chosen = c("A.europaeum_PA11_RUN4_GLM",
                    "A.europaeum_PA10_RUN2_RF"), 
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
  
# REDOING MODELLING IN modelizacion6 ###############################################################

