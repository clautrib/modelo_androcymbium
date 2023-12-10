install.packages("remotes")
remotes::install_github("sjevelazco/flexsdm")

library(readxl)
presencia <- read_excel("data/presencia_ausencia/df_presencia_eval.xlsx")
presencia$idd <- 1:nrow(presencia)

files <- list.files(pattern='\\.tiff$', full=TRUE, path=here::here("data/variablesexpl"))
variablespred <- rast(files) # stack variables ambientales

library(flexsdm)
presencia_filtered <- occfilt_env(
  data = presencia,
  x = "x",
  y = "y",
  id = "idd",
  env_layer = variablespred,
  nbins = 5
)

library(openxlsx)
write.xlsx(presencia_filtered,"data/presencia_ausencia/presencia_filtered.xlsx" )
