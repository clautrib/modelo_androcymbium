##SELECCIÓN DE VARIABLES####################################################### 


# 1. Creación de stack y dataframe de variables predictoras 
library(terra)
library(here)

files <- list.files(pattern='\\.tiff$', full=TRUE, path=here::here("data/variablesexpl/iniciales"))
variablespred <- rast(files) # stack variables ambientales
df <- terra::as.data.frame(variablespred, xy=TRUE, na.rm=TRUE)

# 2.  MATRIZ DE CORRELACIONES 
## 2.1. matriz de correlación básica 

matriz_cor <- cor(df[,3:31]) #correlación de todo el df menos la columna 1 y la 2 

## 2.2. Cor.mtest: 
#declarando una función: cor.mtest (modificada). correlación + p valores de cada correlación 

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

## 2.3. P valor de cada correlación 

p.mat <- cor.mtest(df[,3:31]) # obtención de p valores de cada correlación con la función de arriba

## 2.4. corrplot 
library(corrplot)
#plot ordenado con PCA 
corrplotPCA <- corrplot::corrplot(matriz_cor, method= "square", type = "upper", tl.col = "black", tl.srt = 90, 
                            tl.cex = 0.5,
                            p.mat = p.mat, 
                            sig.level = 0.05, 
                            insig = "blank", 
                            diag = FALSE, 
                            order ="FPC",
                            addCoef.col ='black',
                            number.cex = 0.4, number.digits= 2)


# 3. DENDROGRAMA JERÁRQUICO  

#Primero tomamos una muestra más pequeña de la dataframe original 

library(dplyr)
smalldf <- df[sample(nrow(df), 10000),]
smalldf$x <- NULL
smalldf$y <- NULL

smalldf %>% mutate_all(~(scale(.) %>% as.vector))

## 3.1. Paquete clust of var

library(ClustOfVar)
tree <- hclustvar(X.quanti= smalldf)

dendro <- plot(tree)

## 3.2. estabilidad 
#La estabilidad hace referencia a el nº óptimo de clusters en el árbol 

stab <- stability(tree, B=20) # "B=50" refers to the number of bootstrap samples to use in the estimation.
stab$meanCR
plot(stab)

rect <- rect.hclust(tree, k=9, border="red")
rect

cut <- cutreevar(tree, k = 9, matsim = TRUE)
cut
cut$var
cut$coef


# 4. VIF 
#colinealidad entre las variables, para calcularlo necesito hacer un modelo 

## 4.1. Creación del modelo 
library(readxl)
presencia <- read_excel("data/presencia_ausencia/df_presencia_eval.xlsx")
dfmodelo <- merge(df, presencia, by = c("x", "y"), all=TRUE) #unir las dos df por campos en común, y los datos que no coincidan rellenar con NA 
table(is.na(dfmodelo$layer))

dfmodelo  <- mutate_at(dfmodelo, "layer", ~replace(., is.na(.), 0)) #cambiar los NAs por 0 en las columnas seleccionadas 

library(car)
dfmodelo <- dplyr::select(dfmodelo, -x, -y) #eliminar dos columnas con el paquete dplyr
head(dfmodelo)

modelo <- glm(layer~ DS_URBAN + HIDRO_ACUM+ TP_RSD_A+CLI_TMNI+TP_ES_OE
              +HIDRO_ITH+ DS_AGUA+ DS_CULTI+ HIDRO_ACUM+ FR_VIASC +TP_SU_NO+PREC_ANUAL+PREC_OTO+PREC_INV+TP_RSH_I+TP_EXPO+FR_CULTI+FR_MATDE, family=binomial, data=dfmodelo) #glm binomial binaria xq la variable dependiente es de tipo 1/0

## 4.2. evaluación del VIF 
summary(modelo)
vif <- vif(modelo)


modelo2 <- glm(layer~ DS_URBAN + HIDRO_ACUM+ TP_RSD_A+CLI_TMNI+TP_ES_OE
               +HIDRO_ITH+ DS_AGUA+ DS_CULTI+ HIDRO_ACUM+ FR_VIASC +TP_SU_NO+PREC_OTO+PREC_INV+TP_RSH_I+TP_EXPO+FR_CULTI+FR_MATDE+TP_PEND, family=binomial, data=dfmodelo) #glm binomial binaria xq la variable dependiente es de tipo 1/0

vif(modelo2)









