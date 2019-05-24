install.packages(c("FactoMineR", "factoextra")) #paquetes para ACP
install.packages("corrplot") #para visualizar las correlaciones
library(tidyverse)
library(haven)
library(summarytools)
library(FactoMineR)
library(factoextra)
library(corrplot)
#cargamos la base 

edfisica_df <- read_sav("/Users/mariodominguez/Documents/Cuanti 2 Usach/clase factoriales/cuestionario docentes 26 de julio.sav")
edfisica_tb <- as_tibble(edfisica_df) #lo transformamos a un formato tibble

#análisis factorial exploratorio.

names(edfisica_tb)
edfisica.active <- edfisica_tb %>% select(c(10, 6, 28:42)) #creamos un df con las variables activas.
names(edfisica.active)

#función del ACP
PCA(edfisica.active, scale.unit = TRUE, ncp = 5, graph = TRUE) #esta es la función con 5 dimensiones y gráficos.
res.pca <- PCA(edfisica.active, graph = FALSE) #función sin restricción de dimensiones ni gráficos

print(res.pca)

#interpretación de autovalores (eigenvalues) y varianza total explicada.
eig.val <- get_eigenvalue(res.pca) 
eig.val

#Visualización de los facotres extraídos
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30)) 

#extraemos los facores seleccionados.
var <- get_pca_var(res.pca)
var

#observamos las variable extraídas

# Coordinates:coordenadas de variables para crear un diagrama de dispersión
head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black") #visualización

# Cos2: quality on the factore map:representa la calidad de representación de las 
#variables en el mapa de factores.

head(var$cos2, 4)
corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

#Coloreamos el gráfico de variables y dimensiones con gradientes
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping 
             )
fviz_pca_var(res.pca, alpha.var = "cos2") #versión transparente

# Contributions to the principal components:contiene las contribuciones (en porcentaje)
# de las variables a los componentes principales.
head(var$contrib)

#visualizamos las contribuciones
corrplot(var$contrib, is.corr=FALSE)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

#Total de contribuciones de las variables a las dimensiones 1 y 2
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

#Las variables más importantes (o que contribuyen) se pueden resaltar en la gráfica
#de correlación de la siguiente manera:

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Puede hacerse transparente el gráfico de contribuciones
fviz_pca_var(res.pca, alpha.var = "contrib")

# Crea una variable aleatoria continua de longitud 17 (numero de variables activas)
set.seed(123) 
my.cont.var <- rnorm(17)

#Variables de color por la variable continua.
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

