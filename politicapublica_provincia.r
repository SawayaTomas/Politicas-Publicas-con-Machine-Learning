argentina <- read.csv("../Datos por provincia/argentina.csv", encoding="UTF-8")
library(purrr)
library(cluster)
library(markdown)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(corrplot)
library(FactoMineR)
library(factoextra)

library(devtools)
library(dplyr)
library(ggrepel)
library(rpart)
library(rpart.plot)
library(mclust)
library(caret)
library(lattice)
library(ggmap)
library(knitr)
library(FactoMineR)

knit('InformeProvincias.Rmd', encoding = 'UTF-8')

#Inspeccionamos primeramente el dataset
n = nrow(argentina)
head(argentina)

summary(argentina)


#GDP is a measure of the size of a province's economy. 
#To measure how rich or poor the inhabitants are, economists use per capita GDP,
#which is GDP divided by the province's population.

# Calculamos el pbi per capita

argentina <- argentina %>% 
  mutate(gdp_per_cap = gdp / pop) 

#Calculamos la pobreza per capita



# Top 10 de provincias mas ricas (pbi per capita)

(richProv  <- argentina %>% 
    arrange(by=-gdp_per_cap) %>%
    select(province,gdp_per_cap) %>%
    top_n(10))

#graficos el top10 de provincias mas ricas

ggplot(richProv, aes(x=reorder(province,-gdp_per_cap), gdp_per_cap, fill=province)) +
  geom_bar(stat = "identity", color="black", fill=("#82E0AA"), alpha=0.4) + 
  labs(title = "Top 10 Provincias más ricas", 
       x = "Provincias", y = "PBI per Cap") +
  theme_bw()

  coord_flip()

#fill = rainbow(n=length(richProv$province))
  
# Buscamos el top 10 de provincias con mayor poblacion

(mayor_pop <- argentina %>% 
    arrange(by=-pop)%>%
    select(province,pop)%>%
    #filter(pop>1500000) )
    top_n(10))
  
#Graficamos el top 10 de provincias con > poblacion
  
  ggplot(mayor_pop, aes(x=reorder(province,-pop), pop, fill=province)) +
    geom_bar(stat = "identity", color="black", fill=("#82E0AA")) +
    labs(title = "Top 10 Provincias con Mayor población", 
         x = "Provincias", y = "Población") +
    theme_bw()

# Buscamos el top 10 de provincias mas pobres

(altaPobreza <- argentina %>% 
    arrange(by=-poverty)%>%
    select(province,poverty)%>%
    #filter(pop>1500000) )
    top_n(10))
  
#Graficamos el top 10 de provincias mas pobres
  
  ggplot(altaPobreza, aes(x=reorder(province,-poverty), poverty, fill=province)) +
    geom_bar(stat = "identity", color="black", fill=("#82E0AA")) +
    labs(title = "Top 10 Provincias con más pobres", 
         x = "Provincias", y = "Pobreza") +
    coord_polar() + 
    theme_bw()
  
###################GRAFICOS-PRUEBAS######################
 

  
  


#@####################################################################

#Graficamos un mapa de correlaciones (analisis de componentes principales)

corr <- cor(argentina[,-1])
corrplot(corr, method = "circle",
         addgrid.col = "blue", tl.col="black",
         tl.cex=0.6, tl.srt=25)
argentina.acp <- prcomp(argentina[,-1], scale = T)
plot(argentina.acp)
plot(argentina.acp, type = "lines")

#graficamos un biplot para asegurarnos las correctas correlaciones
#a traves del analisis de componentes principales
biplot(argentina.acp, col = c("black", "blue"),
       main = "Gráfico de BiPlot", xlab=NULL)
argentina.acp$rotation
argentina.acp$sdev
summary(argentina.acp)

biplot(argentina.acp, scale = 0)

(argentina.acp2  <- PCA(argentina[,-1], scale.unit = TRUE))


#####Probando factomineR



#Que vemos aca? que pbi per capita y pob estan totalmente correlacionados
#pbi per capita y cines estan correlacionados
#abandono escolar, pobreza, analfabetismo, deficit infra, etc estan correlacionadas

#las lineas que estan cerca estan correlacionadas
#las correlaciones negativas se dan en los lados opuestos


#####Y por si fuera poco ponemos este grafico que es tremendo:
#Sobre las correlacioens entre los componentes

ggpairs(argentina[,2:8],
        aes(colour = "Reds", 
            alpha = 0.4),
        title = "Análisis multivariante de componentes", 
        upper = list(continuous = "density"),
        lower = list(combo="denstrip"))+
  theme(plot.title = element_text(hjust=0.5))

#Analizando los clusters con K-MEANS

set.seed(1500)

argentina.s <- scale(argentina[,2:11])

km <- kmeans(argentina.s,5)
aggregate(argentina.s, by = list(cluster = km$cluster), mean)
fviz_cluster(km, data = argentina.s, main = "Clusters por Provincias",
             xlab = FALSE, ylab = FALSE,
             labelsize = 10,
             repel = TRUE,
             ggtheme = theme_bw())

clusters_as_factor<-factor(km$cluster)
fviz_pca_ind(argentina.s, 
             title = "Clustered Provinces - PCA", 
             habillage = clusters_as_factor)

argentina <- argentina %>%
  mutate(cluster=clusters_as_factor)

argentina$cluster <- factor(argentina$cluster,
                            levels = c(1,2,3,4,5),
                            labels = c("Grupo 1", "Grupo 2", "Grupo 3",
                                       "Grupo 4", "Grupo 5"))



#cluster por pbi
ggplot(argentina, aes(cluster, gdp, color = cluster)) +
  geom_point()+
  geom_text_repel(aes(label = province), show.legend = TRUE) +
  labs(x = "Cluster", y = "GDP")

#cluster por pbi per capita
ggplot(argentina, aes(cluster, gdp_per_cap, color = cluster)) +
  geom_point()+
  geom_text_repel(aes(label = province), show.legend = T) +
  labs(x = "Cluster", y = "GDP per capita")

#cluster por pobreza
ggplot(argentina, aes(cluster, poverty, color = cluster)) +
  geom_jitter()+
  labs(x = "Cluster", y = "Poverty rate") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

#cluster por no asistencia sanitaria
ggplot(argentina, aes(cluster, no_healthcare, color = cluster)) +
  geom_jitter()+
  labs(x = "Cluster", y = "Sin asistencia sanitaria") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

#cluster por infra deficiente 
ggplot(argentina, aes(cluster, deficient_infra, color = cluster)) +
  geom_jitter()+
  labs(x = "Clusters", y = "Infraestructura Deficiente") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

#cluster por analfabetismo 
ggplot(argentina, aes(cluster, illiteracy, color = cluster)) +
  geom_jitter()+
  labs(x = "Clusters", y = "Analfabetismo") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

#cluster por abandono escolar 
ggplot(argentina, aes(cluster, school_dropout, color = cluster)) +
  geom_jitter()+
  labs(x = "Clusters", y = "Abandono Escolar") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

#cluster por Doctores per capita 
ggplot(argentina, aes(cluster, doctors_per_cap, color = cluster)) +
  geom_jitter()+
  labs(x = "Clusters", y = "Doctores per Cápita") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

#cluster por Mortalidad al nacer 
ggplot(argentina, aes(cluster, birth_mortal, color = cluster)) +
  geom_jitter()+
  labs(x = "Clusters", y = "Mortalidad al Nacer") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

#cluster por cines
ggplot(argentina, aes(cluster, movie_theatres_per_cap, color = cluster)) +
  geom_jitter()+
  labs(x = "Clusters", y = "Cines per Cápita") +
  geom_text_repel(aes(label = province), show.legend = TRUE)

########################################################
#Creando modelo de regresion para estimar variables

#Empezamos con la estimacion de la pobreza

set.seed(2018)
t.id <- createDataPartition(argentina$poverty, p = 0.7, list = F)

mod <- lm(poverty ~ ., data = argentina[t.id,-1])
boxplot(mod$residuals)
sqrt(mean((mod$fitted.values - argentina[t.id,]$poverty)^2)) # 1.629835

pred <- predict(mod, argentina[-t.id, -1])
sqrt(mean((pred - argentina[-t.id,]$poverty)^2))

par(mfrow=c(2,2))
plot(mod)

par(mfrow=c(1,1))


