# AREVALO NIETO MARCH 2024    #
# II second part analysis.    #
# working with the final data #
# =========================== #
#install.packages("sandwich")
library(janitor)  ## Data cleaning
library(naniar) ## Data faltante, % de data que falta
library(gt)  ## Para la creation de tablas
library(gtsummary)   ## Para la creation de tablas
library(ciTools) ## Generación de intervalos de confianza
library(sandwich) ## Funcion de mejora en la estimación de la regresion 
#install.packages("Hmisc")
library(Hmisc)
#install.packages("flextable")
library(flextable) ## generar los archivos de las tablas
library(data.table)
library(sp)
library(rgdal)  # for vector work; sp package should always load with rgdal 
library (raster)   # for metadata/attributes- vectors or rasters
library(elevatr)
library(ggmap)
library(sf)
library(forcats)
# ***** NOTA : PUEDEN COMENZAR DESDE LA LINEA 81 PARA QUE PUEDAN REPRODUCIR EL CODIGO ******

# Organización del código: 
# PARTE I, lineas 29-69 seleccionando campos y cambiando clases de algunas columnas
# PARTE II, lineas 70-159 seleccionando variables independientes cuantitivas y explorando
# con análisis descriptivos, plots y análisis univariados para VI CUANTI
# PARTE III lineas 162-464 seleccionando variables independientes cualitativas,explorando
# con analisis y plots descriptivos, finalmente analisis univariados para VI CUALI
# PARTE IV lineas 465- análisis multivariado  

# PARTE I
# reading  shape file final #
# ========================== #
dat_all <- read_sf("~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/dat_all03_03_2024.shp") 

# escogiendo los campos a usar #
dat_analysis<-dat_all|>
  dplyr::select(id,ndvi,LONGITU,LATITUD,SECTOR,estd_ns,ambnt_nm_x,ambnt_nm_y,
                AVE_INTRA,CUY_INTRA,AVE_PERI,CUY_PERI,GAT_PER,GAT_INT,PER_PER,PER_INT,
                n_femal,n_male,fumigad,insct__,ADOB,ADOB_RE,BLOQ_NO,CALAM,CARR,
                CART,DRYW,LADR_NO,MAD,MET,NOBL,PIED_NO,PIED_PI,PLAS,TEJ,TEL,rastros,
                cap_ttl,pos_ttl,INF,POS,AVE_INTRA_,AVE_PERI_,CUY_INTRA_,CUY_PERI_,elevation)

# CHOOSING ONLY HOUSES INSPECTED #
# ============================== #
dat_analysis<-dat_analysis%>%na.omit(dat_analysis)
sapply(dat_analysis, class)

# univariate analysis.     #
# convirtiendo a dataframe #
# ===================.     #

dt_analysis<-as.data.frame(dat_analysis)
pillar::glimpse(dt_analysis)

# multiplying by the factor
dt_analysis$ndvi<-dt_analysis$ndvi*0.0001 # manual ndvi scale factor

# original data base #
# ================== #
dt_analysis<-dt_analysis %>%  dplyr::select(SECTOR,ndvi,elevation,ambnt_nm_x,ambnt_nm_y,AVE_PERI,AVE_INTRA,CUY_PERI,CUY_INTRA,PER_PER,PER_INT,
                                      GAT_PER,GAT_INT,AVE_INTRA_,CUY_INTRA_,AVE_PERI_,CUY_PERI_,n_femal,n_male,fumigad,rastros,cap_ttl,pos_ttl,INF,POS,
                                      ADOB,ADOB_RE,CARR,TEJ,TEL,PLAS,NOBL,LADR_NO,NOBL,CALAM)
dt_analysis <- dt_analysis %>%
  mutate(CUY_PERI_ = as.factor(CUY_PERI_))
dt_analysis <- dt_analysis %>%
  mutate(CUY_INTRA_ =as.factor(CUY_INTRA_))
dt_analysis <- dt_analysis %>%
  mutate(AVE_INTRA_ = as.factor(AVE_INTRA_))
dt_analysis <- dt_analysis %>%
  mutate(AVE_PERI_ = as.factor(AVE_PERI_))
# creando numero total de personas #
# ================================ #
dt_analysis$n_people<-dt_analysis$n_male+dt_analysis$n_femal

#write.csv(dt_analysis,"~/NORTH_ZDRL/data/dt_analysis18_mar_2024.csv",row.names = FALSE)


dt_analysis<-read.csv("~/NORTH_ZDRL/data/dt_analysis18_mar_2024.csv")

# Creando datasets para distintos analisis #                 #
# ======================================== #
# data con variables cuantitativas discretas y continuas #
# ====================================================== #
dt_cuanti<-dt_analysis %>% dplyr::select(ndvi,elevation,AVE_PERI,AVE_INTRA,CUY_PERI,CUY_INTRA,PER_PER,PER_INT,
                                      GAT_PER,GAT_INT,n_femal,n_male,cap_ttl,pos_ttl,n_people)

#     analisis descriptivos         # 
# cuantitative exploratory analysis #
# ================================= #
# https://stats.oarc.ucla.edu/other/mult-pkg/whatstat/
# Describing cuant variables #
# ========================== #
a<-describe(dt_cuanti)

# Ploting to check relationships #
# ============================== #
library(tidyr)
data_long <- dt_cuanti %>%                        # Apply pivot_longer function
  pivot_longer(colnames(dt_cuanti)) %>% 
  as.data.frame()
head(data_long)   

ggp1 <- ggplot(data_long, aes(x = value)) +    # Draw each column as histogram
  geom_histogram() + 
  facet_wrap(~ name, scales = "free")
ggp1

ggp2 <- ggplot(data_long, aes(x = value)) +    # Draw each column as density
  geom_density() + 
  facet_wrap(~ name, scales = "free")
ggp2
ggp3 <- ggplot(data_long, aes(x = value)) +    # Draw histogram & density
  geom_histogram(aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) + 
  facet_wrap(~ name, scales = "free")
ggp3


plot(cap_ttl ~AVE_PERI,data = dt_analysis)
plot(cap_ttl ~CUY_INTRA,data = dt_analysis)
plot(cap_ttl ~ndvi,data = dt_analysis)
plot(cap_ttl ~elevation,data = dt_analysis)

plot(x = dt_cuanti$ndvi,        # X coordinates
     y = dt_cuanti$cap_ttl)
model <- lm(formula = cap_ttl ~ ndvi, 
            data = dt_cuanti)
abline(model, col = 'blue')

plot(x = dt_cuanti$elevation,        # X coordinates
     y = dt_cuanti$cap_ttl)
model <- lm(formula = cap_ttl ~ elevation, 
            data = dt_cuanti)
abline(model, col = 'blue')

plot(x = dt_cuanti$AVE_PERI,        # X coordinates
     y = dt_cuanti$cap_ttl)
model <- lm(formula = cap_ttl ~ AVE_PERI, 
            data = dt_cuanti)
abline(model, col = 'blue')

# evaluation of asumptions in the data #
# normalidad                           #
# ==================================== #
lapply(dt_cuanti, shapiro.test)

# la univa variable con distribucion normal es elevacion #
# en todas las demas deberemos usar analysis bivariados no parametricos #

# UNIVARIATE ANALYSIS #
# =================== #
# y (infested=1, non infested=0)
wilcox.test(dt_analysis$INF, dt_cuanti$ndvi)
wilcox.test(dt_analysis$INF, dt_cuanti$AVE_PERI)
wilcox.test(dt_analysis$INF, dt_cuanti$CUY_INTRA)
wilcox.test(dt_analysis$INF, dt_cuanti$n_people)
t.test(dt_analysis$INF, dt_analysis$elevation)

# y (t cruzi=1, non t.cruzi=0)
wilcox.test(dt_analysis$POS, dt_cuanti$ndvi)
wilcox.test(dt_analysis$POS, dt_cuanti$AVE_PERI)
wilcox.test(dt_analysis$POS, dt_cuanti$CUY_INTRA)
wilcox.test(dt_analysis$POS, dt_cuanti$n_people)
t.test(dt_analysis$POS, dt_analysis$elevation)


# PARTE III
# data con variables cualitativas #
# =============================== #
dt_cuali<-dt_analysis %>%dplyr::select(SECTOR,ambnt_nm_x,ambnt_nm_y,AVE_INTRA_,CUY_INTRA_,AVE_PERI_,CUY_PERI_,fumigad,rastros,INF,POS, ADOB,ADOB_RE,CARR,TEJ,TEL,PLAS,NOBL,LADR_NO,NOBL,CALAM)

colnames(dt_cuali)[2] <-"amb_intra"
colnames(dt_cuali)[3] <-"amb_peri"

# exploring cuali data #
# ====================== #
# library
library(ggplot2)
library(viridis)
library(hrbrthemes)
# writing a table to plot and exploring #
# ===================================== #
dt_cuali$count<-unlist(1)
dev.off()
b<-as.data.table(xtabs(count~ SECTOR +fumigad+AVE_PERI_+CUY_INTRA_+AVE_PERI_+CUY_PERI_+CARR+TEJ+TEL+PLAS+NOBL+
                         LADR_NO+CALAM+fumigad+rastros+INF+POS,data = dt_cuali))
ggplot(b, aes(fill=INF, y=N, x=SECTOR)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("viviendas infestadas por sector") +
  theme_ipsum() +
  xlab("")

ggplot(b,                         # Draw barplot with grouping & stacking
       aes(x = INF,
           y = N,
           fill = fumigad)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ SECTOR)


# Cualitative variable Y , infestation 1=infestada 0= no infestada #
#     cualitative independent variables.                           #
#.    univariate  analysis for qualitative independent variables
# ================================================================ #
con<-table(dt_cuali$INF,dt_cuali$AVE_PERI_)
colnames(con)<-c("no poultry","with poultry")
row.names(con)<-c("no infestado"," infestado")
con
mosaicplot(con)
prop.table(con)
# by row
prop.table(con, margin = 1)
#by colum
prop.table(con, margin = 2)
# overall
prop.table(con)
addmargins(con)
chisq.test(con)
fisher.test(con)

con1<-table(dt_cuali$INF,dt_cuali$CUY_INTRA_)
colnames(con1)<-c("no guinea pigs","guinea pigs")
row.names(con1)<-c("no infestation","infestation")
con1
mosaicplot(con1)
prop.table(con1)
# by row
prop.table(con1, margin = 1)
#by colum
prop.table(con1, margin = 2)
# overall
prop.table(con1)
addmargins(con1)
chisq.test(con1)
fisher.test(con1)

con3<-table(dt_cuali$INF,dt_cuali$CARR)
colnames(con3)<-c("no carrizo","carrizo")
row.names(con3)<-c("no infestation","infestation")
con3
mosaicplot(con3)
prop.table(con3)
# by row
prop.table(con3, margin = 1)
#by colum
prop.table(con3, margin = 2)

# overall
prop.table(con3)
addmargins(con3)
chisq.test(con3)
fisher.test(con3)

con4<-table(dt_cuali$INF,dt_cuali$TEJ)
colnames(con4)<-c("no teja","teja")
row.names(con4)<-c("no infestation","infestation")
con4
mosaicplot(con4)
prop.table(con4)
# by row
prop.table(con4, margin = 1)
#by colum
prop.table(con4, margin = 2)

# overall
prop.table(con4)
addmargins(con4)
chisq.test(con4)
fisher.test(con4)

con5<-table(dt_cuali$INF,dt_cuali$PLAS)
colnames(con5)<-c("no plastico","plastico")
row.names(con5)<-c("no infestation","infestation")
con5
mosaicplot(con5)
prop.table(con5)
# by row
prop.table(con5, margin = 1)
#by colum
prop.table(con5, margin = 2)

# overall
prop.table(con5)
addmargins(con5)
chisq.test(con5)
fisher.test(con5)

con6<-table(dt_cuali$INF,dt_cuali$CALAM)
colnames(con6)<-c("no calamina","calamina")
row.names(con6)<-c("no infestation","infestation")
con6
mosaicplot(con6)
prop.table(con6)
# by row
prop.table(con6, margin = 1)
#by colum
prop.table(con5, margin = 2)

# overall
prop.table(con6)
addmargins(con6)
chisq.test(con6)
fisher.test(con6)

con7<-table(dt_cuali$INF,dt_cuali$SECTOR)
#colnames(con7)<-c("no calamina","calamina")
row.names(con7)<-c("no infestation","infestation")
con7
mosaicplot(con7)
prop.table(con7)
# by row
prop.table(con7, margin = 1)
#by colum
prop.table(con7, margin = 2)
# overall
prop.table(con7)
addmargins(con7)

chisq.test(con7, simulate.p.value = TRUE)


# Cualitative variable Y , t cruzi 1= T.cruzi 0= no T.cruzi #
#     cualitative independent variables                     #
# ========================================================= #

con<-table(dt_cuali$POS,dt_cuali$AVE_PERI_)
colnames(con)<-c("no poultry","with poultry")
row.names(con)<-c("no T.cruzi"," T. cruzi")
con
mosaicplot(con)
prop.table(con)
# by row
prop.table(con, margin = 1)
#by colum
prop.table(con, margin = 2)

# overall
prop.table(con)
addmargins(con)
chisq.test(con)
fisher.test(con)

con1<-table(dt_cuali$POS,dt_cuali$CUY_INTRA_)
colnames(con1)<-c("no guinea pigs","guinea pigs")
row.names(con1)<-c("no T.cruzi"," T. cruzi")
con1
mosaicplot(con1)
prop.table(con1)
# by row
prop.table(con1, margin = 1)
#by colum
prop.table(con1, margin = 2)
# overall
prop.table(con1)
addmargins(con1)
chisq.test(con1,simulate.p.value = TRUE)
fisher.test(con1)


con3<-table(dt_cuali$POS,dt_cuali$CARR)
colnames(con3)<-c("no carrizo","carrizo")
row.names(con3)<-c("no T.cruzi"," T. cruzi")
con3
mosaicplot(con3)
prop.table(con3)
# by row
prop.table(con3, margin = 1)
#by colum
prop.table(con3, margin = 2)
# overall
prop.table(con3)
addmargins(con3)
chisq.test(con3,simulate.p.value = TRUE)
fisher.test(con3)



con4<-table(dt_cuali$POS,dt_cuali$TEJ)
colnames(con4)<-c("no teja","teja")
row.names(con4)<-c("no T.cruzi"," T. cruzi")
con4
mosaicplot(con4)
prop.table(con4)
# by row
prop.table(con4, margin = 1)
#by colum
prop.table(con4, margin = 2)

# overall
prop.table(con4)
addmargins(con4)
chisq.test(con4,simulate.p.value = TRUE)
fisher.test(con4)


con5<-table(dt_cuali$POS,dt_cuali$PLAS)
colnames(con5)<-c("no plastico","plastico")
row.names(con)<-c("no T.cruzi"," T. cruzi")
con5
mosaicplot(con5)
prop.table(con5)
# by row
prop.table(con5, margin = 1)
#by colum
prop.table(con5, margin = 2)

# overall
prop.table(con5)
addmargins(con5)
chisq.test(con5,simulate.p.value = TRUE)
fisher.test(con5)

con6<-table(dt_cuali$POS,dt_cuali$CALAM)
colnames(con6)<-c("no calamina","calamina")
row.names(con)<-c("no T.cruzi"," T. cruzi")
con6
mosaicplot(con6)
prop.table(con6)
# by row
prop.table(con6, margin = 1)
#by colum
prop.table(con5, margin = 2)
# overall
prop.table(con6)
addmargins(con6)
chisq.test(con6,simulate.p.value = TRUE)
fisher.test(con6)


con7<-table(dt_cuali$POS,dt_cuali$SECTOR)
#colnames(con7)<-c("no calamina","calamina")
row.names(con7)<-c("no T.cruzi"," T. cruzi")
con7
mosaicplot(con7)
prop.table(con7)
# by row
prop.table(con7, margin = 1)
#by colum
prop.table(con7, margin = 2)
# overall
prop.table(con7)
addmargins(con7)
chisq.test(con7, simulate.p.value = TRUE)
chisq.test(con7)
fisher.test(con7)

con8<-table(dt_cuali$INF,dt_cuali$fumigad)
colnames(con8)<-c("no fumigada","fumigada")
row.names(con8)<-c("no T.cruzi"," T. cruzi")
con8
mosaicplot(con8)
prop.table(con8)
# by row
prop.table(con8, margin = 1)
#by colum
prop.table(con8, margin = 2)
# overall
prop.table(con8)
addmargins(con8)
chisq.test(con8, simulate.p.value = TRUE)
chisq.test(con8)
fisher.test(con8)


# PARTE IV 
# ANÁLISIS MULTIVARIADO 
# ====================== #
library(lattice) #Multipanel graphs
library(mgcv) #Smoothing
library(ggplot2) #Multipanel graphs
library(plyr) #Data manipulation
library(GGally) #Multipanel graphs
library(AER)
library(MASS) # to run NB models

# COMENZAREMOS EL ANALISIS CON LA VARIABLE Y INFESTACION COMO CONTEO DE INSECTOS ENCONTRADOS
# (infestadas con T. carrioni 38 viviendas de 143 inspeccionadas)
# USAREMOS GLM con familia poisson o nb si es que la data tuviera sobredispersion
# probaremos ademas si la data esta zero inflated para ver que modelo podria adecuarse mejor

# seleccionando columnas con las variables de interes que han sido exploradas en los 
# analisis bivariados y que según lo visto en campo, podrían tener importancia como factores 
# de riesgo o protectores en la infestacion o la positivad a t . cruzi.

# Tabla con columnas para el analisis de infestacion como conteo #
dt_model<-dt_analysis%>%dplyr::select(ndvi,elevation,fumigad,AVE_PERI,CUY_INTRA,AVE_PERI_,CUY_INTRA_,
                                      n_people,cap_ttl,SECTOR,TEJ,PLAS,CARR)

# STARTING WITHTHE BASIC MODEL POISSON #
summary(model.poisson<-glm(cap_ttl~elevation+ndvi+AVE_PERI+CUY_INTRA+n_people+TEJ+PLAS+CARR+SECTOR, data = dt_model, family = "poisson"))

dispersiontest(model.poisson)
# WE DISCARD NULL HYPOTHESIS SO TEHRE IS OVERDISPERSION 
# WE WILL USE NB MODEL

# overdisperson with NB #
# ===================== #

summary(model.nb<-glm.nb(cap_ttl~elevation+ndvi+AVE_PERI+CUY_INTRA+n_people+TEJ+PLAS+CARR+SECTOR,
                         data = dt_model,link = log))

# Compare models #
# ============== #

logLik(model.poisson)
logLik(model.nb)

lrtest(model.poisson,model.nb)
# Model 1: cap_ttl ~ elevation + ndvi + AVE_PERI + CUY_INTRA + n_people + 
#   TEJ + PLAS + CARR + SECTOR
# Model 2: cap_ttl ~ elevation + ndvi + AVE_PERI + CUY_INTRA + n_people + 
#   TEJ + PLAS + CARR + SECTOR
# #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1  19 -284.79                         
# 2  20 -158.88  1 251.82  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

c(model.poisson$aic,model.nb$aic)
# 607.5753 357.7590
# we are going to choose the model NB #


# testing for excess zeros #
# ======================== #

library(performance) # to test zero inflated
check_zeroinflation(model.nb)
# # Check for zero-inflation
# 
# Observed zeros: 105
# Predicted zeros: 104
# Ratio: 0.99
# 
# Model seems ok, ratio of observed and predicted zeros is within the tolerance range.

#tolerance is at list 5% 

# TO CHECK OUR model we will develop a step forward selection approach
# stepwise forward selection for best model #
#               for infestation.            #
# ========================================= #

fit.all=glm.nb(cap_ttl~., data =dt_model )
summary(fit.all)
formula(fit.all)

fitstart = glm.nb(cap_ttl~1,data = dt_model)

summary(fitstart)

step(fitstart,direction = "forward",scope = formula(fit.all))

# ANALYZING COLINEARITY #
ggcorr(dt_model,palette = "RdBu",label = TRUE,label_round = 2)
vif(fit.all)

# modeling for POS to T. cruzzi #
# ============================= #
# logistic regression #
# =================== #
# AQui tiene mas sentido solo analizar las viviendas que han sido infestadas
# y usar la variable Y como binomial POS=1 NEG=0
# (positiva a T.cruzi, 9 viviendas de 143 inspeccionadas)


# aqui usaremos la tabla de datos siguiente 
dt_pos<-dt_analysis%>%dplyr::filter(dt_model$cap_ttl>=1)
dt_pos<-dt_pos%>%dplyr::select(ndvi,elevation,fumigad,AVE_PERI,CUY_INTRA,AVE_PERI_,CUY_INTRA_,
                                     GAT_PER,GAT_INT,PER_PER,PER_INT,LADR_NO, n_femal,n_male,n_people,pos_ttl,SECTOR,TEJ,PLAS,CARR,POS)

# stepwise forward selection for best model #
#               for infestation.            #
# ========================================= #

model.logit<-glm(POS~elevation+ndvi+fumigad+AVE_PERI+CUY_INTRA+AVE_PERI_+CUY_INTRA_+
                 GAT_PER+GAT_INT+PER_PER+PER_INT+n_femal+n_male+n_people+SECTOR+TEJ+CARR+PLAS,data = dt_pos,family=binomial(link = "logit"))
summary(model.logit)

# CHECKING THE LEVELS OF VARIABLES
sapply(lapply(dt_pos, unique), length)

# we will develop a stepwise forward selection as before 
fit.all.log=glm(POS~., data =dt_pos,family = "binomial")

summary(fit.all.log)
formula(fit.all)

fitstart = glm.nb(POS~1,data = dt_pos)
summary(fitstart)

step(fitstart,direction = "forward",scope = formula(fit.all))
