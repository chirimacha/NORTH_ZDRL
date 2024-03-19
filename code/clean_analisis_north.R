###########NORTE ANALISIS#########
##################################
########CLAUDIA AREVALO###########

# first part cleaning and organizing data  #
# shape files, variables and correct class #
# ======================================== #
library(dplyr)
#library(tidyr)
#library(tidyverse)
library(data.table)
library(sp)
library(rgdal)  # for vector work; sp package should always load with rgdal 
library (raster)   # for metadata/attributes- vectors or rasters
library(elevatr)
library(ggmap)
library(sf)
#library(plyr)
library(mapview)
library(MODISTools)
library(forcats)

#### FIRST PART CLEANING, TRANSFORMING, MERGING AND ORGANIZING DATA SETS ####
####==================================================================== ####

#SETEEMOS EL DIRECTORIO
setwd("Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/data")

#####reading files ######

############### READING FILES FROM METABASE##############
#########################################################
aullan<-read.csv("query_result_2023-12-11T19_29_28.47029Zinspections.csv")
aullan<-aullan[c(2:4,6:19,22:32)]
aullan<-filter(aullan, app_state == "published")
aullan<-aullan %>% 
  mutate(across(where(is.character), toupper))
data_inicial<-filter(aullan, estado_inspeccion == "INSPECCIONADO")

########## RESUMIENDO DATA GENERAL###########
#############################################
# RESUMEN<-aullan
# RESUMEN$count<-unlist(1)
# RESUMEN<-aggregate(count~geopoint_unicode+estado_inspeccion,data = RESUMEN,FUN = sum)
# RESUMEN<-RESUMEN[order(RESUMEN$geopoint_unicode),]
# 
# # Remove duplicated rows based on Sepal.Length
# RESUMEN %>% distinct(geopoint_unicode, .keep_all = TRUE)
# table(RESUMEN$estado_inspeccion)
# CERRADA INSPECCIONADO      RENUENTE
# 83           143             6
##################OBTENIENDO COORDENADAS  PARA SHAPEFILE##############################
#######################################################################################
aullan_points<-aullan[c(1:3)]
aullan_points$count<-unlist(1)
aullan_points<-aggregate(count~geopoint_unicode+lat+lng,data = aullan_points,FUN = sum)
aullan_points<-aullan_points %>% 
  mutate(across(where(is.character), toupper))
aullan_points<-aullan_points[c(1:3)]
names(aullan_points)
#write.csv(aullan_points,"~/Documents/Clases/PROYECTO_208732/RURAL_PLACES_SPATIAL/aullan_points_complete.csv",row.names = FALSE)

###################TRABAJANDO CON INSPECCIONES###########################
#########################################################################

aullan_inspections<-filter(aullan, estado_inspeccion == "INSPECCIONADO")
aullan_inspections$count<-unlist(1)
aullan_inspections<-aggregate(count~geopoint_unicode+estado_inspeccion,data = aullan_inspections,FUN = sum)
aullan_inspections<-merge(aullan_inspections,aullan_points,by='geopoint_unicode',all.x=TRUE)
data_agregada<-aullan_inspections

# Transforming to shape or sf object #
# ================================== #
# Define coordinate reference system
prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
my.projection <- st_crs(prj4string)

# Create sf object
aullan_complete_sf <- st_as_sf(aullan_points, coords = c("lng", "lat"), crs = my.projection)
st_crs(aullan_complete_sf)
plot(aullan_complete_sf)
# Export shapefile
aullan_complete_sf<-unique(aullan_complete_sf)
#st_write(aullan_complete_sf, "~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/aullan_complete_fromr.shp", driver="ESRI Shapefile",append = FALSE)

fileNameShapefile <- '~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/aullan_complete_fromr.shp' 
#fileNameShapefile <- '~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/aullan_complete_con_sectores.shp' 

mapData <- readOGR(dsn = fileNameShapefile,
                   layer = fileNameShapefile %>%
                     gsub('.+\\/|\\..+','',.))

mapData@data %>%  head(10)

data_agregada <- data_agregada %>% 
  rename(gpnt_nc=geopoint_unicode)

mergedData <- mapData %>% 
  merge(data_agregada,by='gpnt_nc')

mergedData %>% names
mergedData %>% as.data.frame %>%  filter(!is.na(estado_inspeccion)) %>% nrow 
plot(mergedData)

mergedData@data %>%  head(10)

######### Variables with animals################
################################################
aullan_animals<-aullan[c(1:3,5,22:24,26)]
aullan_animals<-tidyr::separate(aullan_animals, col = "animals",into = c("animal1","animal2","animal3","animal4","animal5","animal6"))
aullan_animals<-aullan_animals[c(1:5,7:8,13)]
aullan_animals$count<-unlist(1)
aullan_animals<-aggregate(count~ambiente_name+geopoint_unicode+lat+lng+animal2+animal3+tipo_ambiente,data = aullan_animals,FUN = sum)
aullan_animals<-aullan_animals%>%na.omit(aullan_animals)
#aullan_animals<-aullan_animals %>% distinct(geopoint_unicode, .keep_all = TRUE)
aullan_animals<-as.data.table(aullan_animals)
#### amendng mistakes in entering####
aullan_animals<-aullan_animals[geopoint_unicode=="606.618.649.143",tipo_ambiente:="PERI"]
aullan_animals<-aullan_animals[geopoint_unicode=="606.618.649.108",tipo_ambiente:="PERI"]

########## homologous names places and animals####
#================================================#
aullan_animals<-aullan_animals[animal2=="CHANCHO",animal2:="CERDOS"]
aullan_animals<-aullan_animals[animal2=="CERDO",animal2:="CERDOS"]
aullan_animals<-aullan_animals[ambiente_name=="COCINA - CUYERO",ambiente_name:="COCINA_CUYERO"]
aullan_animals<-aullan_animals[ambiente_name=="COCINA/ CUYERO",ambiente_name:="COCINA_CUYERO"]
aullan_animals<-aullan_animals[ambiente_name=="COCINA/ DORMITORIO ",ambiente_name:="DORMITORIO_COCINA"]
aullan_animals<-aullan_animals[ambiente_name=="COCINA/CUYERO",ambiente_name:="COCINA_CUYERO"]
aullan_animals<-aullan_animals[ambiente_name=="CORRAL 1",ambiente_name:="CORRAL"]
aullan_animals<-aullan_animals[ambiente_name=="DEPOSITO-CORRAL",ambiente_name:="DEPOSITO_CORRAL"]
aullan_animals<-aullan_animals[ambiente_name=="DEPOSITO-SALA",ambiente_name:="DEPOSITO_SALA"]
aullan_animals<-aullan_animals[ambiente_name=="DORMITORIO 1",ambiente_name:="DORMITORIO"]
aullan_animals<-aullan_animals[ambiente_name=="DORMITORIO-COCINA-CORRAL",ambiente_name:="DORMITORIO_COCINA_CORRAL"]
aullan_animals<-aullan_animals[ambiente_name=="DORMITORIO-DEPOSITO-COCINA",ambiente_name:="DORMITORIO_DEPOSITO_COCINA"]
aullan_animals<-aullan_animals[ambiente_name=="PATIO ",ambiente_name:="PATIO"]
aullan_animals<-aullan_animals[ambiente_name=="PATIO 1",ambiente_name:="PATIO"]
aullan_animals<-aullan_animals[ambiente_name=="PERI",ambiente_name:="PATIO"]
aullan_animals<-aullan_animals[ambiente_name=="PERI PATIO",ambiente_name:="PATIO"]
test<-aullan_animals[,c(1:2,5:7)]

# shaping table to wide format #
#==============================#
test<- reshape(test, 
                      timevar = "animal2",
                      idvar = c("geopoint_unicode","ambiente_name","tipo_ambiente"),
                      direction = "wide")

test[is.na(test)] = 0

# process by peri e intra places #
#=============================== #
data_intra<-filter(test,tipo_ambiente=="INTRA")
# [1] 77
# asigning names to columns #
# ========================= #
colnames(data_intra)[4] <- "AVE_INTRA"
colnames(data_intra)[5] <- "CANS_INTRA"
colnames(data_intra)[6] <- "CONE_INTRA"
colnames(data_intra)[7] <- "CUY_INTRA"
colnames(data_intra)[8] <- "GAT_INTRA"
colnames(data_intra)[9] <- "PER_INTRA"
colnames(data_intra)[10] <- "MUR_INTRA"
colnames(data_intra)[11] <- "CAB_INTRA"
colnames(data_intra)[12] <- "CER_INTRA"
colnames(data_intra)[13] <- "PAL_INTRA"
colnames(data_intra)[14] <- "PAT_INTRA"

data_intra<-data_intra[,c(1:2,4:14)]

data_peri<-filter(test,tipo_ambiente=="PERI")
# [1] 98
colnames(data_peri)[4] <- "AVE_PERI"
colnames(data_peri)[5] <- "CANS_PERI"
colnames(data_peri)[6] <- "CONE_PERI"
colnames(data_peri)[7] <- "CUY_PERI"
colnames(data_peri)[8] <- "GAT_PERI"
colnames(data_peri)[9] <- "PER_PERI"
colnames(data_peri)[10] <- "MUR_PERI"
colnames(data_peri)[11] <- "CAB_PERI"
colnames(data_peri)[12] <- "CER_PERI"
colnames(data_peri)[13] <- "PAL_PERI"
colnames(data_peri)[14] <- "PAT_PERI"
data_peri<-data_peri[,c(1:2,4:14)]

# ordering in one row per UNICODE #
# =============================== #
# intra data #
# ========== #

data_intra<-data_intra[geopoint_unicode=="606.618.649.95",AVE_INTRA:="5"]
data_intra<-data_intra[geopoint_unicode=="606.618.649.71",CUY_INTRA:="42"]
data_intra<-data_intra[geopoint_unicode=="606.618.649.68",CUY_INTRA:="12"]
data_intra<-data_intra[geopoint_unicode=="606.618.649.3",CUY_INTRA:="49"]
data_intra<-data_intra[geopoint_unicode=="606.618.649.174",CUY_INTRA:="49"]
data_intra<-data_intra[geopoint_unicode=="606.618.649.167",CUY_INTRA:="15"]
data_intra<-data_intra[geopoint_unicode=="606.618.649.15",CUY_INTRA:="20"]
####### eliminate duplicated by rows##########
##############################################
data_intra <- data_intra[-c(28,35,52,55,71,74,75),]

# pero data #
# ========= #
data_peri<-data_peri[geopoint_unicode=="606.618.649.99",CUY_PERI:="20"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.94",CUY_PERI:="30"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.92",CUY_PERI:="10"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.9",CER_PERI:="1"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.86",CUY_PERI:="10"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.76",CONE_PERI:="2"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.60",CUY_PERI:="10"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.54",CUY_PERI:="1"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.53",CUY_PERI:="20"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.53",AVE_PERI:="2"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.48",AVE_PERI:="67"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.41",CUY_PERI:="30"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.36",PER_PERI:="1"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.21",CUY_PERI:="17"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.174",PER_PERI:="2"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.169",CER_PERI:="2"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.145",CONE_PERI:="4"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.142",CER_PERI:="1"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.137C",CER_PERI:="1"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.137B",CUY_PERI:="30"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.136",CUY_PERI:="20"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.126",CUY_PERI:="10"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.117A",CUY_PERI:="20"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.114",CUY_PERI:="25"]
data_peri<-data_peri[geopoint_unicode=="606.618.649.110",CUY_PERI:="20"]

####### eliminate duplicated by rows##########
##############################################
data_peri <- data_peri[-c(106:107,108:110,115:117,119,5,7,8,12,14,56,72:75,83:84,87:89,98),]

# renaming to merge with sf data #
# ============================== #

data_intra <- data_intra %>% 
  rename(gpnt_nc=geopoint_unicode)
data_peri <- data_peri %>% 
  rename(gpnt_nc=geopoint_unicode)

mergedData<- mergedData %>% 
  merge(data_intra,by='gpnt_nc')
mergedData<- mergedData %>% 
  merge(data_peri,by='gpnt_nc')

mergedData@data %>%  head(10)


# people data #
# =========== #
aullan_people<-data_inicial[c(1:3,22,25)]
aullan_people<-tidyr::separate(aullan_people, col = "habitantes",into = c("habitante1","habitante2","habitante3","habitante4","habitante5","habitante6","habitante7","habitante8","habitante9","habitante10","habitante11","habitante12"))
aullan_people<-unique(aullan_people)

aullan_people<-aullan_people[c(1,4,7,9)]
aullan_people<-aullan_people%>%na.omit(aullan_people)
aullan_people<-as.data.table(aullan_people)
aullan_people<-aullan_people[habitante3=="NULL"& geopoint_unicode=="606.618.649.167","habitante3":="35"]
aullan_people<-aullan_people[habitante3=="NULL"& geopoint_unicode=="606.618.649.19","habitante3":="36"]

# same terms for all enterings #
# ============================ #
aullan_people<-aullan_people[ambiente_name=="DORMITORIO 1",ambiente_name:="DORMITORIO"]
aullan_people<-aullan_people[ambiente_name=="DORMITORIO-DORMITORIO - COCINA",ambiente_name:="DORMITORIO_COCINA"]
aullan_people<-aullan_people[ambiente_name=="BODEGA/DORMITORIO ",ambiente_name:="DORMITORIO_BODEGA"]
aullan_people<-aullan_people[ambiente_name=="COCINA/ DORMITORIO ",ambiente_name:="DORMITORIO_COCINA"]
aullan_people<-aullan_people[ambiente_name=="CORRAL-DORMITORIO",ambiente_name:="DORMITORIO_CORRAL"]
aullan_people<-aullan_people[ambiente_name=="DEPOSITO-DORMITORIO",ambiente_name:="DORMITORIO_DEPOSITO"]
aullan_people<-aullan_people[ambiente_name=="DEPÓSITO/HABITACIÓN ",ambiente_name:="DORMITORIO_DEPOSITO"]
aullan_people<-aullan_people[ambiente_name=="DORMITORIO-COCINA-CORRAL",ambiente_name:="DORMITORIO_COCINA_CORRAL"]
aullan_people<-aullan_people[ambiente_name=="DORMITORIO-DEPOSITO",ambiente_name:="DORMITORIO_DEPOSITO"]
aullan_people<-aullan_people[ambiente_name=="DORMITORIO-DEPOSITO-COCINA",ambiente_name:="DORMITORIO_DEPOSITO_COCINA"]
aullan_people<-aullan_people[ambiente_name=="DORMITORIO-SALA",ambiente_name:="DORMITORIO_SALA"]
aullan_people<-aullan_people[ambiente_name=="SALA/DORMITORIO ",ambiente_name:="DORMITORIO_SALA"]
aullan_people<-aullan_people[ambiente_name=="SALA/DORMITORIO  1",ambiente_name:="DORMITORIO_SALA"]
aullan_people$count<-unlist(1)
# aggregating by UNICODE #
# ====================== #
aullan_people_aggregated<-aggregate(count~ambiente_name+geopoint_unicode+habitante5,data = aullan_people,FUN = sum)

# Gender separation and filtering #
# =============================== #
aullan_people_women<-filter(aullan_people_aggregated,habitante5== "FEMENINO")
aullan_people_men<-filter(aullan_people_aggregated,habitante5== "MASCULINO")

# assinging names to columns #
# ========================== #
colnames(aullan_people_women)[3] <- "gender_female"
colnames(aullan_people_men)[3] <- "gender_male"
colnames(aullan_people_women)[4] <- "n_female"
colnames(aullan_people_men)[4] <- "n_male"

# aggregating by unicode number of people #
# ======================================= #
aullan_people_men<-aullan_people_men[c(2:4)]
aullan_people_men<-aggregate(n_male~geopoint_unicode,data = aullan_people_men,FUN = sum)

aullan_people_women<-aullan_people_women[c(2:4)]
aullan_people_women<-aggregate(n_female~geopoint_unicode,data = aullan_people_women,FUN = sum)

# merging both datas #
# ================== #
aullan_people_complete<-merge(aullan_people_men,aullan_people_women,by='geopoint_unicode',all=TRUE)
aullan_people_complete$n_male[is.na(aullan_people_complete$n_male)] <- 0
aullan_people_complete$n_female[is.na(aullan_people_complete$n_female)] <- 0

### getting final information from humans ####
##############################################
data_people<-aullan_people_complete
data_people <- data_people %>% 
  rename(gpnt_nc=geopoint_unicode)
mergedData<- mergedData %>% 
  merge(data_people,by='gpnt_nc')

mergedData@data %>%  head(10)

# population pyramid #
# ==================== #
aullan_people_data<-aullan_people[,c(3:5)]
aullan_people_data$habitante3<-as.integer(aullan_people_data$habitante3)
aullan_people_data<-aggregate(count~habitante3+habitante5,data=aullan_people_data,FUN=sum)

# Create a basic bar chart for one gender
basic_plot <-  ggplot(
  aullan_people_data, 
  aes(
    x = habitante3, 
    fill = habitante5, 
    y = ifelse(
      test = habitante5 == "Male", 
      yes = -count, 
      no = count
    )
  )
) + 
  geom_bar(stat = "identity") 

# Create population pyramids for both genders and combine them
population_pyramid <- basic_plot +
  scale_y_continuous(
    labels = abs, 
    limits = max(aullan_people_data$count) * c(-1,1)
  ) + 
  coord_flip() + 
  theme_minimal() +
  labs(
    x = "Age", 
    y = "Population", 
    fill = "Gender", 
    title = "Population Pyramid"
  )

# material present #
# ================ #
aullan_material<-aullan[c(1,6,8:9,10:11,13:15,18:22,24,27:28)]
aullan_material$count<-unlist(1)
aullan_material<-unique(aullan_material)
aullan_material<-dplyr::filter(aullan_material, estado_inspeccion == "INSPECCIONADO")
# cleaning terms #
# ============== #
aullan_material<-as.data.table(aullan_material)
aullan_material<-aullan_material[ambiente_name=="COCINA - CUYERO",ambiente_name:="COCINA_CUYERO"]
aullan_material<-aullan_material[ambiente_name=="COCINA/ CUYERO",ambiente_name:="COCINA_CUYERO"]
aullan_material<-aullan_material[ambiente_name=="COCINA/ DORMITORIO ",ambiente_name:="DORMITORIO_COCINA"]
aullan_material<-aullan_material[ambiente_name=="COCINA/CUYERO",ambiente_name:="COCINA_CUYERO"]
aullan_material<-aullan_material[ambiente_name=="CORRAL 1",ambiente_name:="CORRAL"]
aullan_material<-aullan_material[ambiente_name=="DEPOSITO-CORRAL",ambiente_name:="DEPOSITO_CORRAL"]
aullan_material<-aullan_material[ambiente_name=="DEPOSITO-SALA",ambiente_name:="DEPOSITO_SALA"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO 1",ambiente_name:="DORMITORIO"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO-COCINA-CORRAL",ambiente_name:="DORMITORIO_COCINA_CORRAL"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO-DEPOSITO-COCINA",ambiente_name:="DORMITORIO_DEPOSITO_COCINA"]
aullan_material<-aullan_material[ambiente_name=="PATIO ",ambiente_name:="PATIO"]
aullan_material<-aullan_material[ambiente_name=="PATIO 1",ambiente_name:="PATIO"]
aullan_material<-aullan_material[ambiente_name=="PERI",ambiente_name:="PATIO"]
aullan_material<-aullan_material[ambiente_name=="PERI PATIO",ambiente_name:="PATIO"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO-DORMITORIO - COCINA",ambiente_name:="DORMITORIO_COCINA"]
aullan_material<-aullan_material[ambiente_name=="BAÑO",ambiente_name:="BANHO"]
aullan_material<-aullan_material[ambiente_name=="BAÑO 1",ambiente_name:="BANHO"]
aullan_material<-aullan_material[ambiente_name=="BODEGA/DORMITORIO ",ambiente_name:="DORMITORIO_BODEGA"]
aullan_material<-aullan_material[ambiente_name=="COCINA-ADOBE REBOCADO",material:="ADOBE_REVOCADO"]
aullan_material<-aullan_material[ambiente_name=="COCINA-ADOBE REBOCADO",ambiente_name:="COCINA"]
aullan_material<-aullan_material[ambiente_name=="CORRAL-DORMITORIO",ambiente_name:="DORMITORIO_CORRAL"]
aullan_material<-aullan_material[ambiente_name=="DEPOSITO-DORMITORIO",ambiente_name:="DORMITORIO_DEPOSITO"]
aullan_material<-aullan_material[ambiente_name=="DEPÓSITO/HABITACIÓN ",ambiente_name:="DORMITORIO_DEPOSITO"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO-COCINA-CORRAL",ambiente_name:="DORMITORIO_COCINA_CORRAL"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO-DEPOSITO",ambiente_name:="DORMITORIO_DEPOSITO"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO-DEPOSITO-COCINA",ambiente_name:="DORMITORIO_DEPOSITO_COCINA"]
aullan_material<-aullan_material[ambiente_name=="DORMITORIO-SALA",ambiente_name:="DORMITORIO_SALA"]
aullan_material<-aullan_material[ambiente_name=="SALA/DORMITORIO ",ambiente_name:="DORMITORIO_SALA"]
aullan_material<-aullan_material[ambiente_name=="SALA/DORMITORIO  1",ambiente_name:="DORMITORIO_SALA"]
aullan_material<-aullan_material[ambiente_name=="COCINA-DEPOSITO",ambiente_name:="COCINA_DEPOSITO"]
aullan_material<-aullan_material[ambiente_name=="DEPOSITO 1",ambiente_name:="DEPOSITO"]
aullan_material<-aullan_material[ambiente_name=="PERIDOMICILLO ",ambiente_name:="PATIO"]
aullan_material<-aullan_material[ambiente_name=="SALA 1 ",ambiente_name:="SALA"]
aullan_material<-aullan_material[ambiente_name=="SALA-DEPOSITO",ambiente_name:="SALA_DEPOSITO"]
aullan_material<-aullan_material[ambiente_name=="SALON COMUNAL ",ambiente_name:="SALON_COMUNAL"]
aullan_material<-aullan_material[ambiente_name=="SALON CONSISTORIAL",ambiente_name:="SALON_CONSISTORIAL"]
aullan_material<-aullan_material[ambiente_name=="TRAPICHE DESTILAR CAÑA",ambiente_name:="TRAPICHE"]
aullan_material<-aullan_material[ambiente_name=="BAÑO-DEPOSITO",ambiente_name:="BANHO_DEPOSITO"]
aullan_material<-aullan_material[ambiente_name=="COCINA 1",ambiente_name:="COCINA"]
aullan_material<-aullan_material[ambiente_name=="SALA 1",ambiente_name:="SALA"]
aullan_material<-aullan_material[material=="ETERNIT",material:="TEJA"]
aullan_material<-aullan_material[material=="MALLA",material:="CARRIZO"]

# obtaining secondary variables #
# ============================= #
# kind of predio #
# ============== #
aux<-aggregate(count~geopoint_unicode+predio,data = aullan_material,FUN = sum)
aux<-aux[,c(1:2)]

aux <- aux %>% 
  rename(gpnt_nc=geopoint_unicode)
mergedData<- mergedData %>% 
  merge(aux,by='gpnt_nc')

mergedData@data %>%  head(10)

# spraying by owner #
# ================= #
aux_1<-aggregate(count~geopoint_unicode+fumigada,data = aullan_material,FUN = sum)
aux_1<-aux_1[,c(1:2)]
aux_1 <- aux_1 %>% 
  rename(gpnt_nc=geopoint_unicode)
mergedData<- mergedData %>% 
  merge(aux_1,by='gpnt_nc')

mergedData@data %>%  head(10)

# insecticide used #
# ================ #
aux_2<-aggregate(count~geopoint_unicode+fumigada+producto_usado,data = aullan_material,FUN = sum)
aux_2<-aux_2[,c(1:3)]
aux_2<-as.data.table(aux_2)
aux_2<-aux_2[producto_usado=="LEJÍA ",producto_usado:="LEJIA"]
aux_2<-aux_2[producto_usado=="LEJIA Y PETROLLO",producto_usado:="LEJIA_PETROLEO"]
aux_2<-aux_2[producto_usado=="MALATHION/TIFON",producto_usado:="MALATION_TIFON"]
aux_2<-aux_2[producto_usado=="RAIY",producto_usado:="RAY"]
aux_2<-aux_2[producto_usado=="RAY MATA INSECTOS",producto_usado:="RAY"]
aux_2<-aux_2[producto_usado=="REMEDIO DE PAPA",producto_usado:="INSECTICIDA-DE_PAPA"]
aux_2<-aux_2[producto_usado=="SAPOLIO ",producto_usado:="SAPOLIO"]
aux_2<-aux_2[producto_usado=="SAPOLLO",producto_usado:="SAPOLIO"]
aux_2<-aux_2[producto_usado=="TIFON ",producto_usado:="TIFON"]
aux_2<-aux_2[producto_usado=="TIFÓN ",producto_usado:="TIFON"]
aux_2<-aux_2[producto_usado=="SANAVE ",producto_usado:="SANAVE"]
aux_2<-aux_2[,c(1,3)]

aux_2 <- aux_2 %>% 
  rename(gpnt_nc=geopoint_unicode)
mergedData<- mergedData %>% 
  merge(aux_2,by='gpnt_nc')

mergedData@data %>%  head(10)

# dangerous insect visualized #
# =========================== #
aux_3<-aggregate(count~geopoint_unicode+insecto_peligroso_visualizado+insectos_visualizados,data = aullan_material,FUN = sum)
aux_3<-tidyr::separate(aux_3, col = "insectos_visualizados",into = c("insecto1","insecto2","insecto3","insecto4"))
aux_3<-as.data.table(aux_3)
aux_3<-aux_3[insecto2=="ALACR",insecto2:="ALACRAN"]
aux_3<-aux_3[insecto2=="ALACRANES",insecto2:="ALACRAN"]
aux_3<-aux_3[insecto3=="MANTA",insecto3:="MANTA_BLANCA"]
aux_3<-aux_3[insecto3=="N",insecto3:=NA]
aux_3<-aux_3[insecto3=="CAMA",insecto3:=NA]
aux_3<-aux_3[insecto3=="BLANCA",insecto3:=NA]
aux_3<-aux_3[,c(1:2,4:6)]
aux_3<-aux_3[,c(1:3)]

aux_3 <- aux_3 %>% 
  rename(gpnt_nc=geopoint_unicode)
mergedData<- mergedData %>% 
  merge(aux_3,by='gpnt_nc')

mergedData@data %>%  head(10)

# material present in the house #
# ============================= #
aux_4<-aullan_material[,c(1:2,14:15,17)]
aux_4<-as.data.table(aux_4)
aux_4<-aux_4[geopoint_unicode=="606.618.649.141"& ambiente_name=="COCINA-ADOBE REBOCADO",rastros_chirimacha:="HUELLA"]
aux_4<-aux_4[geopoint_unicode=="606.618.649.41"& ambiente_name=="DORMITORIO",rastros_chirimacha:="TRIATOMINO"]
aux_4$count<-unlist(1)

aux_4<-aggregate(count~geopoint_unicode+ambiente_name+material+rastros_chirimacha,data = aux_4,FUN = sum)
c<-aggregate(count~geopoint_unicode+material,data = aux_4,FUN = sum)
c <- reshape(c,
             timevar = "material",
             idvar = c("geopoint_unicode"),
             direction = "wide")
colnames(c)[2] <- "ADOB"
colnames(c)[3] <- "ADOB_REV"
colnames(c)[4] <- "BLOQ_NOB"
colnames(c)[5] <- "CALAM"
colnames(c)[6] <- "CARR"
colnames(c)[7] <- "CART"
colnames(c)[8] <- "DRYW"
colnames(c)[9] <- "LADR_NOB"
colnames(c)[10] <- "MAD"
colnames(c)[11] <- "MET"
colnames(c)[12] <- "NOBL"
colnames(c)[13] <- "PIED_NOB"
colnames(c)[14] <- "PIED_PIRC"
colnames(c)[15] <- "PLAS"
colnames(c)[16] <- "TEJ"
colnames(c)[17] <- "TEL"

c[is.na(c)] = 0
c[c==2] <- 1
c[c==3] <- 1
c[c==4] <- 1
c[c==5] <- 1
c[c==6] <- 1
c[c==7] <- 1
c[c==11] <- 1
c[c==12] <- 1

c <- c %>% 
  rename(gpnt_nc=geopoint_unicode)
mergedData<- mergedData %>% 
  merge(c,by='gpnt_nc')

mergedData@data %>%  head(10)


# variable traces and triatomines #
# =============================== #

aux_5<-aggregate(count~geopoint_unicode+rastros_chirimacha+material,data = aux_4,FUN = sum)
aux_5<-aux_5[,c(1:2,4)]
aux_5<-as.data.table(aux_5)
aux_5<-aux_5[rastros_chirimacha=="TRIATOMINIO", rastros_chirimacha:="TRIATOMINO"]

aux_5 <- reshape(aux_5,
             timevar = "rastros_chirimacha",
             idvar = c("geopoint_unicode"),
             direction = "wide")
colnames(aux_5)[2] <- "EXHU"
colnames(aux_5)[3] <- "HUEL"
colnames(aux_5)[4] <- "HUEV"
colnames(aux_5)[5] <- "SIN_HALL"
colnames(aux_5)[6] <- "TRIAT"

aux_5<-aux_5[EXHU>=1,rastros:=1]
aux_5<-aux_5[HUEL>=1,rastros:=1]

aux_5<-aux_5[,c(1,7)]
aux_5[is.na(aux_5)] = 0

aux_5 <- aux_5 %>% 
  rename(gpnt_nc=geopoint_unicode)
mergedData<- mergedData %>% 
  merge(aux_5,by='gpnt_nc')

mergedData@data %>%  head(10)

# format two #
# ========== #

aullan_formatodos<-read.csv("query_result_2023-12-01T14_08_30.674661ZFORMATODOS.csv")
aullan_formatodos<-as.data.table(aullan_formatodos)
aullan_formatodos<-aullan_formatodos[unicode=="606.618.649.129-130",unicode:="606.618.649.129"]
aullan_formatodos<-aullan_formatodos[unicode=="606.618.649.130-129",unicode:="606.618.649.130"]
aullan_formatodos<-aullan_formatodos[intra_peri_code=="606.618.649.137B-PARED",pos_eta4:="0"]
aullan_formatodos<-aullan_formatodos[intra_peri_code=="606.618.649.137B-PARED",pos_eta2:="0"]
aullan_formatodos<-aullan_formatodos[intra_peri_code=="606.618.649.137B-PARED",pos_total:="0"]


##### turning numeric columns with positives and insects #########
aullan_formatodos$cap_total<-as.numeric(aullan_formatodos$cap_total)
aullan_formatodos$pos_total<-as.numeric(aullan_formatodos$pos_total)
aullan_formatodos<-aullan_formatodos[,c(7,11:36)]
aullan_formatodos<-aullan_formatodos[,c(1:4,12,26:27)]
aullan_formatodos<-aullan_formatodos[intra_peri_code=="606.618.649.129-130-DOR",cap_total:=2]
aullan_formatodos<-aullan_formatodos[aullan_formatodos$intra_peri_code=="606.618.649.122-DOR",cap_total:=8]

###### houses with bugs #########
##################################
aullan_infestados<-aggregate(cap_total~unicode+intra_peri+procedencia,data=aullan_formatodos,FUN=sum)
aullan_infestados_resume<-aggregate(cap_total~unicode,data=aullan_formatodos,FUN=sum)
aullan_infestados_resume<-as.data.table(aullan_infestados_resume)

aullan_infestados_resume <- aullan_infestados_resume %>% 
  rename(gpnt_nc=unicode)
mergedData<- mergedData %>% 
  merge(aullan_infestados_resume,by='gpnt_nc')

mergedData@data %>%  head(10)
ggplot(aullan_infestados, aes(fill=intra_peri, y=cap_total, x=procedencia)) + 
  geom_bar(position="stack", stat="identity")

#############observando abientes ############
#############################################
aullan_ambientes_hallazgo<-aggregate(cap_total~unicode+intra_peri,data=aullan_formatodos,FUN=sum)
table(aullan_ambientes_hallazgo$intra_peri)
# intra  peri 
# 37     9 
aullan_ambientes_hallazgo_INTRA<-filter(aullan_ambientes_hallazgo,intra_peri== "intra")
aullan_ambientes_hallazgo_PERI<-filter(aullan_ambientes_hallazgo,intra_peri== "peri")

######## houses with T cruzi ############
#########################################
aullan_cruzi<-aggregate(pos_total~unicode+intra_peri+procedencia,data=aullan_formatodos,FUN=sum)
aullan_cruzi<-filter(aullan_cruzi, pos_total >= 1)
ggplot(aullan_cruzi, aes(fill=intra_peri, y=pos_total, x=procedencia)) + 
  geom_bar(position="stack", stat="identity")

aullan_cruzi<-aggregate(pos_total~unicode,data=aullan_cruzi,FUN=sum)

aullan_cruzi <- aullan_cruzi %>% 
  rename(gpnt_nc=unicode)
mergedData<- mergedData %>% 
  merge(aullan_cruzi,by='gpnt_nc')

mergedData@data %>%  head(10)

# transforming  values for analysis #
# ================================= #
mergedData@data$estado_inspeccion <- ifelse(is.na(mergedData@data$estado_inspeccion),"0",
                                       mergedData@data$estado_inspeccion)
mergedData@data$estado_inspeccion <- ifelse(mergedData@data$estado_inspeccion=="INSPECCIONADO","1",
                                            mergedData@data$estado_inspeccion)
# checking #
# ======== #
table(mergedData@data$estado_inspeccion)
# 0   1 
# 51 143

# normalizing columns with binary values #
# ====================================== #
mergedData@data$cap_total <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$cap_total),0,
                                       mergedData@data$cap_total)
mergedData@data$cap_total <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$cap_total),"NA",
                                    mergedData@data$cap_total)
table(mergedData@data$cap_total)
#   0   1  11  12  16   2  22  28   3   4   5   6   7   8   9  NA 
# 105  10   1   1   1   7   1   1   4   2   1   2   3   3   1  51 

mergedData@data$pos_total <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$pos_total),0,
                                    mergedData@data$pos_total)
mergedData@data$pos_total <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$pos_total),"NA",
                                    mergedData@data$pos_total)
table(mergedData@data$pos_total)
#   0   1  10   3   4  NA 
# 134   5   1   2   1  51

mergedData@data$n_male <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$n_male),"NA",
                                    mergedData@data$n_male)
mergedData@data$n_male <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$n_male),0,
                                    mergedData@data$n_male)
table(mergedData@data$n_male)


mergedData@data$n_female <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$n_female),"NA",
                                    mergedData@data$n_female)
mergedData@data$n_female <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$n_female),0,
                                 mergedData@data$n_female)
table(mergedData@data$n_female)

mergedData@data$AVE_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$AVE_INTRA),"NA",
                                   mergedData@data$AVE_INTRA)
mergedData@data$AVE_INTRA <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$AVE_INTRA),0,
                                   mergedData@data$AVE_INTRA)
table(mergedData@data$AVE_INTRA)

mergedData@data$CANS_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CANS_INTRA),"NA",
                                    mergedData@data$CANS_INTRA)
mergedData@data$CANS_INTRA <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CANS_INTRA),0,
                                    mergedData@data$CANS_INTRA)
table(mergedData@data$CANS_INTRA)

mergedData@data$CONE_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CONE_INTRA),"NA",
                                     mergedData@data$CONE_INTRA)
mergedData@data$CONE_INTRA <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CONE_INTRA),0,
                                     mergedData@data$CONE_INTRA)
table(mergedData@data$CONE_INTRA)

mergedData@data$CUY_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CUY_INTRA),"NA",
                                     mergedData@data$CUY_INTRA)
mergedData@data$CUY_INTRA <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CUY_INTRA),0,
                                     mergedData@data$CUY_INTRA)
table(mergedData@data$CUY_INTRA)

mergedData@data$GAT_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$GAT_INTRA),"NA",
                                     mergedData@data$GAT_INTRA)
mergedData@data$GAT_INTRA<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$GAT_INTRA),0,
                                    mergedData@data$GAT_INTRA)
table(mergedData@data$GAT_INTRA)

mergedData@data$PER_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PER_INTRA),"NA",
                                     mergedData@data$PER_INTRA)
mergedData@data$PER_INTRA<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$PER_INTRA),0,
                                   mergedData@data$PER_INTRA)
table(mergedData@data$PER_INTRA)

mergedData@data$MUR_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$MUR_INTRA),"NA",
                                     mergedData@data$MUR_INTRA)
mergedData@data$MUR_INTRA<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$MUR_INTRA),0,
                                   mergedData@data$MUR_INTRA)
table(mergedData@data$MUR_INTRA)

mergedData@data$CAB_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CAB_INTRA),"NA",
                                     mergedData@data$CAB_INTRA)
mergedData@data$CAB_INTRA<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CAB_INTRA),0,
                                   mergedData@data$CAB_INTRA)
table(mergedData@data$CAB_INTRA)

mergedData@data$CER_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CER_INTRA),"NA",
                                     mergedData@data$CER_INTRA)
mergedData@data$CER_INTRA<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CER_INTRA),0,
                                   mergedData@data$CER_INTRA)
table(mergedData@data$CER_INTRA)

mergedData@data$PAL_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PAL_INTRA),"NA",
                                     mergedData@data$PAL_INTRA)
mergedData@data$PAL_INTRA<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$PAL_INTRA),0,
                                   mergedData@data$PAL_INTRA)
table(mergedData@data$PAL_INTRA)

mergedData@data$PAT_INTRA <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PAT_INTRA),"NA",
                                     mergedData@data$PAT_INTRA)
mergedData@data$PAT_INTRA<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$PAT_INTRA),0,
                                   mergedData@data$PAT_INTRA)
table(mergedData@data$PAT_INTRA)

mergedData@data$AVE_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$AVE_PERI),"NA",
                                    mergedData@data$AVE_PERI)
mergedData@data$AVE_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$AVE_PERI),0,
                                   mergedData@data$AVE_PERI)

mergedData@data$CANS_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CANS_PERI),"NA",
                                     mergedData@data$CANS_PERI)
mergedData@data$CANS_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CANS_PERI),0,
                                  mergedData@data$CANS_PERI)

mergedData@data$CONE_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CONE_PERI),"NA",
                                     mergedData@data$CONE_PERI)
mergedData@data$CONE_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CONE_PERI),0,
                                   mergedData@data$CONE_PERI)

mergedData@data$CUY_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CUY_PERI),"NA",
                                    mergedData@data$CUY_PERI)
mergedData@data$CUY_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CUY_PERI),0,
                                   mergedData@data$CUY_PERI)

mergedData@data$GAT_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$GAT_PERI),"NA",
                                    mergedData@data$GAT_PERI)
mergedData@data$GAT_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$GAT_PERI),0,
                                   mergedData@data$GAT_PERI)

mergedData@data$PER_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PER_PERI),"NA",
                                    mergedData@data$PER_PERI)
mergedData@data$PER_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$PER_PERI),0,
                                   mergedData@data$PER_PERI)

mergedData@data$MUR_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$MUR_PERI),"NA",
                                    mergedData@data$MUR_INTRA)
mergedData@data$MUR_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$MUR_PERI),0,
                                   mergedData@data$MUR_PERI)

mergedData@data$CAB_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CAB_PERI),"NA",
                                    mergedData@data$CAB_PERI)
mergedData@data$CAB_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CAB_PERI),0,
                                   mergedData@data$CAB_PERI)

mergedData@data$CER_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CER_PERI),"NA",
                                    mergedData@data$CER_PERI)
mergedData@data$CER_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$CER_PERI),0,
                                   mergedData@data$CER_PERI)

mergedData@data$PAL_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PAL_PERI),"NA",
                                    mergedData@data$PAL_PERI)
mergedData@data$PAL_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$PAL_PERI),0,
                                   mergedData@data$PAL_PERI)

mergedData@data$PAT_PERI <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PAT_PERI),"NA",
                                    mergedData@data$PAT_PERI)
mergedData@data$PAT_PERI<- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$PAT_PERI),0,
                                   mergedData@data$PAT_PERI)
 
table(mergedData@data$SECTOR)
# AULLAN CENTRO   BUENOS AIRES     CARPACHACA      HUABAMAYO     LAS TORRES     LOS OLIVOS 
# 36             13             26             11             35             15 
# PAMPA AMARILLA      PENCALOMA         PUSHUN           TAYA     TAYA LALIN 
# 10             23              3              6             16 

table(mergedData@data$predio)
mergedData@data$predio <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$predio),"NA",
                                   mergedData@data$predio)

mergedData@data$fumigada <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$fumigada),"NA",
                                 mergedData@data$fumigada)
table(mergedData@data$fumigada)

mergedData@data$producto_usado <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$producto_usado),"NA",
                                   mergedData@data$producto_usado)
table(mergedData@data$producto_usado)

mergedData@data$insecto_peligroso_visualizado <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$insecto_peligroso_visualizado),"NA",
                                         mergedData@data$insecto_peligroso_visualizado)
table(mergedData@data$insecto_peligroso_visualizado)

mergedData@data$ADOB <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$ADOB),"NA",
                                         mergedData@data$ADOB)
table(mergedData@data$ADOB)

mergedData@data$ADOB_REV <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$ADOB_REV),"NA",
                               mergedData@data$ADOB_REV)
mergedData@data$BLOQ_NOB <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$BLOQ_NOB),"NA",
                               mergedData@data$BLOQ_NOB)
mergedData@data$CALAM <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CALAM),"NA",
                               mergedData@data$CALAM)
mergedData@data$CARR <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CARR),"NA",
                               mergedData@data$CARR)
mergedData@data$CART <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$CART),"NA",
                               mergedData@data$CART)
mergedData@data$DRYW <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$DRYW),"NA",
                               mergedData@data$DRYW)
mergedData@data$LADR_NOB <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$LADR_NOB),"NA",
                               mergedData@data$LADR_NOB)
mergedData@data$MAD <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$MAD),"NA",
                               mergedData@data$MAD)
mergedData@data$MET <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$MET),"NA",
                               mergedData@data$MET)
mergedData@data$NOBL <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$NOBL),"NA",
                               mergedData@data$NOBL)
mergedData@data$PIED_NOB <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PIED_NOB),"NA",
                               mergedData@data$PIED_NOB)
mergedData@data$PIED_PIRC <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PIED_PIRC),"NA",
                               mergedData@data$PIED_PIRC)
mergedData@data$PLAS <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$PLAS),"NA",
                               mergedData@data$PLAS)
mergedData@data$TEJ <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$TEJ),"NA",
                               mergedData@data$TEJ)
mergedData@data$TEL <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$TEL),"NA",
                               mergedData@data$TEL)
mergedData@data$rastros <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$rastros),"NA",
                               mergedData@data$rastros)
mergedData@data$ambiente_name.y <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$ambiente_name.y),"NA",
                                  mergedData@data$ambiente_name.y)
mergedData@data$ambiente_name.y <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$ambiente_name.y),"NO_ANIMAL_PERI",
                                          mergedData@data$ambiente_name.y)
mergedData@data$ambiente_name.x <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$ambiente_name.x),"NA",
                                          mergedData@data$ambiente_name.x)
mergedData@data$ambiente_name.x <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$ambiente_name.x),"NO_ANIMAL_INTRA",
                                          mergedData@data$ambiente_name.x)
mergedData@data$insecto_peligroso_visualizado <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$insecto_peligroso_visualizado),"NA",
                                          mergedData@data$insecto_peligroso_visualizado)
mergedData@data$insecto_peligroso_visualizado <- ifelse(mergedData@data$insecto_peligroso_visualizado=="1" & is.na(mergedData@data$insecto_peligroso_visualizado),"NINGUN_INSECTO",
                                          mergedData@data$insecto_peligroso_visualizado)
mergedData@data$insecto2 <- ifelse(mergedData@data$estado_inspeccion=="0" & is.na(mergedData@data$insecto2),"NA",
                                          mergedData@data$insecto2)
mergedData@data$insecto2 <- ifelse(mergedData@data$estado_inspeccion=="1" & is.na(mergedData@data$insecto2),"NO_INSECTO",
                                          mergedData@data$insecto2)

table(mergedData@data$ambiente_name.x)
table(mergedData@data$ambiente_name.y)
table(mergedData@data$insecto_peligroso_visualizado)
table(mergedData@data$insecto2)
table(mergedData@data$AVE_INTRA)
table(mergedData@data$CUY_INTRA)
# getting the variables of interest #
# ================================= #
mergedData <- mergedData[, -(6:8)] 
names(mergedData@data)
# transforming class some columns for analysis #
# =========================================== #
mergedData@data[,c(7:17,19:31)]<-lapply(mergedData@data[,c(7:17,19:31)],as.numeric)

mergedData@data[,c(33,35,37:53)]<-lapply(mergedData@data[,c(33,35,37:53)],as.factor)
mergedData@data[,c(54:55)]<-lapply(mergedData@data[,c(54:55)],as.numeric)

#mergedData@data <- na.omit(mergedData@data)


# # creando variables con valores binaries y con levels #
# # =================================================== #
# 
mergedData@data$INF<-ifelse(mergedData@data$estado_inspeccion==1 & mergedData@data$cap_total>=1 ,c(1),c(0))
mergedData@data$INF[mergedData@data$estado_inspeccion == 0 & mergedData@data$INF==0] <- NA  
mergedData@data$POS<-ifelse(mergedData@data$estado_inspeccion==1 & mergedData@data$pos_total>=1 ,c(1),c(0))
mergedData@data$POS[mergedData@data$estado_inspeccion == 0 & mergedData@data$POS==0] <- NA  

mergedData@data$AVE_INTRA_PRESENTE<-ifelse(mergedData@data$estado_inspeccion==1 & mergedData@data$AVE_INTRA >=1 ,c(1),c(0))
mergedData@data$AVE_INTRA_PRESENTE[mergedData@data$estado_inspeccion == 0 & mergedData@data$AVE_INTRA_PRESENTE==0] <- NA  
mergedData@data$CUY_INTRA_PRESENTE<-ifelse(mergedData@data$estado_inspeccion==1 & mergedData@data$CUY_INTRA>=1 ,c(1),c(0))
mergedData@data$CUY_INTRA_PRESENTE[mergedData@data$estado_inspeccion == 0 & mergedData@data$CUY_INTRA_PRESENTE==0] <- NA  
mergedData@data$AVE_PERI_PRESENTE<-ifelse(mergedData@data$estado_inspeccion==1 & mergedData@data$AVE_PERI>=1 ,c(1),c(0))
mergedData@data$AVE_PERI_PRESENTE[mergedData@data$estado_inspeccion == 0 & mergedData@data$AVE_PERI_PRESENTE==0] <- NA  
mergedData@data$CUY_PERI_PRESENTE<-ifelse(mergedData@data$estado_inspeccion==1 & mergedData@data$CUY_PERI>=1 ,c(1),c(0))
mergedData@data$CUY_PERI_PRESENTE[mergedData@data$estado_inspeccion == 0 & mergedData@data$CUY_PERI_PRESENTE==0] <- NA  

# 

mergedData@data %>%  head(10)
summary(mergedData)
library(pacman)
pacman::p_load(
  rio,           # import/export
  here,          # filepaths
  lubridate,     # working with dates
  forcats,       # factors
  aweek,         # create epiweeks with automatic factor levels
  janitor,       # tables
  #tidyverse      # data mgmt and viz
)
mergedData@data <- mergedData@data %>%
  mutate(SECTOR = fct_relevel(SECTOR))
mergedData@data <- mergedData@data %>%
  mutate(estado_inspeccion = fct_relevel(estado_inspeccion))
mergedData@data <- mergedData@data %>%
  mutate(ambiente_name.x = fct_relevel(ambiente_name.x))
mergedData@data <- mergedData@data %>%
  mutate(ambiente_name.y = fct_relevel(ambiente_name.y))
mergedData@data <- mergedData@data %>%
  mutate(producto_usado = fct_relevel(producto_usado))
mergedData@data <- mergedData@data %>%
  mutate(predio = fct_relevel(predio))

# ======== exploration and analysis =========== #
# ============================================= #
str(mergedData@data) # Provides the structure of the dataset
summary(mergedData@data) # Provides basic descriptivestatistics and frequencies
names(mergedData@data) # Lists variables in the dataset
# missing data #
################
sum(is.na(mergedData@data))# Number of missing in dataset
rowSums(is.na(mergedData@data))# Number of missing per variable
rowMeans(is.na(mergedData@data))*length(mergedData@data)# No. of missing per row



writeOGR(obj = mergedData,dsn = "~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/",layer ="mergedData" ,driver = "ESRI Shapefile")
# writing the mergeData as shapefile #
# ================================== #

# adding NDVI AND ALTITUDE #
# ======================== #
#library(tidyverse)
dat_all <- read_sf("~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/mergedData.shp") 
mapview(dat_all)
dat_elev <- get_elev_point(dat_all, src="aws", z=14)
write_sf(dat_elev,"~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/dat_elev.03_03_2024.shp")
# writing the file with altitud #
dat_all <- dat_all |> 
  #left_join(select(dat_vis, id, fecha:infestada), by="id") |>
  st_join(dat_elev)


# houses have up to %20 of village within 250m, with most around 5-15%, so there will be a decent amount of overlap
# in NDVI values
colSums(matrix(as.double(st_distance(dat_all)) < 250, nrow=nrow(dat_all))) / nrow(dat_all)

# extract MODIS NDVI values for each house's coordinates (this might take a while to run):
#library(dplyr)
#library(MODISTools)
modis <- dat_all |> 
  dplyr::select(site_name=gpnt_nc, lon=LONGITU, lat=LATITUD) |> 
  st_drop_geometry() |> 
  mt_batch_subset("MOD13Q1", band="250m_16_days_NDVI", start="2023-06-01",end = "")

write.csv(modis,"~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/modis.csv",row.names = FALSE)
# rename the unicode to avoid problems for characters
dat_all<-dat_all|>
  rename(id=gpnt_nc) 

# get the most recent NDVI values for each site and join with data
dat_all <- modis |> 
  as_tibble() |> 
  mutate(date=as.Date(calendar_date)) |> 
  group_by(site) |> 
  filter(date == max(date)) |>
  ungroup() |> 
  dplyr::select(id=site, ndvi=value) |> 
  right_join(dat_all, by=c("id")) |> 
  st_as_sf() |> 
  st_zm(drop=TRUE) # also drop the unused Z dimension


# write_sf(dat_all, "~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/dat_all03_03_2024.shp")

#####################################################################################################






























# Trying to insert everything into a table to export #
# ================================================== #

glimpse(new_DATA)
table(new_DATA$AVE_PERI)

tbl_summary_1 <- new_DATA %>%
  #select(ambiente_name.x) %>% 
  tbl_summary()

tbl_summary_1<- tbl_summary_1 %>%
  as_flex_table() 
# Guarda la tabla en un archivo .docx
flextable::save_as_docx(tbl_summary_1, path = "~/Documents/Clases/PROYECTO_208732/ANALISIS_AULLAN_2023/tabla_1.docx")


data_table_1  <- data_draft %>%  select(site ,	edad_cat_1,	sexo,	nivel_educativo_cat_1,	tpca2,	urbano_rural,	personas_hogar_cat_2,	menor_5anos,	adultos_mayores,	vuln_emb_ecro_dfis)
glimpse(data_table_1)
column_order <- c("SECTOR" ,	"ambnt_nm_x",	"ambnt_nm_y",	"AVE_INTRA",	"CUY_INTRA",	"AVE_PERI",	"CUY_PERI",	"GAT_PER",	"GAT_INT",	"PER_PER","PER_INT",
                  "n_femal","n_male","fumigad")
data_table_1 <- data_table_1[, column_order]
glimpse(data_table_1)

table(data_table_1$estd_ns)

tabla_1 <- cuali_DATA %>%
  tbl_summary(by = INF ,
              missing = "no",
              digits = all_categorical() ~ 1) %>%
  add_overall() %>%
  add_p()
tabla_1

tabla_1 <- tabla_1 %>%
  as_flex_table() 

# Guarda la tabla en un archivo .docx
flextable::save_as_docx(tabla_1, path = "tabla_1.docx")

add_p(test.args = all_tests("fisher.test") ~ list(workspace=2e9))
trial<-trial


sm_trial <-
  new_DATA %>% 
  select(fumigad, AVE_PERI,AVE_INTRA,AVE_PERI_,AVE_INTRA_,CUY_INTRA_,CUY_PERI_, SECTOR, INF,POS)
sm_trial <-
  trial %>% 
  select(trt, age, grade, response)

tbl_summary_t <- 
  sm_trial %>%
  #select(-trt) %>% 
  tbl_summary()

tbl_summary_2 <-  sm_trial %>%
  tbl_summary(
    by = INF,
    type = AVE_PERI ~ "continuous2",
    statistic = 
      list(AVE_PERI ~ c("{mean} ({sd})", "{min}, {max}"), 
            SECTOR~ "{n} / {N} ({p}%)"
           
           ),
    
    label = fumigad ~ "Fumigada por el propietario",
    digits = AVE_PERI_ ~ 1
  )






tbl_summary_2 <-  sm_trial %>%
  tbl_summary(
    by = trt,
    type = age ~ "continuous2",
    statistic = 
      list(age ~ c("{mean} ({sd})", "{min}, {max}"), 
           response ~ "{n} / {N} ({p}%)"),
    label = grade ~ "Pathologic tumor grade",
    digits = age ~ 1
  )



# the data to be used for all modeling
dat_mod <- dat_all |> 
  dplyr::select(id, sector=descriptio, estado, infestada, elevation, ndvi) |> 
  # normalize covariates with respect to the values across the whole village
  mutate(
    sector=factor(sector),
    across(c(elevation, ndvi), \(x) (x - mean(x)) / sd(x)), 
    infestada=ifelse(infestada, 1, 0) # INLA requires 0/1 for response
  )

# plot to get a sense of relationship with covariates
dat_mod |> 
  pivot_longer(c(elevation, ndvi), names_to="covar") |> 
  ggplot(aes(size=value)) +
  geom_sf(col="gray40", shape=1) +
  geom_sf(aes(col=as.factor(infestada)), data=~filter(., !is.na(infestada))) +
  facet_wrap(~covar, nrow=1) +
  theme_bw()

dat_mod |> 
  ggplot(aes(col=sector)) +
  geom_sf() +
  theme_bw()

# Fit a toy model with the current data-------------------------------------------

library(INLA)

# FOR NOW, don't allow inspection of uninhabited houses
# (they're also not counted when estimating village-wide prevalence)
dat_mod <- filter(dat_mod, estado != "DESHABITADA")

loc <- st_coordinates(dat_mod)
bound <- inla.nonconvex.hull(loc)

hist(dist(loc)) # check distribution of distances we're working with

mesh <- inla.mesh.2d(
  loc=loc, 
  # boundary=bound,
  cutoff=0.001,
  max.edge=c(0.025, 0.04),
  min.angle=45,
  offset=c(0.0015, 0.004)
)
plot(mesh)
points(loc)
summary(mesh)

spde_mod <- inla.spde2.pcmatern(
  mesh, alpha=2,
  prior.range=c(0.005, 0.1),
  prior.sigma=c(1, 0.1)
)

unobs_idx <- which(is.na(dat_mod$infestada))

# indices for the MESH (doesn't depend on obs/unobs)
u_idx <- inla.spde.make.index(name="u", n.spde = spde_mod$n.spde)

stack <- inla.stack(
  data=list(y=dat_mod$infestada),
  A=list(inla.spde.make.A(mesh=mesh, loc=loc), 1),
  effects=list(
    c(u_idx, list(Intercept=1)),
    select(st_drop_geometry(dat_mod), sector, elevation, ndvi)
  ),
  tag="stack"
)

stack_unvis <- inla.stack(
  data=list(y=dat_mod$infestada[-obs_idx]),
  A=list(inla.spde.make.A(mesh=mesh, loc=loc_unobs), 1),
  effects=list(
    c(u_idx, list(Intercept = 1)),
    select(st_drop_geometry(dat_mod)[-obs_idx,], elevation, ndvi)
  ),
  tag="stack_unvis"
)

stack_full <- inla.stack(stack_vis, stack_unvis)

fit <- inla(
  y ~ elevation + ndvi + sector + f(u, model=spde_mod),
  data=inla.stack.data(stack, spde_mod=spde_mod),
  family="binomial", 
  control.compute=list(dic=TRUE),
  control.predictor=list(A=inla.stack.A(stack), link=1, compute=TRUE),
  control.fixed=list(prec=0.2, prec.intercept=0.1, expand.factor.strategy="inla")
)

summary(fit)

pred_idx <- inla.stack.index(stack, "stack")$data

eta <- fit$summary.linear.predictor[unobs_idx,]
scores <- eta$sd

names(scores) <- rownames(eta)

score_idx <- scores |> 
  sort(decreasing=TRUE) |> 
  names() |> 
  str_extract("\\d+") |> 
  as.double()

batch <- dat_mod[score_idx[1:10],]

dat_mod |> 
  mutate(mean_risk=fit$summary.fitted.values$mean[1:nrow(dat_mod)]) |> 
  ggplot(aes(col=as.factor(infestada), size=mean_risk)) +
  geom_sf(alpha=0.5) +
  geom_sf(data=batch, col="forestgreen", shape=1, size=8) +
  theme_bw()

write_csv(st_drop_geometry(batch), "1st-batch.csv")
ggsave("1st-batch.pdf", width=6.5, height=7)


tabla_1 <- dat_all%>%
  tbl_summary(by = SECTOR ,
              missing = "no",
              digits = all_categorical() ~ 1) %>%
  add_overall() %>%
  add_p()
tabla_1








######### mapping Aullan #########
##################################

plot.new()

ggplot()+
  
  geom_point(data = aullan_inspections, aes(x = lng, y = lat), col='black',pch= 1)+  
  geom_point(data = aullan_infestados, aes(x = lng, y = lat), col='turquoise',cex=((aullan_infestados$cap_total)/5),pch= 1 )+ 
  #geom_polygon(data = manzanas, aes(x=manzanas$long, y = manzanas$lat, group = ident),linewidth=0.2, fill = NA, color = "black")+
  geom_point(data = aullan_cruzi, aes(x = lng, y = lat), col='red',cex=((aullan_cruzi$pos_total)/5),pch= 19 )+ 
  #geom_point(data = aullan_sprayed, aes(x = lng, y = lat), col='blue',pch= 15 )+ 
  
  ggtitle("AULLAN",subtitle = 'CHAGAS VECTORS')+
  xlab('LONGITUDE')+ylab('LATITUDE')

ggplot()+
  
  geom_point(data = aullan_inspections, aes(x = lng, y = lat), col='black',pch= 1)+  
  #geom_point(data = aullan_infestados, aes(x = lng, y = lat), col='turquoise',cex=((aullan_infestados$cap_total)/5),pch= 1 )+ 
  #geom_polygon(data = manzanas, aes(x=manzanas$long, y = manzanas$lat, group = ident),linewidth=0.2, fill = NA, color = "black")+
  #geom_point(data = aullan_cruzi, aes(x = lng, y = lat), col='red',cex=((aullan_cruzi$pos_total)/5),pch= 19 )+ 
  geom_point(data = aullan_sprayed, aes(x = lng, y = lat), col='blue',pch= 15 )+ 
  
  ggtitle("AULLAN",subtitle = 'SPRAYED')+
  xlab('LONGITUDE')+ylab('LATITUDE')

qmap("cutervo")


google_key(key="AIzaSyArpejkSzR4apFCw9iI_tVzFRMmwaf9duk")

test<-ggmap(get_googlemap(center = c(lon = -78.87726, lat = -6.363826),
                          zoom = 11, scale = 2,
                          maptype ='terrain',
                          color = 'color',key="AIzaSyArpejkSzR4apFCw9iI_tVzFRMmwaf9duk"))

register_google(key = "AIzaSyArpejkSzR4apFCw9iI_tVzFRMmwaf9duk", write = TRUE)

















# # Adding new column based on the sum of other columns:
# depr_df <- depr_df %>% rowwise() %>%
#   mutate(DeprIndex = sum(c_across(Depr1:Depr5)))
# 
# aullan<-aullan%>%rowwise()%>%
#   mutate(habitantes=sum(c_across(genero3,edad6,genero8)))
# 
# 
# 
# 
# library(dplyr)
# 
# # Adding column based on other column:
# depr_df %>%
#   mutate(Status = case_when(
#     endsWith(ID, "R") ~ "Recovered",
#     endsWith(ID, "S") ~ "Sick"
#   ))
# 
# # R adding a column to dataframe based on values in other columns:
# depr_df <- depr_df %>% 
#   mutate(C = if_else(A == B, A + B, A - B))
# 
# 
# # creating a column to dataframe based on values in other columns:
# depr_df <- depr_df %>% 
#   mutate(C = if_else(A == B, "Equal", "Not Equal"))
# 
# # Adding new column based on the sum of other columns:
# depr_df <- depr_df %>% rowwise() %>%
#   mutate(DeprIndex = sum(c_across(Depr1:Depr5)))
# 
# # Multiple conditions when adding new column to dataframe:
# depr_df %>% mutate(Group =
#                      case_when(DeprIndex <= 15 ~ "A", 
#                                DeprIndex <= 20 ~ "B",
#                                DeprIndex >= 21 ~ "C")
# )
# 
# 
# library(tibble)
# depr_df <- depr_df %>%
#   add_column(Is_Depressed = 
#                if_else(.$DeprIndex < 18, TRUE, FALSE),
#              .after="ID")
# 
# 
# 
# df2<-tidyr::separate(df2,col="unifecha",into=c("UNICODE","FECHA"),sep="-")
# 
# 
# 
# aullan <- aullan %>% separate(habitantes, c('EDAD','GENERO'))
# 
# 
# 
# 
# # load dplyr and tidyr library
# library(dplyr)
# library(tidyr)
# 
# # Split name column into firstname and last name
# df <- df %>% separate(Name, c('First Name', 'Last Name'))
# 
###### adding altitude to aullan points ########
########

# aullan<-read.csv("~/Documents/Clases/PROYECTO_208732/RURAL_PLACES_SPATIAL/aullan_sin_desha.csv")
# aullan<-aullan[c(1:2,12:13)]
# aullan_gps<-aullan[c(3:4)]

# # Create an example data.frame
# set.seed(65.7)
# examp_df <- data.frame(x = runif(3, min = -73, max = -72.5), y = runif(3, min = 42,
#                                                                        max = 43))
# prj_dd <- "EPSG:4326"
# 
# # Create and example data.frame with additional columns
# cats <- data.frame(category = c("H", "M", "L"))
# 
# examp_df2 <- data.frame(aullan_gps, aullan)
# 
# # Create an example SpatialPoints
# examp_sp <- SpatialPoints(aullan_gps, proj4string = CRS(prj_dd))
# 
# # Create an example SpatialPointsDataFrame
# examp_spdf <- SpatialPointsDataFrame(examp_sp, data = aullan)
# 
# df_elev_epqs <- get_elev_point(aullan_gps, prj = prj_dd, src = "epqs")
# data.frame(df_elev_epqs)
# 
# df2_elev_epqs <- get_elev_point(examp_df2, prj = prj_dd, src = "epqs")
# 
# 
# 
# # Read the .csv file
# plot.locations_aullan <- 
#   read.csv("aullan_sin_desha.csv",
#            stringsAsFactors = FALSE)
# 
# 
# 
# 
# 
# # note that the easting and northing columns are in columns 1 and 2
# 
# plot.locationsSaullan <- SpatialPointsDataFrame(plot.locations_aullan[,12:13],
#                                                 plot.locations_aullan,    #the R object to convert
#                                                 proj4string = utm17nCRS)   # assign a CRS 
# 
# 
# plot.locationsSp_HARV <- SpatialPointsDataFrame(plot.locations_HARV[,1:2],
#                                                 plot.locations_HARV,    #the R object to convert
#                                                 proj4string = utm18nCRS)   # assign a CRS 
# 
# # look at CRS
# crs(plot.locationsSp_HARV)
# 
# ## CRS arguments:
# ##  +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs
# 
# 
# 
# 
# # first, convert the data.frame to spdf
#   r <- SpatialPointsDataFrame(plot.locations_aullan[,12:13],
#                             plot.locations_aullan)
# 
# # second, assign the CRS in one of two ways
# r <- crs("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs 
# 				 +ellps=WGS84 +towgs84=0,0,0" )
# # or
# 
# crs(r) <- "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs 
# 				 +ellps=WGS84 +towgs84=0,0,0"
# 
# prj_dd <- "EPSG:4326"
# 
# aullan_sp <- SpatialPoints(plot.locations_aullan, proj4string = CRS(prj_dd))
# 
# # Creating a sample dataset
# data <- data.frame(
#   Age = c(0:9, 0:9),
#   Gender = c(rep("Male", 10), rep("Female", 10)),
#   Population = c(200, 250, 300, 350, 400, 450, 500, 550, 600, 650,
#                  190, 240, 290, 330, 380, 430, 480, 530, 580, 630)
# )
# sapply(data, class)
# basic_plot <-  ggplot(
#   aullan_people_data, 
#   aes(
#     x = habitante3, 
#     fill = habitante5, 
#     y = ifelse(
#       test = habitante5 == "MASCULINO", 
#       yes = -count, 
#       no = count
#     )
#   )
# ) + 
#   geom_bar(stat = "identity") 
# 
# ############## WORKING WITH SPRAYING ##############
# #######################################################
# 
# aullan_sprayed<-read.csv("query_result_2023-12-01T14_06_18.218079ZSPRAYED.csv")
# aullan_sprayed<-filter(aullan_sprayed, App.State == "published")
# aullan_sprayed<-aullan_sprayed %>% 
#   mutate(across(where(is.character), toupper))
# aullan_sprayed<-aullan_sprayed[c(7,12,13,17)]
# setnames(aullan_sprayed,old = 'Geopoint.Unicode',new = 'geopoint_unicode')
# 
# aullan_sprayed<-merge(aullan_sprayed,aullan_points,by='geopoint_unicode',all.x=TRUE)
# 
