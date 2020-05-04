
rm(list=ls())

library(readxl)
library(tidyverse)
library(data.table)
library(jsonlite)


#---- IMPORT DATA ----

path_particules <- "C:/Users/elise/Documents/Data for good/Particules/DataViz/Particules2/"

#tableaux croisés dynamiques de la population concernée par chaque niveau de pollution pour chaque commune
data_O3_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="O3t",skip=3)
data_NO2_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="NO2t",skip=3)
data_PM_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="PM10-PM2.5t",skip=3)

#feuille permettant de récupérer la correspondance entre les codes INSEE et noms de communes
data_noms_com_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="O3")


#---- DATA PREP ----

#df de correspondance entre le code insee et le nom des communes (classé par département puis ordre alphabétique des communes)
data_noms_com <- data_noms_com_im %>% select(INSEE_COM,NOM_COM)
data_noms_com <- data_noms_com[!duplicated(data_noms_com),]
data_noms_com <- data_noms_com %>% arrange(substr(INSEE_COM,1,2),NOM_COM)

#ajouter le numéro du département après le numéro de la commune
data_noms_com <- data_noms_com %>% mutate(NOM_COM = paste0(NOM_COM," (",substr(INSEE_COM,1,2),")"))


#traitement des tableaux croisés dynamiques pour en sortir une dataframe avec : 
#nom du polluant (TYPE_POLLUANT), commune (COM), niveau de pollution (L), population concernée (P)

data_prep <- function(data){
  #enlever les colonnes et lignes à partir de 'Grand Total' pour ne garder que les numéros de communes et concentrations de pollution
  data <- data %>% select(1:(which(colnames(data)=="Grand Total")-1)) %>%
    rename(COM = `Row Labels`) %>%
    filter(nchar(COM)==5) 
  
  data %>%
    #pivoter pour avoir 3 variables : commune, niv_pollution, population
    pivot_longer(cols = colnames(data)[-1], names_to = "L", values_to = "P") %>%
    #convertir en numeric
    mutate(L = as.numeric(L)) %>%
    #arrondir le nombre d'habitants
    mutate(P = round(P)) %>%
    #supprimer les NA (0 habitants concernés par ce niveau de pollution)
    filter(!is.na(P))
}

#ajouter une colonne avec le type de polluant 
data_O3 <- data_prep(data_O3_im) %>% add_column(TYPE_POLLUANT = "O3", .before=1)
data_NO2 <- data_prep(data_NO2_im) %>% add_column(TYPE_POLLUANT = "NO2", .before=1)
data_PM <- data_prep(data_PM_im) %>% add_column(TYPE_POLLUANT = "PM", .before=1)

#regroupement en une df
data_pol <- bind_rows(data_O3,data_NO2,data_PM)

#supprimer les valeurs de populations == 0 restantes
data_pol <- data_pol %>% filter(P!=0)


#merger avec data_noms_com pour ajouter une colonne avec le nom de la commune
df <- data_noms_com %>% right_join(data_pol,by=c("INSEE_COM" = "COM")) %>% 
  select(-INSEE_COM) %>% arrange(NOM_COM)


#constitution d'une liste de communes, avec pour chacune une liste des types de polluants,
#avec pour chacun une dataframe de la population concernée P par chaque niveau de polluant L

liste_communes <- data_noms_com$NOM_COM

df_list <- list() #initialisation de la liste contenant toutes les données
for(com in 1:length(liste_communes)){ #pour chaque nom de commune

  data_com <- df %>% filter(NOM_COM == liste_communes[com]) #sélection des données de la commune
  list_no2 <- data_com %>% filter(TYPE_POLLUANT == "NO2") %>% select(L,P) #dataframe avec les données pour le NO2
  list_o3 <- data_com %>% filter(TYPE_POLLUANT == "O3") %>% select(L,P) #df avec les données pour l'O3
  list_pm <- data_com %>% filter(TYPE_POLLUANT == "PM") %>% select(L,P) #df avec les données pour les PM
  list_com <- list(NO2=list_no2,O3=list_o3,PM=list_pm) #liste de ces df
  
  df_list[com] <- list(list_com) #ajout de cette liste correspondant à la commune com à df_list
  names(df_list)[com] <- liste_communes[com] 
}

write_json(df_list,paste0(path_particules,"data_histo_noms_com_polluants.json"))


