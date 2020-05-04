
library(readxl)
library(plotly)
library(tidyverse)
library(data.table)



#---- IMPORT DATA ----
path_particules <- "C:/Users/elise/Documents/Data for good/Particules/DataViz/Particules2/"
data_O3_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="O3t",skip=3)
data_NO2_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="NO2t",skip=3)
data_PM_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="PM10-PM2.5t",skip=3)


#---- DATA PREP ----
data_prep <- function(data){
  #enlever les colonnes et lignes à partir de 'Grand Total' pour ne garder que les numéros de communes et concentrations de pollution
  data <- data %>% select(1:(which(colnames(data)=="Grand Total")-1)) %>%
    rename(COM = `Row Labels`) %>%
    filter(nchar(COM)==5) 
  
  data %>%
    #pivoter pour avoir 3 variables : commune, niv_pollution, population
    pivot_longer(cols = colnames(data)[-1], names_to = "POLLUTION", values_to = "POPULATION") %>%
    #convertir en numeric
    mutate(POLLUTION = as.numeric(POLLUTION)) %>%
    #arrondir le nombre d'habitants
    mutate(POPULATION = round(POPULATION)) %>%
    #supprimer les NA (0 habitants concernés par ce niveau de pollution)
    filter(!is.na(POPULATION))
}

#ajouter une colonne avec le type de polluant et
#calculer les concentrations en PM2.5 et PM10 d'après les formules proposées par AtmoSud
data_O3 <- data_prep(data_O3_im) %>% add_column(TYPE_POLLUANT = "O3", .before=1)
data_NO2 <- data_prep(data_NO2_im) %>% add_column(TYPE_POLLUANT = "NO2", .before=1)

data_PM25 <- data_prep(data_PM_im) %>% add_column(TYPE_POLLUANT = "PM2.5", .before=1) %>%
  mutate(POLLUTION = 0.183*POLLUTION+5.92)
data_PM10 <- data_prep(data_PM_im) %>% add_column(TYPE_POLLUANT = "PM10", .before=1) %>%
  mutate(POLLUTION = 0.652*POLLUTION-0.11)

#merger les 4 df
data_pol <- bind_rows(data_O3,data_NO2,data_PM25,data_PM10)

#supprimer les valeurs de populations == 0 restantes
data_pol <- data_pol %>% filter(POPULATION!=0)


#df de correspondance entre le code insee et le nom des communes
data_noms_com_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="O3")
data_noms_com <- data_noms_com_im %>% select(INSEE_COM,NOM_COM)
data_noms_com <- data_noms_com[!duplicated(data_noms_com),]

#ajout du num de département entre parenthèses, notamment pour ne pas avoir de doublons dans les noms de communes (ex : "Aiglun" devient "Aiglun (04)")
data_noms_com <- data_noms_com %>% mutate(NOM_COMMUNE = paste0(NOM_COM," (",substr(INSEE_COM,1,2),")")) %>%
  select(-NOM_COM)

#merger avec data_pol pour ajouter une colonne avec le nom de la commune
df <- data_noms_com %>% right_join(data_pol,by=c("INSEE_COM" = "COM"))

#faire une liste de dataframes par commune
df <- df %>% arrange(NOM_COMMUNE)
dfl <- split(df, f = df$NOM_COMMUNE)

#liste par commune de liste par polluant de dataframes (voir si utile pour combiner les sélecteurs de commune et de polluant)
dfl2 <- list()
for(li in 1:length(dfl)){
  dfl2[[li]] <- list(dfl[[li]]$NOM_COMMUNE[1],split(dfl[[li]], f = dfl[[li]]$TYPE_POLLUANT))
}


#---- VALEURS UTILES ----
#liste des communes
liste_communes <- as.list(data_noms_com['NOM_COMMUNE'])$NOM_COMMUNE
liste_arts_marseille <- liste_communes[grep("Marseille ", liste_communes)]

#max observés de chaque polluant (pour l'échelle des abscisses - pour les min on prendra 0 ; 80 pour l'O3 
max_no2 <- df %>% filter(TYPE_POLLUANT == "NO2") %>% summarise(max_no2=max(POLLUTION))
max_no2 <- max_no2[[1]]
max_o3 <- df %>% filter(TYPE_POLLUANT == "O3") %>% summarise(max_o3=max(POLLUTION)) ; max_o3 <- max_o3[[1]]
max_pm25 <- df %>% filter(TYPE_POLLUANT == "PM2.5") %>% summarise(max_pm=max(POLLUTION)) ; max_pm25 <- max_pm25[[1]]
max_pm10 <- df %>% filter(TYPE_POLLUANT == "PM10") %>% summarise(max_pm=max(POLLUTION)) ; max_pm10 <- max_pm10[[1]]




#---- V1 HISTOGRAMME : sélecteur du polluant ----

#ajouter une barre verticale avec le seuil recommandé par l'OMS : 
#PM2.5 : 10 µg/m3 moyenne annuelle, 25 µg/m3 moyenne sur 24 heures 
#PM10 : 20 µg/m3 moyenne annuelle, 50 µg/m3 moyenne sur 24 heures 
#O3 : 100 µg/m3 moyenne sur 8 heures
#NO2 : 40 µg/m3 moyenne annuelle, 200 µg/m3 moyenne sur 24 heures 


input_commune <- "Marseille 1er Arrondissement (13)"

col_no2 <- '#454545'
col_o3 <- '#016FBF'
col_pm25 <- '#F54A4A'
col_pm10 <- '#FFA55E'

#graph de base
fig <- plot_ly(type = "bar", name='') 

#pour chaque polluant : ajout de l'histogramme, de la ligne rouge de seuil OMS, et du texte "Seuil de l'OMS"
fig <- fig %>% 
  #NO2
  add_bars(data = df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="NO2"),
           x = ~POLLUTION,  y = ~POPULATION,
           name = "NO2", marker=list(color=col_no2), visible = FALSE) %>% 
  add_segments(x=40,xend=40, y=0,yend=1.2*(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="NO2") %>% summarise(max(POPULATION)))[[1]], 
               line=list(color='red'), visible = FALSE) %>%
  add_text(x=40+0.5,y=(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="NO2") %>% summarise(max(POPULATION)))[[1]],
           text = 'Seuil de l\'OMS', textposition = "top right", visible = FALSE) %>%
  
  #O3
  add_bars(data = df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="O3"),
           x = ~POLLUTION,  y = ~POPULATION,
           name = "O3", marker=list(color=col_o3), visible = FALSE) %>% 
  add_segments(x=100,xend=100, y=0,yend=1.2*(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="O3") %>% summarise(max(POPULATION)))[[1]], 
               line=list(color='red'), visible = FALSE) %>%
  add_text(x=100+0.5,y=(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="O3") %>% summarise(max(POPULATION)))[[1]],
           text = 'Seuil de l\'OMS', textposition = "top right", visible = FALSE) %>%
  
  #PM2.5 (qui s'affichent au démarragent : on ne met pas le paramètre "visible=FALSE")
  add_bars(data = df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="PM2.5"),
           x = ~POLLUTION,  y = ~POPULATION,
           name = "PM2.5", marker=list(color=col_pm25)) %>%
  add_segments(x=10,xend=10, y=0,yend=1.2*(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="PM2.5") %>% summarise(max(POPULATION)))[[1]], 
               line=list(color='red')) %>%
  add_text(x=10+0.2,y=(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="PM2.5") %>% summarise(max(POPULATION)))[[1]],
           text= 'Seuil de l\'OMS', textposition = "top right") %>%
  
  #PM10
  add_bars(data = df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="PM10"),
           x = ~POLLUTION,  y = ~POPULATION,
           name = "PM10", marker=list(color=col_pm10), visible = FALSE) %>%
  add_segments(x=20,xend=20, y=0,yend=1.2*(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="PM10") %>% summarise(max(POPULATION)))[[1]], 
               line=list(color='red'), visible = FALSE) %>%
  add_text(x=20+0.5,y=(df %>% filter(NOM_COMMUNE==input_commune, TYPE_POLLUANT=="PM10") %>% summarise(max(POPULATION)))[[1]],
           text= 'Seuil de l\'OMS', textposition = "top right", visible = FALSE)



#ajout des sélecteurs
fig <- fig %>%
  layout(updatemenus = list(
    
    #boutons pour sélectionner le polluant
    list(
      active = -1,
      type= 'buttons', direction = "right",
      y = 0.92, x=0.95,
      buttons = list(
        list(method = "update",
             args = list(list(visible = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                         list(title = paste0("Population soumise aux différents niveaux de concentration en NO2 à ",input_commune),
                         xaxis=list(title="Niveau de pollution (µg/m3)",range = c(0,max_no2)))),
             label = "NO2"),
        
        list(method = "update",
             args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                         list(title = paste0("Population soumise aux différents niveaux de concentration en O3 à ",input_commune),
                              xaxis=list(title="Niveau de pollution (µg/m3)",range = c(80,max_o3)))),
              label = "O3"),

        list(method = "update",
             args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)),
                         list(title = paste0("Population soumise aux différents niveaux de concentration en PM2.5 à ",input_commune),
                              xaxis=list(title="Niveau de pollution (µg/m3)",range = c(0,max_pm25)))),
             label = "PM2.5"),

        list(method = "update",
             args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)),
                         list(title = paste0("Population soumise aux différents niveaux de concentration en PM10 à ",input_commune),
                              xaxis=list(title="Niveau de pollution (µg/m3)",range = c(0,max_pm10)))),
             label = "PM10")
      ))))



#ajout des éléments de layout
fig <- fig %>% layout(title = paste0("Population soumise aux différents niveaux de concentration en polluants à ",input_commune),
                      showlegend=FALSE,
                      xaxis=list(title="Niveau de pollution (µg/m3)",range = c(0,max_pm25)),
                      yaxis=list(title="Population concernée"))


fig

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      
      dccGraph(
        #input_commune <- "Aix-en-Provence (13)",
        figure=fig
        ) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)


#---- V2 HISTOGRAMME : sélecteur de la commune ----


#création de la liste de paramètres "visible" (TRUE/FALSE)
recap_option_com <- list()
list_visible <- list()

for (i in 1:length(dfl)){
  if (i==1)
    list_visible[[i]] <- c(rep(list(TRUE),3),rep(list(FALSE),(3*(length(dfl)-1))))
  if (i>=2 & i<length(dfl))
    list_visible[[i]] <- c(rep(list(FALSE),3*(i-1)),rep(list(TRUE),3),
                         rep(list(FALSE),(3*(length(dfl)-i))))
  if (i==length(dfl))
    list_visible[[i]] <- c(rep(list(FALSE),(3*(length(dfl)-1))),rep(list(TRUE),3))
  
  recap_option_com[[i]] <- list(method="restyle",
                          args=list("visible",list_visible[[i]]),
                          label=names(dfl)[i])
}


#Pour les PM2.5 : pour la 1ère commune création du barplot et ajout de la ligne du seuil de l'OMS
dd <- dfl[[1]]
max_commune <- (dd %>% filter(TYPE_POLLUANT=="PM2.5") %>% summarise(max(POPULATION)))[[1]]

fig2 <- plot_ly(type = "bar", name='') %>%
  
  add_bars(data = dd %>% filter(TYPE_POLLUANT=="PM2.5"),
           x = ~POLLUTION,  y = ~POPULATION,
           name = "PM2.5", marker=list(color=col_pm25)) %>%
  add_segments(x=10,xend=10, y=0,yend=1.1*max_commune, line=list(color='red')) %>%
  add_text(x=10+0.2,y=max_commune, text= 'Seuil de l\'OMS', textposition = "top right") 


#idem pour les autres communes
for (i in 2:length(dfl)){ 
  dd <- dfl[[i]]
  max_commune <- (dd %>% filter(TYPE_POLLUANT=="PM2.5") %>% summarise(max(POPULATION)))[[1]]
  
  fig2 <- fig2 %>%
    
    add_bars(data = dd %>% filter(TYPE_POLLUANT=="PM2.5"),
             x = ~POLLUTION,  y = ~POPULATION,
             name = "PM2.5", marker=list(color=col_pm25), visible = FALSE) %>% 
    add_segments(x=10,xend=10, y=0,yend=1.1*max_commune, line=list(color='red'), visible = FALSE) %>%
    add_text(x=10+0.2,y=max_commune, text= 'Seuil de l\'OMS', textposition = "top right", visible = FALSE) 
} 


#ajout des éléments de layout : titre&co et boutons
fig2 <- fig2 %>% layout(title = "Population soumise aux différents niveaux de concentration en PM2.5",
               showlegend=FALSE,
               xaxis=list(title="Niveau de pollution (µg/m3)",range = c(0,max_pm25)),
               yaxis=list(title="Population concernée"),
               updatemenus=list(
                  list(
                    y = 0.85, x=0.95,
                    buttons=recap_option_com
                  )
                ))

fig2
