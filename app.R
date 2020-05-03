rm(list=ls()) #instruction permettant d'effacer la mémoire R

library(jsonlite)
library(plotly)
library(tidyverse)
library(data.table)

library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashTable)



#---- PARAMETRES CSS ----

type_font <- "barlow"
size_title <- 24
size_text <- 18
size_text_axis <- 12

col_title <- "#454545"
col_text <- "#6A6A6A"
col_graph <- "#1680A5"
col_bleu_fonce <- "#194561"
col_gris_leger <- "#F4F4F4"



#---- IMPORT DATA ----

path_particules <- "C:/Users/elise/Documents/Data for good/Particules/DataViz/Particules2/"

df_list <- read_json(paste0(path_particules,"data_histo_noms_com_polluants2.json"))



#---- DATA PREP ----

#fonction pour enlever les accents sur value pour éviter les problèmes d'encodage
Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}



#---- VALEURS UTILES ----

#liste des communes 
liste_communes <- names(df_list)

#paramètres par défaut dans les menus déroulants
input_commune <- "Marseille 1er Arrondissement (13)"
input_polluant <- "PM2.5"



#---- Fonction createHistogram ----

createHistogram <- function(input_commune, input_polluant){
  
  #-- Paramètres par polluant :
  #les max et min serviront comme paramètres pour l'échelle des axes des absisses. Les valeurs à utiliser ont été déterminées à partir des données brutes
  #les seuils sont les valeurs recommandées par l'OMS
  input_polluant_list <- input_polluant #variable utilisée pour le calcul des PM2.5 et PM10 selon PM
  if(input_polluant == "NO2"){seuil_pol <- 40 ; min_pol <- 0 ; max_pol <- 80 
  }else if(input_polluant == "O3"){seuil_pol <- 100 ; min_pol <- 80 ; max_pol <- 160
  }else if(input_polluant == "PM2.5"){seuil_pol <- 10 ; min_pol <- 0 ; max_pol <- 30 ; input_polluant_list <- "PM"
  }else if(input_polluant == "PM10"){seuil_pol <- 20 ; min_pol <- 0 ; max_pol <- 70 ; input_polluant_list <- "PM"}
  
  
  #--Dataframe à utiliser (variables : POPULATION et POLLUTION) pour cette commune et ce polluant
  #on prend les données dans la liste df_list, et on les agrège sous forme de dataframe
  data_com_pol <- do.call(rbind.data.frame, df_list[[input_commune]][[input_polluant_list]]) %>%
    rename(POPULATION = P, POLLUTION = L)
  
  if(input_polluant_list=="PM"){
    data_com_pol <- data_com_pol %>%
      #on calcule la valeur de la pollution pour les PM2.5 et PM10, par la formule proposée par AtmoSud
      mutate(POLLUTION = case_when(
        input_polluant=="PM2.5" ~ 0.183*POLLUTION+5.92,
        input_polluant=="PM10" ~ 0.652*POLLUTION-0.11
      ))
  }
  
  
  #--Graph 
  fig <- plot_ly(type = "bar", name = '', height = 200, width = 600) 
  
  #pour chaque polluant : ajout de l'histogramme, de la ligne rouge de seuil OMS, et du texte "Seuil de l'OMS"
  fig <- fig %>% 
    
    add_bars(data = data_com_pol,
             x = ~POLLUTION,  y = ~POPULATION,
             name = input_polluant, marker=list(color=col_graph)) %>% 
    add_segments(x=seuil_pol,xend=seuil_pol, y=0,yend=1.2*(data_com_pol %>% summarise(max(POPULATION)))[[1]], 
                 line=list(color='red')) %>%
    add_text(x=seuil_pol*1.002,y=(data_com_pol %>% summarise(max(POPULATION)))[[1]],
             text = 'Seuil de l\'OMS', textposition = "top right") 
  
  #ajout des éléments de layout
  fig <- fig %>% layout(showlegend=FALSE,
                        title=input_polluant,
                        font = list(family = type_font, size = size_text_axis, color = col_text),
                        xaxis=list(title = "Niveau de pollution (µg/m3)", range = c(min_pol,max_pol)),
                        yaxis=list(title = "Nombre d'habitants"), 
                        plot_bgcolor = col_gris_leger)
  
  fig
}



#---- APP DASH ----

appName <- Sys.getenv("DASH_APP_NAME")
if (appName != ""){
  pathPrefix <- sprintf("/%s/", appName)
  
  Sys.setenv(DASH_ROUTES_PATHNAME_PREFIX = pathPrefix,
             DASH_REQUESTS_PATHNAME_PREFIX = pathPrefix)
}


app <- Dash$new()


app$layout(
  htmlDiv(
    list(
      # Titre
      htmlDiv(
        htmlH2(
          "Habitants exposés à différents niveaux de pollution par commune",
          id = "title",
          style=list(marginTop = "40px", marginBottom = "30px", marginLeft = "160px", 
                     fontFamily = type_font, fontSize = size_title, color = col_title)),
        className = "banner"
      ),
      # Sélecteur de la commune
      htmlDiv(
        id = "controls-card",
        className = "container",
        style = list(fontFamily = type_font, fontSize = size_text, color = col_text,
                     height = "10%", justifyContent = "right", display = "flex", 
                     marginBottom = "40px", marginRight = "160px"),
        children = list(
          htmlDiv(
            id = "commune-select-outer",
            style = list(width = "23%"),
            children = list(
              htmlLabel("Sélectionnez une commune"),
              htmlDiv(
                id = "commune-select-dropdown-outer",
                dccDropdown(
                  id = "commune-select",
                  options = lapply(
                    as.list(liste_communes),
                    function(x){list(label = x, value = Unaccent(x))}
                  ),
                  value=Unaccent(input_commune),
                  style=list(backgroundColor = col_gris_leger),
                  searchable = TRUE #permet l'autocomplétion
                )
              )
            )
          )
        )
      ),
      
      # Graphs
      htmlDiv(
        id = "graphs",
        style = list(justifyContent = "space-between", boxSizing = "border-box"),
        children = list(
          htmlDiv( # Graphs du haut
            id = "top-graphs",
            style = list(display = "flex", justifyContent = "space-between", 
                         width = "80%", height = "40vh", margin = "0 auto"),
            children = list(
              htmlDiv(
                id = "left-top-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-pm25",
                    figure = createHistogram(input_commune,"PM2.5")
                  )
                )
              ),
              htmlDiv(
                id = "right-top-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-pm10",
                    figure = createHistogram(input_commune,"PM10")
                  )
                )
              )
            )
          ),
          htmlDiv( # Graphs du bas
            id = "bottom-graphs",
            style = list(display = "flex", justifyContent = "space-between", 
                         width = "80%", height = "30vh", margin = "0 auto"),
            children = list(
              htmlDiv(
                id = "left-bottom-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-no2",
                    figure = createHistogram(input_commune,"NO2")
                  )
                )
              ),
              htmlDiv(
                id = "right-bottom-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-o3",
                    figure = createHistogram(input_commune,"O3")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


# Réactivité avec les callbacks qui modifient les graphiques en fonction de la commune

app$callback(
  output("id-histo-pm25", "figure"),
  list(input("commune-select", "value")),
  
  function(input_commune){
    createHistogram(input_commune,"PM2.5")
  }
)

app$callback(
  output("id-histo-pm10", "figure"),
  list(input("commune-select", "value")),
  
  function(input_commune){
    createHistogram(input_commune,"PM10")
  }
)

app$callback(
  output("id-histo-no2", "figure"),
  list(input("commune-select", "value")),
  
  function(input_commune){
    createHistogram(input_commune,"NO2")
  }
)

app$callback(
  output("id-histo-o3", "figure"),
  list(input("commune-select", "value")),
  
  function(input_commune){
    createHistogram(input_commune,"O3")
  }
)


if (appName != "") {
  app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050)) 
} else {
  app$run_server(debug = TRUE)
}