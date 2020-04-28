rm(list=ls())

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

col_titre <- "#454545"
col_texte <- "#6A6A6A"

col_no2 <- '#454545'
col_o3 <- '#016FBF'
col_pm25 <- '#F54A4A'
col_pm10 <- '#FFA55E'



#---- IMPORT DATA ----
path_particules <- "C:/Users/elise/Documents/Data for good/Particules/DataViz/Particules2/"

df_list <- read_json(paste0(path_particules,"data_histo_noms_com_polluants2.json"))



#---- DATA PREP ----

#fonction pour enlever les accents
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
  if(input_polluant == "NO2"){col_pol <- col_no2 ; seuil_pol <- 40 ; min_pol <- 0 ; max_pol <- 80 
  }else if(input_polluant == "O3"){col_pol <- col_o3 ; seuil_pol <- 100 ; min_pol <- 80 ; max_pol <- 160
  }else if(input_polluant == "PM2.5"){col_pol <- col_pm25 ; seuil_pol <- 10 ; min_pol <- 0 ; max_pol <- 30 ; input_polluant_list <- "PM"
  }else if(input_polluant == "PM10"){col_pol <- col_pm10 ; seuil_pol <- 20 ; min_pol <- 0 ; max_pol <- 70 ; input_polluant_list <- "PM"}
  
  
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
  fig <- plot_ly(type = "bar", name='') 
  
  #pour chaque polluant : ajout de l'histogramme, de la ligne rouge de seuil OMS, et du texte "Seuil de l'OMS"
  fig <- fig %>% 
    
    add_bars(data = data_com_pol,
             x = ~POLLUTION,  y = ~POPULATION,
             name = input_polluant, marker=list(color=col_pol)) %>% 
    add_segments(x=seuil_pol,xend=seuil_pol, y=0,yend=1.2*(data_com_pol %>% summarise(max(POPULATION)))[[1]], 
                 line=list(color='red')) %>%
    add_text(x=seuil_pol*1.002,y=(data_com_pol %>% summarise(max(POPULATION)))[[1]],
             text = 'Seuil de l\'OMS', textposition = "top right") 
  
  #ajout des éléments de layout
  fig <- fig %>% layout(showlegend=FALSE,
                        title=input_polluant,
                        xaxis=list(title="Niveau de pollution (µg/m3)",range = c(min_pol,max_pol)),
                        yaxis=list(title="Population concernée"))
  
  
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
      htmlDiv(
        list(
          htmlDiv(
            list(
              htmlH2(
                "Population concernée par les différents niveaux de concentration en polluants",
                id = "title",
                style=list("margin-bottom"= "60px","margin-left"= "60px", "font-family"= type_font, 
                           "font-size" = 18, color = col_titre)
              )
            )
          )
        ),
        className = "banner"
      ),
      # Body
      htmlDiv(
        id = "controls-card",
        className = "container",
        style = list(height = "10%", display = "flex", justifyContent = "space-around",
                     paddingBottom = "3rem", width = "80%"),
        children = list(
          htmlDiv(
            id = "commune-select-outer",
            style = list(width = "25%"),
            children = list(
              htmlLabel("Sélectionnez une commune",style=list("font-family"= type_font, 
                                                              "font-size" = 14, color = col_texte)),
              htmlDiv(
                id = "commune-select-dropdown-outer",
                children = list(
                  dccDropdown(
                    id = "commune-select",
                    options = lapply(
                      as.list(liste_communes),
                      function(x){list(label = Unaccent(x), value = x)}
                    ),
                    value=input_commune,
                    style=list("font-family"= type_font, "font-size" = 14, color = col_texte),
                    searchable = TRUE #permet l'autocomplétion
                  )
                )
              )
            )
          ),
          
          htmlDiv(
            id = "polluant-select-outer",
            children = list(
              htmlLabel("Sélectionnez un polluant",style=list("font-family"= type_font, 
                                                              "font-size" = 14, color = col_texte)),
              dccDropdown(
                id = "polluant-select",
                options = list(list(label = 'NO2', value = 'NO2'),
                               list(label = 'O3', value = 'O3'),
                               list(label = 'PM2.5', value = 'PM2.5'),
                               list(label = 'PM10', value = 'PM10')),
                value="PM2.5",
                style=list("font-family"= type_font, "font-size" = 14, color = col_texte)
              )
            )
          )
        )
      ),
      htmlDiv(
        list(
          htmlDiv(
            id = "top-graphs",
            style = list(display = "flex", justifyContent = "space-between", 
                         width = "80%", margin = "0 auto"),
            children = list(
              htmlDiv(
                id = "left-top-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-pm25",
                    figure = createHistogram(input_commune,"PM2.5")
                  )
                ),
                style = list(width = "100%", float = "center", boxSizing = "border-box")
              ),
              htmlDiv(
                id = "right-top-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-pm10",
                    figure = createHistogram(input_commune,"PM10")
                  )
                ),
                style = list(width = "100%", float = "center", boxSizing = "border-box")
              )
            )
          ),
          htmlDiv(
            id = "bottom-graphs",
            style = list(display = "flex", justifyContent = "space-between", 
                         width = "80%", margin = "0 auto"),
            children = list(
              htmlDiv(
                id = "left-bottom-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-no2",
                    figure = createHistogram(input_commune,"NO2")
                  )
                ),
                style = list(width = "100%", float = "center", boxSizing = "border-box")
              ),
              htmlDiv(
                id = "right-bottom-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo-o3",
                    figure = createHistogram(input_commune,"O3")
                  )
                ),
                style = list(width = "100%", float = "center", boxSizing = "border-box")
              )
            )
          )
        )
      )
    )
  )
)



app$callback(
  output("id-histo-pm25", "figure"),
  list(input("commune-select", "value"),
       input("polluant-select", "value")),
  
  function(input_commune,input_polluant){
    createHistogram(input_commune,"PM2.5")
  }
)

app$callback(
  output("id-histo-pm10", "figure"),
  list(input("commune-select", "value"),
       input("polluant-select", "value")),
  
  function(input_commune,input_polluant){
    createHistogram(input_commune,"PM10")
  }
)

app$callback(
  output("id-histo-no2", "figure"),
  list(input("commune-select", "value"),
       input("polluant-select", "value")),
  
  function(input_commune,input_polluant){
    createHistogram(input_commune,"NO2")
  }
)

app$callback(
  output("id-histo-o3", "figure"),
  list(input("commune-select", "value"),
       input("polluant-select", "value")),
  
  function(input_commune,input_polluant){
    createHistogram(input_commune,"O3")
  }
)


if (appName != "") {
  app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050)) 
} else {
  app$run_server(debug = TRUE)
}