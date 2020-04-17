rm(list=ls())

library(readxl)
library(plotly)
library(tidyverse)
library(data.table)

library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashTable)

#options(encoding = "UTF-8")


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


#df de correspondance entre le code insee et le nom des communes (classé par département puis ordre alphabétique des communes)
data_noms_com_im <- read_xlsx(paste0(path_particules,"Exposition_2018_v2019_com.xlsx"),sheet="O3")
data_noms_com <- data_noms_com_im %>% select(INSEE_COM,NOM_COM)
data_noms_com <- data_noms_com[!duplicated(data_noms_com),]
data_noms_com <- data_noms_com %>% arrange(substr(INSEE_COM,1,2),NOM_COM)

#merger avec data_pol pour ajouter une colonne avec le nom de la commune
df <- data_noms_com %>% right_join(data_pol,by=c("INSEE_COM" = "COM"))

#ne garder que le département au lieu du num de la commune
df <- df %>% mutate(INSEE_COM = substr(INSEE_COM,1,2)) %>%
  rename(DEPT = INSEE_COM) %>% 
  arrange(DEPT,NOM_COM) %>% 
  mutate(DEPT = case_when(
    DEPT=="04" ~ "04 - Alpes-de-Haute-Provence",
    DEPT=="05" ~ "05 - Hautes-Alpes",
    DEPT=="06" ~ "06 - Alpes-Maritimes",
    DEPT=="13" ~ "13 - Bouches-du-Rhône",
    DEPT=="30" ~ "30 - Gard",
    DEPT=="83" ~ "83 - Var",
    DEPT=="84" ~ "84 - Vaucluse"
  ))
  
#ajouter des variables avec les noms de communes et de départements sans accents          
          
#suppression des accents pour éviter les bugs d'encodage
#ä -> \U{00E4} ; â -> \U{00E2} ; à -> \U{00E0} ; À -> \U{00C0} ; Â -> \U{00C2}
#ç -> \U{00E7} ; Ç -> \U{00C7} ; œ -> \U{0153}
#é -> \U{00E9} ; É -> \U{00C9} ; è -> \U{00E8} ; È -> \U{00C8}
#ê -> \U{00EA} ; Ê -> \U{00CA} ; ë -> \U{00EB}
#ï -> \U{00EF} ; î -> \U{00EE}
#ô -> \U{00F4} ; ö -> \U{00F6} ;
#ù -> \U{00F9} ; û -> \U{00FB} ; ü -> \U{00FC}

#recherche d'éléments
#grep(pattern = "è", liste_communes, value = TRUE, fixed=FALSE)
#remplacement de caractères
#liste_departements <- gsub(pattern = "ô" , replacement = "\U{00F4}" , liste_departements, fixed = TRUE)
#df$DEPT <- gsub(pattern = "ô" , replacement = "\U{00F4}" , df$DEPT, fixed = TRUE)

#df$DEPT <- gsub(pattern = "ô" , replacement = "o" , df$DEPT, fixed = TRUE)


#---- VALEURS UTILES ----
#liste des communes & départements (nb : il y a des noms de communes utilisés dans plusieurs départements)
liste_communes <- as.list(data_noms_com['NOM_COM'])$NOM_COM
liste_arts_marseille <- liste_communes[grep("Marseille ", liste_communes)]
liste_departements <- unique(df$DEPT)
df_dept_com <- df %>% select(DEPT,NOM_COM) %>% distinct()


#max observés de chaque polluant (pour l'échelle des abscisses - pour les min on prendra 0 ; 80 pour l'O3 
max_no2 <- df %>% filter(TYPE_POLLUANT == "NO2") %>% summarise(max_no2=max(POLLUTION))
max_no2 <- max_no2[[1]]
max_o3 <- df %>% filter(TYPE_POLLUANT == "O3") %>% summarise(max_o3=max(POLLUTION)) ; max_o3 <- max_o3[[1]]
max_pm25 <- df %>% filter(TYPE_POLLUANT == "PM2.5") %>% summarise(max_pm=max(POLLUTION)) ; max_pm25 <- max_pm25[[1]]
max_pm10 <- df %>% filter(TYPE_POLLUANT == "PM10") %>% summarise(max_pm=max(POLLUTION)) ; max_pm10 <- max_pm10[[1]]





#---- APP DASH ----

appName <- Sys.getenv("DASH_APP_NAME")
if (appName != ""){
  pathPrefix <- sprintf("/%s/", appName)

  Sys.setenv(DASH_ROUTES_PATHNAME_PREFIX = pathPrefix,
             DASH_REQUESTS_PATHNAME_PREFIX = pathPrefix)
}


input_dept <- "13 - Bouches-du-Rhône" #\U{00F4}
input_commune <- "Aix-en-Provence"
input_polluant <- "PM2.5"

col_no2 <- '#454545'
col_o3 <- '#016FBF'
col_pm25 <- '#F54A4A'
col_pm10 <- '#FFA55E'



#######################################################################

app <- Dash$new(name = "Population soumise à la pollution")


#éléments spécifiques à chaque polluant : nom, couleur col_pol, seuil OMS seuil_pol, range (min-80 O3 sinon 0- &max)

# Create histogram
createHistogram <- function(input_dept, input_commune, input_polluant){
  
  #paramètres par polluant
  if(input_polluant == "NO2"){col_pol <- col_no2 ; seuil_pol <- 40 ; min_pol <- 0 ; max_pol <- max_no2
  }else if(input_polluant == "O3"){col_pol <- col_o3 ; seuil_pol <- 100 ; min_pol <- 80 ; max_pol <- max_o3
  }else if(input_polluant == "PM2.5"){col_pol <- col_pm25 ; seuil_pol <- 10 ; min_pol <- 0 ; max_pol <- max_pm25
  }else if(input_polluant == "PM10"){col_pol <- col_pm10 ; seuil_pol <- 20 ; min_pol <- 0 ; max_pol <- max_pm10}
  
  #graph de base
  fig <- plot_ly(type = "bar", name='') 
  
  #pour chaque polluant : ajout de l'histogramme, de la ligne rouge de seuil OMS, et du texte "Seuil de l'OMS"
  fig <- fig %>% 

    add_bars(data = df %>% filter(DEPT==input_dept, NOM_COM==input_commune, TYPE_POLLUANT==input_polluant),
             x = ~POLLUTION,  y = ~POPULATION,
             name = input_polluant, marker=list(color=col_pol)) %>% 
    add_segments(x=seuil_pol,xend=seuil_pol, y=0,yend=1.2*(df %>% filter(DEPT==input_dept, NOM_COM==input_commune, TYPE_POLLUANT==input_polluant) %>% summarise(max(POPULATION)))[[1]], 
                 line=list(color='red')) %>%
    add_text(x=seuil_pol*1.002,y=(df %>% filter(DEPT==input_dept, NOM_COM==input_commune, TYPE_POLLUANT==input_polluant) %>% summarise(max(POPULATION)))[[1]],
             text = 'Seuil de l\'OMS', textposition = "top right") 
    
  #ajout des éléments de layout
  fig <- fig %>% layout(showlegend=FALSE,
                        xaxis=list(title="Niveau de pollution (µg/m3)",range = c(min_pol,max_pol)),
                        yaxis=list(title="Population concernée"))
  
  
  fig
}



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
                style=list("margin-bottom"= "60px", "font-family"= "calibri")
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
        style = list(
          height = "10%",
          display = "flex",
          justifyContent = "space-around",
          paddingBottom = "3rem",
          width = "80%"
          
        ),
        children = list(
          htmlDiv(
            id = "departement-select-outer",
            style = list(width = "25%"),
            children = list(
              htmlLabel("Sélectionnez un département",style=list("font-family"= "calibri")),
              dccDropdown(
                id = "departement-select",
                options = lapply(
                  liste_departements, function(x){
                    list(label = x, value = x)
                  }
                ),
                value = input_dept,
                clearable = FALSE,
                style=list("font-family"= "calibri")
              ),
              htmlDiv(
                id = "commune-select-outer",
                children = list(
                  htmlLabel("Sélectionnez une commune",style=list("font-family"= "calibri")),
                  htmlDiv(
                    id = "commune-select-dropdown-outer",
                    children = list(
                      dccDropdown(
                        id = "commune-select",
                        # options = lapply(
                        #   as.list((df_dept_com %>% filter(DEPT==input_dept) %>% select(NOM_COM))$NOM_COM),
                        #   function(x){
                        #     list(label = x, value = x)
                        #   }
                        # ),
                        # value=input_commune,
                        # style=list("font-family"= "calibri"),
                        # searchable = TRUE #permet l'autocomplétion
                      )
                    )
                  )
                  
                )
              )
            )
          ),
          
          htmlDiv(
            id = "polluant-select-outer",
            children = list(
              htmlLabel("Sélectionnez un polluant",style=list("font-family"= "calibri")),
              dccDropdown(
                id = "polluant-select",
                options = list(list(label = 'NO2', value = 'NO2'),
                               list(label = 'O3', value = 'O3'),
                               list(label = 'PM2.5', value = 'PM2.5'),
                               list(label = 'PM10', value = 'PM10')),
                value="PM2.5",
                style=list("font-family"= "calibri")
              )
            )
          )
        )
      ),
      htmlDiv(
        list(
          htmlDiv(
            id = "top-graphs",
            style = list(
              display = "flex", 
              justifyContent = "space-between", 
              width = "80%",
              margin = "0 auto"
            ),
            children = list(
              htmlDiv(
                id = "left-top-graphs",
                className = "container",
                list(
                  dccGraph(
                    id = "id-histo",
                    figure = createHistogram(
                      input_dept,
                      input_commune,
                      input_polluant
                    )
                  )
                ),
                style = list(
                  width = "100%", 
                  float = "center", 
                  boxSizing = "border-box"
                )
              )
            ),
            #style = list(
            #display = "flex",
            #marginTop = "5rem", 
            #width = "100%", 
            #float = "left", 
            #boxSizing = "border-box"
            #)
          )
        )
      )
    )
  )
)


# app$callback(
#   output("dep-name", "children"),
#   list(input("departement-select", "value")),
#   function(input_dept){
#     input_dept
#   }
# )

# app$callback(
#   output("commune-select-dropdown-outer", "style"),
#   list(input("departement-select", "value")),
#   function(input_dept){
#     if (input_dept == "13 - Bouches-du-Rhône"){
#       return(list(display = "block"))
#     }
#     return(list(display = "none"))
#   }
# )



app$callback(
  output("commune-select-dropdown-outer", "children"),
  list(input("departement-select", "value")),
  function(input_dept){
    df_dept <- df %>% filter(DEPT==input_dept)
    dccDropdown(
      id = "commune-select",
      options = lapply(
        as.list((df_dept_com %>% filter(DEPT==input_dept) %>% select(NOM_COM))$NOM_COM),
        function(x){
          list(label = x, value = x)
        }
      ),
      value=(df_dept_com %>% filter(DEPT==input_dept) %>% select(NOM_COM))$NOM_COM[1],
      style=list("font-family"= "calibri"),
      searchable = TRUE #permet l'autocomplétion
    )
  }
)




app$callback(
  output("id-histo", "figure"),
  list(input("departement-select", "value"),
       input("commune-select", "value"),
       input("polluant-select", "value")),

  function(input_dept,input_commune,input_polluant){
    createHistogram(input_dept,input_commune,input_polluant)
  }
)


if (appName != "") {
  app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050)) 
} else {
  app$run_server(debug = TRUE)
}