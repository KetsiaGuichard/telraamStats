

# Packages
library(tidyverse)



# Fonction pour fusion de données de telraam et meteo
new_data = function(df_meteo, data_mouv){

  df_meteo$date = lubridate::ymd_h(df_meteo$DATE)

  df_meteo$id_join = paste0(date(df_meteo$date), "_", hour(df_meteo$date))

  df_meteo = df_meteo %>% select(id_join, date,GLO,U,VV, T, RR1)   # Sélection des variables pertinentes de météo

  data_mouv = data_mouv %>% mutate(vehicule = car + heavy) %>%  select(date, segment_name, uptime, v85, vacation, holiday, weekday, vehicule, pedestrian, bike, uptime_quality, type)

  # Convertir la chaîne en datetime et spécifier la timezone CEST
  datetime_cest <- lubridate::ymd_hms(data_mouv$date, tz = "Europe/Paris")

  # Convertir la datetime en UTC
  datetime_utc <- with_tz(datetime_cest, tzone = "UTC")

  data_mouv$date = datetime_utc

  data_mouv$id_join = paste0(date(data_mouv$date), "_", hour(data_mouv$date))


  result = left_join(data_mouv, df_meteo, by="id_join")

  return(result)
}




# Fonction pour fusion de données de telraam (capteurs V2) et meteo
new_dataV2 = function(df_meteo, data_mouv){

  df_meteo$date = lubridate::ymd_h(df_meteo$DATE)

  df_meteo$id_join = paste0(date(df_meteo$date), "_", hour(df_meteo$date))

  df_meteo = df_meteo %>% select(id_join, date,GLO,U,VV, T)   # Sélection des variables pertinentes de météo

  data_mouv = data_mouv %>% mutate(vehicule = car + heavy) %>%  select(date, segment_name, uptime, v85, vacation, holiday, weekday, vehicule, pedestrian, bike, uptime_quality, type, brightness, sharpness)

  # Convertir la chaîne en datetime et spécifier la timezone CEST
  datetime_cest <- lubridate::ymd_hms(data_mouv$date, tz = "Europe/Paris")

  # Convertir la datetime en UTC
  datetime_utc <- with_tz(datetime_cest, tzone = "UTC")

  data_mouv$date = datetime_utc

  data_mouv$id_join = paste0(date(data_mouv$date), "_", hour(data_mouv$date))


  result = left_join(data_mouv, df_meteo, by="id_join")

  return(result)
}




#Fonction de visu de proportion de NA
visu_na = function(data){
  # Entrée: données quelconques
  # Sortie: graphique de proportion de valeurs manquantes par variable

  miss_proportion = colMeans(is.na(data))

  missing_data = data.frame(variable = names(miss_proportion),
                            proportion_missing = miss_proportion)

  p = ggplot(missing_data, aes(x = reorder(variable, proportion_missing), y=proportion_missing, fill=variable)) +
    geom_bar(stat = "identity")+
    geom_text(aes(label=round(proportion_missing,3)), hjust=-0.1)+
    coord_flip()+
    labs(title = "Proportion de valeurs manquante par variable",
         x="Variables",
         y="Proportion de valeurs manquantes")+
    theme_minimal()

  return(p)
}



# Fonction pour analyse de corrélation (de spearman) des variables météo

#' Calculate and Plot Correlations Between Uptime and Meteorological Variables
#'
#' This function calculates the correlations between uptime and specified meteorological variables,
#' and then visualizes these correlations using a bar plot.
#'
#' @param complete_data A data frame containing the 'uptime' column and the specified meteorological variables.
#' @param liste_var_meteo A character vector specifying the names of the meteorological variables.
#' @return A ggplot object displaying the bar plot of correlations.
#' @import ggplot2
#' @examples
#' # Assuming complete_data is a data frame containing 'uptime' and the meteorological variables
#' # complete_data <- data.frame(uptime = rnorm(100), GLO = rnorm(100), T = rnorm(100), U = rnorm(100), VV = rnorm(100))
#' # corrélation_uptime(complete_data, c("GLO", "T", "U", "VV"))
corrélation_uptime <- function(complete_data, liste_var_meteo = c("GLO", "T", "U", "VV")) {

  # Vérification des arguments
  if (!is.data.frame(complete_data)) {
    stop("complete_data must be a data frame")
  }
  if (!all(c("uptime", liste_var_meteo) %in% colnames(complete_data))) {
    stop("complete_data must contain 'uptime' and the specified meteorological variables")
  }
  if (!is.character(liste_var_meteo)) {
    stop("liste_var_meteo must be a character vector")
  }

  # Initialiser un vecteur pour stocker les corrélations
  correlation <- numeric(length(liste_var_meteo))

  # Calculer les corrélations pour chaque variable météo
  for (i in seq_along(liste_var_meteo)) {
    var <- liste_var_meteo[i]
    correlation[i] <- cor(complete_data$uptime, complete_data[[var]], use = "complete.obs")
  }

  # Créer un data frame avec les variables météo et leurs corrélations
  correlation_df <- data.frame(Variable = liste_var_meteo, Correlation = correlation)

  # Visualiser avec un diagramme en barres
  plot <- ggplot(correlation_df, aes(x = Variable, y = Correlation, fill = Correlation > 0)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Correlation, 2)), vjust = -0.3) +
    labs(title = "Corrélation entre uptime et les variables météo",
         x = "Variables météo",
         y = "Corrélation", subtitle = ifelse(length(unique(complete_data$segment_name))==1, unique(complete_data$segment_name[[1]]), "Ensemble des capteurs")) +
    theme_minimal()
  result = list(correlation_df, plot)
  return(result)
}







# Fonction pour l'analyse de corrélation des variables météo avec les types de traffic

# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(tidyr)

#' Calculate and Plot Correlations Between Meteorological Variables and Traffic Types
#'
#' This function calculates the correlations between specified meteorological variables
#' and different types of traffic (bikes, vehicles, pedestrians), and visualizes these
#' correlations using a grouped bar plot.
#'
#' @param complete_data A data frame containing the traffic data and meteorological variables.
#' @param liste_var_meteo A character vector specifying the names of the meteorological variables.
#' @return A ggplot object displaying the grouped bar plot of correlations.
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @examples
#' # Assuming complete_data is a data frame containing traffic data and meteorological variables
#' # complete_data <- data.frame(bike = rnorm(100), vehicule = rnorm(100), pedestrian = rnorm(100), GLO = rnorm(100), T = rnorm(100), U = rnorm(100), VV = rnorm(100))
#' # correlation_traffic(complete_data, c("GLO", "T", "U", "VV"))
correlation_traffic <- function(complete_data, liste_var_meteo = c("GLO", "T", "U", "VV", "RR1")) {

  # Vérification des arguments
  if (!is.data.frame(complete_data)) {
    stop("complete_data must be a data frame")
  }
  if (!is.character(liste_var_meteo)) {
    stop("liste_var_meteo must be a character vector")
  }
  if (!all(liste_var_meteo %in% colnames(complete_data))) {
    stop("complete_data must contain the specified meteorological variables")
  }

  # Initialiser des vecteurs pour stocker les corrélations
  correlation_velo = c()
  correlation_vehicules = c()
  correlation_pieton = c()

  # Calculer les corrélations pour chaque variable météo et chaque type de traffic
  for (var in 1:length(liste_var_meteo)){
    correlation_velo = c(correlation_velo, cor(complete_data$bike, complete_data[liste_var_meteo[var]] , use = "complete.obs", method = "spearman"))
    correlation_vehicules = c(correlation_vehicules, cor(complete_data$vehicule, complete_data[liste_var_meteo[var]] , use = "complete.obs", method = "spearman"))
    correlation_pieton = c(correlation_pieton, cor(complete_data$pedestrian, complete_data[liste_var_meteo[var]] , use = "complete.obs", method = "spearman"))
  }

  # création d'un data frame pour la corrélation entre les variables météo et les variables de traffic
  correlation_df_traffic = data.frame(liste_var_meteo, correlation_velo, correlation_vehicules, correlation_pieton)

  # Pivot de la table pour la visualisation
  correlation_df_traffic = correlation_df_traffic %>% pivot_longer(cols = c(correlation_velo, correlation_vehicules, correlation_pieton), names_to = "Traffic", values_to = "Correlation")


  # Visualiser avec un diagramme en barres groupées
  plot <- ggplot(correlation_df_traffic, aes(x = liste_var_meteo, y = Correlation, fill = Traffic)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(label = round(Correlation, 2)), vjust = -0.3, position = position_dodge(width = 0.9)) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Corrélation entre les variables météo et les variables de traffic", x = "Variables météo", y = "Corrélation",
         , subtitle = ifelse(length(unique(complete_data$segment_name))==1, unique(complete_data$segment_name[[1]]), "Ensemble des capteurs"))

  return(plot)
}

# Exemple d'utilisation
# Assurez-vous que complete_data est un data frame contenant les colonnes 'bike', 'vehicule', 'pedestrian', et les variables météorologiques
# complete_data <- data.frame(bike = rnorm(100), vehicule = rnorm(100), pedestrian = rnorm(100), GLO = rnorm(100), T = rnorm(100), U = rnorm(100), VV = rnorm(100))
# correlation_traffic(complete_data, c("GLO", "T", "U", "VV"))




library(httr)
library(jsonlite)
library(readr)
library(lubridate) # Pour une manipulation plus facile des dates

get_weather_data <- function(start_date, end_date, id_station = "35281001", api_key) {

  # Convertir les dates en objets date-time pour la vérification
  start_datetime <- (start_date)
  end_datetime <- (end_date)

  # Vérifier que l'écart entre les deux dates est inférieur à un an
  if (difftime(end_datetime, start_datetime, units = "days") >= 365) {
    stop("Impossible d'avoir des données sur 1 an ou plus")
  }

  # Construire l'URL de l'API Météo France
  base_url <- "https://public-api.meteofrance.fr/public/DPClim/v1/commande-station/horaire"
  id_station <- "35281001"
  url <- paste0(base_url, "?id-station=", id_station, "&date-deb-periode=", start_date, "T00%3A00%3A00Z&date-fin-periode=", end_date, "T00%3A00%3A00Z")

  cat("URL de requête:", url, "\n")

  # Faire la requête GET avec l'en-tête d'authentification approprié
  response <- tryCatch({
    GET(url, add_headers(Accept = "*/*", `apikey` = api_key))
  }, error = function(e) {
    cat("Erreur lors de la requête GET:", e$message, "\n")
    return(NULL)
  })

  if (is.null(response)) {
    stop("La requête initiale a échoué.")
  }

  # Vérifier le statut de la réponse
  if (status_code(response) == 202) {
    # Récupérer l'ID de la commande
    id <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)$elaboreProduitAvecDemandeResponse$return

    # Pause de 10 secondes avant la seconde requête
    cat("Pause de 7 secondes avant la seconde requête...\n")
    Sys.sleep(7)

    # Construire l'URL pour télécharger le fichier
    url2 <- paste0("https://public-api.meteofrance.fr/public/DPClim/v1/commande/fichier?id-cmde=", id)

    cat("URL pour télécharger le fichier:", url2, "\n")

    # Faire la requête pour télécharger le fichier
    response2 <- tryCatch({
      GET(url2, add_headers(Accept = "*/*", `apikey` = api_key))
    }, error = function(e) {
      cat("Erreur lors de la requête GET pour le fichier:", e$message, "\n")
      return(NULL)
    })

    if (is.null(response2)) {
      stop("La requête pour télécharger le fichier a échoué.")
    }

    # Vérifier le statut de la réponse
    if (status_code(response2) == 201) {
      # Lire le contenu de la réponse comme un fichier CSV
      content_text <- content(response2, "text", encoding = "UTF-8")
      con <- textConnection(content_text)
      data <- read.csv(con, sep = ";", header = TRUE, stringsAsFactors = FALSE, dec = ",")
      close(con)

      # Retourner les données
      return(data)
    } else {
      stop(paste("Erreur lors du téléchargement du fichier : ", status_code(response2), content(response2, "text")))
    }
  } else {
    stop(paste("Erreur : ", status_code(response), content(response, "text")))
  }
}

# Utilisation de la fonction pour récupérer les données météo
# weather_data <- get_weather_data(start_date = "2024-01-10", end_date =  "2024-01-20", api_key = "your_api_key")
