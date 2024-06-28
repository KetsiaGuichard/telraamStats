

# Packages
library(tidyverse)



# Fonction pour fusion de données de telraam et meteo
new_data = function(df_meteo, data_mouv){

  df_meteo$date = lubridate::ymd_h(df_meteo$DATE)

  df_meteo$id_join = paste0(date(df_meteo$date), "_", hour(df_meteo$date))

  df_meteo = df_meteo %>% select(id_join, date,GLO,U,VV, T, RR1)   # Sélection des variables pertinentes de météo

  data_mouv = data_mouv %>% mutate(vehicule = car + heavy) #%>%  select(date, segment_name, uptime, v85, vacation, holiday, weekday, vehicule, pedestrian, bike, uptime_quality, type)

  # Convertir la chaîne en datetime et spécifier la timezone CEST
  datetime_cest <- lubridate::ymd_hms(data_mouv$date, tz = "Europe/Paris")

  # Convertir la datetime en UTC
  datetime_utc <- with_tz(datetime_cest, tzone = "UTC")

  data_mouv$date = datetime_utc

  data_mouv$id_join = paste0(data_mouv$day, "_", data_mouv$hour)

  df_meteo = df_meteo %>% select(!date)

  result = left_join(data_mouv, df_meteo, by="id_join")

  result$id_join = NULL

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



# Charger les bibliothèques nécessaires
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

get_weather_data <- function(start_date, end_date, id_station = "35281001", api_key) {
  start = Sys.time()
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

      end = Sys.time()
      cat("Temps d'exécution:", end - start, "secondes\n")
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
#weather_data <- get_weather_data(start_date = "2023-05-18", end_date =  "2024-05-16", api_key = "eyJ4NXQiOiJZV0kxTTJZNE1qWTNOemsyTkRZeU5XTTRPV014TXpjek1UVmhNbU14T1RSa09ETXlOVEE0Tnc9PSIsImtpZCI6ImdhdGV3YXlfY2VydGlmaWNhdGVfYWxpYXMiLCJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJrZXZpbmtlbkBjYXJib24uc3VwZXIiLCJhcHBsaWNhdGlvbiI6eyJvd25lciI6Imtldmlua2VuIiwidGllclF1b3RhVHlwZSI6bnVsbCwidGllciI6IlVubGltaXRlZCIsIm5hbWUiOiJEZWZhdWx0QXBwbGljYXRpb24iLCJpZCI6MTMyNjcsInV1aWQiOiJkNjU0MWRjOC1jMGY5LTQyYjAtYTk5Zi1hZjgxMDIxMDU1NjQifSwiaXNzIjoiaHR0cHM6XC9cL3BvcnRhaWwtYXBpLm1ldGVvZnJhbmNlLmZyOjQ0M1wvb2F1dGgyXC90b2tlbiIsInRpZXJJbmZvIjp7IjUwUGVyTWluIjp7InRpZXJRdW90YVR5cGUiOiJyZXF1ZXN0Q291bnQiLCJncmFwaFFMTWF4Q29tcGxleGl0eSI6MCwiZ3JhcGhRTE1heERlcHRoIjowLCJzdG9wT25RdW90YVJlYWNoIjp0cnVlLCJzcGlrZUFycmVzdExpbWl0IjowLCJzcGlrZUFycmVzdFVuaXQiOiJzZWMifX0sImtleXR5cGUiOiJQUk9EVUNUSU9OIiwic3Vic2NyaWJlZEFQSXMiOlt7InN1YnNjcmliZXJUZW5hbnREb21haW4iOiJjYXJib24uc3VwZXIiLCJuYW1lIjoiRG9ubmVlc1B1YmxpcXVlc0NsaW1hdG9sb2dpZSIsImNvbnRleHQiOiJcL3B1YmxpY1wvRFBDbGltXC92MSIsInB1Ymxpc2hlciI6ImFkbWluX21mIiwidmVyc2lvbiI6InYxIiwic3Vic2NyaXB0aW9uVGllciI6IjUwUGVyTWluIn1dLCJleHAiOjE3MTk0Nzc5NjcsInRva2VuX3R5cGUiOiJhcGlLZXkiLCJpYXQiOjE3MTk0NzIzNjcsImp0aSI6ImI4ODc4YTE1LTdlYWItNGZkYy1iZDIxLTZiN2YxNjBkODNmNSJ9.mRb5ZV8ViJnzLCmove59gPvb6Y2PG1xp6poEtbXquv586MB9Cxqb3DCuJODhWO3UpDjDMLAf_DsM-J_XBlKtZKL-pJhz2nMVJ7qMXgAcmtKIaqz9kxwRermsT73QFMRoYwpjgccGS0TMlhCmd8CJGyrcj7Hw46hne4XTFvdtM0OZ3BjWy96JzSNn_uPpp5TGdpm16fwUPxwViUDOQhQ6bzo6oS_TD8KGvkn-6spZ3mxnpEkb-jqK3OhLbtKWdGccaNuBs-tGSY_OjszwlOSLJscvbdk11t6QuOt1w3lXjGO4cS7wytj8J0vWZ6BGqxClw-S7z865gnJH930fV3mapA==")
# Environ 11.40 secondes pour récupérer les données météo pour une année




source("fonctions_utiles.R")

#' Analyse de l'impact de la pluie sur le trafic
#'
#' @param data Un data frame contenant les données météo et de trafic
#' @param seuils Un vecteur numérique des seuils de pluie à tester
#' @param weekend Logical. TRUE pour analyser uniquement les weekends, FALSE pour les jours de semaine
#' @param heure_pointe Character. "matin" pour 7h-8h, "soir" pour 17h-18h, ou NULL pour toute la journée
#' @param vacances Logical. TRUE pour analyser uniquement les périodes de vacances scolaires, FALSE pour hors vacances, NULL pour toutes les périodes
#'
#' @return Un graphique combiné montrant l'impact de la pluie sur le trafic des vélos et des véhicules
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import gridExtra
analyser_impact_pluie <- function(data, seuils = seq(0.1, 6, by = 0.5),
                                  weekend = NULL, heure_pointe = NULL, vacances = NULL,
                                  approche = "moyenne") {
  # Filtrage des données
  data_filtre <- data %>%
    filter(!is.na(RR1))   # Pour avoir une table avec des données de précipitations

  if (!is.null(weekend)) {    # Filtre pour les jours de la semaine (weekend ou non weekend ou toute la semaine)

    if (weekend) {
      data_filtre <- data_filtre %>%
        filter(weekday %in% c("Saturday", "Sunday"))
    } else {
      data_filtre <- data_filtre %>%
        filter(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    }

  }

  if (!is.null(heure_pointe)) {   # Filtre pour les heures de pointe (matin, soir ou toute la journée)
    data_filtre <- data_filtre %>%
      filter(case_when(
        heure_pointe == "matin" ~ hour %in% 7:8,
        heure_pointe == "soir" ~ hour %in% 17:18,
        TRUE ~ TRUE
      ))
  }

  if (!is.null(vacances)) {       # Filtre pour les périodes de vacances scolaires
    data_filtre <- data_filtre %>%
      filter(holiday == vacances)
  }

  # Vérification du max de seuil enregistré
  if (max(data_filtre$RR1) <= 2) {
    warning("Pas assez de précipitations enregistrées sur cette période pour une analyse")
    return(NULL)
  }

  # Vérification de la quantité de données sur la période
  if (nrow(data_filtre) < 500) {
    warning("Pas assez de données pour une analyse significative")
    return(NULL)
  }

  # Initialisation des tableaux de résultats
  resultats_velo <- data.frame(seuil = numeric(), condition = character(),
                               valeur = numeric(), p_value = numeric())
  resultats_vehicule <- data.frame(seuil = numeric(), condition = character(),
                                   valeur = numeric(), p_value = numeric())

  # Calcul des moyennes/proportions et des p-values pour chaque seuil
  for (i in seuils) {
    data_mod <- data_filtre %>%
      mutate(pluie = ifelse(RR1 > i, "pluie", "non pluie"))  # Création de la variable pluie

    if (length(unique(data_mod$pluie)) == 2) {
      if (approche == "moyenne") {
        resultat_velo <- calculer_moyenne(data_mod, "bike", i)
        resultat_vehicule <- calculer_moyenne(data_mod, "vehicule", i)

        test_w_velo <- wilcox.test(bike ~ pluie, data = data_mod)
        test_w_vehicule <- wilcox.test(vehicule ~ pluie, data = data_mod)

        resultat_velo$p_value <- test_w_velo$p.value
        resultat_vehicule$p_value <- test_w_vehicule$p.value
      } else if (approche == "proportion") {
        resultat_velo <- calculer_proportion(data_mod, "bike", i)
        resultat_vehicule <- calculer_proportion(data_mod, "vehicule", i)

        resultat_velo$p_value <- NA   # Pas de test de proportion (compléter en cas de besoin par un test de proportion)
        resultat_vehicule$p_value <- NA   # Pas de test de proportion
      } else {
        stop("Approche non reconnue. Choisissez 'moyenne' ou 'proportion'.")
      }

      resultats_velo <- rbind(resultats_velo, resultat_velo)
      resultats_vehicule <- rbind(resultats_vehicule, resultat_vehicule)
    }
  }

  # Création des graphiques
  p_velo <- creer_graphique(resultats_velo, "vélos", approche)
  p_vehicule <- creer_graphique(resultats_vehicule, "véhicules", approche)

  # Combinaison des graphiques
  if (approche == "moyenne") {
    bar_velo <- creer_barre_significativite(resultats_velo, "Différence")
    bar_vehicule <- creer_barre_significativite(resultats_vehicule, "Différence")
    graphique_combine <- gridExtra::grid.arrange(
      p_velo, p_vehicule, bar_velo, bar_vehicule,
      ncol = 2, heights = c(3, 1, 3, 1))
  } else {
    graphique_combine <- gridExtra::grid.arrange(
      p_velo, p_vehicule,
      ncol = 2
    )
  }
  return(graphique_combine)
}
