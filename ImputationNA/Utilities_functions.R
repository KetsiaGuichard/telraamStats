

# Function to plot the seasonality of missing values for all sensors
plot_NA <- function(data) {
  # Filter data for rows with uptime_quality as FALSE (i.e., missing values)
  # Select only the date and segment_name columns
  data_long <- data %>%
    filter(!uptime_quality) %>%
    select(date, segment_name) %>%
    # Extract a numeric value from the segment_name (last two characters)
    mutate(value = as.numeric(substr(as.character(segment_name), nchar(as.character(segment_name)) - 1, nchar(as.character(segment_name)))),
           # Convert segment_name to a factor for plotting
           segment_name = as.factor(as.character(segment_name)))

  # Plot the data using ggplot2
  # x-axis: date, y-axis: extracted numeric value, color: segment_name
  data_long %>% ggplot(aes(x = date, y = value, color = segment_name)) +
    geom_point() +
    labs(title = "Seasonality of Missing Values for All Sensors",
         x = "Date",
         y = "Sensor Value",
         color = "Segment Name") +
    theme_minimal()
}



Create_test_data_unif <- function(data,num_missing=NA,seed = NA,prop_missing=NA){

  #Creer la seed si spécifié
  if(!is.na(seed)){set.seed(seed)}

  if(is.na(num_missing) & is.na(prop_missing)){
    stop("You must specify either the number of missing values or the proportion of missing values")
  }

  if(!is.na(num_missing) & !is.na(prop_missing)){
    stop("You must specify either the number of missing values or the proportion of missing values")
  }

  if(!"vehicle" %in% colnames(data)){
    data <- data %>% mutate(vehicle = car + heavy)
  }

  if(!is.na(prop_missing)){
    num_missing <- round(nrow(data)*prop_missing)
  }

  data <- data %>% na.omit() %>% filter(uptime_quality) %>% mutate(vehicle = car + heavy)


  #Dans un premier temps on peut créer nos valeurs manquante
  temoin <- data
  test <- data

  vec_sample <- sample(1:nrow(data),num_missing,replace = F)
  test$vehicle[vec_sample] <- NA


  list(temoin=temoin,test=test)
}


#On crée une pipeline pour tester les différentes stratégies d'imputation
Create_test_data_suite <- function(data, num_missing = NA, seed = NA, prop_missing = NA, max_na_length = 72) {

  # Créer la seed si spécifié
  if (!is.na(seed)) {
    set.seed(seed)
  }

  if (is.na(num_missing) & is.na(prop_missing)) {
    stop("You must specify either the number of missing values or the proportion of missing values")
  }

  if (!is.na(num_missing) & !is.na(prop_missing)) {
    stop("You must specify either the number of missing values ou la proportion de missing values")
  }

  if (!is.na(prop_missing)) {
    num_missing <- round(nrow(data) * prop_missing)
  }


  #Seulement si la colonne vehicle n'existe pas encore
  if(!"vehicle" %in% colnames(data)){
    data <- data %>% mutate(vehicle = car + heavy)
  }
  data <- data %>% filter(uptime_quality)

   # Vérifier si le nombre total de NA demandés est plus que les lignes de données disponibles
  if (num_missing > nrow(data)) {
    stop("The number of missing values exceeds the number of available data points.")
  }

  # Dans un premier temps on peut créer nos valeurs manquantes
  temoin <- data
  test <- data

  # Créer des suites de NA avec une longueur maximale de 72
  na_lengths <- c()
  while (sum(na_lengths) < num_missing) {
    remaining_na <- num_missing - sum(na_lengths)
    na_lengths <- c(na_lengths, sample(1:min(max_na_length, remaining_na), 1,replace=T))
  }

  # Vérifier que nous n'avons pas dépassé le nombre de NA nécessaires
  na_lengths <- na_lengths[1:which(cumsum(na_lengths) >= num_missing)[1]]

  # Calculer les positions de début pour les séquences de NA
  start_positions <- sample(1:(nrow(test) - max(na_lengths) + 1), length(na_lengths))

  # Introduire les NA dans les données
  for (i in seq_along(start_positions)) {
    start <- start_positions[i]
    end <- min(nrow(test), start + na_lengths[i] - 1)
    test$vehicle[start:end] <- NA
  }

  list(temoin = temoin, test = test)
}

Create_test_data_with_na_distribution <- function(data, historical_na_proportion, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  if (!"vehicle" %in% colnames(data)) {
    data <- data %>% mutate(vehicle = car + heavy)
  }
  test <- data
  # Calculer le nombre de valeurs manquantes à introduire
  na_distribution <- test %>%
    group_by(segment_name, hour) %>%
    summarise(nb_obs = n(), .groups = "drop") %>%
    left_join(historical_na_proportion, by = c("hour"), relationship = "many-to-many") %>%
    mutate(nb_na = ifelse(nb_obs > 2, ceiling(nb_obs * mean_prop_NA), 0))
  print(na_distribution)
  # Afficher le total de NA à introduire
  cat("Total de NA à introduire:", sum(na_distribution$nb_na, na.rm = TRUE), "\n")
  # Introduire les valeurs manquantes
  for (i in 1:nrow(na_distribution)) {
    seg <- na_distribution$segment_name[i]
    hr <- na_distribution$hour[i]
    n_na <- na_distribution$nb_na[i]
    if (!is.na(n_na) && n_na > 0) {
      indices <- which(test$segment_name == seg & test$hour == hr)
      na_indices <- sample(indices, size = min(n_na, length(indices)), replace = FALSE)
      test$vehicle[na_indices] <- NA
    }
  }
  # Afficher le nombre de NA effectivement introduits
  cat("Nombre de NA effectivement introduits:", sum(is.na(test$vehicle)), "\n")
  return(list(temoin = data, test = test))
}


quarterly_to_hourly <- function(sensors){
  sensors  %>% mutate(date = as.POSIXct(date),interval="hourly", segment_name = str_split(segment_fullname, "-", simplify = TRUE)[,2],
                      holiday = ifelse(public_holiday=="No public holiday", FALSE, TRUE), uptime_quality = ifelse(uptime<0.5, FALSE, TRUE)) %>%
    select(-car_lft,-car_rgt,-heavy_lft,-heavy_rgt,-bike_lft,-bike_rgt,-pedestrian_lft,-pedestrian_rgt,-uptime,-car,-heavy,-bike,-pedestrian, -public_holiday) %>%
    filter(minute(date) == 0 ) %>%
    left_join(sensors  %>%
                group_by(segment_id, day, hour) %>%
                summarise(
                  time_sum = sum(uptime * 15, na.rm = TRUE),
                  vehicle = sum(car + heavy, na.rm = TRUE),
                  car = sum(car, na.rm = TRUE),
                  heavy = sum(heavy, na.rm = TRUE),
                  bike = sum(bike, na.rm = TRUE),
                  pedestrian = sum(pedestrian, na.rm = TRUE),
                  car_lft = sum(car_lft, na.rm = TRUE),
                  car_rgt = sum(car_rgt, na.rm = TRUE),
                  heavy_lft = sum(heavy_lft, na.rm = TRUE),
                  heavy_rgt = sum(heavy_rgt, na.rm = TRUE),
                  bike_lft = sum(bike_lft, na.rm = TRUE),
                  bike_rgt = sum(bike_rgt, na.rm = TRUE),
                  pedestrian_lft = sum(pedestrian_lft, na.rm = TRUE),
                  pedestrian_rgt = sum(pedestrian_rgt, na.rm = TRUE), .groups = 'keep'
                ),
              by = c("segment_id", 'day', 'hour')
    ) %>%
    mutate(uptime = time_sum / 60)
}


get_weather_data <- function(start_date, end_date, id_station = "35281001", api_key) {
  library(httr)
  library(jsonlite)
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


new_data = function(df_meteo, data_mouv){

  df_meteo$date = lubridate::ymd_h(df_meteo$DATE)

  df_meteo$id_join = paste0(date(df_meteo$date), "_", hour(df_meteo$date))

  df_meteo = df_meteo %>% select(id_join, date,GLO,U,VV, T, RR1)   # Sélection des variables pertinentes de météo

  # Convertir la chaîne en datetime et spécifier la timezone CEST
  datetime_cest <- lubridate::ymd_hms(data_mouv$date, tz = "Europe/Paris")

  # Convertir la datetime en UTC
  datetime_utc <- with_tz(datetime_cest, tzone = "UTC")

  data_mouv$date = datetime_utc

  data_mouv$id_join = paste0(data_mouv$day, "_", data_mouv$hour)

  data_mouv$date <- NULL

  result = left_join(data_mouv, df_meteo, by="id_join")

  result$id_join <- NULL

  return(result)
}

