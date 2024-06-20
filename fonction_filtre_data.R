# Importation des librairies

library(telraamStats)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(prophet)
library(corrplot)



# Fonctions pour filtrer les données abscentes

# Enlever les heures ou il fait nuit en fonction de la saisons Enlever les périodes ou les capteurs ne fonctionnait pas pendant une longue periode


#Filtrer par saison les heures à conserver avec un seuil de missing data
filtered_hours_data <- function(data, seuil = 0.85){

  nrow_start <- nrow(data)

  #Transform date column to date format
  data$date <- ymd_hms(data$date)

  # Add a column for the season or each month
  data$season <- ifelse(month(data$date) %in% c(3,4,5), "Spring",
                        ifelse(month(data$date) %in% c(6,7,8), "Summer",
                               ifelse(month(data$date) %in% c(9,10,11), "Autumn", "Winter")))


  #Group by hour of the day and season for all data and missing data
  group_all <- data %>%  group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)

  group_missing <- data %>% filter(data$uptime < 0.5) %>% group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)


  #Merge the two dataframes and calculate the proportion of missing data
  group <- merge(group_all, group_missing, by = c("season","hour"), all = TRUE) %>% rename(n_missing = n.y, n = n.x)
  group$prop_missing <- group$n_missing / group$n * 100


  #Create a first plot of the proportion of missing data per hour of the day per season without filtering

  p1 <- group %>% ggplot(aes(x = hour, y = prop_missing, color = season)) + geom_line() + geom_point() +
    labs(title = "Proportion of missing data per hour \n of the day per season before filtering", x = "Hour of the day", y = "Proportion of missing data")


  # filter the hours where the proportion of missing data is higher than the threshold
  hour_missing <- group %>% filter(prop_missing > seuil) %>% select(hour, season) %>% unique()


  #Keep the data where the hour is not in the list of hours with high proportion of missing data and where the uptime
  data <- data %>% filter(!(hour(date) %in% hour_missing$hour & season %in% hour_missing$season))


  #Count the number of missing data
  number_missing <- nrow(data %>% filter(data$uptime < 0.5))

  #Count the number of rows removed
  nrow_substract <-  nrow_start - nrow(data)

  #Create a second plot of the proportion of missing data per hour of the day per season after filtering
  #Group by hour of the day and season for all data and missing data
  group_all_filtered <- data %>%  group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)

  group_missing_filtered <- data %>% filter(data$uptime < 0.5) %>% group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)


  #Merge the two dataframes and calculate the proportion of missing data
  group_filtered <- merge(group_all_filtered, group_missing_filtered, by = c("season","hour"), all = TRUE) %>% rename(n_missing = n.y, n = n.x)
  group_filtered$prop_missing <- group_filtered$n_missing / group_filtered$n * 100

  p2 <- group_filtered %>% ggplot(aes(x = hour, y = prop_missing, color = season)) + geom_line() + geom_point() +
    labs(title = "Proportion of missing data per hour \n of the day per season after filtering", x = "Hour of the day", y = "Proportion of missing data") + ylim(0,100)

  p <- patchwork::wrap_plots(p1,p2)


  #Return a list with the filtered data, the plot, the threshold, the number of missing data and the number of rows removed
  list_return <-  list(hour_filter = hour_missing, data = data,plot =  p,threshold = seuil,number_missing = number_missing, nrow_substract = nrow_substract)
  return(list_return)

}

substract_successive_NA <- function(data_raw, threshold = 24) {
  # Initialisation d'une nouvelle colonne pour stocker les périodes de données manquantes successives
  data_raw$successive_missing <- 0


  # Sélectionner les colonnes pertinentes et filtrer les données manquantes
  data_false <- data_raw %>% filter(!uptime_quality) %>% arrange(date)
  data_true <- data_raw %>% filter(uptime_quality) %>% arrange(date)



  # Initialiser l'index pour la boucle
  i <- 1

  while (i < nrow(data_false)) {
    # Compter le nombre de données manquantes successives
    succ <- 1
    while ((i + succ <= nrow(data_false)) && (difftime(data_false$date[i + succ], data_false$date[i + succ - 1], units = "hours") <= 1)) {
      succ <- succ + 1
    }

    # Assigner la valeur de succ à toutes les lignes successives trouvées
    data_false$successive_missing[i:(i + succ - 1)] <- succ

    # Avancer l'index de la taille de la période successives trouvée
    i <- i + succ
  }

  filtered_data_false <- data_false %>% filter(successive_missing < threshold)

  # Fusionner les données manquantes
  data <- rbind(filtered_data_false, data_true) %>% arrange(date)


  return(data)
}

#Nettoyage NA


Clean_NA_segments <- function(list_data,threshold_missing = 85, threshold_successive = 24) {
  #if list_data is not a list then consider it as a dataframe
  if (!is.data.frame(list_data)){

    segments_clear <- data.frame()
    for(i in 1:length(list_data)){
      seg <- list_data[i]
      data <- substract_successive_NA(data.frame(seg), threshold_successive)
      segments_clear <- rbind(segments_clear,filtered_hours_data(data, threshold_missing)$data)
    } }

  else{

    segments_clear <- substract_successive_NA(data.frame(list_data), threshold_successive)
    segments_clear <- filtered_hours_data(segments_clear, threshold_missing)$data
  }

  return(segments_clear)
}



# Fonction de filtre des heures en fonction des saisons


filtered_hours_data <- function(data, seuil = 0.85){

  nrow_start <- nrow(data)
  # Add a column for the season or each month
  data$season <- ifelse(month(data$date) %in% c(3,4,5), "Spring",
                        ifelse(month(data$date) %in% c(6,7,8), "Summer",
                               ifelse(month(data$date) %in% c(9,10,11), "Autumn", "Winter")))


  #Group by hour of the day and season for all data and missing data
  group_all <- data %>%  group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)

  group_missing <- data %>% filter(data$uptime < 0.5) %>% group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)


  #Merge the two dataframes and calculate the proportion of missing data
  group <- merge(group_all, group_missing, by = c("season","hour"), all = TRUE) %>% rename(n_missing = n.y, n = n.x)
  group$prop_missing <- group$n_missing / group$n * 100


  #Create a first plot of the proportion of missing data per hour of the day per season without filtering

  p1 <- group %>% ggplot(aes(x = hour, y = prop_missing, color = season)) + geom_line() + geom_point() +
    labs(title = "Proportion of missing data per hour \n of the day per season before filtering", x = "Hour of the day", y = "Proportion of missing data")


  # filter the hours where the proportion of missing data is higher than the threshold
  hour_missing <- group %>% filter(prop_missing > seuil) %>% select(hour, season) %>% unique()


  #Keep the data where the hour is not in the list of hours with high proportion of missing data and where the uptime
  data <- data %>% filter(!(hour(date) %in% hour_missing$hour & season %in% hour_missing$season))


  #Count the number of missing data
  number_missing <- nrow(data %>% filter(data$uptime < 0.5))

  #Count the number of rows removed
  nrow_substract <-  nrow_start - nrow(data)

  #Create a second plot of the proportion of missing data per hour of the day per season after filtering
  #Group by hour of the day and season for all data and missing data
  group_all_filtered <- data %>%  group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)

  group_missing_filtered <- data %>% filter(data$uptime < 0.5) %>% group_by(season,hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)


  #Merge the two dataframes and calculate the proportion of missing data
  group_filtered <- merge(group_all_filtered, group_missing_filtered, by = c("season","hour"), all = TRUE) %>% rename(n_missing = n.y, n = n.x)
  group_filtered$prop_missing <- group_filtered$n_missing / group_filtered$n * 100

  p2 <- group_filtered %>% ggplot(aes(x = hour, y = prop_missing, color = season)) + geom_line() + geom_point() +
    labs(title = "Percentage of missing data per hour \n of the day per season after filtering", x = "Hour of the day", y = "Percentage of missing data") + ylim(0,100)

  p <- patchwork::wrap_plots(p1,p2)


  #Return a list with the filtered data, the plot, the threshold, the number of missing data and the number of rows removed
  list_return <-  list(hour_filter = hour_missing, data = data,plot =  p,threshold = seuil,number_missing = number_missing, nrow_substract = nrow_substract, group = group, group_all=group_all, group_missing = group_missing, group_miss_fil = group_missing_filtered)
  return(list_return)

}



# Fonction pour identifier les données de type 1 et 2


NA_Type <- function(segments_clear,threshold = 50){
  #Identifier pour chaque données manquantes sont type 1 ou 2

  summarise_NA <- segments_clear %>% group_by(date) %>% summarise(n_missing = sum(uptime < 0.5)) %>% arrange(date)
  summarise_NA$prop_missing <- summarise_NA$n_missing / length(unique(segments_clear$segment_name)) * 100

  #Si plus de 50% des données sont manquantes, on considère que la donnée est de type 2 sinon de type 1
  summarise_NA$type <- ifelse(summarise_NA$prop_missing > threshold, 2, 1)

  #Pour chaque date de data_clear on va ajouter le type
  segments_clear <- left_join(segments_clear, summarise_NA %>% select(date, type), by = "date")
  segments_clear$type[which(segments_clear$uptime_quality == T)] <- NA
  return(segments_clear)
}


stop_sensor<- function(df_init,uptime_choice=0.1,successive_day=2,remove_data=FALSE)
{
  # Input parameter :
  #       - df_init : Initial Dataframe
  #       - uptime_choice : Real which define the quality of the data below which we do not need the data
  #       - successive_day : Integer which define number of successives days above which we do not need the data
  #       - remove_data : Booleen which choose if we keep the row or not
  # Output parameter :
  #       - df_fin : Dataframe
  # Description :
  # Remove hours where we have no information as the night or less than 10% of information
  # Remove periods when the sensor did not work
  #
  # Packages :
  #        - lubridate
  #        - dplyr

  #Create a column season
  df_init$date <- ymd_hms(df_init$date) #mettre au format date
  df_init$season <- ifelse(month(df_init$date) %in% c(3,4,5), "Spring",
                           ifelse(month(df_init$date) %in% c(6,7,8), "Summer",
                                  ifelse(month(df_init$date) %in% c(9,10,11), "Autumn", "Winter")))


  # Remove hours with no information by season
  df_season<-df_init %>% group_by(segment_id,season,hour) %>% summarise(condition=any(car!=0 & uptime>uptime_choice))

  df_init <- df_init %>%
    semi_join(df_season %>% filter(condition), by = c("segment_id","season", "hour"))

  #Inactivity periods
  rm<-c()
  rm_fin<-c()

  list_clear_data <- list()
  seg_id<-unique(df_init$segment_id)

  for(id in 1:length(seg_id))
  {
    df_segment<-df_init[df_init$segment_id==seg_id[id],]
    for(i in 1:length(df_segment$car))
    { j=i
    while ((df_segment$car[i]==0 | df_segment$uptime[i]<uptime_choice) & i<length(df_segment$car))
    {rm<-c(rm,i)
    i<-i+1}

    diff_days<-abs(as.numeric(difftime(df_segment$day[i], df_segment$day[j], units = "days")))

    if(diff_days>successive_day)
    {rm_fin<-c(rm_fin,rm)}

    rm<-c()}

    if (is.null(rm_fin))
    {df_segment<-df_segment}

    else
    { if(remove_data==TRUE)
    {df_segment<-df_segment[-rm_fin,]}

      else
      {df_segment[rm_fin,7:18]=NA} #correspond aux colonnes avec le nombre de véhicules
    }
    list_clear_data[[id]]<-df_segment
  }

  #Recombine the final dataframe
  df_fin<-list_clear_data[[1]]
  if(length(seg_id)>1)
  {for(i in 2:length(seg_id))
  {df_fin<-rbind(df_fin,list_clear_data[[i]])}}

  return(df_fin)
}


quarterly_to_hourly <- function(sensors){
  sensors  %>% mutate(date = as.POSIXct(date),interval="hourly", segment_name = str_split(segment_fullname, "-", simplify = TRUE)[,2],
                      holiday = ifelse(public_holiday=="No public holiday", FALSE, TRUE), uptime_quality = ifelse(uptime<0.5, FALSE, TRUE)) %>%
    select(-car_lft,-car_rgt,-heavy_lft,-heavy_rgt,-bike_lft,-bike_rgt,-pedestrian_lft,-pedestrian_rgt,-uptime,-car,-heavy,-bike,-pedestrian, -public_holiday) %>%
    filter(minute(date) == 0 ) %>%
    left_join(sensors  %>%
                group_by(segment_id,day,hour) %>%
                summarise(uptime = mean(uptime),
                          vehicle = sum(car + heavy),
                          car = sum(car),
                          heavy = sum(heavy),
                          bike=sum(bike),
                          pedestrian = sum(pedestrian),
                          car_lft = sum(car_lft),
                          car_rgt = sum(car_rgt),
                          heavy_lft = sum(heavy_lft),
                          heavy_rgt = sum(heavy_rgt),
                          bike_lft = sum(bike_lft),
                          bike_rgt = sum(bike_rgt),
                          pedestrian_lft = sum(pedestrian_lft),
                          pedestrian_rgt = sum(pedestrian_rgt)), by =c("segment_id",'day','hour'))
}
