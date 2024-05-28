

# Packages
library(tidyverse)


# Foction de sélection de variables météo pertinentes
clean_data_meteo = function(df_meteo){

  # entrée: données de météo brute
  # sortie: données de météo pertinentes

  df_meteo$DATE = lubridate::ydm_h(df_meteo$DATE)
  var_pertinentes = c("DATE","N","RR1","GLO","U","VV", "T")
  df_meteo = df_meteo[var_pertinentes]
  return(df_meteo)
}




# Fonction pour fusion de données de telraam et meteo
new_data = function(df_meteo, data_mouv){
  df_meteo = df_meteo %>% select(DATE,N,RR1,GLO,U,VV, T)   # Sélection des variables pertinentes de météo au cas ou on aurait oublié de faire la sélection au préalable
  data_mouv = data_mouv %>% mutate(vehicule = car + heavy) %>%  select(date, uptime, v85, vacation, holiday, weekday, vehicule, pedestrian)

  df_meteo$DATE = lubridate::ymd_h(df_meteo$DATE)
  data_mouv$date = lubridate::as_datetime(data_mouv$date)

  colnames(df_meteo)[1] = "date"

  result = left_join(data_mouv, df_meteo, by="date")

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

corrélation_uptime = function(complete_data, liste_var_selected =NULL){
  if( !is.null((liste_var_selected))){
    selected_vars <- c("uptime", liste_var_selected)
    mat_var = cor(method = "spearman" ,drop_na(complete_data %>%select(all_of(selected_vars))))
  }else{mat_var = cor(method = "spearman" , drop_na(complete_data %>% select(!date)))}
  corrplot(mat_var, type = "upper", "number")
}


