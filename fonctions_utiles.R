# Dans ce fichier est créé un ensemble de fonction permettant de créér des fonctions plus complexes





# Les fonctions suivantes sont utilisées pour créer analyser_impact_pluie() contenue dans le fichier fonction_meteo.R

#' Calculer la moyenne pour un type de véhicule donné
#'
#' @param data Un data frame contenant les données
#' @param type_vehicule Le type de véhicule ("bike" ou "vehicule")
#' @param seuil Le seuil de pluie
#'
#' @return Un data frame avec les moyennes calculées
calculer_moyenne <- function(data, type_vehicule, seuil) {
  data %>%
    group_by(pluie) %>%
    summarise(valeur = mean(!!sym(type_vehicule), na.rm = TRUE)) %>%
    mutate(seuil = seuil, condition = pluie) %>%
    select(seuil, condition, valeur)
}


#' Calculer la proportion pour un type de véhicule donné
#'
#' @param data Un data frame contenant les données
#' @param type_vehicule Le type de véhicule ("bike" ou "vehicule")
#' @param seuil Le seuil de pluie
#'
#' @return Un data frame avec les proportions calculées
calculer_proportion <- function(data, type_vehicule, seuil) {
  data %>%
    group_by(pluie) %>%
    summarise(
      total = sum(!!sym(type_vehicule), na.rm = TRUE),
      total_trafic = sum(vehicule + bike, na.rm = TRUE)
    ) %>%
    mutate(valeur = total / total_trafic) %>%
    mutate(seuil = seuil, condition = pluie) %>%
    select(seuil, condition, valeur)
}



#' Créer un graphique pour un type de véhicule en fonction de l'approche choisie
#'
#' @param data Les données pour le graphique
#' @param type_vehicule Le type de véhicule ("vélos" ou "véhicules")
#'
#' @return Un objet ggplot
creer_graphique <- function(data, type_vehicule, approche="moyenne") {
  y_label <- if(approche == "moyenne") paste("Moyenne des", type_vehicule) else paste("Proportion de", type_vehicule)
  y_col <- ifelse(type_vehicule == "vélos", "moyenne_bike", "moyenne_vehicule")

  ggplot(data, aes(x = seuil, y = valeur, color = condition, group = condition)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Évolution de la", tolower(y_label), "en fonction du seuil de pluie"),
         x = "Seuil de pluie (mm)",
         y = y_label,
         color = "Condition") +
    theme_minimal()
}


#' Créer une barre de significativité
#'
#' @param data Les données pour la barre
#' @param title Le titre de la légende
#'
#' @return Un objet ggplot
creer_barre_significativite <- function(data, title) {
  data <- data %>%
    mutate(significant = ifelse(p_value < 0.05, "Significatif", "Non significatif"))

  ggplot(data, aes(x = seuil, y = 1, fill = significant)) +
    geom_tile() +
    scale_fill_manual(values = c("Significatif" = "red", "Non significatif" = "blue")) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x = "Seuil de pluie (mm)", fill = title)
}
