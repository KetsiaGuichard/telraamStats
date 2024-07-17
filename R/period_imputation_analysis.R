
#' Piste pour l'analyse de données basé sur les points de rupture.

#' Analyse combiné des outliers et de la qualité d'imputations trafic pour un segment routier spécifique
#'
#' Cette fonction effectue une analyse complète des données de trafic pour un segment routier spécifique.
#' Elle combine plusieurs techniques d'analyse et de visualisation, notamment l'imputation des données manquantes,
#' la détection des points de changement, et la création de graphiques interactifs.
#'
#' @param data Un data frame contenant les données de trafic.
#' @param segment_id L'identifiant du segment routier à analyser.
#' @param start_date Date de début de l'analyse (optionnel).
#' @param end_date Date de fin de l'analyse (optionnel).
#' @param type Le type de véhicule à analyser (par défaut "vehicle").
#'
#' @return Une liste contenant deux éléments :
#'   \item{plot}{Un graphique interactif combinant plusieurs visualisations des données de trafic}
#'   \item{table}{Un tableau interactif résumant les informations par période}
#'
#' @details
#' La fonction effectue les opérations suivantes :
#' 1. Filtre les données pour le segment et la période spécifiés
#' 2. Impute les données manquantes
#' 3. Applique un lissage et détecte les points de changement avec l'algorithme PELT
#' 4. Fusionne les périodes courtes
#' 5. Crée des visualisations interactives incluant :
#'    - Un graphique des points de changement
#'    - Des box plots pour les données imputées et non imputées
#'    - Un tableau récapitulatif des informations par période
#'
#' @import dplyr zoo changepoint ggplot2 plotly purrr
#'
#' @examples
#' result <- analyze_traffic_combined(traffic_data, segment_id = 9000001844,
#'                                    start_date = "2022-01-01", end_date = "2022-12-31")
#' result$plot
#' result$table
#'
#' @export





period_imputation_analysis <- function(data, segment_id, start_date = NULL, end_date = NULL, type = "vehicle") {
  library(dplyr)
  library(zoo)
  library(changepoint)
  library(ggplot2)
  library(plotly)
  library(purrr)

  # Filtrer les données pour le segment spécifié
  segment_data <- data %>% filter(segment_id == !!segment_id)

  # Filtrer les données pour la période spécifiée
  if (!is.null(start_date)) {
    segment_data <- segment_data %>% filter(date >= !!start_date)
  }
  if (!is.null(end_date)) {
    segment_data <- segment_data %>% filter(date <= !!end_date)
  }

  # Vérifier si le type de véhicule est valide
  if (!(type %in% colnames(segment_data))) {
    stop("Transport type does not exist in the data")
  }




  segment_name <- unique(segment_data$segment_name)

  # Créer une copie des données non imputées
  segment_data_non_imputed <- segment_data %>% mutate(vehicle = car + heavy) %>%
    rename(transport = !!type) %>% filter(uptime_quality)

  # Imputer les données manquantes
  segment_data_imputed <- impute_missing_data(segment_data,transport_type = type ) %>% rename(transport = !!type)
  print(segment_data_imputed %>% filter(imputed == "imputed"))
  # Appliquer le lissage sur les données imputées
  smoothed_data <- rollmean(segment_data_imputed$transport, k = 7, fill = NA, align = "center")

  # Supprimer les NA du début et de la fin pour PELT
  smoothed_data_clean <- na.omit(smoothed_data)

  # Appliquer PELT sur les données lissées et nettoyées
  model_mbic <- cpt.meanvar(smoothed_data_clean, method = "PELT", penalty = "MBIC")
  changepoints <- cpts(model_mbic)

  # Ajuster les points de changement pour correspondre aux indices originaux
  first_non_na <- which(!is.na(smoothed_data))[1]
  changepoints <- changepoints + first_non_na - 1

  # Fonction pour fusionner les périodes courtes
  merge_short_periods <- function(changepoints, data) {
    periods <- diff(c(0, changepoints, nrow(data)))
    merged <- c()
    i <- 1
    while (i <= length(periods)) {
      if (periods[i] < 24 && i < length(periods)) { # Moins d'un jour (24 heures)
        # Calculer les moyennes des périodes adjacentes
        mean_current <- mean(data$transport[(sum(periods[1:(i-1)]) + 1):(sum(periods[1:i]))], na.rm = TRUE)
        mean_next <- mean(data$transport[(sum(periods[1:i]) + 1):(sum(periods[1:(i+1)]))], na.rm = TRUE)
        mean_prev <- if(i > 1) mean(data$transport[(sum(periods[1:(i-2)]) + 1):(sum(periods[1:(i-1)]))], na.rm = TRUE) else Inf

        # Vérifier que les moyennes ne sont pas NA
        if (!is.na(mean_current) && !is.na(mean_next) && !is.na(mean_prev)) {
          # Fusionner avec la période la plus proche en moyenne
          if (abs(mean_current - mean_prev) <= abs(mean_current - mean_next) && i > 1) {
            periods[i-1] <- periods[i-1] + periods[i]
            periods <- periods[-(i)]
          } else {
            periods[i+1] <- periods[i] + periods[i+1]
            periods <- periods[-(i)]
          }
        } else {
          i <- i + 1
        }
      } else {
        i <- i + 1
      }
    }
    return(cumsum(periods)[-length(periods)])
  }

  # Appliquer la fusion des périodes courtes
  changepoints <- merge_short_periods(changepoints, segment_data_imputed)

  date_changepoints <- segment_data_imputed$date[changepoints]

  # Créer un dataframe pour la visualisation
  df_plot <- segment_data_imputed %>%
    mutate(changepoint = ifelse(row_number() %in% changepoints, "Yes", "No"))

  # Plot PELT
  plot_pelt <- ggplot() +
    geom_line(data = df_plot, aes(x = date, y = round(transport, 0))) +
    geom_point(data = df_plot, aes(x = date, y = round(transport, 0), color = as.factor(imputed)), size = 0.65) +
    geom_vline(xintercept = as.numeric(date_changepoints), color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("PELT Changepoints for Segment", segment_name),
         x = "Time", y = paste(type, "Count"))

  plot_pelt <- ggplotly(plot_pelt)

  # Préparation des données pour les boxplots
  all_breaks <- c(min(segment_data_imputed$date), date_changepoints, max(segment_data_imputed$date))

  assign_period <- function(date, breaks) {
    period <- findInterval(date, breaks)
    return(paste0("P", period))
  }

  boxplot_data_imputed <- segment_data_imputed %>%
    mutate(period = sapply(date, assign_period, breaks = all_breaks))

  boxplot_data_non_imputed <- segment_data_non_imputed %>%
    mutate(period = sapply(date, assign_period, breaks = all_breaks))

  # Identifier les outliers pour chaque période et type de données
  identify_outliers <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    return(x < lower_bound | x > upper_bound)
  }

  boxplot_data_imputed <- boxplot_data_imputed %>%
    group_by(period) %>%
    mutate(is_outlier = identify_outliers(transport)) %>%
    ungroup()

  boxplot_data_non_imputed <- boxplot_data_non_imputed %>%
    group_by(period) %>%
    mutate(is_outlier = identify_outliers(transport)) %>%
    ungroup()

  # Créer le boxplot pour les données imputées
  plot_boxplot_imputed <- suppressWarnings(
    ggplot(boxplot_data_imputed, aes(x = factor(period, levels = unique(period)), y = transport)) +
      geom_boxplot(outliers = F, fill = "indianred2") +
      geom_point(data = subset(boxplot_data_imputed, is_outlier),
                 aes(text = paste("Date:", format(date, "%Y-%m-%d %H:%M"),
                                  "<br>Vehicle Count:", round(transport,0),
                                  "<br>Imputed:", imputed),
                     color = as.factor(imputed)),
                 size = 1.5) +
      theme_minimal()  +
      labs(title = paste("Traffic Periods for", segment_name, "(Imputed Data)"),
           x = "Period", y = "Vehicle Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)))

  plot_boxplot_imputed <- ggplotly(plot_boxplot_imputed, tooltip = "text")

  # Créer le boxplot pour les données non imputées
  plot_boxplot_non_imputed <- suppressWarnings(
    ggplot(boxplot_data_non_imputed, aes(x = factor(period, levels = unique(period)), y = transport)) +
      geom_boxplot(fill = "lightblue", outliers = FALSE) +
      geom_point(data = subset(boxplot_data_non_imputed, is_outlier),
                 aes(text = paste("Date:", format(date, "%Y-%m-%d %H:%M"),
                                  "<br>Vehicle Count:", round(transport, 0)))) +
      theme_minimal() +
      labs(title = paste("Traffic Periods for", segment_name, "(Non-Imputed Data)"),
           x = "Period", y = "Vehicle Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )

  plot_boxplot_non_imputed <- ggplotly(plot_boxplot_non_imputed, tooltip = "text")
  # Calculer les informations de période pour les données imputées
  period_info <- boxplot_data_imputed %>%
    group_by(period) %>%
    summarise(
      start_date = min(date),
      end_date = max(date),
      n_days = as.numeric(round(difftime(max(date), min(date), units = "days"), 0)),
      n_total_data = n(),
      n_original_data = sum(imputed == "original"),
      n_imputed_data = n_total_data - n_original_data,  # Nombre de données imputées
      pct_imputed = ifelse(n_total_data == 0, 0, round(100 * n_imputed_data / n_total_data, 2)),  # Éviter la division par zéro
      n_outliers_after_imputation = sum(ifelse(is_outlier, 1, 0)),  # Nombre d'outliers après imputation
      n_imputed_outliers = sum(ifelse(is_outlier & imputed == "imputed", 1, 0)),  # Nombre d'outliers imputés
      n_outliers_original = n_outliers_after_imputation - n_imputed_outliers
    ) %>%
    arrange(start_date)

  # Remplacer les NA par 0 pour les colonnes numériques
  period_info[is.na(period_info)] <- 0

  # Calculer le pourcentage de données imputées par période
  period_info <- period_info %>%
    mutate(
      pct_imputed = ifelse(is.na(pct_imputed), 0, pct_imputed),  # Remplacer les NA par 0
      n_imputed_data = ifelse(is.na(n_imputed_data), 0, n_imputed_data)
    )

  # Calculer les outliers avant imputation
  n_outliers_before_imputation <- boxplot_data_non_imputed %>%
    group_by(period) %>%
    summarise(n = sum(ifelse(is_outlier, 1, 0))) %>%
    arrange(start_date)

  # Créer une table pour la légende
  legend_table <- plot_ly(
    type = 'table',
    header = list(values = c("Period", "Start", "End", "Days", "Total Data", "Original Data", "Imputed Data", "% Imputed", "Outliers (Before Imputation)", "Outliers (After Imputation)", "Outliers (Original)", "Imputed Outliers"),
                  align = c("left", "center", "center", "center", "center", "center", "center", "center", "center", "center", "center", "center"),
                  line = list(width = 1, color = 'black'),
                  fill = list(color = 'grey'),
                  font = list(family = "Arial", size = 10, color = "white")),
    cells = list(values = list(period_info$period,
                               format(period_info$start_date, "%Y-%m-%d %H:%M"),
                               format(period_info$end_date, "%Y-%m-%d %H:%M"),
                               period_info$n_days,
                               period_info$n_total_data,
                               period_info$n_original_data,
                               period_info$n_imputed_data,
                               period_info$pct_imputed,
                               n_outliers_before_imputation$n,
                               period_info$n_outliers_after_imputation,
                               period_info$n_outliers_original,
                               period_info$n_imputed_outliers),
                 align = c("left", "center", "center", "center", "center", "center", "center", "center", "center", "center", "center", "center"),
                 line = list(color = "black", width = 1),
                 font = list(family = "Arial", size = 9))
  )

  # Combiner les graphiques
  combined_plot <- subplot(
    list(plot_pelt, plot_boxplot_imputed, plot_boxplot_non_imputed),
    nrows = 3
  ) %>%
    layout(
      title = paste("Traffic Analysis for Segment", segment_name)
    )

  result <- list(plot = combined_plot, table = legend_table)
  return(result)
}
