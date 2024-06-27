
# Import packages

library(tidyverse)



# Import Données météo du 01 janvier 2021 au 10 mai 2024

load("data/data_meteo.RData") # Importé sous le nom de meteo


# Import des données des capteurs du 01 Mai 2022 au 02 Mai 2024

load("data/segments.RData")

# données de tous les capteurs
sensors = bind_rows(list(data_01, data_02, data_04, data_05,data_06, data_07, data_08,data_10, data_11, data_13, data_14, data_15, data_16, data_18))

capteurs_V2 = read.csv("data/20230518_20240517_v2_sensors_extract.csv", sep=",", header=TRUE)

# Import des données des capteurs V2 avec la météo

load("data/V2_meteo.Rdata")
