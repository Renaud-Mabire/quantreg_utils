
library(usethis)
library(readr)

# Charger les données
data <- read_csv("data_rq_tutorial.csv")

# Enregistrer les données dans le dossier /data de votre package
usethis::use_data(data, overwrite = TRUE)
