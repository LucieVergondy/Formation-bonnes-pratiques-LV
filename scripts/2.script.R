#rm(list = ls())
# attention : la ligne précédente va vider tout l'environnement

#getwd()

#gestion de l'environnement----

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")


library(tidyverse)
library(forcats)

# définition de fonctions ----
source("scripts/fonctions.R", encoding = "UTF-8")


# api_token <- "trotskitueleski$1917"
api_token<-rstudioapi::askForPassword("Entrez votre jeton d'API :")



# import des données----

# j'importe les données avec read_csv2 parce que c'est un csv avec des ; et que read_csv attend comme separateur des ,
?read.csv
df <- readr::read_csv(
  "data/RPindividus_24.csv",
  col_select = c(
    "REGION", "AGED", "ANAI", "CATL", "COUPLE",
    "SEXE", "SURF", "TP", "TRANS", "IPONDI"
  )
)

# LV : pourquoi le commentaire dit utiliser read.csv2 alors que c'est read.csv qui est utilisé ?

#retraitement des données----

# df |>
#   group_by(AGED) |>
#   summarise(n = sum(IPONDI))
# LV : ne sert à rien, je le mets en commentaire



# stats trans par statut
df3 <- df |>
  group_by(COUPLE, TRANS) |>
  summarise(x = sum(IPONDI)) |>
  group_by(COUPLE) |>
  mutate(y = 100 * x / sum(x))


df <- df |>
  mutate(SEXE = as.character(SEXE)) |>
  mutate(SEXE = fct_recode(SEXE, Homme = "1", Femme = "2"))


#statistiques descriptives----


calculer_stats(df |> filter(SEXE == "Homme") |> pull(AGED))
calculer_stats(df |> filter(SEXE == "Femme") |> pull(AGED))

#41.17, 44,19


#graphiques----
ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(AGED) / 5), weight = IPONDI), stat = "count")



p <- # part d'homme dans chaque cohort
  df |>
  group_by(AGED, SEXE) |>
  summarise(SH_sexe = sum(IPONDI)) |>
  group_by(AGED) |>
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) |>
  filter(SEXE == "Homme") |>
  ggplot() +
  geom_bar(aes(x = AGED, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = AGED, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

p
ggsave("p.png", p)


# modelisation----

df3 <- df |>
  filter(SURF != "Z") |>
  sample_n(1000)
df3 <- df3 |> mutate(SURF = factor(SURF, ordered = TRUE))
df3 |>
  filter(COUPLE == 2 & between(AGED, 40, 60))
MASS::polr(SURF ~ factor(COUPLE) + factor(TP), df3)
