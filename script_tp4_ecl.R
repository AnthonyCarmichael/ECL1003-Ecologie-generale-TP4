###########################################################################
# Auteur : Anthony Carmichael
# UQTR ECL1003 - Écologie générale
# TP4 : Manipulation des données issues des cimetières 
# Github: https://github.com/AnthonyCarmichael/ECL1003-Ecologie-generale-TP4
##########################################################################

library(readxl)
library(ggplot2)
library(dplyr)

dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# donnée
don <- read_excel("./data/Donnees_TP4_Cimetiere.xlsx")

sum(is.na(don)) # Donne 0 donc aucune donnée NA

###############################################################################
#                                 Étape 1                                     #
# #############################################################################
# 1. Nuage de point en fonction de l'age de mort en fonction de l'année de mort
graph1 <- don |> ggplot(mapping = aes(x = mort,y = age)) + 
  geom_point () + 
  geom_smooth(method = "lm", se=FALSE)+
  theme_bw()
modele <- lm(age ~ mort, data = don)
# p-value < 2.2e-16 on regette l'hypothèse nul avec une grande confiance
summary(modele)
ggsave('./output/figure1.png',graph1,width = 6, height=5)

# 2. a) age moyen par intervalle par classe

# Ajout colonne periode pour faire des intervalles
don <- don |> mutate(
  periode = cut( # Permet de faire des intervalles 
    mort, # valeur x
    breaks = c(-Inf,seq(1800, 2025, by = 25), Inf), # Les intervalles
    right = FALSE, # exclusion données limite de droite [1925,1950[
    dig.lab = 4 # 4 chiffre significatif pour les classes
  )) 

# Calculer la moyenne par classe d'intervalles
moy_age_by_intervals <- don |> group_by(periode) |>
  summarise(mean = mean(age), nb = n()) 

# graph moyenne d'age par periode
moy_age_by_intervals |>
  ggplot(mapping = aes(x = periode,y = mean)) + 
  geom_point () + 
  theme_bw() 
ggsave('./output/figure2.png',width = 12, height=5)

###############################################################################
#                                 Étape 2                                     #
# #############################################################################

# Graph age en fonction gender
# sd() donne écart type d'un échantillon (n-1)
 moy_age_by_gender <- don |>
  group_by(sexe_m_f) |>
  summarise(mean = mean(age), ecart_type = sd(age))

ggplot(moy_age_by_gender, mapping = aes(x = sexe_m_f, y = mean)) + 
  geom_col() + geom_errorbar(aes( ymin = mean - ecart_type,
                                  ymax = mean + ecart_type), width = 0.2 )
ggsave('./output/figure3.png',width = 6, height=5)

# graph age en fonction langue
moy_age_by_langue <- don |>
  group_by(langue) |>
  summarise(mean = mean(age), ecart_type = sd(age))

ggplot(moy_age_by_langue, mapping = aes(x = langue, y = mean)) + 
  geom_col() + geom_errorbar(aes( ymin = mean - ecart_type,
                                  ymax = mean + ecart_type), width = 0.2 )
ggsave('./output/figure4.png',width = 6, height=5)






