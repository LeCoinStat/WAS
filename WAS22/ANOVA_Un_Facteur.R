# Création des données
publicite_tv <- c(291.94, 324.45, 304.17, 319.53, 307.13, 314.13, 300.21)
publicite_radio <- c(315.72, 282.54, 288.04, 317.66, 253.04, 254.59, 299.39)
publicite_rs <- c(326.54, 388.87, 341.73, 335.05, 388.46, 379.61, 387.35)

# Combinaison des données dans un data frame
donnees <- data.frame(
  vente = c(publicite_tv, publicite_radio, publicite_rs),
  groupe = factor(rep(c("TV", "Radio", "RS"), each = 7))
)

View(donnees)

# Réalisation de l'ANOVA
resultats_anova <- aov(vente ~ groupe, data = donnees)

# Affichage du tableau de l'ANOVA
summary(resultats_anova)


# Test de Bartlett pour l'égalité des variances
test_bartlett <- bartlett.test(vente ~ groupe, data = donnees)

# Affichage des résultats du test de Bartlett
print(test_bartlett)




# Test de normalité de Shapiro-Wilk pour chaque groupe
test_normalite_tv <- shapiro.test(publicite_tv)
test_normalite_radio <- shapiro.test(publicite_radio)
test_normalite_rs <- shapiro.test(publicite_rs)

# Affichage des résultats des tests de normalité
print(test_normalite_tv)
print(test_normalite_radio)
print(test_normalite_rs)


# ANOVA dans la pratique
# Étape 0: Installation et chargement des packages nécessaires
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

library(ggplot2)
library(dplyr)

# Étape 1: Nettoyage des données et analyse univariée
publicite_tv <- c(291.94, 324.45, 304.17, 319.53, 307.13, 314.13, 300.21)
publicite_radio <- c(315.72, 282.54, 288.04, 317.66, 253.04, 254.59, 299.39)
publicite_rs <- c(326.54, 388.87, 341.73, 335.05, 388.46, 379.61, 387.35)

# Combinaison des données dans un data frame
donnees <- data.frame(
  vente = c(publicite_tv, publicite_radio, publicite_rs),
  groupe = factor(rep(c("TV", "Radio", "RS"), each = 7))
)

# Analyse univariée (ici, simple résumé statistique pour chaque groupe)
summary(donnees$vente)
summary(donnees$groupe)



# Histogramme des ventes
ggplot(donnees, aes(x = vente)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.7) +
  labs(title = "Histogramme des ventes", 
       x = "Ventes", 
       y = "Fréquence")

# Diagramme en barres pour la variable qualitative (groupe de publicité)
ggplot(donnees, aes(x = groupe, fill = groupe)) +
  geom_bar() +
  labs(title = "Nombre d'observations par type de publicité", 
       x = "Type de publicité", 
       y = "Nombre d'observations")





# Étape 2: Création d'un boxplot bivarié
ggplot(donnees, aes(x = groupe, y = vente, fill = groupe)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot des ventes par type de publicité", 
       x = "Type de publicité", 
       y = "Ventes")

# Étape 3: Réalisation de l'ANOVA et analyse du résumé
resultats_anova <- aov(vente ~ groupe, data = donnees)
summary(resultats_anova)

# Étape 4: Validation de l'ANOVA avec des tests statistiques de normalité et d'égalité de variances
# Test de normalité de Shapiro-Wilk pour chaque groupe
test_normalite_tv <- shapiro.test(donnees$vente[donnees$groupe == "TV"])
test_normalite_radio <- shapiro.test(donnees$vente[donnees$groupe == "Radio"])
test_normalite_rs <- shapiro.test(donnees$vente[donnees$groupe == "RS"])

# Affichage des résultats des tests de normalité
print(test_normalite_tv)
print(test_normalite_radio)
print(test_normalite_rs)

# Test de Bartlett pour l'égalité des variances
test_bartlett <- bartlett.test(vente ~ groupe, data = donnees)
print(test_bartlett)