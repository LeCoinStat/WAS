###########################################################################
# Code Analyse des relations entre des variables
#1-Quali-Quali
#2-Quanti-Quanti
#3-Quanti-Quali
###########################################################################



###########################################################################
# Analyse de la liaison entre deux variables qualitatives -----------------
# Hypothèse nulle: il y a pas de lien
# Hopytèse alternative : il y a un lien entre les deux variables
###########################################################################

# Charger les données depuis un fichier CSV
data <- read.csv("/Users/natachanjongwayepnga/Documents/LeCoinStat/WAS17/data/base_de_donnees_media_sociaux.csv", header = TRUE, sep = ",")

#Visualiser la base de données
View(data)

# Question 1: Relation entre l'âge et la plateforme de média social : Y a-t-il une association entre la tranche d'âge des utilisateurs et leur préférence pour une plateforme de média social spécifique ?

# Etape 1: Décrire les deux variables sur le plan univarié
library(ggplot2)# librairie pour faire des graphiques
# Pour la variable 'Tranche_d_age'
age_table <- table(data$Tranche_d_age)

#Répartition par tranche d'âge
print(age_table)
barplot(age_table, main="Distribution des tranches d'âge", xlab="Tranche d'âge", ylab="Fréquence")

# Calcul des proportions pour la variable 'Tranche_d_age'
age_counts <- table(data$Tranche_d_age)
age_df <- data.frame(Age = names(age_counts), Count = as.numeric(age_counts))
age_df$Proportion <- age_df$Count / sum(age_df$Count)

# Barplot pour 'Tranche_d_age'
ggplot(age_df, aes(x = Age, y = Proportion, fill = Age)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion des tranches d'âge", x = "Tranche d'âge", y = "Proportion") +
  theme_minimal()

# Pour la variable 'Plateforme_Media_Social'
platform_table <- table(data$Plateforme_Media_Social)
print(platform_table)
barplot(platform_table, main="Distribution des plateformes de média social", xlab="Plateforme", ylab="Fréquence")


# Calcul des proportions pour la variable 'Plateforme_Media_Social'
platform_counts <- table(data$Plateforme_Media_Social)
platform_df <- data.frame(Platform = names(platform_counts), Count = as.numeric(platform_counts))
platform_df$Proportion <- platform_df$Count / sum(platform_df$Count)

# Barplot pour 'Plateforme_Media_Social'
ggplot(platform_df, aes(x = Platform, y = Proportion, fill = Platform)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion des plateformes de média social", x = "Plateforme", y = "Proportion") +
  theme_minimal()


# Étape 2 : Description bivariée des variables et formulation d'une hypothèse

# Tableau de contingence
contingency_table <- table(data$Tranche_d_age, data$Plateforme_Media_Social)
print(contingency_table)


# Conversion en dataframe pour le graphique
contingency_df <- as.data.frame(contingency_table)
names(contingency_df) <- c("Tranche_d_age", "Plateforme_Media_Social", "Count")

# Calcul des proportions pour chaque combinaison de tranche d'âge et plateforme
contingency_df$Proportion <- with(contingency_df, Count / sum(Count[Tranche_d_age == contingency_df$Tranche_d_age]))



library(ggplot2)

# Créer un graphique de répartition par tranche d'âge et par plateforme
age_platform_plot <- ggplot(data, aes(x = Tranche_d_age, fill = Plateforme_Media_Social)) +
  geom_bar(position = "fill") +
  labs(title = "Répartition par Tranche d'âge et par Plateforme", x = "Tranche d'âge", y = "Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Afficher le graphique
print(age_platform_plot)

# Créer un graphique de répartition par plateforme et par tranche d'âge
platform_age_plot <- ggplot(data, aes(x = Plateforme_Media_Social, fill = Tranche_d_age)) +
  geom_bar(position = "fill") +
  labs(title = "Répartition par Plateforme et par Tranche d'âge", x = "Plateforme de média social", y = "Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Afficher le graphique
print(platform_age_plot)


# Etape 3: Test de chi-deux d'indépendance 
?chisq.test # Pour voir le fonctionnement
# Test du Chi-deux d'indépendance
chi_squared_test <- chisq.test(contingency_table)

# Afficher les résultats du test
print(chi_squared_test)

#install.packages("vcd")
library(vcd)
assocstats(contingency_table)

# Calcul du coefficient de Cramer (V)
cramer_v <- sqrt(chi_squared_test$statistic / (nrow(data) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))
cramer_v

# Etape 4: Conclusion


# Question 2: Influence du niveau d'éducation sur le choix de la plateforme : Le niveau d'éducation des utilisateurs affecte-t-il de manière significative leur choix de plateforme de média social ?



###########################################################################
# Analyse de la corrélation entre deux variables quantitatives -----------------
# Hypothèse nulle: il y a pas de corrélation
# Hopytèse alternative : il y a un corrélation entre les deux variables
###########################################################################

# Importation des données
data <- read.csv("/Users/natachanjongwayepnga/Documents/LeCoinStat/WAS17/data/data_marketing.csv", header = TRUE, sep = ",")

View(data)

# Étape 1 : Description univariée des variables Revenu et Dépenses Publicitaires
# Statistiques descriptives pour Revenu et Dépenses Publicitaires
summary(data$Revenu)
summary(data$Depenses_Publicitaires)

# Boxplot pour Revenu
boxplot(data$Revenu, main = "Boxplot de Revenu", ylab = "Revenu")

# Boxplot pour Dépenses Publicitaires
boxplot(data$Depenses_Publicitaires, main = "Boxplot de Dépenses Publicitaires", ylab = "Dépenses Publicitaires")


# Etape 2: Description bivariée des variables Reveni et Dépenses Publicitaires

# Nuage de points pour Revenu et Dépenses Publicitaires
plot(data$Revenu, data$Depenses_Publicitaires, 
     main = "Nuage de points entre Revenu et Dépenses Publicitaires",
     xlab = "Revenu", ylab = "Dépenses Publicitaires")



# Étape 3 : Corrélation de Pearson, Spearman et Kendall


# Calcul de la corrélation de Pearson
correlation_pearson <- cor(data$Revenu, data$Depenses_Publicitaires)

# Afficher la corrélation de Pearson
correlation_pearson

# Calcul de la corrélation de Spearman
correlation_spearman <- cor(data$Revenu, data$Depenses_Publicitaires, method = "spearman")

# Afficher la corrélation de Spearman
correlation_spearman


# Calcul de la corrélation de Kendall
correlation_kendall <- cor(data$Revenu, data$Depenses_Publicitaires, method = "kendall")

# Afficher les corrélations
correlation_kendall


# Test de corrélation de Pearson
cor_test_pearson <- cor.test(data$Revenu, data$Depenses_Publicitaires, method = "pearson")

# P-valeur de la corrélation de Pearson
p_value_pearson <- cor_test_pearson$p.value

# Test de corrélation de Spearman
cor_test_spearman <- cor.test(data$Revenu, data$Depenses_Publicitaires, method = "spearman")

# P-valeur de la corrélation de Spearman
p_value_spearman <- cor_test_spearman$p.value

# Test de corrélation de Kendall
cor_test_kendall <- cor.test(data$Revenu, data$Depenses_Publicitaires, method = "kendall")

# P-valeur de la corrélation de Kendall
p_value_kendall <- cor_test_kendall$p.value

# Afficher les p-valeurs
print(paste("P-valeur de la corrélation de Pearson : ", p_value_pearson))
print(paste("P-valeur de la corrélation de Spearman : ", p_value_spearman))
print(paste("P-valeur de la corrélation de Kendall : ", p_value_kendall))


# Exercice: Utiliser la base water_oxygen_data.csv pour analyser la relation entre la température de l'eau et le niveau d'oxygène



###########################################################################
# Analyse de la liaison entre deux variables ordinales -----------------
# Hypothèse nulle: il y a pas de lien
# Hopytèse alternative : il y a un lien entre les deux variables
###########################################################################






# Charger les données depuis un fichier CSV
data <- read.csv("/Users/natachanjongwayepnga/Documents/LeCoinStat/WAS17/data/recommandation_satisfaction.csv", header = TRUE, sep = ",")


# Légende des modalités pour 'Niveau_de_Satisfaction' :
# 1 - Très Insatisfait
# 2 - Insatisfait
# 3 - Neutre
# 4 - Satisfait
# 5 - Très Satisfait

# Légende des modalités pour 'Probabilite_de_Recommandation' :
# 1 - Très peu probable : Il est très peu probable que le client recommande le produit/service
# 2 - Peu probable : Il est peu probable que le client recommande le produit/service
# 3 - Probable : Il est probable que le client recommande le produit/service
# 4 - Très probable : Il est très probable que le client recommande le produit/service


# Etape 1: Description univariée des deux variables
# Pour la variable 'Niveau_de_Satisfaction'
niveau_satisfaction_table <- table(data$Niveau_de_Satisfaction)
print(niveau_satisfaction_table)
barplot(niveau_satisfaction_table, main="Distribution des niveaux de satisfaction", xlab="Niveau de Satisfaction", ylab="Fréquence")

# Pour la variable 'Probabilite_de_Recommandation'
probabilite_recommandation_table <- table(data$Probabilite_de_Recommandation)
print(probabilite_recommandation_table)
barplot(probabilite_recommandation_table, main="Distribution de la probabilité de recommandation", xlab="Probabilité de Recommandation", ylab="Fréquence")


# Etape 2: Description bivariée des variables
# Tableau de contingence
contingency_table <- table(data$Niveau_de_Satisfaction, data$Probabilite_de_Recommandation)
print(contingency_table)


# Charger les bibliothèques nécessaires
library(ggplot2)


# Assurez-vous que les données sont du bon type
data$Niveau_de_Satisfaction <- factor(data$Niveau_de_Satisfaction, ordered = TRUE)
data$Probabilite_de_Recommandation <- factor(data$Probabilite_de_Recommandation, ordered = TRUE)

# Créer un graphique de répartition de la probabilité de recommandation par niveau de satisfaction
satisfaction_recommendation_plot <- ggplot(data, aes(x = Niveau_de_Satisfaction, fill = Probabilite_de_Recommandation)) +
  geom_bar(position = "fill") +
  labs(title = "Répartition de la probabilité de recommandation par niveau de satisfaction",
       x = "Niveau de Satisfaction",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Afficher le graphique
print(satisfaction_recommendation_plot)






# Etape 3: Test de chi-deux d'indépendance, calcul du V de Cramer et corrélations
# Test du Chi-deux d'indépendance
chi_squared_test <- chisq.test(contingency_table)

# Afficher les résultats du test
print(chi_squared_test)

# Calcul du coefficient de Cramer (V)
#install.packages("vcd")
library(vcd)
assocstats(contingency_table)

# Calcul du coefficient de Cramer (V)

n <- sum(chi_squared_test$observed) # Nombre total d'observations
phi2 <- chi_squared_test$statistic / n
r <- nrow(contingency_table)
k <- ncol(contingency_table)
cramer_v <- sqrt(phi2 / (min(k-1, r-1)))
print(cramer_v)

# Corrélation de Spearman
cor_spearman <- cor(as.numeric(data$Niveau_de_Satisfaction), as.numeric(data$Probabilite_de_Recommandation), method = "spearman")
print(cor_spearman)

# Corrélation de Kendall
cor_kendall <- cor(as.numeric(data$Niveau_de_Satisfaction), as.numeric(data$Probabilite_de_Recommandation), method = "kendall")
print(cor_kendall)

# Etape 4: Conclusion




###########################################################################
# Analyse de la liaison entre une variable qualitative et une variable quantitaive -----------------
# Hypothèse nulle: les moyennes/médianes par groupes de la variable quantitative sont égale
# Hopytèse alternative : les moyennes/médianes sont différentes
###########################################################################



########################################################################################
# Cas avec deux modalités: les groupes sont de deux populations indépendantes
########################################################################################




# Importation des données 

data <- read.csv("/Users/natachanjongwayepnga/Documents/LeCoinStat/WAS17/data/donnees_marketing.csv", header = TRUE, sep = ",")


# Question: Est-ce que le montant d'achat est le même chez les clients du groupe 1 ou les clients du groupe 2?

#Étape 1 : Description Univariée des Variables
library(ggplot2)  # Librairie pour faire des graphiques

# Pour la variable 'data'
group_marketing_table <- table(data$Groupe)

# Répartition par groupe de marketing
print(group_marketing_table)
barplot(group_marketing_table, main="Distribution des groupes de marketing", xlab="Groupe de Marketing", ylab="Fréquence")

# Pour la variable 'Montant_Achat'
ggplot(data, aes(x = Montant_Achat)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution des Montants d'Achat", x = "Montant d'Achat", y = "Fréquence") +
  theme_minimal()

#Étape 2 : Description Bivariée et Formulation d'une Hypothèse
# Créer un boxplot bivarié pour 'Montant_Achat' par 'Groupe_Marketing'
ggplot(data, aes(x = Groupe, y = Montant_Achat, fill = Groupe)) +
  geom_boxplot() +
  labs(title = "Montant d'Achat par Groupe de Marketing", x = "Groupe de Marketing", y = "Montant d'Achat") +
  theme_minimal()

# Étape 3 : Tests Statistiques

# Test paramétrique (Test de Student et Test de Welch)
#Hypothèse: le montant d'achat moyen est le même dans les deux groupes

# Test de Student
t_test <- t.test(Montant_Achat ~ Groupe, data = data, var.equal = TRUE)
print(t_test)



# Vérification des hypothèses: égalité de la variance avec le test de levene

# install.packages("car")
library(car)

# Test de Levene
levene_test <- leveneTest(Montant_Achat ~ Groupe, data = data)
print(levene_test)


# Test de Welch (Si les variances sont inégales)
welch_test <- t.test(Montant_Achat ~ Groupe, data = data)
print(welch_test)


# Vérifier la normalité
# Test de Shapiro-Wilk pour la variable 'Montant_Achat'
shapiro_test_total <- shapiro.test(data$Montant_Achat)
print(shapiro_test_total)


# Test non paramétrique (Test de wilcoxon et kolmogorov smirnov): si la normalité n'est pas vérifié


# Séparation des données par groupe de marketing
montant_achat_A <- data$Montant_Achat[data$Groupe == 'A']
montant_achat_B <- data$Montant_Achat[data$Groupe== 'B']

# Test de Wilcoxon pour deux échantillons indépendants
# Hypothèse: la médiane est la même dans les deux groupes
wilcoxon_test <- wilcox.test(montant_achat_A, montant_achat_B)
print(wilcoxon_test)

# Test de Kolmogorov-Smirnov pour comparer les distributions des deux groupes
ks_test <- ks.test(montant_achat_A, montant_achat_B)
print(ks_test)



########################################################################################
# Cas avec plus de deux modalités: les groupes sont de deux population indépendantes
########################################################################################

# Chargement des bibliothèques nécessaires
library(tidyverse)
library(car)

# Importation des données
data <- read.csv("/Users/natachanjongwayepnga/Documents/LeCoinStat/WAS17/data/donnees_marketing_3_groupes.csv", header = TRUE, sep = ",")

# Étape 1 : Description Univariée des Variables
# Distribution des groupes de marketing
group_marketing_table <- table(data$Groupe_Marketing)
barplot(group_marketing_table, main="Répartition des groupes de marketing", xlab="Groupe de Marketing", ylab="Fréquence")

# Distribution des Montants d'Achat
ggplot(data, aes(x = Montant_Achat)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution des Montants d'Achat", x = "Montant d'Achat", y = "Fréquence") +
  theme_minimal()

# Étape 2 : Description Bivariée et Formulation d'une Hypothèse
# Boxplot Montant d'Achat par Groupe de Marketing
ggplot(data, aes(x = Groupe_Marketing, y = Montant_Achat, fill = Groupe_Marketing)) +
  geom_boxplot() +
  labs(title = "Montant d'Achat par Groupe de Marketing", x = "Groupe de Marketing", y = "Montant d'Achat") +
  theme_minimal()

# Étape 3 : Tests Statistiques
# Vérification de la normalité
shapiro_test_total <- shapiro.test(data$Montant_Achat)
shapiro_test_total
# Test d'égalité des variances (Levene)
levene_test <- leveneTest(Montant_Achat ~ Groupe_Marketing, data = data)
levene_test
# Tests paramétriques et non-paramétriques
# Test ANOVA (si les variances sont égales et les données normalement distribuées)
anova_test <- aov(Montant_Achat ~ Groupe_Marketing, data = data)
summary(anova_test)


# Test de Kruskal-Wallis (si les hypothèses de l'ANOVA ne sont pas respectées)
kruskal_test <- kruskal.test(Montant_Achat ~ Groupe_Marketing, data = data)
kruskal_test



#Etape 4: Interprétation



