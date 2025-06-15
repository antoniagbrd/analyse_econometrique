library(dplyr)

### PREPARATION DES GROUPES TRAITE ET CONTROLE
# Résumé des votes par groupe et parti
votes_par_parti <- donnees_tout %>%
  group_by(lieu, annee_fichier, etranger, parti) %>%
  summarise(votos = sum(votos), .groups = "drop")

# Création d'une catégorie "Spain" qui donne le nombre total de vote dans tous le pays pour chaque indicateur dans les autres colonnes
spain <- votes_par_parti %>% group_by(annee_fichier, etranger, parti) %>% summarise(votos = sum(votos), .groups = "drop") %>%
  mutate(lieu = "Spain") %>% select(lieu, annee_fichier, etranger, parti, votos)

votes_par_parti <- bind_rows(votes_par_parti, spain)

# Total des votes par groupe (lieu, année, etranger)
totaux <- votes_par_parti %>%
  group_by(lieu, annee_fichier, etranger) %>%
  summarise(total_votes = sum(votos), .groups = "drop")

df_jointure <- left_join(totaux, votes_par_parti, by = c("lieu", "etranger", "annee_fichier"))
resultats <- df_jointure %>%
  group_by(lieu, annee_fichier, etranger) %>%
  summarise(
    total_votes = unique(total_votes),
    pct_conservateur = sum(votos[parti == "Parti conservateur"], na.rm = TRUE) / total_votes * 100,
    pct_socialiste = sum(votos[parti == "Parti socialiste"], na.rm = TRUE) / total_votes * 100,
    .groups = "drop"
  ) %>%
  mutate(ratio_conservateur_socialiste = pct_conservateur / pct_socialiste)

#Groupe traité
groupe_traite <- resultats %>%
  filter(etranger == "non") %>%
  select(lieu, annee_fichier, pct_conservateur, ratio_conservateur_socialiste) %>%
  pivot_wider(
    names_from = annee_fichier,
    values_from = c(pct_conservateur, ratio_conservateur_socialiste),
    names_glue = "{.value}_{annee_fichier}"
  )
# Groupe contrôle
groupe_controle <- resultats %>%
  filter(etranger == "oui") %>%
  select(lieu, annee_fichier, pct_conservateur, ratio_conservateur_socialiste) %>%
  pivot_wider(
    names_from = annee_fichier,
    values_from = c(pct_conservateur, ratio_conservateur_socialiste),
    names_glue = "{.value}_{annee_fichier}"
  )
#selection les variables d'interêt
vars_utiles <- c(
  "pct_conservateur_1989",
  "pct_conservateur_1993",
  "ratio_conservateur_socialiste_1996",
  "ratio_conservateur_socialiste_2000"
)




### CREATION DE X0 ET X1
# X1: vecteur des caractéristiques du grp traité
# Option 1 : on prend une seule province traité en 2004), par exemple la première province du tableau
# X1 <- groupe_traite[1, vars_utiles] %>% as.numeric() %>% matrix(ncol = 1)
# Option 2 : on considère l'enseble des états espagnols
X1 <- groupe_traite[ 50, vars_utiles] %>% as.numeric() %>% matrix(ncol = 1) # on sélectionne l'Espagne
X0 <- groupe_controle[-50, vars_utiles] %>% as.matrix() # -on pense à enlever l'Espagne

dim(X1)  # Doit être 4 x 1 ou K x 1 (on a 4 caractéristiques pour un seul individu)
dim(X0)  # Doit être 52 x 4 ou JxK (on a 4 caractéristiques (K) pour chacune des 52 provinces (J) (non residents))
install.packages("quadprog") 
library(quadprog)

# Preparer les matrices X0 et X1
X1 <- as.matrix(X1) # K variables x 1 région
X0 <- as.matrix(X0) #  J régions x K variables




### MINIMISER LA MATRICE SOUS CONTRAINTES
# préparation des matrices
X0_t <- t(X0)  # K x J (on transpose X0)
dim(X0_t)
Dmat <- X0 %*% X0_t + diag(1e-6, ncol(X0_t))  # J x J (matrice D = X0' x X0)
dim(Dmat)
dvec <- X0 %*% X1  # J x 1 (vecteur d = X0' x X1)
dim(dvec)

# contraintes : somme des poids égale à 1, chaque poids supérieur ou égal à 0 (At x W supérieur ou égal à b)
Amat <- cbind(rep(1, ncol(X0_t)), diag(ncol(X0_t)))  # la première colonne (1...1) vérifie que la somme des pondérations est supérieure ou égale à 1. Les colonnes 2 à J+1 vérifient les contraintes de positivité sur chaque pondération (matrice identité de taille JxJ). Au total : J x (1 + J)
bvec <- c(1, rep(0, ncol(X0_t)))  # taille = 1 + J (colonne de 0 de taille J)
meq <- 1  # 1 contrainte d'égalité (la première colonne)

# résolution 
sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
W <- sol$solution # Jx1
length(W) # doit être égal à J = 52

# vérification #(doit être très proche de 1)
sum(W)  # c'est validé sum(W) = 1




# PREPARER LES DONNEES CONTROLE SYNTHETIQUE ET TRAITE
# Groupe contrôle synthétique : Sélectionner les variables expliquées pour 2004 
Y0_2004 <- groupe_controle[-50,] %>% # enlever l'espagne du groupe contrôle
  select(pct_conservateur_2004, ratio_conservateur_socialiste_2004) %>%
  as.matrix() # J x K

# Appliquer les poids W pour obtenir les valeurs synthétiques
Y0_2004 <- t(Y0_2004) %*% W # 2 x 1
View(Y0_2004)

# Afficher les résultats
colnames(Y0_2004) <- "Groupe de contrôle synthétique"
rownames(Y0_2004) <- c("pct_conservateur_2004", "ratio_conservateur_socialiste_2004")
Y0_2004

# Groupe contrôle synthétique : sélectionner toutes les variables
X0_complet <- groupe_controle[-50,] %>%
  select(-lieu) %>%
  as.matrix()
View(X0_complet) # 52 x 10

# Appliquer les poids W sur toutes les caractéristiques
groupe_synthetique_valeurs <- t(X0_complet) %*% W # 10 x 1

# Transformer en dataframe
groupe_synthetique_df <- data.frame(
  variable = colnames(X0_complet),
  valeur_synthetique = as.numeric(groupe_synthetique_valeurs))

# Groupe traité :
groupe_controle_df <- groupe_traite %>%
  filter(lieu == "Spain") %>%
  pivot_longer(
    cols = -lieu, 
    names_to = "variable",
    values_to = "valeur_traite") %>%
  select(-lieu)

# Fusionner les deux dataframes 
resultats <- left_join(groupe_controle_df, groupe_synthetique_df, by = "variable")
View(resultats)


### PRESENTER LES RESULTATS
#créer un tableau similaire à la table 3 de l'article (format latex)
''' install.packages("knitr")'''
library(xtable)

table3 <- resultats[c(1, 2, 8, 9), ]
table3$variable <- c(
  "Percentage of votes for the conservative party in 1989",
  "Percentage of votes for the conservative party in 1993",
  "Ratio of voters of the conservative party over the socialist party in 1996",
  "Ratio of voters of the conservative party over the socialist party in 200")
colnames(table3) <- c("Variables", "Treated", "Synthetic")
View(table3)
table_tex <- xtable(table3, caption = "Comparison groups")
print(table_tex, file = "tableau.tex", include.rownames = FALSE)

#Créer un graphique similaire à la figure 3 de l'article
library(ggplot2)

fig3_data  <- resultats[c(6,7,8,9,10), ] 
fig3_data$variable <- c(1989,1993,1996,2000,2004)
colnames(fig3_data) <- c("Year","Treated","Synthetic")
fig3_data <- fig3_data %>%
  pivot_longer(cols = c(Treated, Synthetic),
               names_to = "Group",
               values_to = "Value")

View(fig3_data)
ggplot(fig3_data, aes(x = Year, y = Value, color = Group, linetype = Group)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Treated" = "red", "Synthetic" = "blue")) +
  scale_linetype_manual(values = c("Treated" = "solid", "Synthetic" = "dashed")) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Ratio votes conservatives over socialists",
    color = "Unit",
    linetype = "Unit") + ggsave("figure_3.pdf", width = 9, height = 4)



### ROOT MEAN SQUARED ERROR
RMSE_data = resultats[c(6,7,8,9), ] 
RMSE <- sqrt(mean((RMSE_data$valeur_traite - RMSE_data$valeur_synthetique)^2))
print(RMSE)


### SI ON N'A PAS FAIT LE LIEU "SPAIN" AU PREALABLE (moyenne imparfaites)

# Extraire les valeurs numériques des 4 caractéristiques du groupe traité
valeurs_traite <- groupe_traite %>%
  select(
    pct_conservateur_1989,
    pct_conservateur_1993,
    ratio_conservateur_socialiste_1996,
    ratio_conservateur_socialiste_2000
  ) %>%
  unlist(use.names = FALSE)
valeurs_synthetique <- groupe_synthetique_df %>%
  filter(variable %in% c(
    "pct_conservateur_1989",
    "pct_conservateur_1993",
    "ratio_conservateur_socialiste_1996",
    "ratio_conservateur_socialiste_2000"
  )) %>%
  pull(valeur_synthetique)

# Noms lisibles pour le tableau
noms_lisibles <- c(
  "Pourcentage de votes pour le parti conservateur en 1989",
  "Pourcentage de votes pour le parti conservateur en 1993",
  "Proportion d'électeurs du parti conservateur par rapport au parti socialiste en 1996",
  "Proportion d'électeurs du parti conservateur par rapport au parti socialiste en 2000")

# Calcul des moyennes du groupe traité pour chaque variable
moyennes_traite <- groupe_traite %>%
  summarise(
    `Pourcentage de votes pour le parti conservateur en 1989` = mean(pct_conservateur_1989, na.rm = TRUE),
    `Pourcentage de votes pour le parti conservateur en 1993` = mean(pct_conservateur_1993, na.rm = TRUE),
    `Ratio conservateur/socialiste en 1996` = mean(ratio_conservateur_socialiste_1996, na.rm = TRUE),
    `Ratio conservateur/socialiste en 2000` = mean(ratio_conservateur_socialiste_2000, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Traité")


# Filtrer uniquement les lignes correspondant aux variables d'intérêt
groupe_synthetique_df_filtre <- groupe_synthetique_df %>%
  filter(variable %in% c("pct_conservateur_1989", 
                         "pct_conservateur_1993", 
                         "ratio_conservateur_socialiste_1996", 
                         "ratio_conservateur_socialiste_2000"))

#ajouter un id commun entre les deux dataframe
moyennes_traite$annee <- seq_len(nrow(moyennes_traite))
groupe_synthetique_df_filtre$annee <- seq_len(nrow(groupe_synthetique_df_filtre))
#obtenir le tableau de comparaison:
fusion <- left_join(moyennes_traite, groupe_synthetique_df_filtre, by = "annee")
fusion_reduite <- fusion %>%
  select(Variable, Traité, valeur_synthetique)




