library(dplyr)
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
# Filtrer les non-résidents (groupe contrôle potentiel)
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
#creation de X0 et X1
# X1: vecteur des caractéristiques du grp traité
# Option 1 : on prend une seule province traité en 2004), par exemple la première province du tableau
# X1 <- groupe_traite[1, vars_utiles] %>% as.numeric() %>% matrix(ncol = 1)
# Option 2 : on considère l'enseble des états espagnols
X1 <- groupe_traite[ 50, vars_utiles] %>% as.numeric() %>% matrix(ncol = 1)
X0 <- groupe_controle[, vars_utiles] %>% as.matrix()

dim(X1)  # Doit être 4 x 1 (on a 4 caractéristiques pour un seul individu)
dim(X0)  # Doit être 52 x 4 (on a 4 caractéristiques pour chacune des 52 provinces (non residents))
install.packages("quadprog") 
library(quadprog)
#preparer les matrices X0 et X1
X1 <- as.matrix(X1)
X0 <- as.matrix(X0)
#minimiser la matrice sous des contraites: 
X0_t <- t(X0)  # J x K
Dmat <- t(X0_t) %*% X0_t + diag(1e-6, ncol(X0_t))  # J x J
dvec <- t(X0_t) %*% X1  # J x 1
#contraintes: 
Amat <- cbind(rep(1, ncol(X0_t)), diag(ncol(X0_t)))  # J x (1 + J)
bvec <- c(1, rep(0, ncol(X0_t)))  # taille = 1 + J
meq <- 1  # 1 contrainte d'égalité (la première colonne)
#resolution 
sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
W <- sol$solution
sum(W)  # Doit être très proche de 1
# Extraire uniquement les valeurs 2004 des variables à prédire
Y0_2004 <- groupe_controle %>%
  select(pct_conservateur_2004, ratio_conservateur_socialiste_2004) %>%
  as.matrix()
# Appliquer les poids W pour obtenir les valeurs synthétiques
groupe_synthetique_2004 <- t(Y0_2004) %*% W
# Afficher les résultats
colnames(groupe_synthetique_2004) <- "Synthétique"
rownames(groupe_synthetique_2004) <- c("pct_conservateur_2004", "ratio_conservateur_socialiste_2004")
groupe_synthetique_2004
X0_complet <- groupe_controle %>%
  select(-lieu) %>%
  as.matrix()
# Appliquer les poids sur toutes les caractéristiques
groupe_synthetique_valeurs <- t(X0_complet) %*% W
# Transformer en dataframe
groupe_synthetique_df <- data.frame(
  variable = colnames(X0_complet),
  valeur_synthetique = as.numeric(groupe_synthetique_valeurs)
)
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
  "Proportion d'électeurs du parti conservateur par rapport au parti socialiste en 2000"
)
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


