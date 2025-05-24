source(preparation.R)
library(ggplot2)  
#### verifications des nombres de votes
# ... pour les "resident" voters 
#somme_voix_parti_lieu_an <- data_non %>%  #ou data_non ?
  #group_by(annee_fichier, parti, lieu) %>% 
  #summarise(somme_voix_parti_lieu_an = sum(votos))
#print(somme_voix_parti_lieu_an)

#somme_votants_par_an_parti <- data_non %>%  #ou data_non ?
  #group_by(annee_fichier, parti) %>% 
  #summarise(somme_votants_par_an_parti = sum(votos))
#print(somme_votants_par_an_parti)

#somme_votants_par_an <- data_non %>%  #ou data_non ?
  #group_by(annee_fichier) %>% 
  #summarise(somme_votants_par_an = sum(votos))
#print(somme_votants_par_an)




# dataframe des ratios pour les residential voters 
ratio_espagne_residential <- data_non %>%
  # Filtrer les données pour les partis d'intérêt
  filter(parti %in% c("Parti conservateur", "Parti socialiste")) %>%
  # Grouper par année et parti, puis calculer le nombre total de votes
  group_by(annee_fichier, parti) %>%
  summarise(total_votes = sum(votos, na.rm = TRUE), .groups = 'drop') %>%
  # Pivoter les données pour avoir les votes par parti en colonnes
  pivot_wider(names_from = parti, values_from = total_votes) %>%
  # Calculer le ratio
  mutate(`ratio conservatives over socialists` = `Parti conservateur` / `Parti socialiste`) %>%
  # Sélectionner les colonnes d'intérêt
  select(annee_fichier, `ratio conservatives over socialists`)

# Afficher le nouveau dataframe
print(ratio_espagne_residential)

# dataframe des ratios pour les residential voters 
ratio_espagne_non_residential <- data_oui %>%
  # Filtrer les données pour les partis d'intérêt
  filter(parti %in% c("Parti conservateur", "Parti socialiste")) %>%
  # Grouper par année et parti, puis calculer le nombre total de votes
  group_by(annee_fichier, parti) %>%
  summarise(total_votes = sum(votos, na.rm = TRUE), .groups = 'drop') %>%
  # Pivoter les données pour avoir les votes par parti en colonnes
  pivot_wider(names_from = parti, values_from = total_votes) %>%
  # Calculer le ratio
  mutate(`ratio conservatives over socialists` = `Parti conservateur` / `Parti socialiste`) %>%
  # Sélectionner les colonnes d'intérêt
  select(annee_fichier, `ratio conservatives over socialists`)

# Afficher le nouveau dataframe
print(ratio_espagne_non_residential)


# Créer le graphique
ggplot() +
  # Ajouter la courbe pour `nouveau_df` avec une ligne pleine
  geom_line(data = ratio_espagne_residential, aes(x = annee_fichier, y = `ratio conservatives over socialists`), color = "blue", linewidth = 1) +
  # Ajouter la courbe pour `autre_df` avec une ligne en pointillés
  geom_line(data = ratio_espagne_non_residential, aes(x = annee_fichier, y = `ratio conservatives over socialists`), color = "red", linetype = "dashed", linewidth = 1) +
  # Ajouter des lignes horizontales aux ratios de 2, 1 et 0
  geom_hline(yintercept = c(0, 1, 2), color = "grey", linetype = "solid", linewidth = 0.5) +
  # Ajouter des étiquettes et un titre
  labs(title = "Ratio des votes conservateurs sur socialistes par année",
       x = "Année",
       y = "Ratio conservateurs/socialistes") +
  scale_x_continuous(breaks = ratio_espagne_residential$annee_fichier) +
  # Afficher la légende
  theme_minimal()

################### A FAIRE / VERIFIER ###############################

# soustraire le nombre de votes de l'etranger au total pour avoir ceux pas à l'etranger pour chaque province ?
# election day voters (treatment group) VS Spanish residents abroad (controp group)

