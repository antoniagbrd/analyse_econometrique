source(preparation.R)
#Obtenir statistiques descriptives

df <- donnees_tout%>%
  # Filtrer les données pour les partis d'intérêt
  filter(parti %in% c("Parti conservateur", "Parti socialiste")) %>%
  # Grouper par année et parti, puis calculer le nombre total de votes
  group_by(annee_fichier, parti, lieu, etranger) %>%
  summarise(total_votes = sum(votos, na.rm = TRUE), .groups = 'drop') %>%
  # Pivoter les données pour avoir les votes par parti en colonnes
  pivot_wider(names_from = parti, values_from = total_votes) %>%
  # Calculer le ratio
  mutate(`ratio` = `Parti conservateur` / `Parti socialiste`) %>%
  # Sélectionner les colonnes d'intérêt
  select(annee_fichier, lieu, etranger, ratio)
################ A FAIRE / A SAVOIR  ########################

    # rappel : control group = residents abroad (ie etranger = oui) vs treatment group = spanish residents (etranger = non
# OLS <- lm(ratio ~ lambda + G_g + DG_gt + nu_gt + u_it, data = df)
# transformer la colonne etranger pour avoir 1/0 à la place de oui/non
# creer / ajouter la colonne DG -> vaut un pour election de 2004 pour group des non etrangers 
# ajouter lambda et nu 
