library(dplyr)
# Résumé des votes par groupe et parti
votes_par_parti <- donnees_tout %>%
  group_by(lieu, annee_fichier, etranger, parti) %>%
  summarise(votos = sum(votos), .groups = "drop")
# Total des votes par groupe (lieu, année, etranger)
totaux <- votes_par_parti %>%
  group_by(lieu, annee_fichier, etranger) %>%
  summarise(total_votes = sum(votos), .groups = "drop")

df_jointure <- left_join(totaux, votes_par_parti, by = c("lieu", "etranger", "annee_fichier"))
head(df_jointure)
resultats <- df_jointure %>%
  group_by(lieu, annee_fichier, etranger) %>%
  summarise(
    total_votes = unique(total_votes),
    pct_conservateur = sum(votos[parti == "Parti conservateur"], na.rm = TRUE) / total_votes * 100,
    pct_socialiste = sum(votos[parti == "Parti socialiste"], na.rm = TRUE) / total_votes * 100,
    .groups = "drop"
  )
resultats <- resultats %>%
  mutate(ratio_conservateur_socialiste = pct_conservateur / pct_socialiste)
install.packages("Synth")
library(Synth)

