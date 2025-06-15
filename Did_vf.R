###### PREPARATION ET EXPLORATION ######
# Nettoyage environnement
rm(list = ls())
graphics.off()

# Chargement des librairies
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(fixest)

# Colonnes utiles
colonnes <- c(
  "Tipo convocatoria", "Id convocatoria", "ccaa", "prv",
  "circunscripcion", "municipio", "distrito", "candidatura",
  "votos", "votos validos", "votos censo", "votos candidaturas",
  "tipo de representante", "representantes"
)

# Lecture et fusion des fichiers
fichiers <- list.files("Donnees", pattern = "\\.xlsx$", full.names = TRUE)
donnees <- map_dfr(fichiers, function(fichier) {
  df <- read_excel(fichier)
  df <- select(df, all_of(colonnes))
  df$votos <- as.numeric(str_remove_all(df$votos, "\\."))
  nom <- tools::file_path_sans_ext(basename(fichier))
  df$annee <- as.numeric(str_extract(nom, "\\d{4}"))
  df$etranger <- ifelse(str_detect(nom, "- etr"), 1, 0)
  df$lieu <- str_trim(str_remove(nom, "^\\d{4}\\s*|- etr.*"))
  return(df)
}, .id = "source")

# Aperçus utiles
cat("Aperçu structurel des données :\n")
glimpse(donnees)

cat("\nRésumé statistique :\n")
summary(donnees)

cat("\nNombre de provinces uniques :\n")
print(n_distinct(donnees$lieu))

cat("\nExemples de valeurs pour 'candidatura' :\n")
print(unique(donnees$candidatura)[1:20])

###### CONSTRUCTION DU RATIO DE VOTE ET VARIABLES DID ######
# Identification partis principaux
donnees <- donnees %>%
  mutate(parti = case_when(
    str_detect(candidatura, "SOCIALIST") ~ "socialiste",
    str_detect(candidatura, "PP|POPULAR") ~ "conservateur",
    TRUE ~ "autres"
  ))

# Agrégation des votes par groupe / lieu / annee : variable dépendante (vote ratio PP/PSOE)
votes_agg <- donnees %>%
  filter(parti %in% c("socialiste", "conservateur")) %>%
  group_by(annee, lieu, etranger, parti) %>%
  summarise(votes = sum(votos, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = parti, values_from = votes, values_fill = 0) %>%
  mutate(ratio = conservateur / socialiste)


cat("\nAperçu des ratios PP/PSOE :\n")
print(head(votes_agg, 10))

# Vérification cohérence nombre de députés
deputes_par_annee <- donnees %>%
  group_by(annee) %>%
  summarise(deputes_total = sum(representantes, na.rm = TRUE))

cat("\nNombre de députés par année :\n")
print(deputes_par_annee)

# Graphe de vérification visuelle (tendance pré-traitement) -> à améliorer
ggplot(votes_agg, aes(x = annee, y = ratio, color = as.factor(etranger))) +
  geom_line(aes(group = lieu), alpha = 0.5) +
  labs(title = "Ratio PP / PSOE par année et par groupe",
       x = "Année", y = "Ratio conservateurs / socialistes", color = "Expatrié") +
  theme_minimal()

# Graphe ratio conservateurs/socialistes – agrégé par groupe et année
votes_agg %>%
  group_by(annee, etranger) %>%
  summarise(ratio_moyen = mean(ratio, na.rm = TRUE), .groups = 'drop') %>%
  mutate(groupe = ifelse(etranger == 1, "Étrangers (contrôle)", "Résidents (traitement)")) %>%
  ggplot(aes(x = annee, y = ratio_moyen, color = groupe, linetype = groupe)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "black") +
  labs(title = "Évolution du ratio conservateurs/socialistes (par groupe)",
       subtitle = "Réplique de la Figure 2 de Montalvo (2011)",
       x = "Année électorale", y = "Ratio PP / PSOE",
       color = "Groupe", linetype = "Groupe") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Graphe boxplot des ratios par année et par groupe
votes_agg %>%
  mutate(groupe = ifelse(etranger == 1, "Étrangers", "Résidents")) %>%
  ggplot(aes(x = factor(annee), y = ratio, fill = groupe)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Distribution du ratio PP/PSOE par année et groupe",
       x = "Année", y = "Ratio conservateur/socialiste") +
  theme_minimal()

# Variables DiD
# construction de la variable de traitement
# selon Montalvo (2011) :
# - groupe traité : résidents en Espagne (etranger == 0)
# - groupe contrôle : espagnols à l'étranger (etranger == 1)
# - année de traitement : 2004
# donc l'interaction "traitement" == résidents & "post" == 2004

votes_did <- votes_agg %>%
  mutate(
    treated = ifelse(etranger == 0, 1, 0), # 1 si groupe traité
    post = ifelse(annee == 2004, 1, 0),    # 1 si après attentat (élection 2004)
    did = treated * post,                  # interaction : effet du traitement
    placebo = ifelse(annee == 2000, 1, 0),
    did_placebo = treated * placebo,
    log_ratio = log(ratio)
  )

###### VERIFICATION DES HYPOTHÈSES DU DID ######

# Tendance parallèle
votes_did %>%
  filter(annee < 2004) %>%
  group_by(annee, etranger) %>%
  summarise(ratio_moy = mean(ratio, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = annee, y = ratio_moy, color = as.factor(etranger))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "red"), labels = c("Résidents", "Étrangers")) +
  labs(title = "Test de tendance parallèle avant traitement",
       x = "Année", y = "Ratio PP/PSOE", color = "Groupe") +
  theme_minimal()

###### VERIFICATION DES HYPOTHESES ######

# hypothèse clé : tendance parallèle entre groupes avant 2004
# on peut la vérifier visuellement 
# analyse : les courbes devraient évoluer parallèlement avant 2004
# sinon, biais potentiel dans l'estimation DiD

ggplot(
  votes_did,
  aes(x = annee, y = ratio, group = as.factor(etranger), color = as.factor(etranger))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Ratio conservateurs/socialistes par groupe",
       y = "Ratio PP/PSOE", color = "Groupe") +
  scale_color_manual(values = c("blue", "red"), labels = c("Résidents", "Étrangers")) +
  theme_minimal()

###### ESTIMATION DU MODELE DID ######

## Modèle simple
# did est l'effet moyen du traitement (DiD) :il mesure l’impact de l’attentat 
# sur le vote des résidents en 2004, comparé à leur tendance préexistante et au groupe contrôle.

mod_simple <- lm(ratio ~ treated + post + did, data = votes_did)
summary(mod_simple)

## Effets fixes (province et année)
# contrôle pour les spécificités régionales et les chocs communs à une élection.
# interprétation : le coefficient sur "did" mesure l'effet causal du traitement
# c’est-à-dire : l'effet des attentats de Madrid sur le ratio PP/PSOE
# pour les électeurs résidant en Espagne (comparé à ceux à l'étranger)

mod_fe <- feols(ratio ~ did | lieu + annee, data = votes_did)
summary(mod_fe)

## Erreurs robustes (cluster sur les provinces)
#test robuste aux hétéroscédasticités/spécificités par province.

mod_fe_cluster <- feols(ratio ~ did | lieu + annee, cluster = ~lieu, data = votes_did)
summary(mod_fe_cluster)

###### TEST DE ROBUSTESSE PAR PLACEBO (ANNEE 2000) ######
# ce test vérifie que l'effet estimé en 2004 n’apparaît pas dans d'autres années non traitées.

# test avec robustesse des erreurs (clustering par lieu)
mod_placebo <- feols(ratio ~ did_placebo | lieu + annee, data = votes_did)
summary(mod_placebo)

# version placebo : appliquer un faux traitement en 2000 pour vérifier l'absence d’effet
# si le coefficient sur did_placebo est significatif => danger (fausse identification)
votes_did <- votes_did %>%
  mutate(
    placebo = ifelse(annee == 2000, 1, 0),
    did_placebo = placebo * treated
  )

did_placebo_model <- feols(ratio ~ did_placebo | lieu + annee, data = votes_did)
summary(did_placebo_model)

###### ESTIMATION EN LOG(RATIO) ######
# tester l'effet sur log(ratio) pour stabiliser la variance des résidus (meilleure normalité)
# tester l'effet sur les niveaux de vote (pas juste les ratios)
# possible en répétant l’analyse pour la variable `conservateur` seule

mod_log <- feols(log_ratio ~ did | lieu + annee, data = votes_did)
summary(mod_log)

###### VISUALISATIONS D’ACCOMPAGNEMENT ######

# Histogramme du ratio
hist(votes_did$ratio, breaks = 40, col = "skyblue", main = "Distribution du ratio PP/PSOE")

# Histogramme du log(ratio)
ggplot(votes_did, aes(x = log(ratio))) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution de log(ratio)", x = "log(ratio)", y = "n") +
  theme_minimal()

# Reproduction de la figure 2
ratio_figure2 <- votes_did %>%
  group_by(annee, etranger) %>%
  summarise(ratio_moy = mean(ratio, na.rm = TRUE), .groups = "drop") %>%
  mutate(Groupe = ifelse(etranger == 0, "Résidents", "Étrangers"))

ggplot(ratio_figure2, aes(x = annee, y = ratio_moy, color = Groupe, linetype = Groupe)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 2004, linetype = "dotted", color = "black") +
  labs(title = "Ratio PP/PSOE par groupe (réplication Figure 2)",
       y = "Ratio moyen conservateurs/socialistes") +
  theme_minimal()

###### RECAP ######
# TABLEAU RÉCAP DES RÉSULTATS 
etable(list(mod_fe, mod_fe_cluster, mod_log, mod_placebo),
       headers = c("FE", "FE Cluster", "Log FE", "Placebo"),
       tex = FALSE)

mod_simple_feols <- feols(ratio ~ treated + post + did, data = votes_did)

etable(list(mod_simple_feols, mod_fe, mod_fe_cluster, mod_log, mod_placebo),
       headers = c("OLS", "FE", "FE Cluster", "Log FE", "Placebo"),
       tex = FALSE)
