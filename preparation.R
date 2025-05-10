rm(list=ls())  # Effacer les donnees en memoire
graphics.off() # Fermer les graphiques

library(readxl)
library(tidyr) # -> contient plusieurs librairies utiles pour l'analyse de donnees
library(stringr)
library(purrr)

 #### LECTURE ET CHARGEMENT DES DONNEES ####

columns <- c(
              "Tipo convocatoria", # ? 
              "Id convocatoria", # date de l election sous la forme "aaaamm" 
              "ccaa",
              "prv", # numero identifiant la province (?)  
              "circunscripcion", 
              "municipio", 
              "distrito", 
              "candidatura", # parti politique
              "votos", 
              "votos validos", # pourcentage obtenu
              "votos censo",
              "votos candidaturas", 
              "tipo de representante", 
              "representantes" 
              )


fichiers <- list.files("Donnees", pattern = "\\.xlsx$", full.names = TRUE) # Liste tous les fichiers .xlsx dans le dossier "Donnees"

donnees_tout <- map_dfr(fichiers, function(f) {
  df <- read_excel(f)
  df <- dplyr::select(df, all_of(columns))
  
  # conversion des types des colonnes (les erreurs deviennent NA)
  df$votos <- suppressWarnings(as.numeric(df$votos))  
  
  # Ajouter une colonne "fichier" avec juste le nom du fichier
  nom_fichier <- tools::file_path_sans_ext(basename(f))
  
  # Extraire l'année (premier nombre de 4 chiffres trouvé dans le nom)
  df$annee_fichier <- as.numeric(str_extract(nom_fichier, "\\d{4}")) #nouvelle colonne 
  
  # Déterminer si le fichier a été obtenu pour des votants à l'étranger ou non en extrayant "- etr"
  df$etranger <- ifelse(str_detect(nom_fichier, "- etr"), "oui", "non") #nouvelle colonne
  
  # Extraire le lieu
  reste_nom <- str_remove(nom_fichier, "^\\d{4}\\s*") # enlever la date
  lieu <- ifelse( 
    str_detect(reste_nom, "- etr"),
    str_trim(str_remove(reste_nom, "- etr.*")),  # enlever "- etr" et tout ce qui suit
    str_trim(reste_nom)  # sinon prendre tout le reste
  )
  
  df$lieu <- str_trim(lieu)  # assure qu'il n'y a pas d'espace avant et après le nom du lieu
  df
}, .id = "source")

  
  
  
          ### TESTS SUR LES DONNEES ###

length(unique(donnees_tout$lieu)) # doit renvoyer 52, pour 52 provinces
length(unique(donnees_tout$prv)) # idem

# Exploration des données

etranger_counts <- donnees_tout %>% count(etranger)
print(etranger_counts) # doit renvoyer le même nombre pour "oui" et "non"

#xtabs(~ lieu + etranger + annee_fichier, data = donnees_tout)
#xtabs(~ prv + etranger + annee_fichier, data = donnees_tout)

#xtabs(~ prv + etranger, data = donnees_tout)
#xtabs(~ prv + lieu, data = donnees_tout[donnees_tout["prv"]==35,])

#xtabs(~ candidatura + votos, data = donnees_tout)


# Creer deux dataframes distincts selon que les données ont etranger à "oui" ou "non"
data_oui <- donnees_tout %>% filter(etranger == "oui")
#data_oui = data_oui[c('annee_fichier', 'prv', 'lieu', 'candidatura')]

data_non <- donnees_tout %>% filter(etranger == "non")
#data_non = data_non[c('annee_fichier', 'prv', 'lieu', 'candidatura')]


# check qu'il y'a bien 350 deputes elus par an 
total_deputes <- donnees_tout %>%  #ou data_non ?
  group_by(annee_fichier) %>% 
  summarise(total_deputes = sum(representantes))
print(total_deputes)

### Il n'y en a que 346 en 2004 !!!!!!! ####


# Calculer le nombre total de députés élus par année et par parti
# total_deputes <- donnees_tout %>%  #ou data_non ?
  # group_by(annee_fichier, candidatura) %>% 
  # summarise(total_deputes = sum(representantes))

### IDENTIFER LES PARTIS QUI TOMBENT DANS LE SPECTRE CONSERVATEUR / SOCIALISTE

donnees_tout <- donnees_tout %>%
  mutate(
    parti = case_when(
      str_detect(candidatura, "SOCIALIST") |
      str_detect(candidatura, "PER CATALUNYA VERDS-ESQUERRA UNIDA") ~ "Parti socialiste",
      
      str_detect(candidatura, "POPULAR")|
      str_detect(candidatura, "PP") ~ "Parti conservateur",
      
      TRUE ~ NA_character_ 
    )
  )


total_deputes_conservateurs_socialistes <- donnees_tout %>%  #ou data_non ?
  group_by(annee_fichier, parti) %>% 
  summarise(total_deputes_conservateurs_socialistes = sum(representantes))



################### A FAIRE / VERIFIER ###############################


# soustraire le nombre de votes de l'etranger au total pour avoir ceux pas à l'etranger pour chaque province ?
# election day voters (treatment group) VS Spanish residents abroad (controp group)
# recreer figure 2 #