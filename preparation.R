rm(list=ls())  # Effacer les donnees en memoire
graphics.off() # Fermer les graphiques

library(readxl)
library(tidyr) # -> contient plusieurs librairies utiles pour l'analyse de donnees
library(stringr)
library(purrr)
# Lecture des données de 1989, snas Melilla et sans Ceuta

columns <- c(
              "Tipo convocatoria", # ? 
              # "ano", # année de l'election (colonne ajoutee, obtenue à partir de Id convocatoria) 
              "Id convocatoria", # date de lelection sous la forme "aaaamm" 
              "ccaa", #? 
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
              # "etranger" # = "oui" ou "non" selon si le votant est à l'étranger ou non (colonne ajoutee)
              )

            # test avec tous fichiers de 1989 

# Liste tous les fichiers .xlsx dans le dossier "Donnees"
fichiers <- list.files("Donnees", pattern = "\\.xlsx$", full.names = TRUE)

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

# tests sur les donnees
length(unique(donnees_tout$lieu)) # doit renvoyer 52, pour 52 provinces


# renomme les donnees
dplyr::rename(donnees_tout, 
              "annee" = ano, 
              "latitude" = lat
              )

# Exploration des données
#str(donnees)
#summary(donnees)

# soustraire le nombre de votes de l'etranger au total pour avoir ceux pas à l'etranger pour chaque province ? 