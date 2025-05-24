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
              "ccaa", # numero identifiant la province 1/2
              "prv", # numero identifiant la province 2/2 
              "circunscripcion", # NA
              "municipio", # NA
              "distrito", # NA
              "candidatura", # parti politique
              "votos", # nombre de votes
              "votos validos", # pourcentage obtenu
              "votos censo",
              "votos candidaturas", 
              "tipo de representante", # = depute
              "representantes" # nombre de députés élus
              )

fichiers <- list.files("Donnees", pattern = "\\.xlsx$", full.names = TRUE) # Liste tous les fichiers .xlsx dans le dossier "Donnees"

donnees_tout <- map_dfr(fichiers, function(f) {
  df <- read_excel(f)
  df <- dplyr::select(df, all_of(columns))
  # Convertir la colonne "votos" en entiers
  
  #df$votos <- suppressWarnings(as.numeric(df$votos))  # conversion des types des colonnes (les erreurs deviennent NA)
  #df$votos <- as.integer(round(df$votos))
  df$votos <- as.numeric(str_remove_all(df$votos, "\\."))
  nom_fichier <- tools::file_path_sans_ext(basename(f)) # ajout d'une colonne "fichier" contenant le nom du fichier dont les lignes proviennent
  
  df$annee_fichier <- as.numeric(str_extract(nom_fichier, "\\d{4}")) # ajout d'une colonne contenant l'année, extraite du nom du fichier source (premier nombre de 4 chiffres trouvé dans le nom)
  
  df$etranger <- ifelse(str_detect(nom_fichier, "- etr"), "oui", "non") # ajout d'une colonne identifiant le type de votants, à l'étranger ou non, en extrayant "- etr" du nom du fichier
  
  # Ajout d'une colonne contenant le nom du lieu (province)
  reste_nom <- str_remove(nom_fichier, "^\\d{4}\\s*") # enlever la date
  lieu <- ifelse( 
    str_detect(reste_nom, "- etr"),
    str_trim(str_remove(reste_nom, "- etr.*")),  # enlever "- etr" et tout ce qui suit
    str_trim(reste_nom)  # sinon prendre tout le reste
  )
  
  df$lieu <- str_trim(lieu)  # assure qu'il n'y a pas d'espace avant et après le nom du lieu
  df
}, .id = "source")
#### 
donnees_tout <- donnees_tout %>%
  mutate(
    votos = as.numeric(str_remove_all(votos, "\\."))
  )   # Suppression des points dans les valeurs du nombre de votes
  

  
          ### TESTS SUR LES DONNEES ###

length(unique(donnees_tout$lieu)) # doit renvoyer 52, pour 52 provinces
length(unique(donnees_tout$prv)) # idem
unique(donnees_tout[c('lieu', 'prv')]) # doit renvoyer 52 lignes

etranger_counts <- donnees_tout %>% count(etranger)
print(etranger_counts) # doit renvoyer le même nombre pour "oui" et "non"
 # autres explorations
xtabs(~ lieu + etranger + annee_fichier, data = donnees_tout)
xtabs(~ prv + etranger + annee_fichier, data = donnees_tout)

xtabs(~ prv + etranger, data = donnees_tout)

# ...



# check qu'il y'a bien 350 deputes elus par an 
total_deputes <- donnees_tout %>%  #ou data_non ?
  group_by(annee_fichier) %>% 
  summarise(total_deputes = sum(representantes))
print(total_deputes)

### Il n'y en a que 346 en 2004 !!!!!!! ####




### IDENTIFICATION DES PARTIS QUI TOMBENT DANS LE SPECTRE CONSERVATEUR vs SOCIALISTE

donnees_tout <- donnees_tout %>%
  mutate(
    parti = case_when(
      str_detect(candidatura, "SOCIALIST") & !str_detect(candidatura, "NUEVO PARTIDO") & !str_detect(candidatura, "OCTUBRE") & !str_detect(candidatura, "UN NOU PARTIT") & !str_detect(candidatura, "INDEPENDIENTES") & !str_detect(candidatura, "IND.") & !str_detect(candidatura, "TRABAJADORES") & !str_detect(candidatura, "TRABALLADORES") & !str_detect(candidatura, "INTERNACIONAL")
      # str_detect(candidatura, "PER CATALUNYA VERDS-ESQUERRA UNIDA") 
      #str_detect(candidatura, "^PARTIDO SOCIALISTA OBRERO ESPAÃ‘OL$") 
      #str_detect(candidatura, "PARTIDO SOCIALISTA") 
      ~ "Parti socialiste",
      
      str_detect(candidatura, "POPULAR")|
      str_detect(candidatura, "PP") 
      #str_detect(candidatura, "^PARTIDO POPULAR$") 
      #str_detect(candidatura, "PARTIDO POPULAR")
      ~ "Parti conservateur",
      
      TRUE ~ "autres partis" 
    )
  )
# Afficher tous les partis qui entrent dans la catégorie "Parti socialiste" (#par année)
donnees_tout %>%
  filter(parti == "Parti socialiste"
         #,annee_fichier==1989
         ) %>%
  distinct(candidatura, .keep_all = FALSE) %>%
  print(n=26)

# Afficher tous les partis qui entrent dans la catégorie "Parti conservateur" (#par année)
donnees_tout %>%
  filter(parti == "Parti conservateur"
         #, annee_fichier == 1989
         ) %>%
  distinct(candidatura, .keep_all = FALSE) %>%
  print(n=14)

# Creer deux dataframes distincts selon que les données ont etranger à "oui" ou "non"
data_oui <- donnees_tout %>% filter(etranger == "oui")
data_oui = data_oui[c('annee_fichier', 'prv', 'lieu', 'candidatura', 'votos', 'parti')]

data_non <- donnees_tout %>% filter(etranger == "non")
data_non = data_non[c('annee_fichier', 'prv', 'lieu', 'candidatura', 'votos', 'parti')]


 #### si je decide de supprimer les valeurs des votes de xxx_oui dans xxx_non 


# Effectuer une jointure entre data_non et data_oui
data_non <- data_non %>%
  left_join(data_oui, by = c("annee_fichier", "prv", "lieu", "candidatura", "parti"), suffix = c("", "_oui"))

# Soustraire les valeurs de "votos" de data_oui de celles de data_non
data_non <- data_non %>%
  mutate(votos = votos - votos_oui) %>%
  select(-votos_oui) # Supprimer la colonne temporaire "votos_oui"
