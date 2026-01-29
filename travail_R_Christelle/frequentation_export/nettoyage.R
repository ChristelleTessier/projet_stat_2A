library(dplyr)
library(tidyverse)
library(writexl)
library(readxl)

setwd("travail_R")
rm(list = ls())

# Fonctions de vérification nbNA et format et repartition valeur numérique
##########################################
get_diagnostic_NA <- function(df) {
  diag <- df %>%
    summarise(across(everything(), list(
      NAreel  = function(x) sum(is.na(x)),
      NAtxt   = function(x) if(is.character(x)) sum(x == "NA", na.rm = TRUE) else 0,
      inconnu = function(x) if(is.character(x)) sum(x == "inconnu", na.rm = TRUE) else 0,
      n999num = function(x) if(is.numeric(x)) sum(x == -999, na.rm = TRUE) else 0,
      n999txt = function(x) if(is.character(x)) sum(x == "-999", na.rm = TRUE) else 0
    ))) %>%
    pivot_longer(
      everything(),
      names_to = c("colonne", ".value"), # On garde "colonne" ici pour la compatibilité
      names_pattern = "(.*)_(NAreel|NAtxt|inconnu|n999num|n999txt)"
    ) %>%
    mutate(Total_manquants = NAreel + NAtxt + inconnu + n999num + n999txt)

  return(diag)
}

verifier_difference_NA <- function(df_init, df_transfo) {
  # Création des diagnostics
  diag1 <- get_diagnostic_NA(df_init)
  diag2 <- get_diagnostic_NA(df_transfo)

  # Jointure des deux tableaux sur la colonne 'colonne'
  comparaison <- diag1 %>%
    select(colonne, Total_Avant = Total_manquants) %>%
    left_join(
      diag2 %>% select(colonne, Total_Apres = Total_manquants),
      by = "colonne"
    ) %>%
    mutate(Difference = Total_Apres - Total_Avant) %>%
    filter(Difference != 0)

  # Affichage
  if (nrow(comparaison) == 0) {
    return(NULL)
  } else {
    return(comparaison)
  }
}


get_formats <- function(df) {
  df %>%
    summarise(across(everything(), ~ class(.x)[1])) %>%
    # On pivote pour avoir une colonne "Variable" et une colonne "Format"
    pivot_longer(everything(), names_to = "Variable", values_to = "Format")
}

verifier_formats <- function(df_init, df_transfo) {
  f1 <- get_formats(df_init)
  f2 <- get_formats(df_transfo)

  comp <- f1 %>%
    rename(Avant = Format) %>%
    inner_join(f2 %>% rename(Apres = Format), by = "Variable") %>%
    filter(Avant != Apres)

  if (nrow(comp) == 0) {
    return(NULL)
    }
  else {
    return(comp) # Retourne un tableau clair : une ligne par variable modifiée
  }
}

get_repartition_num <- function(df, cols) {
  df %>%
    select(all_of(intersect(cols, names(.)))) %>%
    # On passe tout en format long pour traiter toutes les colonnes d'un coup
    pivot_longer(everything(), names_to = "Variable", values_to = "Valeur") %>%
    mutate(Valeur = as.character(Valeur)) %>%
    # Filtre de tes valeurs à ignorer
    filter(
      !is.na(Valeur),
      Valeur != "-999",
      Valeur != "NA"
    ) %>%
    group_by(Variable, Valeur) %>%
    summarise(Freq = n(), .groups = "drop")
}

comparer_repartitions_num <- function(df_init, df_transfo, cols) {

  # Fonction interne de calcul (version optimisée)
  calculer_ecart <- function(colonne) {
    # 1. Récupération des répartitions
    rep_init    <- get_repartition_num(df_init, colonne)
    rep_transfo <- get_repartition_num(df_transfo, colonne)

    # 2. Jointure et calcul du delta
    bilan <- full_join(
      rep_init,
      rep_transfo,
      by = "Valeur",
      suffix = c("_Initial", "_Transfo")
    ) %>%
      mutate(
        across(starts_with("Freq"), ~replace_na(., 0)),
        Difference = Freq_Transfo - Freq_Initial,
        Variable = colonne # On marque la provenance
      ) %>%
      filter(Difference != 0)

    return(bilan)
  }

  # On applique la fonction interne à chaque élément de 'cols'
  resultats <- map_dfr(cols, calculer_ecart)

  # Gestion des messages de sortie
  if (nrow(resultats) == 0) {
    return(NULL)
  }
  else {
    return(resultats)
  }
}

verifications <- function(df_init, df_transfo, cols){
  compNA <- verifier_difference_NA(df_init, df_transfo)
  compFormat <- verifier_formats(df_init, df_transfo)
  compRepartition <- comparer_repartitions_num(df_init, df_transfo, cols)
}

verifications <- function(df_init, df_transfo, cols) {

  # 1. Calcul des différents diagnostics
  # On utilise 'suppressMessages' si on veut gérer l'affichage nous-mêmes
  resNA         <- verifier_difference_NA(df_init, df_transfo)
  resFormat     <- verifier_formats(df_init, df_transfo)
  resRepartition <- comparer_repartitions_num(df_init, df_transfo, cols)

  # 2. Construction de la liste de sortie
  bilan <- list(
    NA_Differences = resNA,
    Format_Changes = resFormat,
    Distribution_Gaps = resRepartition
  )

  # 3. Affichage personnalisé dans la console
  cat("\n--- BILAN DES VÉRIFICATIONS ---\n")

  if (!is.null(resNA)) {
    cat("\n❌ Écarts de NA trouvés :\n")
    print(resNA)
  }

  if (!is.null(resFormat)) {
    cat("\n❌ Changements de format trouvés :\n")
    print(resFormat)
  }

  if (!is.null(resRepartition)) {
    cat("\n❌ Écarts de répartition trouvés :\n")
    print(resRepartition)
  }

  if (is.null(resNA) && is.null(resFormat) && is.null(resRepartition)) {
    cat("\n✅ Tout est conforme !\n")
  }

  # On retourne la liste de manière invisible pour pouvoir l'assigner si besoin
  return(invisible(bilan))
}

# récupération des données
#######################################
data <- read_table("donnees/FREQUENTATION_export.txt")

# Mise en correspondance nom colonne avec frequentation_export_2005_2014
#######################################
noms_fichier1 <- c(
  "AMP", "numSortie", "nbMoyen", "periodEchant", "annee", "mois", "jour", 
  "saison", "typeJ", "heure", "meteo", "nebulosite", "directionVent", 
  "forceVent", "etatMer", "lune", "zone", "zonagePAMPA", "groupe", 
  "typeBat", "tailleBat", "immat", "nbPers", "nbLigne", "mouillage", 
  "natureFond", "latitude", "longitude", "act1", "categAct1", "act2", 
  "categAct2", "questInfo", "sens1", "sens2"
)

colnames(data) <- noms_fichier1



cols_numeriques <- c("annee","mois","jour","nebulosite", "forceVent", "etatMer", "nbPers", "nbLigne")


# transformation des NA txt en NA
#######################################
data_transfo <- data %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

verifications(data, data_transfo,cols_numeriques)

# transformation des "inconnu" en NA
#######################################
data_transfo <- data_transfo %>%
  mutate(across(where(is.character), ~ na_if(.x, "inconnu")))

verifications(data, data_transfo,cols_numeriques)

# Néttoyage des valeurs "-999"
#######################################

# Repérage des colonnes contenant -999
cols_avec_erreur <- data_transfo %>%
  select(where(~ any(.x == -999 | .x == "-999", na.rm = TRUE))) %>%
  colnames()

# Transformation de -999 en NA
data_transfo <- data_transfo %>%
  mutate(across(all_of(cols_avec_erreur), ~ {
    # On remplace -999 par NA
    x <- ifelse(.x == "-999" | .x == -999, NA, .x)
  }))

verifications(data, data_transfo,cols_numeriques)

# Transformation des formats
#################################

data_transfo <- data_transfo %>%
  mutate(
    across(everything(), as.character),
    # On transforme ensuite les colonnes cibles en numérique
    across(all_of(intersect(cols_numeriques, names(.))), as.numeric)
  )

verifications(data, data_transfo,cols_numeriques)


# Enregistrement
#######################################
write_xlsx(data_transfo, "donnees/data_PMCB.xlsx")

data_bis <- read_excel("donnees/data_PMCB.xlsx",
                   sheet = 1)

verifications(data_bis, data_transfo,cols_numeriques)

