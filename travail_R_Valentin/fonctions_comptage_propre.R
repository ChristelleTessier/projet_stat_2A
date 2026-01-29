##### Présentation #####

#### Le programme ci-après permet un filtrage et un comptage dynamique 
#### d'une base de données.
#### Il est composé de trois parties : un script et deux fonctions (filtre et comptage)

##### Le script convertit les types de données ne correspondant pas au traitement :
##### les variables de comptages sous format charactères sont convertit au format numérique
##### une variable est construite pour étudier les heures de fréquentations à partir de l'heure
##### exacte : une observation à 10 h 24 et une observation à 10 h 48 auront une heure entière 
##### égale à 10 h

##### La fonction filtre() permet de sélectionner les données de manière dynamique :
##### L'avantage de cette fonction est d'échaper à l'écriture filter() du package dplyr
##### Les arguments ont une valeur par défaut (NULL) qui permet d'afficher l'ensemble
##### de la table lorsque aucun argument n'est spécifié.
##### Pour filtrer des données, seuls les arguments à filtrer (les variables de la base)
##### doivent être spécifié.

##### La fonction comptage() utilise la fonction filtre (avec les mêmes arguments) ET
##### un argument de regroupement : il contient le vecteur des variables sur lesquels
#####                                                les statistiques sont réalisées



##### Chargements des packages #####

library(readxl)
library(dplyr)
library(purrr)
library(sf)
library(mapview)
library(dplyr)

##### Chargements des bases de données #####

FREQUENTATION_2005_2013 <- read_excel("~/ENSAI-Scolarité/projet statistique/donnees/FREQUENTATION_export_2005-2013.xlsx")

library(readxl)
library(dplyr)

# 1. Définir explicitement les types pour chaque colonne
# 'text' pour les caractères, 'numeric' pour les chiffres, 'skip' pour ignorer
types_colonnes <- c(
  "text",    # AMP
  "text",    # numSortie
  "numeric", # nbMoyen (on le force en numérique ici)
  "text",    # periodEchant
  "text", # annee
  "text", # mois
  "text", # jour
  "text",    # saison
  "text",    # typeJ
  "text",    # heure
  "text",    # meteo
  "text", # nebulosite
  "text",    # directionVent
  "numeric", # forceVent
  "text", # etatMer (souvent numérique ou texte, selon data_frequentation)
  "text",    # lune
  "text",    # zone
  "text",    # zonagePAMPA
  "text",    # groupe
  "text",    # typeBat
  "text",    # tailleBat
  "text",    # immat
  "numeric", # nbPers
  "numeric", # nbLigne
  "text",    # mouillage
  "text",    # natureFond
  "numeric", # latitude (indispensable en numérique)
  "numeric", # longitude (indispensable en numérique)
  "text",    # act1
  "text",    # categAct1
  "text",    # act2
  "text",    # categAct2
  "text",    # questInfo
  "text",    # sens1
  "text",    # sens2
  "text"     # doubleSortie
)

# 2. Charger les données
data_propre <- read_excel("~/ENSAI-Scolarité/projet statistique/donnees/data_2005_2014.xlsx", col_types = types_colonnes)

# 3. Petit ajustement post-chargement si nécessaire
# Parfois, 'heure' (10h30) doit rester en texte avant d'être traité manuellement





ref_geo <- st_read("~/ENSAI-Scolarité/projet statistique/referentiels cartographiques/format gpkg/RefSpatial_08012014.gpkg")

#### Transformations des types de variables ####


str(FREQUENTATION_2005_2013)

# types de variables à changer
## annee : num -> chr
## mois : num -> chr
## jour : num -> chr
## nebulosite : num -> chr
## forceVent : num -> chr

## nbPers : chr -> num
## nbLigne : chr -> num

## latitude : chr -> coord geo ?
## longitude : chr -> coord geo ?

FREQUENTATION_2005_2013$annee <- as.character(FREQUENTATION_2005_2013$annee)
FREQUENTATION_2005_2013$mois <- as.character(FREQUENTATION_2005_2013$mois)
FREQUENTATION_2005_2013$jour <- as.character(FREQUENTATION_2005_2013$jour)
FREQUENTATION_2005_2013$nebulosite <- as.character(FREQUENTATION_2005_2013$nebulosite)
FREQUENTATION_2005_2013$forceVent <- as.character(FREQUENTATION_2005_2013$forceVent)

FREQUENTATION_2005_2013$nbPers <- as.numeric(FREQUENTATION_2005_2013$nbPers)
FREQUENTATION_2005_2013$nbLigne <- as.numeric(FREQUENTATION_2005_2013$nbLigne)


# récupération des heures

FREQUENTATION_2005_2013$heure_entiere <- substr(FREQUENTATION_2005_2013$heure, 1, 2)


##### Filtrages des données #####

#attach(FREQUENTATION_2005_2013)

filtre <- function(data = data_propre,
                   AMP = NULL, periodEchant = NULL, annee = NULL, mois = NULL,
                   jour = NULL, saison = NULL, typeJ = NULL,
                   heure_entiere = NULL, meteo = NULL,
                   nebulosite = NULL, directionVent = NULL,
                   forceVent = NULL, etatMer = NULL, lune = NULL,
                   zone = NULL, zonagePAMPA = NULL, groupe = NULL,
                   typeBat = NULL, tailleBat = NULL, mouillage = NULL,
                   natureFond = NULL, categAct1 = NULL, categAct2 = NULL) {
  
  # 1. Optionnel mais conseillé : permettre de passer n'importe quel dataframe
  donnees_filtrees <- data
  
  # 2. Logique de filtrage (Utilisation de purrr::reduce et if/else pour plus de clarté)
  
  # Créer une liste de conditions de filtrage
  conditions <- list()
  
  # Fonction utilitaire pour ajouter une condition si l'argument n'est pas NULL
  ajouter_condition <- function(colonne, valeur) {
    # Utilisation de {{}} pour la quasiquotation (si vous utilisez dplyr >= 1.0.0)
    if (!is.null(valeur)) {
      return(rlang::quo(!!rlang::sym(colonne) %in% !!valeur))
    } else {
      return(NULL) # Pas de condition si l'argument est NULL
    }
  }
  
  conditions <- list(
    ajouter_condition("AMP", AMP),
    ajouter_condition("periodEchant", periodEchant),
    ajouter_condition("annee", annee),
    ajouter_condition("mois", mois),
    ajouter_condition("jour", jour),
    # ... ajoutez toutes les autres variables ici ...
    ajouter_condition("saison", saison),
    ajouter_condition("typeJ", typeJ),
    ajouter_condition("heure_entiere", heure_entiere),
    ajouter_condition("meteo", meteo),
    ajouter_condition("nebulosite", nebulosite),
    ajouter_condition("directionVent", directionVent),
    ajouter_condition("forceVent", forceVent),
    ajouter_condition("etatMer", etatMer),
    ajouter_condition("lune", lune),
    ajouter_condition("zone", zone),
    ajouter_condition("zonagePAMPA", zonagePAMPA),
    ajouter_condition("groupe", groupe),
    ajouter_condition("typeBat", typeBat),
    ajouter_condition("tailleBat", tailleBat),
    ajouter_condition("mouillage", mouillage),
    ajouter_condition("natureFond", natureFond),
    ajouter_condition("categAct1", categAct1),
    ajouter_condition("categAct2", categAct2)
  )
  
  # Enlever les NULLs (les arguments non utilisés par l'utilisateur)
  conditions <- purrr::compact(conditions)
  
  # Appliquer toutes les conditions avec 'AND' (avec purrr::reduce)
  if (length(conditions) > 0) {
    condition_finale <- purrr::reduce(conditions, function(x, y) rlang::quo(!!x & !!y))
    donnees_filtrees <- donnees_filtrees %>% 
      dplyr::filter(!!condition_finale)
  }
  
  return(donnees_filtrees)
}


##### Fonctions de comptage #####

comptage <- function(data = data_propre,
                     AMP = NULL, periodEchant = NULL, annee = NULL, mois = NULL,
                     jour = NULL, saison = NULL, typeJ = NULL,
                     heure_entiere = NULL, meteo = NULL,
                     nebulosite = NULL, directionVent = NULL,
                     forceVent = NULL, etatMer = NULL, lune = NULL,
                     zone = NULL, zonagePAMPA = NULL, groupe = NULL,
                     typeBat = NULL, tailleBat = NULL, mouillage = NULL,
                     natureFond = NULL, categAct1 = NULL, categAct2 = NULL,
                     groupement = NULL) # L'argument groupement contient le vecteur 
  # des variables sur lesquels les statistiques 
  # sont réalisées
{
  
  # Les données sont d'abord filtrées selon les arguments (variables) spécifiés
  
  donnees_filtrees <- filtre(data, AMP, periodEchant, annee, mois, jour, saison, typeJ, heure_entiere,
                             meteo, nebulosite, directionVent, forceVent, etatMer, lune,
                             zone, zonagePAMPA, groupe, typeBat, tailleBat, mouillage,
                             natureFond, categAct1, categAct2)
  
  # Le traitement diffère selon la nécessité ou non des statistiques par variable   
  if (is.null(groupement)) # Les statistiques sont globales sur les données filtrées
  {
    donnees_agregees <- donnees_filtrees %>% summarise(nb_sorties = n(),
                                                       nbPers = sum(nbPers, na.rm = TRUE),
                                                       nbLigne = sum(nbLigne, na.rm = TRUE))
  }
  else # Les statistiques sont réalisées pour chaque variable de regroupement (encapsulées)
  {
    donnees_agregees <- donnees_filtrees %>% group_by(across(any_of(groupement))) %>% summarise(nb_sorties = n(),
                                                                                                nbPers = sum(nbPers, na.rm = TRUE),
                                                                                                nbLigne = sum(nbLigne, na.rm = TRUE))
  }
  
  return(donnees_agregees)
}


#### ajout des métriques à la fonction de comptage ####

comptage_metrique <- function(metrique = NULL, 
                              data = data_propre,
                              ref_geographie = ref_geo,
                              groupement = NULL,
                              indicateur = NULL,
                              AMP = NULL,  periodEchant = NULL, annee = NULL, mois = NULL,
                              jour = NULL, saison = NULL, typeJ = NULL,
                              heure_entiere = NULL, meteo = NULL,
                              nebulosite = NULL, directionVent = NULL,
                              forceVent = NULL, etatMer = NULL, lune = NULL,
                              zone = NULL, zonagePAMPA = NULL, groupe = NULL,
                              typeBat = NULL, tailleBat = NULL, mouillage = NULL,
                              natureFond = NULL, categAct1 = NULL, categAct2 = NULL) {
  
  # --- 1. Validation de la métrique ---
  metriques_valides <- c("pression", "impact", "opinion", "pratique", "socio-economique")
  if (is.null(metrique) || metrique == "" || !(metrique %in% metriques_valides)) {
    stop("Préciser une métrique valide : pression, impact, opinion, pratique, socio-economique")
  }
  
  # --- 2. Filtrage des données ---
  donnees_filtrees <- filtre(data, AMP, periodEchant, annee, mois, jour, saison, typeJ, 
                             heure_entiere, meteo, nebulosite, directionVent, 
                             forceVent, etatMer, lune, zone, zonagePAMPA, 
                             groupe, typeBat, tailleBat, mouillage,
                             natureFond, categAct1, categAct2)
  
  # --- 3. Normalisation des clés ---
  cles_geo_possibles <- c("AMP", "zone", "groupe", "zonagePAMPA")
  donnees_filtrees <- donnees_filtrees %>%
    mutate(across(any_of(cles_geo_possibles), ~ toupper(trimws(as.character(.x)))))
  
  # --- 4. Calculs des agrégats ---
  res <- donnees_filtrees %>% group_by(across(any_of(groupement)))
  
  if (metrique == "pression") {
    donnees_agregees <- res %>% 
      summarise(nbPers = sum(nbPers, na.rm = TRUE),
                nbBat = n(),
                nbLigne = sum(nbLigne, na.rm = TRUE), .groups = "drop")
  } else if (metrique == "impact") {
    donnees_agregees <- res %>% 
      summarise(nbBat = n(),
                nbBeaches = sum(mouillage == "BC", na.rm = TRUE),
                nbAncres = sum(mouillage == "AC", na.rm = TRUE), .groups = "drop") %>% 
      mutate(txBeaches = (nbBeaches / nbBat) * 100,
             txAncres = (nbAncres / nbBat) * 100)
  } else {
    donnees_agregees <- res %>% summarise(nbObservations = n(), .groups = "drop")
  }
  
  # --- 5. Cartographie et Évolution Temporelle ---
  if (!is.null(indicateur) && (indicateur %in% names(donnees_agregees))) {
    
    map_ref_cols <- c("zone" = "CODE_site", "AMP" = "CAS_ETUDE_", 
                      "groupe" = "GROUPE", "zonagePAMPA" = "STATUT_PAM")
    
    cle_geo_data <- intersect(groupement, names(map_ref_cols))[1]
    
    if (!is.na(cle_geo_data)) {
      col_ref_geo <- map_ref_cols[cle_geo_data]
      
      # --- RÉPARATION GÉOMÉTRIE (Correction erreur wk_handle / S2) ---
      original_s2 <- sf_use_s2()
      sf_use_s2(FALSE) # Désactivation de S2 pour gérer les erreurs de sommets
      
      ref_geo_temp <- st_as_sf(ref_geographie) %>% 
        st_make_valid() %>% 
        filter(!st_is_empty(.))
      
      if (!(col_ref_geo %in% names(ref_geo_temp))) {
        sf_use_s2(original_s2)
        stop(paste("Colonne", col_ref_geo, "absente de ref_geo. Vérifiez l'orthographe."))
      }
      
      # Dissolution spatiale propre avec identifiant temporaire
      ref_geo_prepared <- ref_geo_temp %>%
        mutate(join_id = toupper(trimws(as.character(.data[[col_ref_geo]])))) %>%
        select(join_id) %>% 
        group_by(join_id) %>%
        summarise(.groups = "drop") %>%
        st_make_valid()
      
      # --- LOGIQUE D'AFFICHAGE ---
      dims_temps <- intersect(groupement, c("periodEchant", "annee", "mois", "jour"))
      
      if (length(dims_temps) > 0) {
        dim_prio <- dims_temps[1]
        donnees_evol <- donnees_agregees %>%
          arrange(across(any_of(c("annee", "mois", "jour")))) %>%
          group_by(.data[[cle_geo_data]]) %>%
          summarise(
            val_debut = first(.data[[indicateur]]),
            val_fin = last(.data[[indicateur]]),
            label_debut = first(.data[[dim_prio]]),
            label_fin = last(.data[[dim_prio]]),
            evolution_pourcent = round(((val_fin - val_debut) / ifelse(val_debut == 0, 1, val_debut)) * 100, 2),
            .groups = "drop"
          )
        
        donnees_geo <- ref_geo_prepared %>% 
          inner_join(donnees_evol, by = setNames(cle_geo_data, "join_id"))
        
        if(nrow(donnees_geo) > 0) {
          titre <- paste0("Evol % ", indicateur, " (", min(donnees_evol$label_debut), " -> ", max(donnees_evol$label_fin), ")")
          print(mapview(donnees_geo, zcol = "evolution_pourcent", layer.name = titre))
        }
      } else {
        donnees_geo <- ref_geo_prepared %>% 
          inner_join(donnees_agregees, by = setNames(cle_geo_data, "join_id"))
        
        if(nrow(donnees_geo) > 0) {
          print(mapview(donnees_geo, zcol = indicateur, layer.name = indicateur))
        }
      }
      
      sf_use_s2(original_s2) # Réactivation du moteur S2
    }
  }
  
  return(donnees_agregees)
}



comptage_metrique_elevation <- function(metrique = NULL, 
                                        data = data_propre,
                                        calendrier = CalendrierGeneral,
                                        ref_geographie = ref_geo,
                                        groupement = NULL,
                                        indicateur = NULL,
                                        AMP = NULL, periodEchant = NULL, annee = NULL, mois = NULL,
                                        jour = NULL, saison = NULL, typeJ = NULL,
                                        heure_entiere = NULL, meteo = NULL,
                                        nebulosite = NULL, directionVent = NULL,
                                        forceVent = NULL, etatMer = NULL, lune = NULL,
                                        zone = NULL, zonagePAMPA = NULL, groupe = NULL,
                                        typeBat = NULL, tailleBat = NULL, mouillage = NULL,
                                        natureFond = NULL, categAct1 = NULL, categAct2 = NULL) {
  
  # --- 1. Validation de la métrique ---
  metriques_valides <- c("pression", "impact", "opinion", "pratique", "socio-economique")
  if (is.null(metrique) || metrique == "" || !(metrique %in% metriques_valides)) {
    stop("Préciser une métrique valide : pression, impact, opinion, pratique, socio-economique")
  }
  
  # --- 2. Filtrage des données (via votre fonction externe filtre) ---
  donnees_filtrees <- filtre(data, AMP, periodEchant, annee, mois, jour, saison, typeJ, 
                             heure_entiere, meteo, nebulosite, directionVent, 
                             forceVent, etatMer, lune, zone, zonagePAMPA, 
                             groupe, typeBat, tailleBat, mouillage,
                             natureFond, categAct1, categAct2)
  
  # --- 3. Jointure avec le Calendrier (Sécurisation et types) ---
  calendrier_unique <- calendrier %>%
    mutate(Annee = as.character(Annee), 
           Mois = as.character(Mois)) %>%
    group_by(Annee, Mois) %>%
    summarise(PdsJS = first(PdsJS), 
              PdsJW = first(PdsJW), .groups = "drop")
  
  donnees_filtrees <- donnees_filtrees %>%
    mutate(annee = as.character(annee),
           mois = as.character(mois)) %>%
    left_join(calendrier_unique, by = c("annee" = "Annee", "mois" = "Mois"))
  
  # --- 4. Calculs des agrégats avec Redressement ---
  cols_temporelles <- c("annee", "mois", "periodEchant", "saison")
  groupement_interne <- unique(c(groupement, c("annee", "mois")))
  
  # Pré-calcul des poids inverses
  donnees_redressees <- donnees_filtrees %>%
    mutate(
      poids_inv = case_when(
        typeJ == "JS" ~ 1 / PdsJS,
        typeJ == "JW" ~ 1 / PdsJW,
        TRUE ~ 0
      )
    )
  
  if (metrique == "pression") {
    # Agrégation mensuelle redressée
    donnees_agregees <- donnees_redressees %>% 
      group_by(across(any_of(groupement_interne))) %>% 
      summarise(
        elevPers = sum(nbPers * poids_inv, na.rm = TRUE),
        elevLigne = sum(nbLigne * poids_inv, na.rm = TRUE),
        elevBat = sum(poids_inv, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Ré-agrégation vers le grain géographique choisi
    if (!any(c("annee", "mois") %in% groupement)) {
      donnees_agregees <- donnees_agregees %>%
        group_by(across(any_of(groupement))) %>%
        summarise(nbPers = sum(elevPers, na.rm = TRUE),
                  nbLigne = sum(elevLigne, na.rm = TRUE),
                  nbBat = sum(elevBat, na.rm = TRUE), .groups = "drop")
    } else {
      donnees_agregees <- donnees_agregees %>% rename(nbPers=elevPers, nbLigne=elevLigne, nbBat=elevBat)
    }
    
  } else if (metrique == "impact") {
    # Agrégation impact redressée
    donnees_agregees <- donnees_redressees %>% 
      group_by(across(any_of(groupement_interne))) %>%
      summarise(
        elevBat = sum(poids_inv, na.rm = TRUE),
        elevBeaches = sum(poids_inv[mouillage == "BC"], na.rm = TRUE),
        elevAncres = sum(poids_inv[mouillage == "AC"], na.rm = TRUE), 
        .groups = "drop"
      )
    
    if (!any(c("annee", "mois") %in% groupement)) {
      donnees_agregees <- donnees_agregees %>%
        group_by(across(any_of(groupement))) %>%
        summarise(nbBat = sum(elevBat, na.rm = TRUE),
                  nbBeaches = sum(elevBeaches, na.rm = TRUE),
                  nbAncres = sum(elevAncres, na.rm = TRUE), .groups = "drop")
    } else {
      donnees_agregees <- donnees_agregees %>% rename(nbBat=elevBat, nbBeaches=elevBeaches, nbAncres=elevAncres)
    }
    
    donnees_agregees <- donnees_agregees %>%
      mutate(txBeaches = (nbBeaches / nbBat) * 100,
             txAncres = (nbAncres / nbBat) * 100)
  }
  
  # --- 5. Cartographie et Évolution Temporelle ---
  if (!is.null(indicateur) && (indicateur %in% names(donnees_agregees))) {
    
    map_ref_cols <- c("zone" = "CODE_SITE", "AMP" = "CAS_ETUDE_", 
                      "groupe" = "GROUPE", "zonagePAMPA" = "STATUT_PAM")
    
    cle_geo_data <- intersect(groupement, names(map_ref_cols))[1]
    dims_temps_presentes <- intersect(groupement, cols_temporelles)
    
    if (!is.na(cle_geo_data)) {
      col_ref_geo <- map_ref_cols[cle_geo_data]
      
      # Logique de calcul d'évolution si dimension temporelle détectée
      if (length(dims_temps_presentes) > 0) {
        dim_prio <- dims_temps_presentes[1]
        
        donnees_affichage <- donnees_agregees %>%
          arrange(.data[[dim_prio]]) %>%
          group_by(.data[[cle_geo_data]]) %>%
          summarise(
            val_debut = first(.data[[indicateur]]),
            val_fin = last(.data[[indicateur]]),
            label_debut = first(.data[[dim_prio]]),
            label_fin = last(.data[[dim_prio]]),
            evolution_pourcent = round(((val_fin - val_debut) / ifelse(val_debut == 0, 1, val_debut)) * 100, 2),
            .groups = "drop"
          )
        var_a_cartographier <- "evolution_pourcent"
        titre_map <- paste("Evol %", indicateur, "(", min(donnees_affichage$label_debut), "->", max(donnees_affichage$label_fin), ")")
      } else {
        donnees_affichage <- donnees_agregees
        var_a_cartographier <- indicateur
        titre_map <- indicateur
      }
      
      # Préparation SIG
      original_s2 <- sf::sf_use_s2(); sf::sf_use_s2(FALSE)
      
      ref_geo_prepared <- sf::st_as_sf(ref_geographie) %>% 
        sf::st_make_valid() %>% 
        dplyr::mutate(join_id = toupper(trimws(as.character(.data[[col_ref_geo]])))) %>%
        dplyr::select(join_id) %>% 
        dplyr::group_by(join_id) %>% 
        dplyr::summarise(.groups = "drop")
      
      # Jointure et Rendu
      donnees_geo <- ref_geo_prepared %>% 
        dplyr::inner_join(donnees_affichage %>% 
                            mutate(target_key = toupper(trimws(as.character(.data[[cle_geo_data]])))), 
                          by = c("join_id" = "target_key"))
      
      if(nrow(donnees_geo) > 0) {
        print(mapview::mapview(donnees_geo, zcol = var_a_cartographier, layer.name = titre_map))
      }
      sf::sf_use_s2(original_s2)
    }
  }
  
  return(donnees_agregees)
}


calcul_metrique_elevation <- function(metrique = NULL, 
                                      data = data_propre,
                                      calendrier = CalendrierGeneral,
                                      groupement = NULL,
                                      AMP = NULL, periodEchant = NULL, annee = NULL, mois = NULL,
                                      jour = NULL, saison = NULL, typeJ = NULL,
                                      heure_entiere = NULL, meteo = NULL,
                                      nebulosite = NULL, directionVent = NULL,
                                      forceVent = NULL, etatMer = NULL, lune = NULL,
                                      zone = NULL, zonagePAMPA = NULL, groupe = NULL,
                                      typeBat = NULL, tailleBat = NULL, mouillage = NULL,
                                      natureFond = NULL, categAct1 = NULL, categAct2 = NULL) {
  
  # --- 1. Validation ---
  metriques_valides <- c("pression", "impact", "opinion", "pratique", "socio-economique")
  if (is.null(metrique) || !(metrique %in% metriques_valides)) {
    stop("Préciser une métrique valide : pression, impact, opinion, pratique, socio-economique")
  }
  
  # --- 2. Filtrage ---
  donnees_filtrees <- filtre(data, AMP, periodEchant, annee, mois, jour, saison, typeJ, 
                             heure_entiere, meteo, nebulosite, directionVent, 
                             forceVent, etatMer, lune, zone, zonagePAMPA, 
                             groupe, typeBat, tailleBat, mouillage,
                             natureFond, categAct1, categAct2)
  
  # --- 3. Redressement ---
  calendrier_unique <- calendrier %>%
    mutate(Annee = as.character(Annee), Mois = as.character(Mois)) %>%
    group_by(Annee, Mois) %>%
    summarise(PdsJS = first(PdsJS), PdsJW = first(PdsJW), .groups = "drop")
  
  donnees_redressees <- donnees_filtrees %>%
    mutate(annee = as.character(annee), mois = as.character(mois)) %>%
    left_join(calendrier_unique, by = c("annee" = "Annee", "mois" = "Mois")) %>%
    mutate(poids_inv = case_when(
      typeJ == "JS" ~ 1 / PdsJS,
      typeJ == "JW" ~ 1 / PdsJW,
      TRUE ~ 0
    ))
  
  # --- 4. Agrégation ---
  groupement_interne <- unique(c(groupement, c("annee", "mois")))
  
  if (metrique == "pression") {
    res <- donnees_redressees %>% 
      group_by(across(any_of(groupement_interne))) %>% 
      summarise(elevPers = sum(nbPers * poids_inv, na.rm = TRUE),
                elevLigne = sum(nbLigne * poids_inv, na.rm = TRUE),
                elevBat = sum(poids_inv, na.rm = TRUE), .groups = "drop")
    
    if (!any(c("annee", "mois") %in% groupement)) {
      res <- res %>% group_by(across(any_of(groupement))) %>%
        summarise(nbPers = sum(elevPers, na.rm = TRUE),
                  nbLigne = sum(elevLigne, na.rm = TRUE),
                  nbBat = sum(elevBat, na.rm = TRUE), .groups = "drop")
    } else {
      res <- res %>% rename(nbPers=elevPers, nbLigne=elevLigne, nbBat=elevBat)
    }
    
  } else if (metrique == "impact") {
    res <- donnees_redressees %>% 
      group_by(across(any_of(groupement_interne))) %>%
      summarise(elevBat = sum(poids_inv, na.rm = TRUE),
                elevBeaches = sum(poids_inv[mouillage == "BC"], na.rm = TRUE),
                elevAncres = sum(poids_inv[mouillage == "AC"], na.rm = TRUE), .groups = "drop")
    
    if (!any(c("annee", "mois") %in% groupement)) {
      res <- res %>% group_by(across(any_of(groupement))) %>%
        summarise(nbBat = sum(elevBat, na.rm = TRUE),
                  nbBeaches = sum(elevBeaches, na.rm = TRUE),
                  nbAncres = sum(elevAncres, na.rm = TRUE), .groups = "drop")
    } else {
      res <- res %>% rename(nbBat=elevBat, nbBeaches=elevBeaches, nbAncres=elevAncres)
    }
    res <- res %>% mutate(txBeaches = (nbBeaches / nbBat) * 100, txAncres = (nbAncres / nbBat) * 100)
  }
  
  return(res)
}





carte_metrique_elevation_brouillon <- function(donnees_agregees, 
                                               indicateur, 
                                               groupement, 
                                               ref_geographie = ref_geo) {
  
  if (!(indicateur %in% names(donnees_agregees))) {
    stop("L'indicateur spécifié n'existe pas dans les données.")
  }
  
  map_ref_cols <- c("zone" = "CODE_SITE", "AMP" = "CAS_ETUDE_", 
                    "groupe" = "GROUPE", "zonagePAMPA" = "STATUT_PAM")
  
  cle_geo_data <- intersect(groupement, names(map_ref_cols))[1]
  cols_temporelles <- c("annee", "mois", "periodEchant", "saison")
  dims_temps_presentes <- intersect(groupement, cols_temporelles)
  
  if (is.na(cle_geo_data)) stop("Aucune dimension géographique reconnue dans le groupement.")
  
  # --- Logique d'évolution ou valeur simple ---
  if (length(dims_temps_presentes) > 0) {
    dim_prio <- dims_temps_presentes[1]
    donnees_affichage <- donnees_agregees %>%
      arrange(.data[[dim_prio]]) %>%
      group_by(.data[[cle_geo_data]]) %>%
      summarise(
        val_debut = first(.data[[indicateur]]),
        val_fin = last(.data[[indicateur]]),
        label_debut = first(.data[[dim_prio]]),
        label_fin = last(.data[[dim_prio]]),
        evolution_pourcent = round(((val_fin - val_debut) / ifelse(val_debut == 0, 1, val_debut)) * 100, 2),
        .groups = "drop"
      )
    var_a_cartographier <- "evolution_pourcent"
    titre_map <- paste("Evol %", indicateur)
  } else {
    donnees_affichage <- donnees_agregees
    var_a_cartographier <- indicateur
    titre_map <- indicateur
  }
  
  # --- Préparation SIG et Rendu ---
  col_ref_geo <- map_ref_cols[cle_geo_data]
  original_s2 <- sf::sf_use_s2(); sf::sf_use_s2(FALSE)
  
  ref_geo_prepared <- sf::st_as_sf(ref_geographie) %>% 
    sf::st_make_valid() %>% 
    dplyr::mutate(join_id = toupper(trimws(as.character(.data[[col_ref_geo]])))) %>%
    dplyr::group_by(join_id) %>% summarise(.groups = "drop")
  
  donnees_geo <- ref_geo_prepared %>% 
    dplyr::inner_join(donnees_affichage %>% 
                        mutate(target_key = toupper(trimws(as.character(.data[[cle_geo_data]])))), 
                      by = c("join_id" = "target_key"))
  
  sf::sf_use_s2(original_s2)
  
  if(nrow(donnees_geo) > 0) {
    return(mapview::mapview(donnees_geo, zcol = var_a_cartographier, layer.name = titre_map))
  } else {
    message("Aucune correspondance géographique trouvée pour la carte.")
  }
}


carte_metrique_elevation <- function(donnees_agregees, 
                                     indicateur, 
                                     groupement, 
                                     ref_geographie = ref_geo) {
  
  if (!(indicateur %in% names(donnees_agregees))) {
    stop("L'indicateur spécifié n'existe pas dans les données.")
  }
  
  # --- Configuration des correspondances ---
  map_ref_cols <- c("zone" = "CODE_SITE", "AMP" = "CAS_ETUDE_", 
                    "groupe" = "GROUPE", "zonagePAMPA" = "STATUT_PAM")
  
  cle_geo_data <- intersect(groupement, names(map_ref_cols))[1]
  cols_temporelles <- c("annee", "mois", "periodEchant", "saison")
  dims_temps_presentes <- intersect(groupement, cols_temporelles)
  
  if (is.na(cle_geo_data)) stop("Aucune dimension géographique reconnue dans le groupement.")
  
  # --- Logique d'évolution ou valeur brute ---
  if (length(dims_temps_presentes) > 0) {
    dim_prio <- dims_temps_presentes[1]
    donnees_affichage <- donnees_agregees %>%
      arrange(.data[[dim_prio]]) %>%
      group_by(.data[[cle_geo_data]]) %>%
      summarise(
        val_debut = first(.data[[indicateur]]),
        val_fin = last(.data[[indicateur]]),
        evolution_pourcent = round(((val_fin - val_debut) / ifelse(val_debut == 0, 1, val_debut)) * 100, 2),
        .groups = "drop"
      )
    var_a_cartographier <- "evolution_pourcent"
    titre_map <- paste("Evol %", indicateur)
  } else {
    donnees_affichage <- donnees_agregees
    var_a_cartographier <- indicateur
    titre_map <- indicateur
  }
  
  # --- Préparation SIG ---
  col_ref_geo <- map_ref_cols[cle_geo_data]
  original_s2 <- sf::sf_use_s2(); sf::sf_use_s2(FALSE)
  
  ref_geo_prepared <- sf::st_as_sf(ref_geographie) %>% 
    sf::st_make_valid() %>% 
    dplyr::mutate(join_id = toupper(trimws(as.character(.data[[col_ref_geo]])))) %>%
    dplyr::group_by(join_id) %>% 
    summarise(.groups = "drop")
  
  donnees_geo <- ref_geo_prepared %>% 
    dplyr::inner_join(donnees_affichage %>% 
                        mutate(target_key = toupper(trimws(as.character(.data[[cle_geo_data]])))), 
                      by = c("join_id" = "target_key"))
  
  sf::sf_use_s2(original_s2)
  
  # --- Rendu Cartographique avec 10 Modalités ---
  if(nrow(donnees_geo) > 0) {
    
    # Palette divergente de 10 couleurs
    pal_div <- grDevices::colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(10)
    
    # Calcul des bornes symétriques pour 10 classes (donc 11 points de coupure)
    limite_symetrique <- max(abs(donnees_geo[[var_a_cartographier]]), na.rm = TRUE)
    if(limite_symetrique == 0) limite_symetrique <- 1 
    
    # Génération de 11 bornes pour créer 10 intervalles
    bornes <- seq(-limite_symetrique, limite_symetrique, length.out = 11)
    
    return(
      mapview::mapview(
        donnees_geo, 
        zcol = var_a_cartographier, 
        layer.name = titre_map,
        col.regions = pal_div,
        at = bornes
      )
    )
  } else {
    message("Aucune correspondance géographique trouvée pour la carte.")
  }
}



ajouter_evolutions <- function(data, groupement_fixe, grain_temporel) {
  
  if (is.null(data) || nrow(data) == 0) return(data)
  
  # 1. Préparation de l'ordre chronologique
  ordre_periodes <- c("2005_2006", "2006_2007", "2008_2009", "2009_2010", 
                      "2010_2011", "2011_2012", "2013_2014")
  
  # 2. Identification stricte des indicateurs (exclure les clés de groupement)
  # On ne veut que les colonnes de résultats (nbBat, txBeaches, etc.)
  tous_groupements <- c(groupement_fixe, grain_temporel, "annee", "mois")
  cols_stats <- setdiff(names(data)[sapply(data, is.numeric)], tous_groupements)
  
  # 3. Préparation des données (Tri et Facteurs)
  df_travail <- data %>%
    mutate(across(any_of("periodEchant"), ~ factor(., levels = ordre_periodes))) %>%
    arrange(across(all_of(grain_temporel))) %>%
    group_by(across(all_of(groupement_fixe)))
  
  # 4. Calcul colonne par colonne (évite l'erreur d'attribut names)
  for (col in cols_stats) {
    nom_evol <- paste0("evol_", col, "_pourcent")
    
    df_travail <- df_travail %>%
      mutate(!!nom_evol := {
        # Extraction sécurisée de la première valeur du groupe
        valeurs <- .data[[col]]
        v_init <- valeurs[1]
        
        # Calcul du pourcentage par rapport à la référence
        round(((valeurs - v_init) / ifelse(v_init == 0 | is.na(v_init), 1, v_init)) * 100, 2)
      })
  }
  
  return(ungroup(df_travail))
}


##### Exemples d'utilisation des fonctions #####

donnees_metrique <- calcul_metrique_elevation(periodEchant = c("2006_2007", "2008_2009"), metrique = "impact", groupement = c("zone", "periodEchant")) # fonctionne
donnees_metrique

donnees_metrique_evolution <- ajouter_evolutions(donnees_metrique, groupement_fixe = "zone", grain_temporel = "periodEchant")

donnees_metrique_pression <- calcul_metrique_elevation(periodEchant = c("2006_2007", "2008_2009"), metrique = "pression", groupement = c("zone", "periodEchant"))
donnees_metrique_pression

donnees_metrique_pression_evolution <- ajouter_evolutions(donnees_metrique_pression, groupement_fixe = "zone", grain_temporel = "periodEchant")

donnees_metrique <- calcul_metrique_elevation(periodEchant = c("2006_2007", "2008_2009"), metrique = "impact", groupement = c("zone", "periodEchant")) # fonctionne
donnees_metrique

carte_metrique_elevation(donnees_metrique, indicateur = "txAncres", groupement = "zone")




## filtre()

FREQUENTATION_2005_2013 %>% filter(AMP == "NC", mois == "10") # 2 538 obs

FREQUENTATION_2005_2013 %>% filter(AMP %in% "NC") # 47 688 obs
FREQUENTATION_2005_2013 %>% filter(AMP %in% "NC", mois %in% "9") # 3 055 obs

filtre() # 62 708 obs

FREQUENTATION_2005_2013 %>% filter(AMP == "NC") # 47 688 obs
filtre(AMP = "NC") # 47 688 obs
filtre(AMP = "NC", mois = "9") # 3 055 obs

## comptage()

comptage() # 62708 sorties
comptage(AMP = "NC", mois = "9") # 3055 sorties


comptage(AMP = "NC", mois = c("9", "10")) # 5593 sorties
comptage(AMP = "NC", mois = c("9", "10"), groupement = "mois") # 2538+3055 = 5593
comptage(AMP = "NC", mois = c("9", "10"), groupement = "zone")
test_multi_gpmt <- comptage(AMP = "NC", mois = c("9", "10"), groupement = c("mois", "zone"))



comptage_metrique()
comptage_metrique(metrique = "abc")
comptage_metrique(metrique = "pression")
comptage_metrique(groupement = "zonagePAMPA", metrique = "pression", indicateur = "abc")
comptage_metrique(groupement = "zonagePAMPA", metrique = "pression", indicateur = "nbPers")
comptage_metrique(groupement = "zonagePAMPA", metrique = "pression", indicateur = "nbBat")
comptage_metrique(groupement = "zonagePAMPA", metrique = "pression", indicateur = "nbLigne")

comptage_metrique(groupement = "groupe", metrique = "pression", indicateur = "nbLigne")

comptage_metrique(groupement = "zonagePAMPA", metrique = "impact", indicateur = "nbBat")
comptage_metrique(groupement = "zonagePAMPA", metrique = "impact", indicateur = "nbBeaches")
comptage_metrique(groupement = "zonagePAMPA", metrique = "impact", indicateur = "nbAncres")
comptage_metrique(groupement = "zonagePAMPA", metrique = "impact", indicateur = "txBeaches")
comptage_metrique(groupement = "zonagePAMPA", metrique = "impact", indicateur = "txAncres")


comptage_metrique(annee = c("2013"), groupement = "zonagePAMPA", metrique = "pression", indicateur = "nbBat")
comptage_metrique(annee = c("2013"), groupement = "zonagePAMPA", metrique = "impact")

comptage_metrique(annee = c("2013", "2014"), groupement = c("annee", "zonagePAMPA"), metrique = "impact")
comptage_metrique(annee = c("2013", "2014"), groupement = c("annee", "zonagePAMPA"), metrique = "impact", indicateur = "txBeaches")

comptage_metrique(groupement = "AMP", metrique = "pression", indicateur = "nbPers") # fonctionne
comptage_metrique(groupement = "zone", metrique = "pression", indicateur = "nbPers")
comptage_metrique(groupement = "groupe", metrique = "pression", indicateur = "nbPers")


comptage_metrique(groupement = "zonagePAMPA", metrique = "pression", indicateur = "nbBat")
comptage_metrique(groupement = c("zonagePAMPA", "annee"), metrique = "pression", indicateur = "nbBat")

comptage_metrique(groupement = c("zonagePAMPA", "mois"), metrique = "pression", indicateur = "nbBat")
comptage_metrique(groupement = c("zonagePAMPA", "annee", "mois"), metrique = "pression", indicateur = "nbBat")

comptage_metrique(groupement = "zonagePAMPA", metrique = "impact", indicateur = "txBeaches")
comptage_metrique(groupement = c("zonagePAMPA", "annee"), metrique = "impact", indicateur = "txBeaches")

comptage_metrique(groupement = c("zonagePAMPA", "annee"), metrique = "impact")
comptage_metrique(groupement = c("zonagePAMPA", "periodEchant"), metrique = "impact") # fonctionne
comptage_metrique(groupement = c("periodEchant", "zonagePAMPA"), metrique = "impact", indicateur = "txAncres") # fonctionne

comptage_metrique(groupement = "periodEchant", metrique = "impact")
comptage_metrique(groupement = "periodEchant", metrique = "pression")

unique(data_propre %>% filter(periodEchant == "2013_2014") %>% select(nbPers)) # NA
unique(FREQUENTATION_2005_2013 %>% filter(periodEchant == "2013_2014") %>% select(nbPers)) # NA


comptage_metrique(groupement = "zonagePAMPA", metrique = "impact", indicateur = "abc")

comptage_metrique_elevation(metrique = "pression") 

comptage_metrique_elevation(metrique = "impact")

comptage_metrique_elevation(metrique = "impact", groupement = "zone", indicateur = "txAncres")

comptage_metrique_elevation(metrique = "impact", groupement = "periodEchant")

comptage_metrique_elevation(periodEchant = c("2006_2007", "2008_2009"), metrique = "impact", groupement = c("zone", "periodEchant"), indicateur = "txBeaches")


##### séparation comptages et carte #####

donnees_metrique <- calcul_metrique_elevation(metrique = "pression", groupement = c("zone")) # fonctionne
donnees_metrique



unique(donnees_metrique$nbPers)

donnees_metrique <- calcul_metrique_elevation(periodEchant = c("2006_2007", "2008_2009"), metrique = "impact", groupement = c("zone", "periodEchant")) # fonctionne
donnees_metrique

ajouter_evolutions(donnees_metrique, groupement_fixe = "zone", grain_temporel = "periodEchant")


carte_metrique_elevation(donnees_metrique, groupement = "zone", indicateur = "txBeaches")
carte_metrique_elevation(donnees_metrique, groupement = c("zone", "periodEchant"), indicateur = "txBeaches")



### Evolutions annuelles des fréquentations globales des AMP

sorties <- c()
for (an in unique(annee)) {
  sorties <- c(sorties, comptage(annee = an)$nb_sorties)
}

names(sorties) <- unique(annee)

sorties

plot(sorties, type = "l")
