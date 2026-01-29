# ==============================================================================
# FICHIER : mes_fonctions.R
# OBJET : Fonctions de traitement, calcul et visualisation pour le rapport PAMPA
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PARTIE : PRÉTRAITEMENT ET UTILITAIRES
# Intérêt : Nettoyage du jeu de données brut, harmonisation des variables 
# catégorielles pour la comparaison inter-campagnes et gestion de l'ordre de tri.
# ------------------------------------------------------------------------------

#' Préparation et agrégation des données d'enquêtes
#' 
#' @description Nettoie les données brutes, calcule les plages horaires et harmonise les catégories de bateaux.
#' @param df Dataframe brut issu de l'import Excel.
#' @param periodes_cibles Vecteur des campagnes à conserver (ex: c("2008_2009", ...)).
#' @return Un dataframe nettoyé avec les colonnes harmonisées 'taille_agg' et 'type_agg'.
preparer_donnees_enquetes <- function(df, periodes_cibles) {
  df_prep <- df %>%
    mutate(
      date_obs = make_date(annee, mois, jour),
      h_num = suppressWarnings(as.numeric(sub("h.*", "", heure)))
    ) %>%
    mutate(plageHoraire = case_when(
      h_num < 8  ~ "Avant 08h",
      h_num >= 8  & h_num < 10 ~ "08h-10h",
      h_num >= 10 & h_num < 12 ~ "10h-12h",
      h_num >= 12 & h_num < 14 ~ "12h-14h",
      h_num >= 14 & h_num < 16 ~ "14h-16h",
      h_num >= 16 & h_num < 18 ~ "16h-18h",
      h_num >= 18 ~ "Après 18h",
      TRUE ~ "NA"
    )) %>%
    mutate(
      taille_agg = case_when(
        tailleBat %in% c("<5m", "5-7m", "inf.7m") ~ "0-7m",
        tailleBat %in% c("7-10m", "7-12m")         ~ "7-12m",
        tailleBat %in% c(">10m", "sup.12m")        ~ "12-.m",
        tailleBat %in% c("KY", "JS", "Z")          ~ "Léger/Spécifique (KY/JS/Z)",
        tailleBat == "NM"                          ~ "Moteur (Non spécifié)",
        TRUE                                       ~ "Inconnu/NA"
      ),
      type_agg = case_when(
        typeBat %in% c("M", "NM") ~ "Moteur",
        typeBat == "V"            ~ "Voilier",
        typeBat %in% c("Z", "PI") ~ "Pneumatique/Autre",
        typeBat %in% c("KY", "JS") ~ "Léger (Jet/Kayac)",
        TRUE                       ~ "Autre/Inconnu"
      )
    ) %>%
    filter(periodEchant %in% periodes_cibles) %>%
    select(periodEchant, nbPers, nbLigne, tailleBat, typeBat, type_agg, taille_agg,
           mois, saison, typeJ, plageHoraire, date_obs,
           zonagePAMPA, groupe, mouillage, natureFond,
           meteo, lune, nebulosite, directionVent, forceVent, etatMer,
           act1, categAct1, act2, categAct2, sens1, sens2)
  
  return(df_prep)
}

#' Récupération de l'ordre logique des colonnes
#' 
#' @description Détermine l'ordre de tri numérique ou alphabétique pour garantir une lecture logique des tableaux.
#' @param df Dataframe source contenant les données.
#' @param nom_col Nom de la colonne servant de base au tri (ex: "mois", "saison").
#' @return Une liste contenant les vecteurs 'ordre' et 'ordre_total' (avec mention "TOTAL").
recuperer_ordre_colonnes <- function(df, nom_col) {
  valeurs_uniques <- unique(df[[nom_col]])
  valeurs_sans_na <- valeurs_uniques[!is.na(valeurs_uniques)]
  
  if (all(!is.na(suppressWarnings(as.numeric(valeurs_sans_na))))) {
    ordre_base <- as.character(sort(as.numeric(valeurs_sans_na)))
  } else {
    ordre_base <- as.character(sort(valeurs_sans_na))
  }
  
  if (any(is.na(df[[nom_col]]))) {
    ordre_base <- c(ordre_base, "NA")
  }
  
  return(list(ordre = ordre_base, ordre_total = c(ordre_base, "TOTAL")))
}

# ------------------------------------------------------------------------------
# 2. PARTIE : CALCULS STATISTIQUES
# Intérêt : Agrégation des données pour extraire des indicateurs de fréquentation, 
# d'effort de pêche et de couverture temporelle.
# ------------------------------------------------------------------------------

#' Génération de la synthèse d'effort et fréquentation par campagne
#' 
#' @description Calcule l'effort de terrain et les statistiques globales par campagne (totaux et moyennes).
#' @param df_prepare Dataframe nettoyé issu de 'preparer_donnees_enquetes'.
#' @return Un dataframe de synthèse ordonné pour le rapport PDF.
generer_synthese_effort <- function(df_prepare) {
  synthese <- df_prepare %>%
    group_by(periodEchant) %>%
    summarise(
      date_debut = min(date_obs, na.rm = TRUE),
      date_fin   = max(date_obs, na.rm = TRUE),
      nb_jours_enquetes = n_distinct(date_obs, na.rm = TRUE),
      nb_enquete = n(),
      nbPers_NA = sum(is.na(nbPers)),
      nbLigne_NA = sum(is.na(nbLigne)),
      sum_nbPers = if(sum(is.na(nbPers)) == n()) NA_real_ else sum(nbPers, na.rm = TRUE),
      sum_nbLigne = if(sum(is.na(nbLigne)) == n()) NA_real_ else sum(nbLigne, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      duree_calendaire = as.numeric(difftime(date_fin, date_debut, units = "days")) + 1,
      proportion_jour    = round((nb_jours_enquetes / duree_calendaire) * 100, 2),
      proportion_enquete = round((nb_enquete / sum(nb_enquete)) * 100, 2),
      nbmy_enquete = round(nb_enquete / nb_jours_enquetes, 2),
      nbmy_nbPers  = round(sum_nbPers / nb_jours_enquetes, 2),
      nbmy_nbligne = round(sum_nbLigne / nb_jours_enquetes, 2)
    ) %>%
    select(periodEchant, date_debut, date_fin, duree_calendaire, 
           nb_jours_enquetes, proportion_jour, 
           nb_enquete, nbmy_enquete, proportion_enquete,
           nbPers_NA, sum_nbPers, nbmy_nbPers,
           nbLigne_NA, sum_nbLigne, nbmy_nbligne)
  
  return(synthese)
}

#' Tableau croisé TypeBat vs TailleBat paramétrable
#' 
#' @description Génère un croisement trié avec gestion explicite des NA et des totaux par ligne/colonne.
#' @param df Le dataframe source.
#' @param periode La période d'échantillonnage cible.
#' @param ordre_type Vecteur définissant l'ordre des lignes.
#' @param ordre_taille Vecteur définissant l'ordre des colonnes.
#' @return Un dataframe pivoté au format large avec totaux.
croiser_bat <- function(df, periode, ordre_type, ordre_taille) {
  df_clean <- df %>%
    filter(periodEchant == periode) %>%
    mutate(
      typeBat = trimws(as.character(typeBat)),
      tailleBat = trimws(as.character(tailleBat)),
      typeBat = ifelse(is.na(typeBat) | typeBat %in% c("", "-", "NA", "NaN"), "NA", typeBat),
      tailleBat = ifelse(is.na(tailleBat) | tailleBat %in% c("", "-", "NA", "NaN"), "NA", tailleBat)
    )
  
  tab_croise <- df_clean %>%
    group_by(typeBat, tailleBat) %>%
    summarise(nb = n(), .groups = "drop") %>%
    pivot_wider(names_from = tailleBat, values_from = nb, values_fill = 0)
  
  cols_num <- setdiff(names(tab_croise), "typeBat")
  tab_final <- tab_croise %>%
    mutate(TOTAL = rowSums(across(all_of(cols_num)))) %>%
    bind_rows(summarise(., typeBat = "TOTAL", across(where(is.numeric), sum)))
  
  niveaux_lignes <- trimws(ordre_type)
  if (!"TOTAL" %in% niveaux_lignes) niveaux_lignes <- c(niveaux_lignes, "TOTAL")
  
  tab_final <- tab_final %>%
    mutate(typeBat = factor(typeBat, levels = niveaux_lignes)) %>%
    filter(!is.na(typeBat)) %>% arrange(typeBat) %>% filter(TOTAL > 0)
  
  niveaux_cols <- trimws(ordre_taille)
  if (!"TOTAL" %in% niveaux_cols) niveaux_cols <- c(niveaux_cols, "TOTAL")
  cols_finales <- intersect(niveaux_cols, names(tab_final))
  
  return(tab_final %>% select(typeBat, all_of(cols_finales)))
}

#' Calcul des statistiques de fréquentation par catégorie
#' 
#' @description Calcule le total, la moyenne journalière et la densité par embarcation.
#' @param df_prepare Dataframe nettoyé.
#' @param var_interet Variable de calcul ("nbPers" ou "nbLigne").
#' @param var_cat Variable de catégorie ("type_agg" ou "taille_agg").
#' @return Une liste contenant les dataframes au format LARGE pour affichage et LONG pour graphiques.
calculer_stats_frequentation <- function(df_prepare, var_interet, var_cat) {
  effort_jours <- df_prepare %>%
    group_by(periodEchant) %>%
    summarise(total_jours = n_distinct(date_obs, na.rm = TRUE), .groups = "drop")
  
  stats_base <- df_prepare %>%
    group_by(periodEchant, !!sym(var_cat)) %>%
    summarise(
      sum_val = if(all(is.na(!!sym(var_interet)))) NA_real_ else sum(!!sym(var_interet), na.rm = TRUE),
      nb_enquetes_cat = n(),
      .groups = "drop"
    ) %>%
    left_join(effort_jours, by = "periodEchant") %>%
    mutate(nbmy_jour = round(sum_val / total_jours, 2),
           nbmy_embarq = round(sum_val / nb_enquetes_cat, 2))
  
  tab_sum <- stats_base %>%
    select(periodEchant, !!sym(var_cat), sum_val) %>%
    pivot_wider(names_from = !!sym(var_cat), values_from = sum_val) %>%
    mutate(TOTAL_LIGNE = rowSums(across(where(is.numeric)), na.rm = TRUE),
           TOTAL_LIGNE = ifelse(TOTAL_LIGNE == 0 & rowSums(!is.na(across(where(is.numeric)))) == 0, NA, TOTAL_LIGNE))
  
  tab_moy_j <- stats_base %>%
    select(periodEchant, !!sym(var_cat), nbmy_jour) %>%
    pivot_wider(names_from = !!sym(var_cat), values_from = nbmy_jour)
  
  tab_moy_e <- stats_base %>%
    select(periodEchant, !!sym(var_cat), nbmy_embarq) %>%
    pivot_wider(names_from = !!sym(var_cat), values_from = nbmy_embarq)
  
  return(list(data_long = stats_base, table_sum = tab_sum, table_jour = tab_moy_j, table_embarq = tab_moy_e, var_nom = var_interet))
}

#' Calcul de la synthèse globale par variable d'étude
#' 
#' @description Calcule l'effort, la couverture calendaire et les densités moyennes pour une variable d'intérêt.
#' @param df Dataframe source.
#' @param var_etude Nom technique de la variable (ex: "mois", "saison").
#' @return Une liste de 5 tableaux formatés (jours, couverture, enquêtes, personnes, lignes).
calculer_synthese_par_variable <- function(df, var_etude) {
  
  # 1. Calcul du référentiel temps (inchangé)
  cols_ref <- unique(c("periodEchant", var_etude, "mois"))
  ref_temps <- df %>% 
    group_by(across(all_of(cols_ref))) %>% 
    summarise(jours_mois = unname(days_in_month(first(date_obs))), .groups = "drop") %>%
    group_by(periodEchant, !!sym(var_etude)) %>% 
    summarise(total_calendaire = sum(jours_mois), .groups = "drop")
  
  # 2. Calcul des statistiques (avec arrondis à 2 chiffres)
  stats <- df %>% 
    group_by(periodEchant, !!sym(var_etude)) %>%
    summarise(
      nb_jours = n_distinct(date_obs, na.rm = TRUE), 
      nb_enquete = n(),
      avg_pers = if(all(is.na(nbPers))) NA_real_ else sum(nbPers, na.rm = TRUE) / n(),
      avg_ligne = if(all(is.na(nbLigne))) NA_real_ else sum(nbLigne, na.rm = TRUE) / n(), 
      .groups = "drop"
    ) %>%
    group_by(periodEchant) %>% 
    mutate(prop_enquetes = round((nb_enquete / sum(nb_enquete)) * 100, 2)) %>% 
    ungroup() %>%
    left_join(ref_temps, by = c("periodEchant", var_etude)) %>%
    mutate(prop_couv = if(var_etude %in% c("mois", "saison")) round((nb_jours / total_calendaire) * 100, 2) else NA_real_) %>%
    mutate(across(c(avg_pers, avg_ligne), ~ round(.x, 2)))
  
  # 3. Pivotement pour les tableaux LARGE (inchangé)
  tab_j <- stats %>% select(periodEchant, !!sym(var_etude), nb_jours) %>% pivot_wider(names_from = !!sym(var_etude), values_from = nb_jours)
  tab_enq <- stats %>% select(periodEchant, !!sym(var_etude), nb_enquete) %>% pivot_wider(names_from = !!sym(var_etude), values_from = nb_enquete)
  tab_prop <- stats %>% select(periodEchant, !!sym(var_etude), prop_enquetes) %>% pivot_wider(names_from = !!sym(var_etude), values_from = prop_enquetes)
  tab_p <- stats %>% select(periodEchant, !!sym(var_etude), avg_pers) %>% pivot_wider(names_from = !!sym(var_etude), values_from = avg_pers)
  tab_l <- stats %>% select(periodEchant, !!sym(var_etude), avg_ligne) %>% pivot_wider(names_from = !!sym(var_etude), values_from = avg_ligne)
  tab_c <- if (var_etude %in% c("mois", "saison")) stats %>% select(periodEchant, !!sym(var_etude), prop_couv) %>% pivot_wider(names_from = !!sym(var_etude), values_from = prop_couv) else NULL
  
  return(list(jours = tab_j, couv = tab_c, effectifs = tab_enq, prop_enq = tab_prop, 
              pers = tab_p, ligne = tab_l, data_graph = stats)) # <--- Renvoi du format long
}
# ------------------------------------------------------------------------------
# 3. PARTIE : FONCTIONS D'AFFICHAGE ET VISUALISATION
# Intérêt : Mise en forme LaTeX pour les PDF Quarto et génération de graphiques 
# avec gestion automatique de la mise en page (facettage).
# ------------------------------------------------------------------------------

#' Affichage LaTeX sécurisé et multi-lignes
#' 
#' @description Formate un dataframe en tableaux LaTeX avec protection des caractères spéciaux et découpage des colonnes.
#' @param df Dataframe à afficher.
#' @param legende Titre du tableau.
#' @param ordre_colonnes Vecteur optionnel définissant l'ordre de tri des colonnes.
#' @param col_groupe Nom de la variable d'index à protéger.
#' @param nb_col Optionnel. Nombre de colonnes par tableau pour le découpage.
afficher_tableau_page <- function(df, legende, ordre_colonnes = NULL, col_groupe, nb_col = NULL) {
  latex_protect <- function(x) {
    if (is.character(x) | is.factor(x)) {
      x <- gsub("_", "\\\\_", x); x <- gsub("%", "\\\\%", x); x <- gsub("&", "\\\\&", x)
    }
    return(x)
  }
  legende_safe <- latex_protect(legende)
  ordre_final <- if (!is.null(ordre_colonnes)) c(col_groupe, intersect(as.character(ordre_colonnes), names(df))) else names(df)
  
  df_clean <- df %>%
    select(any_of(ordre_final)) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | . == "NaN" | . == "NA", "-", .))) %>%
    mutate(across(everything(), ~ latex_protect(.)))
  
  names(df_clean) <- latex_protect(names(df_clean))
  col_pivot_safe <- latex_protect(col_groupe)
  cols_data <- setdiff(names(df_clean), col_pivot_safe)
  
  if (!is.null(nb_col) && is.numeric(nb_col)) {
    fins <- cumsum(nb_col); debuts <- c(1, fins[-length(fins)] + 1)
    indices_valides <- which(debuts <= length(cols_data))
    out <- lapply(indices_valides, function(i) {
      df_clean %>% select(all_of(c(col_pivot_safe, cols_data[debuts[i]:min(fins[i], length(cols_data))]))) %>%
        kable(booktabs = TRUE, format = "latex", caption = paste0(legende_safe, " (P. ", i, ")"), align = "c", escape = FALSE) %>%
        kable_styling(latex_options = c("striped", "hold_position", "scale_down"), full_width = FALSE) %>% as.character()
    })
    cat(paste(Filter(Negate(is.null), out), collapse = "\n\n"))
  } else {
    print(df_clean %>% kable(booktabs = TRUE, format = "latex", caption = legende_safe, align = "c", escape = FALSE) %>%
            kable_styling(latex_options = c("striped", "hold_position", "scale_down"), full_width = FALSE))
  }
}

#' Affichage de deux graphiques côte à côte avec légende centrée
#' 
#' @description Utilise le facettage pour afficher deux métriques avec une légende commune en bas.
#' @param df_long Dataframe format long issu de 'calculer_stats_frequentation'.
#' @param var_cat Variable de catégorie (axe X).
#' @param metrics Vecteur des colonnes à afficher (ex: c("nbmy_jour", "nbmy_embarq")).
#' @param titre_global Titre du graphique complet.
afficher_cote_a_cote <- function(df_long, var_cat, metrics, titre_global) {
  df_facette <- df_long %>%
    pivot_longer(cols = all_of(metrics), names_to = "type_stat", values_to = "valeur") %>%
    mutate(type_stat = case_when(type_stat == "nbmy_jour" ~ "Moyenne / Jour", 
                                 type_stat == "nbmy_embarq" ~ "Moyenne / Embarcation", TRUE ~ type_stat))
  
  df_facette$periodEchant <- as.factor(df_facette$periodEchant)
  
  p <- ggplot(df_facette, aes(x = !!sym(var_cat), y = valeur, fill = periodEchant)) +
    geom_bar(stat = "identity", position = "dodge", color = "white", linewidth = 0.2) +
    facet_wrap(~ type_stat, scales = "free_y") + 
    scale_fill_brewer(palette = "Set1", drop = FALSE) +
    theme_minimal() +
    labs(title = titre_global, x = "Catégorie", y = "Valeur moyenne", fill = "Campagne") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          strip.background = element_rect(fill = "grey92", color = NA), strip.text = element_text(face = "bold"))
  print(p)
}

#' Visualisation de la fréquentation par catégorie (Barplot simple)
#' 
#' @description Génère un graphique en barres simple par campagne.
#' @param stats_cat Dataframe format long.
#' @param var_cat Variable de catégorie (axe X).
#' @param metric Métrique à tracer (axe Y).
#' @param titre Titre du graphique.
visualiser_frequentation <- function(stats_cat, var_cat, metric, titre) {
  stats_cat$periodEchant <- as.factor(stats_cat$periodEchant)
  ggplot(stats_cat, aes(x = !!sym(var_cat), y = !!sym(metric), fill = periodEchant)) +
    geom_bar(stat = "identity", position = "dodge", color = "white", linewidth = 0.2) +
    scale_fill_brewer(palette = "Set1", drop = FALSE) + theme_minimal() +
    labs(title = titre, x = "Catégorie", y = "Valeur", fill = "Campagne") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
}

#' Visualisation de la synthèse : % Enquêtes et Moyennes
#' @description Affiche la répartition des enquêtes et les densités moyennes avec légende commune.
afficher_synthese_graphique <- function(df_stats, var_etude, titre_global) {
  
  # 1. Préparation des données pour le facettage
  df_plot <- df_stats %>%
    select(periodEchant, !!sym(var_etude), prop_enquetes, avg_pers, avg_ligne) %>%
    pivot_longer(cols = c(prop_enquetes, avg_pers, avg_ligne), 
                 names_to = "type_stat", values_to = "valeur") %>%
    mutate(type_stat = case_when(
      type_stat == "prop_enquetes" ~ "1. Répartition des enquêtes (%)",
      type_stat == "avg_pers"      ~ "2. Moyenne Pers. / Enquête",
      type_stat == "avg_ligne"     ~ "3. Moyenne Lignes / Enquête"
    ))
  
  # 2. Construction du graphique unique
  p <- ggplot(df_plot, aes(x = !!sym(var_etude), y = valeur, fill = factor(periodEchant))) +
    geom_bar(stat = "identity", position = "dodge", color = "white", linewidth = 0.2) +
    # facet_wrap avec ncol = 2 pour organiser l'espace
    facet_wrap(~ type_stat, scales = "free_y", ncol = 2) + 
    scale_fill_brewer(palette = "Set1") +
    theme_minimal() +
    labs(title = titre_global, x = "Catégorie", y = "Valeur", fill = "Campagne") +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  print(p)
}

# ------------------------------------------------------------------------------
# 4. PARTIE : WRAPPERS D'ANALYSE (AUTOMATISATION)
# Intérêt : Enchaînement automatique des calculs et des affichages pour produire 
# les sections du rapport de manière standardisée.
# ------------------------------------------------------------------------------

#' Comparaison inter-campagnes en proportions (%)
#' 
#' @description Wrapper générant les tableaux croisés Type vs Taille en proportions pour chaque campagne.
comparer_campagnes_proportions <- function(df_prepare, periodes, ordre_type_agg, ordre_taille_agg) {
  for (p in periodes) {
    df_p <- df_prepare %>% filter(periodEchant == p)
    total_enquetes_p <- nrow(df_p)
    tab_eff <- df_p %>% group_by(type_agg, taille_agg) %>% summarise(nb = n(), .groups = "drop") %>%
      pivot_wider(names_from = taille_agg, values_from = nb, values_fill = 0)
    
    cols_num <- setdiff(names(tab_eff), "type_agg")
    tab_prop <- tab_eff %>% mutate(TOTAL_LIGNE = rowSums(across(all_of(cols_num)))) %>%
      mutate(across(all_of(c(cols_num, "TOTAL_LIGNE")), ~ round(.x / total_enquetes_p * 100, 2)))
    
    tab_prop <- bind_rows(tab_prop, tab_prop %>% summarise(type_agg = "TOTAL", across(where(is.numeric), sum)))
    ord_lignes <- if("TOTAL" %in% ordre_type_agg) ordre_type_agg else c(ordre_type_agg, "TOTAL")
    ord_cols <- if("TOTAL" %in% ordre_taille_agg) ordre_taille_agg else c(ordre_taille_agg, "TOTAL_LIGNE")
    
    tab_affichage <- tab_prop %>% mutate(type_agg = factor(type_agg, levels = ord_lignes)) %>%
      filter(!is.na(type_agg)) %>% arrange(type_agg) %>% select(type_agg, any_of(ord_cols)) %>% rename(TOTAL = TOTAL_LIGNE)
    
    afficher_tableau_page(tab_affichage, paste("Répartition en % (Type vs Taille) - Campagne", p, "(N =", total_enquetes_p, ")"), col_groupe = "type_agg")
  }
}

#' Automatisation complète de l'analyse de fréquentation
#' 
#' @description Enchaîne le calcul, l'affichage des 3 tableaux (somme, jour, embarcation) et le graphique facetté.
analyser_et_afficher_frequentation <- function(df, var_interet, var_cat, label_interet, label_cat) {
  res <- calculer_stats_frequentation(df, var_interet, var_cat)
  ordre_cols <- recuperer_ordre_colonnes(df, var_cat)$ordre
  cat(paste0("\n\n\\FloatBarrier\n### ", label_interet, " par ", label_cat, "\n\n"))
  
  afficher_tableau_page(res$table_sum, paste("Total", label_interet, "par", label_cat), ordre_cols, "periodEchant")
  afficher_tableau_page(res$table_jour, paste("Moyenne journalière par", label_cat), ordre_cols, "periodEchant")
  afficher_tableau_page(res$table_embarq, paste("Moyenne par embarcation par", label_cat), ordre_cols, "periodEchant")
  
  afficher_cote_a_cote(res$data_long, var_cat, c("nbmy_jour", "nbmy_embarq"), paste(label_interet, ": Comparaison des moyennes"))
  cat("\n\n")
}

#' Wrapper complet d'analyse par variable d'intérêt
#' 
#' @description Enchaîne le calcul et l'affichage de tous les tableaux de synthèse incluant le poids relatif des enquêtes.
#' @param df Dataframe source préparé.
#' @param var_etude Nom technique de la variable (ex: "mois", "zonagePAMPA").
#' @param label_etude Libellé pour les titres des tableaux.
#' @param nb_col Optionnel. Vecteur numérique pour le découpage des colonnes.
analyser_et_afficher_synthese_variable <- function(df, var_etude, label_etude, nb_col = NULL) {
  res <- calculer_synthese_par_variable(df, var_etude)
  ordre_cols <- recuperer_ordre_colonnes(df, var_etude)$ordre
  
  cat(paste0("\n\n\\FloatBarrier\n## Synthèse générale par ", label_etude, "\n\n"))
  
  # Affichage des 6 tableaux (inchangé)
  afficher_tableau_page(res$jours, paste("Nombre de jours de sortie par", label_etude), ordre_cols, "periodEchant", nb_col)
  if (!is.null(res$couv)) {
    afficher_tableau_page(res$couv, "Couverture calendaire de la période (%)", ordre_cols, "periodEchant", nb_col)
  }
  afficher_tableau_page(res$effectifs, paste("Nombre d'enquêtes par", label_etude), ordre_cols, "periodEchant", nb_col)
  afficher_tableau_page(res$prop_enq, "Répartition des enquêtes (%)", ordre_cols, "periodEchant", nb_col)
  afficher_tableau_page(res$pers, "Moyenne nbPers/enquête", ordre_cols, "periodEchant", nb_col)
  afficher_tableau_page(res$ligne, "Moyenne nbLigne/enquête", ordre_cols, "periodEchant", nb_col)
  
  # AFFICHAGE DU GRAPHIQUE DE SYNTHÈSE
  afficher_synthese_graphique(
    df_stats = res$data_graph, 
    var_etude = var_etude, 
    titre_global = paste("Profil de fréquentation et puissance par", label_etude)
  )
  
  cat("\n\n")
}