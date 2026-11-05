# =============================================================================
# Pipeline : construire la base une fois, puis l'interroger pour les modèles
# =============================================================================
#
#   base   <- build_medicam_base()                                 # cache + journaux
#   series <- get_medicam_series(base, niveau = "atc", freq = "trimestre")
#   split  <- split_temporel(series, debut_test = "2024-01-01")
#
# Voir README.md pour le détail.

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

#' Lit la liste des ATC d'intérêt
#'
#' @param path Fichier CSV `code_atc, molecule, actif, commentaire`.
#' @param actif_only Ne garder que les lignes `actif = TRUE`.
#' @return Un vecteur de codes ATC.
get_liste_atc <- function(path = "config/atc_interet.csv", actif_only = TRUE) {
  if (!file.exists(path)) stop("Liste des ATC introuvable : ", path)
  atc <- read_csv(path, col_types = cols(.default = col_character()), comment = "#") %>%
    mutate(code_atc = str_squish(code_atc),
           actif = coalesce(as.logical(str_to_upper(str_squish(actif))), TRUE)) %>%
    filter(!is.na(code_atc), code_atc != "")
  if (actif_only) atc <- atc %>% filter(actif)
  unique(atc$code_atc)
}

# Lit la table des substituts (code_atc, code_atc_substitut)
.read_substitution <- function(path) {
  empty <- tibble(code_atc = character(), code_atc_substitut = character())
  if (is.null(path) || !file.exists(path)) return(empty)
  read_csv(path, col_types = cols(.default = col_character()), comment = "#") %>%
    transmute(code_atc = str_squish(code_atc), code_atc_substitut = str_squish(code_atc_substitut)) %>%
    filter(!is.na(code_atc), !is.na(code_atc_substitut), code_atc != code_atc_substitut) %>%
    distinct()
}

# -----------------------------------------------------------------------------
# Construction de la base (avec cache)
# -----------------------------------------------------------------------------

# Empreinte des entrées : si elle ne change pas, le cache est réutilisé
.signature_entrees <- function(paths, liste_atc) {
  paths <- unique(paths[!is.na(paths) & file.exists(paths)])
  md5 <- if (length(paths) > 0) unname(tools::md5sum(paths)) else character()
  paste(c(sort(paste(basename(paths), md5)), sort(liste_atc)), collapse = "|")
}

#' Construit la base Medic'AM enrichie (une ligne par CIP × mois) et la met en cache
#'
#' @description
#' Enchaîne `GET_dose_bdd()`, `parse_medicam_files()`, `add_dosage_info()` et
#' `check_ref_conflicts()`, enregistre la base en RDS et écrit des journaux de
#' qualité en CSV. Si aucune entrée n'a changé (fichiers Medic'AM, Thériaque,
#' tables de correction et de conversion, liste d'ATC, code R), le cache est relu.
#'
#' @param liste_atc Codes ATC à garder (défaut : `get_liste_atc()`).
#' @param medicam_dir Dossier des fichiers `.xls` Medic'AM.
#' @param path_to_theriaque Export Thériaque, ou `NULL`.
#' @param correction_path,conversion_ddd_path Tables de correction et de conversion DDD.
#' @param cache_dir Dossier du cache (base RDS + cache BDPM).
#' @param log_dir Dossier des journaux CSV.
#' @param rerun `TRUE` : reconstruit la base même si le cache est à jour.
#' @param rerun_bdpm `TRUE` : retélécharge la BDPM (sinon le cache BDPM est utilisé s'il existe).
#' @param strict Passé à `add_dosage_info()` ("error", "warn", "none").
#' @param ... Autres arguments passés à `add_dosage_info()` (ex. `exclude_forme_pharma`).
#'
#' @return La base (tibble), avec les attributs `unparsed`, `excluded`, `liste_atc`,
#'   `signature` et `date_construction`.
build_medicam_base <- function(liste_atc = get_liste_atc(),
                               medicam_dir = "data/medicam",
                               path_to_theriaque = NULL,
                               correction_path = "data/correction_dosage.csv",
                               conversion_ddd_path = "config/conversion_ddd.csv",
                               cache_dir = "data/fichier_intermediaire",
                               log_dir = "outpout/logs",
                               rerun = FALSE,
                               rerun_bdpm = FALSE,
                               strict = "warn",
                               ...) {

  base_path <- file.path(cache_dir, "medicam_base.rds")
  bdpm_path <- file.path(cache_dir, "base_dosage.csv")

  entrees <- c(
    list.files(medicam_dir, pattern = "\\.xls$", full.names = TRUE),
    path_to_theriaque, correction_path, conversion_ddd_path, bdpm_path,
    list.files("R", pattern = "\\.R$", full.names = TRUE)
  )
  signature <- .signature_entrees(entrees, liste_atc)

  # 1. Cache
  if (!rerun && !rerun_bdpm && file.exists(base_path)) {
    base <- readRDS(base_path)
    if (identical(attr(base, "signature"), signature)) {
      message("Base Medic'AM lue depuis le cache (", format(attr(base, "date_construction")), ") : ", base_path)
      return(base)
    }
    message("Entrées modifiées depuis le dernier cache : reconstruction de la base.")
  }

  # 2. Construction
  ref_cip <- GET_dose_bdd(rerun = rerun_bdpm || !file.exists(bdpm_path),
                          bdd_path = bdpm_path,
                          path_to_theriaque = path_to_theriaque)

  medoc <- parse_medicam_files(folder_path = medicam_dir, liste_atc = liste_atc)

  absents <- setdiff(liste_atc, unique(medoc$code_atc))
  if (length(absents) > 0) {
    warning("ATC sans aucune ligne dans Medic'AM : ", paste(absents, collapse = ", "))
  }

  base <- add_dosage_info(medoc, ref_cip, strict = strict,
                          correction_path = correction_path,
                          conversion_ddd_path = conversion_ddd_path, ...)
  conflits <- check_ref_conflicts(medoc, ref_cip, correction_path = correction_path)

  # 3. Journaux de qualité
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  write_csv(attr(base, "unparsed"), file.path(log_dir, "unparsed.csv"))
  write_csv(attr(base, "excluded"), file.path(log_dir, "excluded.csv"))
  write_csv(conflits, file.path(log_dir, "conflits.csv"))
  write_csv(couverture_medicam(base), file.path(log_dir, "couverture_atc.csv"))

  # 4. Cache
  attr(base, "liste_atc")         <- liste_atc
  attr(base, "signature")         <- signature
  attr(base, "date_construction") <- Sys.time()
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  # la signature est recalculée : base_dosage.csv peut avoir été créé à l'étape 2
  attr(base, "signature") <- .signature_entrees(entrees, liste_atc)
  saveRDS(base, base_path)
  message("Base Medic'AM construite : ", nrow(base), " lignes, ", n_distinct(base$cip13), " CIP, ",
          n_distinct(base$code_atc), " ATC. Journaux : ", log_dir)
  base
}

#' Couverture des doses par ATC (part des boîtes avec une dose / une DDD connue)
couverture_medicam <- function(base) {
  base %>%
    group_by(code_atc) %>%
    summarise(
      classe_atc      = last(classe_atc),
      nb_cip          = n_distinct(cip13),
      nb_boite_total  = sum(nb_boite, na.rm = TRUE),
      pct_boites_dose = sum(nb_boite[!is.na(total_dispensed_dose)], na.rm = TRUE) / nb_boite_total,
      pct_boites_ddd  = sum(nb_boite[!is.na(nb_ddd)], na.rm = TRUE) / nb_boite_total,
      unites_dose     = paste(sort(unique(na.omit(dose_unit))), collapse = ", "),
      sources_dose    = paste(sort(unique(na.omit(src_dose))), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(pct_boites_dose)
}

# -----------------------------------------------------------------------------
# Séries pour les modèles
# -----------------------------------------------------------------------------

.pas_periode <- function(freq) {
  switch(freq, mois = "1 month", trimestre = "3 months", semestre = "6 months", annee = "1 year")
}

.label_from_date <- function(date, freq) {
  .label_periode(as.integer(format(date, "%Y")), as.integer(format(date, "%m")), freq)
}

#' Renvoie des séries temporelles prêtes pour les modèles
#'
#' @param base Sortie de `build_medicam_base()` (ou de `add_dosage_info()`).
#' @param niveau `"atc"` ou `"cip"`.
#' @param freq `"mois"`, `"trimestre"`, `"semestre"` ou `"annee"`.
#' @param mesures Parmi `"nb_boite"`, `"total_dispensed_dose"`, `"nb_ddd"`.
#' @param atc Sous-ensemble de codes ATC (`NULL` : tous).
#' @param debut,fin Bornes de date (`"2018-01-01"`), incluses. `NULL` : pas de borne.
#' @param complete Rend les séries régulières :
#'   - niveau ATC : périodes manquantes à 0 sur toute la plage ;
#'   - niveau CIP : 0 entre la première et la dernière période où le CIP figure
#'     dans Medic'AM, NA en dehors (non commercialisé ≠ pénurie).
#' @param substitution Ajoute les mesures des ATC substituts (`config/substitution.csv`)
#'   dans des colonnes `subst_*`.
#' @param substitution_path Table des substituts.
#' @param covariables Covariables à joindre (voir `join_covariables()`).
#' @param covariables_agg Règle d'agrégation par covariable (voir `join_covariables()`).
#' @param calendrier Ajoute `annee`, `mois`, `trimestre`, `semestre`.
#' @param format `"long"` (une ligne par série × période) ou `"wide"`
#'   (une ligne par période, une colonne `<mesure>__<id>` par série).
#'
#' @return Un tibble : `id`, `libelle`, `code_atc`, `periode`, `date`, mesures,
#'   `pct_boites_dose`, `pct_boites_ddd`, `dose_unit`, puis substituts et covariables.
#'   Attributs : `niveau`, `freq`, `mesures`.
get_medicam_series <- function(base,
                               niveau = c("atc", "cip"),
                               freq = c("mois", "trimestre", "semestre", "annee"),
                               mesures = c("nb_boite", "total_dispensed_dose", "nb_ddd"),
                               atc = NULL,
                               debut = NULL, fin = NULL,
                               complete = TRUE,
                               substitution = FALSE,
                               substitution_path = "config/substitution.csv",
                               covariables = NULL,
                               covariables_agg = NULL,
                               calendrier = FALSE,
                               format = c("long", "wide")) {

  niveau <- match.arg(niveau)
  freq   <- match.arg(freq)
  format <- match.arg(format)
  mesures <- match.arg(mesures, several.ok = TRUE)

  # Substituts : on garde leurs ATC dans la base filtrée pour pouvoir les agréger
  subst_map <- if (substitution) .read_substitution(substitution_path) else NULL
  if (!is.null(atc)) {
    manquants <- setdiff(atc, unique(base$code_atc))
    if (length(manquants) > 0) warning("ATC absents de la base : ", paste(manquants, collapse = ", "))
  }

  series <- .series_brutes(base, niveau, freq, mesures, atc, debut, fin, complete)

  # Substituts
  if (substitution) {
    if (nrow(subst_map) == 0) {
      warning("substitution = TRUE mais aucune ligne active dans ", substitution_path)
    } else {
      series <- .joindre_substituts(series, base, subst_map, freq, mesures, debut, fin, complete)
    }
  }

  # Calendrier
  if (calendrier) {
    series <- series %>%
      mutate(annee     = as.integer(format(date, "%Y")),
             mois      = as.integer(format(date, "%m")),
             trimestre = as.integer(ceiling(mois / 3)),
             semestre  = as.integer(ceiling(mois / 6)))
  }

  attr(series, "niveau")  <- niveau
  attr(series, "freq")    <- freq
  attr(series, "mesures") <- mesures
  attr(series, "cip_atc") <- base %>% distinct(cip13, code_atc)

  # Covariables
  if (!is.null(covariables)) {
    series <- join_covariables(series, covariables, agg = covariables_agg)
  }

  if (format == "wide") series <- .series_wide(series)

  attr(series, "niveau")  <- niveau
  attr(series, "freq")    <- freq
  attr(series, "mesures") <- mesures
  series
}

# Agrège, renomme et complète les séries d'un niveau donné
.series_brutes <- function(base, niveau, freq, mesures, atc, debut, fin, complete) {
  if (!is.null(atc)) base <- base %>% filter(code_atc %in% atc)
  mesures_dose <- intersect(setdiff(mesures, "nb_boite"), names(base))

  agg <- aggregate_medicam(base, freq = freq, by_cip = niveau == "cip", mesures = mesures_dose)

  series <- if (niveau == "atc") {
    agg %>% mutate(id = code_atc, libelle = classe_atc)
  } else {
    agg %>% mutate(id = cip13, libelle = nom_court)
  }
  cols_mesures <- c(mesures, intersect(c("pct_boites_dose", "pct_boites_ddd", "dose_unit"), names(series)))
  cols_mesures <- intersect(cols_mesures, names(series))
  series <- series %>% select(id, libelle, code_atc, periode, date, all_of(cols_mesures))

  # Bornes : sur toute la base (pas seulement l'ATC), pour aligner toutes les séries
  date_min <- if (!is.null(debut)) as.Date(debut) else min(series$date)
  date_max <- if (!is.null(fin))   as.Date(fin)   else max(series$date)
  date_min <- .date_periode(as.integer(format(date_min, "%Y")), as.integer(format(date_min, "%m")), freq)

  if (complete && nrow(series) > 0) {
    grille <- seq(date_min, date_max, by = .pas_periode(freq))
    ids <- series %>%
      group_by(id) %>%
      summarise(libelle = last(libelle), code_atc = first(code_atc),
                premiere = min(date), derniere = max(date),
                dose_unit_id = if ("dose_unit" %in% names(series)) first(na.omit(dose_unit)) else NA_character_,
                .groups = "drop")

    plein <- tidyr::expand_grid(ids, date = grille) %>%
      left_join(series %>% select(-libelle, -code_atc, -periode) %>% mutate(.present = TRUE),
                by = c("id", "date")) %>%
      mutate(
        periode = .label_from_date(date, freq),
        # ATC : 0 partout ; CIP : 0 seulement pendant la période de présence
        actif = if (niveau == "atc") TRUE else date >= premiere & date <= derniere
      )
    # seules les périodes absentes de Medic'AM sont mises à 0 (une dose inconnue reste NA)
    absent <- is.na(plein$.present)
    for (m in intersect(mesures, names(plein))) {
      plein[[m]] <- if_else(absent & plein$actif, 0, plein[[m]])
    }
    if ("dose_unit" %in% names(plein)) plein$dose_unit <- coalesce(plein$dose_unit, plein$dose_unit_id)
    series <- plein %>%
      select(id, libelle, code_atc, periode, date, all_of(cols_mesures))
  }

  series %>%
    filter(date >= date_min, date <= date_max) %>%
    arrange(id, date)
}

# Ajoute les colonnes subst_* : somme des mesures des ATC substituts (même fréquence)
.joindre_substituts <- function(series, base, subst_map, freq, mesures, debut, fin, complete) {
  atc_base <- unique(base$code_atc)
  absents  <- setdiff(subst_map$code_atc_substitut, atc_base)
  if (length(absents) > 0) {
    warning("ATC substituts absents de la base (à ajouter dans config/atc_interet.csv) : ",
            paste(absents, collapse = ", "))
  }
  subst_map <- subst_map %>% filter(code_atc_substitut %in% atc_base)
  if (nrow(subst_map) == 0) return(series)

  subst <- .series_brutes(base, "atc", freq, mesures, unique(subst_map$code_atc_substitut),
                          debut, fin, complete = TRUE)
  mesures_subst <- intersect(mesures, names(subst))

  par_atc <- subst_map %>%
    inner_join(subst %>% select(-code_atc, -libelle, -periode), by = c("code_atc_substitut" = "id"),
               relationship = "many-to-many") %>%
    group_by(code_atc, date) %>%
    summarise(
      subst_atc = paste(sort(unique(code_atc_substitut)), collapse = "+"),
      across(all_of(intersect(mesures_subst, c("nb_boite", "nb_ddd"))), ~ sum(.x, na.rm = FALSE)),
      # dose : sommée seulement si tous les substituts ont la même unité
      total_dispensed_dose = if ("total_dispensed_dose" %in% mesures_subst) {
        if (n_distinct(na.omit(dose_unit)) <= 1) sum(total_dispensed_dose) else NA_real_
      } else NULL,
      .groups = "drop"
    ) %>%
    rename_with(~ paste0("subst_", .x), all_of(intersect(mesures_subst, c("nb_boite", "nb_ddd", "total_dispensed_dose"))))

  series %>% left_join(par_atc, by = c("code_atc", "date"))
}

# Format large : une ligne par période, colonnes <mesure>__<id>
.series_wide <- function(series) {
  cles_date <- attr(series, "covariables_date") %||% character()
  valeurs <- setdiff(names(series), c("id", "libelle", "code_atc", "periode", "date", "dose_unit", "subst_atc",
                                      cles_date, "annee", "mois", "trimestre", "semestre"))
  valeurs <- valeurs[vapply(series[valeurs], is.numeric, logical(1))]
  large <- series %>%
    select(id, periode, date, all_of(valeurs)) %>%
    tidyr::pivot_wider(names_from = id, values_from = all_of(valeurs), names_glue = "{.value}__{id}")
  communes <- intersect(c("annee", "mois", "trimestre", "semestre", cles_date), names(series))
  if (length(communes) > 0) {
    large <- large %>% left_join(series %>% distinct(date, across(all_of(communes))), by = "date")
  }
  large %>% arrange(date)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------------------
# Covariables
# -----------------------------------------------------------------------------

#' Joint des covariables à des séries
#'
#' @description
#' Le type de jointure est déduit des colonnes de la covariable :
#' | colonnes                 | jointure                                                 |
#' |--------------------------|----------------------------------------------------------|
#' | `date`                   | toutes les séries (ex. épidémie, confinement)            |
#' | `code_atc` + `date`      | par ATC (ex. période de pénurie)                         |
#' | `cip13` + `date`         | par CIP ; agrégée par ATC si `niveau = "atc"`            |
#' | `code_atc` ou `cip13`    | statique                                                 |
#'
#' Les dates sont ramenées au début de la période de la série (`freq`), puis les
#' valeurs sont agrégées avec la règle choisie (`"mean"` par défaut, `"max"` pour
#' les colonnes logiques, ex. indicatrice de pénurie).
#'
#' @param series Sortie de `get_medicam_series()` au format long.
#' @param covariables Liste nommée de data.frames ou de chemins CSV
#'   (ex. `list(grippe = "data/covariables/grippe.csv", penurie = df)`), ou un data.frame seul.
#' @param agg Liste nommée de règles (`"mean"`, `"sum"`, `"max"`, `"min"`, `"last"`),
#'   par covariable ou par colonne (ex. `list(penurie = "max")`).
#' @param freq Fréquence des séries (défaut : `attr(series, "freq")`).
#'
#' @return `series` avec une colonne par variable de covariable, préfixée par le nom
#'   de la covariable quand elle a plusieurs colonnes (`<nom>_<colonne>`).
#'   Les valeurs absentes restent NA.
join_covariables <- function(series, covariables, agg = NULL, freq = attr(series, "freq")) {

  if (is.null(freq)) stop("`freq` inconnue : passer freq = 'mois' / 'trimestre' / ...")
  niveau  <- attr(series, "niveau") %||% (if ("cip13" %in% names(series)) "cip" else "atc")
  cip_atc <- attr(series, "cip_atc")
  attrs   <- attributes(series)
  attrs   <- attrs[intersect(names(attrs), c("niveau", "freq", "mesures", "cip_atc", "covariables_date"))]

  if (is.data.frame(covariables)) covariables <- list(covariable = covariables)
  if (is.null(names(covariables)) || any(names(covariables) == "")) {
    stop("`covariables` doit être une liste nommée : list(nom = data.frame ou chemin CSV)")
  }

  fonctions <- list(
    mean = function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
    sum  = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE),
    max  = function(x) if (all(is.na(x))) NA else if (is.logical(x)) any(x, na.rm = TRUE) else max(x, na.rm = TRUE),
    min  = function(x) if (all(is.na(x))) NA else if (is.logical(x)) all(x, na.rm = TRUE) else min(x, na.rm = TRUE),
    last = function(x) dplyr::last(na.omit(x))
  )
  cov_date <- attrs$covariables_date %||% character()

  for (nom in names(covariables)) {
    cov <- covariables[[nom]]
    if (is.character(cov)) {
      entete <- names(read_csv(cov, n_max = 0, show_col_types = FALSE))
      types  <- rep(list(col_character()), sum(entete %in% c("cip13", "code_atc")))
      names(types) <- intersect(entete, c("cip13", "code_atc"))
      cov <- read_csv(cov, col_types = do.call(cols, c(types, list(.default = col_guess()))),
                      comment = "#", show_col_types = FALSE)
    }
    cov <- as_tibble(cov)
    if ("cip13" %in% names(cov)) cov$cip13 <- str_extract(as.character(cov$cip13), "\\d{13}")

    cles    <- intersect(c("cip13", "code_atc", "date"), names(cov))
    valeurs <- setdiff(names(cov), cles)
    if (length(valeurs) == 0) stop("Covariable '", nom, "' : aucune colonne de valeur.")
    if (!any(c("cip13", "code_atc", "date") %in% cles)) {
      stop("Covariable '", nom, "' : il faut au moins une colonne date, code_atc ou cip13.")
    }

    # Noms de sortie
    noms_sortie <- if (length(valeurs) == 1) nom else paste0(nom, "_", valeurs)
    cov <- cov %>% rename_with(~ noms_sortie, all_of(valeurs))

    # Règle d'agrégation par colonne
    regle <- function(col) {
      r <- agg[[col]] %||% agg[[nom]] %||% (if (is.logical(cov[[col]])) "max" else "mean")
      if (!r %in% names(fonctions)) stop("Règle d'agrégation inconnue : ", r)
      fonctions[[r]]
    }

    # Dates ramenées au début de période
    if ("date" %in% cles) {
      d <- as.Date(cov$date)
      cov$date <- .date_periode(as.integer(format(d, "%Y")), as.integer(format(d, "%m")), freq)
    }

    # CIP -> ATC si les séries sont au niveau ATC
    if ("cip13" %in% cles && niveau == "atc") {
      if (is.null(cip_atc)) stop("Covariable '", nom, "' par CIP : correspondance CIP -> ATC inconnue.")
      cov <- cov %>% inner_join(cip_atc, by = "cip13") %>% select(-cip13)
      cles <- union(setdiff(cles, "cip13"), "code_atc")
    }
    # Agrégation sur les clés (plusieurs dates d'une même période, plusieurs CIP d'un ATC)
    cov <- cov %>%
      group_by(across(all_of(cles))) %>%
      summarise(across(all_of(noms_sortie), ~ regle(cur_column())(.x)), .groups = "drop")

    # Jointure : colonne de la série <- colonne de la covariable
    cle_serie <- c(cip13 = "id", code_atc = "code_atc", date = "date")
    by <- setNames(cles, cle_serie[cles])
    deja <- intersect(noms_sortie, names(series))
    if (length(deja) > 0) stop("Covariable '", nom, "' : colonnes déjà présentes : ", paste(deja, collapse = ", "))
    series <- series %>% left_join(cov, by = by)

    if (identical(cles, "date")) cov_date <- c(cov_date, noms_sortie)
  }

  for (a in names(attrs)) attr(series, a) <- attrs[[a]]
  attr(series, "covariables_date") <- unique(cov_date)
  series
}

# -----------------------------------------------------------------------------
# Sorties, découpage, graphiques
# -----------------------------------------------------------------------------

#' Écrit des séries en RDS + CSV, avec un fichier de métadonnées
#'
#' @param series Sortie de `get_medicam_series()`.
#' @param dir Dossier de sortie.
#' @param nom Nom de fichier sans extension (défaut : `<niveau>_<freq>`, ex. `atc_trimestre`).
#' @return Les chemins écrits (invisible).
write_medicam_series <- function(series, dir = "outpout/series", nom = NULL) {
  niveau <- attr(series, "niveau") %||% "series"
  freq   <- attr(series, "freq") %||% "freq"
  nom    <- nom %||% paste(niveau, freq, sep = "_")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  chemins <- file.path(dir, paste0(nom, c(".rds", ".csv", "_metadata.json")))
  saveRDS(series, chemins[1])
  write_csv(series, chemins[2], na = "")

  meta <- list(
    date_ecriture = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    niveau        = niveau,
    freq          = freq,
    mesures       = attr(series, "mesures"),
    nb_series     = if ("id" %in% names(series)) n_distinct(series$id) else NA,
    periode_debut = as.character(min(series$date)),
    periode_fin   = as.character(max(series$date)),
    nb_lignes     = nrow(series),
    colonnes      = names(series),
    covariables_date = attr(series, "covariables_date")
  )
  jsonlite::write_json(meta, chemins[3], auto_unbox = TRUE, pretty = TRUE)
  message("Séries écrites : ", paste(chemins, collapse = ", "))
  invisible(chemins)
}

#' Découpe des séries en apprentissage / test sur la date
#'
#' @param series Sortie de `get_medicam_series()`.
#' @param debut_test Première date de la période de test (incluse).
#' @param fin_test Dernière date de test (incluse), `NULL` : jusqu'à la fin.
#' @return `list(train, test)`.
split_temporel <- function(series, debut_test, fin_test = NULL) {
  debut_test <- as.Date(debut_test)
  fin_test   <- if (is.null(fin_test)) max(series$date) else as.Date(fin_test)
  out <- list(
    train = series %>% filter(date <  debut_test),
    test  = series %>% filter(date >= debut_test, date <= fin_test)
  )
  for (k in names(out)) {
    for (a in c("niveau", "freq", "mesures", "covariables_date")) attr(out[[k]], a) <- attr(series, a)
  }
  message(sprintf("train : %s -> %s (%d lignes) | test : %s -> %s (%d lignes)",
                  min(out$train$date), max(out$train$date), nrow(out$train),
                  min(out$test$date), max(out$test$date), nrow(out$test)))
  out
}

#' Un graphique PDF par série
#'
#' @param series Sortie de `get_medicam_series()` au format long.
#' @param mesure Colonne à tracer.
#' @param dir Dossier de sortie.
plot_medicam_series <- function(series, mesure = "nb_boite", dir = "outpout/plots_atc") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  titres_y <- c(nb_boite = "Nombre de boîtes remboursées", total_dispensed_dose = "Dose totale dispensée",
                nb_ddd = "Nombre de DDD")
  freq <- attr(series, "freq") %||% "mois"

  series %>%
    group_split(id) %>%
    walk(function(df_sub) {
      unite <- if ("dose_unit" %in% names(df_sub) && mesure == "total_dispensed_dose") {
        paste0(" (", first(na.omit(df_sub$dose_unit)), ")")
      } else ""
      p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = date, y = .data[[mesure]])) +
        ggplot2::geom_line(color = "#2b5c8f", linewidth = 1) +
        ggplot2::geom_point(color = "#2b5c8f", size = 2) +
        ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = if (freq == "mois") "3 months" else "6 months") +
        ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
        ggplot2::labs(
          title = paste("Évolution -", df_sub$libelle[1]),
          subtitle = paste("Identifiant :", df_sub$id[1]),
          x = "Date", y = paste0(titres_y[[mesure]] %||% mesure, unite)
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13),
                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      ggplot2::ggsave(file.path(dir, paste0("plot_", df_sub$id[1], "_", mesure, ".pdf")),
                      plot = p, width = 9, height = 5, device = "pdf")
    })
  invisible(NULL)
}
