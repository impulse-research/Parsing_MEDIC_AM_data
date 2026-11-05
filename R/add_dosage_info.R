library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)

# =============================================================================
# Paramètres par défaut
# =============================================================================

# Formes topiques à exclure (regex, comparées sans accents ni casse) :
# - sur `forme_pharmaceutique` de la BDPM
# - sur le libellé long ANSM de Thériaque
# "gel\\b" ne matche pas "gélule" ; "(?<!sous.)cutane" exclut "sous-cutanée" (injectable) ;
# "(?<!trans)dermique" exclut les patchs transdermiques (systémiques)
.default_topiques <- c(
  "collyre", "pommade", "creme", "gel\\b", "lotion", "(?<!sous.)cutane",
  "(?<!trans)dermique", "ophtalm", "auricul", "nasal", "vaginal", "ovule", "shampo"
)

# Même logique sur les libellés Medic'AM (`nom_court`, en majuscules, abréviations)
# "GEL" seul n'est PAS utilisé : dans Medic'AM il signifie souvent "gélule"
.default_topiques_label <- paste0(
  "COLLYRE|\\bCOLL\\b|POMMADE|\\bPOMM?\\b|CREME|LOTION|\\bOPH|AURIC|NASAL|\\bNAS\\b|",
  "OVULE|VAGIN|\\bVAG\\b|(?<!SOUS.)CUTANE|\\bCUT\\b|(?<!TRANS)DERMIQUE|SHAMPO|",
  "GEL\\s+(?:CUT|DERM|NAS|VAG|OPH|BUCC|GING)"
)

# =============================================================================
# Helpers de parsing (partagés avec R/check_dosage.R)
# =============================================================================

.ascii <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

# Unités de masse/quantité reconnues (ordre important : MCG avant MG avant G)
.regex_dose_unit <- "MICROGRAMMES?|MCG|µG|UG|MUI|MG|G|UI|MMOL|%"

# Un composant de dosage : "80", "12.5MG", "1 G", "5 ML" (pas de \\b : "12.5MG" doit matcher)
.regex_dose_comp <- paste0("\\d+(?:\\.\\d+)?\\s*(?:", .regex_dose_unit, "|ML)?(?![A-Z])")

# Une chaîne de composants séparés par "/" : "80/12.5MG", "250MG/5ML", "100MG/12.5MG/ML"
.regex_dose_chain <- paste0(.regex_dose_comp, "(?:\\s*/\\s*(?:", .regex_dose_comp, "|ML(?![A-Z])))*")

# Mise en forme commune : majuscules, virgule décimale -> point, "P. 100" -> "%", "U.I." -> "UI"
.normalise_label <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("(?<=\\d),(?=\\d)", ".") %>%
    str_replace_all("(\\d+(?:\\.\\d+)?)\\s*(?:P\\.?\\s*100|POUR CENT)", "\\1 %") %>%
    str_replace_all("\\bU\\s?\\.\\s?[IL]\\s?\\.?(?![A-Z])", "UI") %>%
    str_squish()
}

# Conversion d'une valeur dans l'unité cible (mg quand c'est possible)
.to_mg <- function(value, unit) {
  if (is.na(unit) || is.na(value))                      return(list(NA_real_, NA_character_))
  if (unit == "G")                                      return(list(value * 1000, "mg"))
  if (unit == "MG")                                     return(list(value, "mg"))
  if (unit %in% c("MCG", "µG", "UG") || str_detect(unit, "^MICROGRAMME")) return(list(value / 1000, "mg"))
  if (unit == "UI")                                     return(list(value, "ui"))
  if (unit == "MUI")                                    return(list(value * 1e6, "ui"))
  if (unit == "MMOL")                                   return(list(value, "mmol"))
  if (unit == "%")                                      return(list(value, "%"))
  list(NA_real_, NA_character_)
}

#' Somme des composants (règle B) : unités homogènes -> somme ;
#' unités mélangées (ex. mg + UI) -> somme des seuls composants en mg
.combine_doses <- function(vals, units) {
  ok <- !is.na(vals) & !is.na(units)
  if (!any(ok)) return(list(value = NA_real_, unit = NA_character_))
  vals <- vals[ok]; units <- units[ok]
  if (n_distinct(units) > 1) {
    if (!"mg" %in% units) return(list(value = NA_real_, unit = NA_character_))
    vals <- vals[units == "mg"]; units <- units[units == "mg"]
  }
  list(value = sum(vals), unit = units[1])
}

#' Parse une chaîne de dosage et somme ses composants
#'
#' "80/12.5MG" -> 92.5 mg ; "1G/125MG" -> 1125 mg ; "250MG/5ML" -> 250 mg pour 5 ml.
#' Un composant sans unité hérite de l'unité suivante (ou précédente à défaut).
#'
#' @return list(value, unit, per_ml) ; per_ml = volume de référence en ml si concentration, sinon NA
.parse_dose_chain <- function(chain) {
  empty <- list(value = NA_real_, unit = NA_character_, per_ml = NA_real_)
  if (is.na(chain) || chain == "") return(empty)

  comps <- str_split(chain, "\\s*/\\s*")[[1]]
  num   <- suppressWarnings(as.numeric(str_extract(comps, "^\\d+(?:\\.\\d+)?")))
  unit  <- str_extract(comps, paste0("(?:", .regex_dose_unit, "|ML)$"))

  # Dénominateur volumique (concentration) : "/5ML", "/ML"
  is_ml  <- !is.na(unit) & unit == "ML"
  per_ml <- if (any(is_ml)) coalesce(num[is_ml][1], 1) else NA_real_

  num  <- num[!is_ml]
  unit <- unit[!is_ml]
  if (length(num) == 0 || all(is.na(num))) return(empty)

  # Héritage d'unité : d'abord vers la droite ("80/12.5MG"), sinon vers la gauche
  for (i in rev(seq_along(unit))) {
    if (is.na(unit[i]) && i < length(unit)) unit[i] <- unit[i + 1]
  }
  for (i in seq_along(unit)) {
    if (is.na(unit[i]) && i > 1) unit[i] <- unit[i - 1]
  }
  if (any(is.na(unit)) || any(is.na(num))) return(empty)

  conv <- map2(num, unit, .to_mg)
  dose <- .combine_doses(map_dbl(conv, 1), map_chr(conv, 2))
  if (is.na(dose$value)) return(empty)

  list(value = dose$value, unit = dose$unit, per_ml = per_ml)
}

#' Parse un libellé Medic'AM (`nom_court`)
#'
#' Gère les 3 formats rencontrés :
#' - ancien format court   : "AMOXICILLINE MYLAN 250MG/5ML PDR FL 1/60 ML"
#' - format court récent    : "VALSARTAN/HCTZ AHCL 80/12,5MG  30"
#' - format long tronqué    : "ATENOLOL BIOGARAN 100 MG 1 BOITE DE 28, COMPRIMES"
#'
#' @return list(unit_count_per_box, unit_dose, dose_unit, conc_per_ml)
#'   `conc_per_ml` : concentration (dans `dose_unit`) par ml pour les formes liquides,
#'   utilisée avec la colonne `volume_ml` de la table de correction.
parse_label <- function(label) {
  out <- list(unit_count_per_box = NA_real_, unit_dose = NA_real_,
              dose_unit = NA_character_, conc_per_ml = NA_real_)
  if (is.na(label) || str_squish(label) == "") return(out)

  x <- .normalise_label(label)

  # Libellé tronqué au milieu du dosage ("… 500 MG/62,") -> somme non fiable
  dosage_tronque <- str_detect(x, "/\\s*\\d+(?:\\.\\d*)?,?$")
  volume_ml <- NA_real_
  count_fin_libelle <- FALSE

  # --- A. CONDITIONNEMENT (puis retiré du libellé avant le parsing du dosage) ---

  # A1. Ancien format "… 1/60 ML", "… 8/1.57 G", "… 12/3 G"
  m <- str_match(x, "\\s(\\d+)\\s*/\\s*(\\d+(?:\\.\\d+)?)\\s*(ML|G|MG)$")
  if (!is.na(m[1, 1])) {
    out$unit_count_per_box <- as.numeric(m[1, 2])
    if (m[1, 4] == "ML") volume_ml <- as.numeric(m[1, 3])
    x <- str_sub(x, 1, nchar(x) - nchar(m[1, 1]))
  }

  # A2. Format long "… 1 BOITE DE 28, COMPRIMES", "… 1 FLACON DE 60 ML A"
  if (is.na(out$unit_count_per_box)) {
    pos <- str_locate(x, "\\s\\d+\\s+(?:BOITE|FLACON|AMPOULE|SERINGUE|STYLO|TUBE|POCHE|RECIPIENT)")[1, "start"]
    if (!is.na(pos)) {
      cond <- str_sub(x, pos)
      x    <- str_sub(x, 1, pos - 1)

      n_boite     <- str_match(cond, "BOITE DE (\\d+)")[1, 2]
      n_contenant <- str_match(cond, "^\\s(\\d+)\\s+(?:FLACON|AMPOULE|SERINGUE|STYLO|TUBE|POCHE|RECIPIENT)")[1, 2]
      out$unit_count_per_box <- as.numeric(coalesce(n_boite, n_contenant))

      vol <- str_match(cond, "(?:FLACON|AMPOULE|SERINGUE|POCHE|TUBE)S?\\s+DE\\s+(\\d+(?:\\.\\d+)?)\\s*ML")[1, 2]
      if (!is.na(vol)) volume_ml <- as.numeric(vol)
    }
  }

  # A3. "… 30 CPR", "… 12 GELULES"
  if (is.na(out$unit_count_per_box)) {
    m <- str_match(x, "\\s(\\d+)\\s*(?:CPR|CP|COMPRIMES?|GELULES?|GEL|CAPSULES?|SACHETS?)$")
    if (!is.na(m[1, 1])) {
      out$unit_count_per_box <- as.numeric(m[1, 2])
      x <- str_sub(x, 1, nchar(x) - nchar(m[1, 1]))
    }
  }

  # A4. Nombre isolé en fin de libellé "… GEL  30", "… PDR FL 1" (pas après un "/")
  if (is.na(out$unit_count_per_box) && !dosage_tronque) {
    m <- str_match(x, "(?<!/)\\s(\\d+)$")
    if (!is.na(m[1, 1])) {
      out$unit_count_per_box <- as.numeric(m[1, 2])
      count_fin_libelle <- TRUE
      x <- str_sub(x, 1, nchar(x) - nchar(m[1, 1]))
    }
  }

  # A5. Volume explicite "60 ML EN FLACON"
  if (is.na(volume_ml)) {
    vol <- str_match(x, "(\\d+(?:\\.\\d+)?)\\s*ML\\s+EN\\s+FLACON")[1, 2]
    if (!is.na(vol)) volume_ml <- as.numeric(vol)
  }

  # A6. Volume isolé : "BUV 40ML", "FL 40ML" (hors dénominateur de concentration "/5ML")
  #     Non utilisé pour les poudres à reconstituer ni les dosages "/5ML" (voir B) :
  #     "BUV 54ML" y désigne le godet doseur, pas le volume de suspension
  volume_isole <- NA_real_
  if (is.na(volume_ml)) {
    x_sans_conc <- str_remove_all(x, "/\\s*\\d*(?:\\.\\d+)?\\s*ML(?![A-Z])")
    vol <- str_match(x_sans_conc, "(\\d+(?:\\.\\d+)?)\\s*ML(?![A-Z])")[1, 2]
    if (!is.na(vol)) volume_isole <- as.numeric(vol)
  }
  poudre <- str_detect(x, "\\b(?:PDR|PDRE|POUDRE)\\b")

  # --- B. DOSAGE ---
  if (dosage_tronque) return(out)

  chains <- str_extract_all(x, .regex_dose_chain)[[1]]
  # Première chaîne contenant une vraie unité de dose (exclut "5ML" seul, "SACH 2", …)
  chains <- chains[str_detect(chains, paste0("(?:", .regex_dose_unit, ")(?![A-Z])"))]
  if (length(chains) == 0) return(out)

  dose <- .parse_dose_chain(chains[1])
  if (is.na(dose$value)) return(out)

  # Volume isolé retenu seulement hors poudre et pour une concentration "/ML" (ex. 200 MG/ML)
  if (is.na(volume_ml) && !is.na(volume_isole) && !poudre && coalesce(dose$per_ml, 1) < 2) {
    volume_ml <- volume_isole
  }

  # Pourcentage -> concentration en mg/ml (1 % = 10 mg/ml) si un volume est connu
  if (dose$unit == "%" && !is.na(volume_ml)) {
    dose <- list(value = dose$value * 10, unit = "mg", per_ml = 1)
  }

  # Poudre + solvant ("CLAMOXYL 500MG/5ML PDR ET SOL INJ IM") : le "/5ML" est le volume
  # de solvant, pas une concentration -> dose par flacon
  if (!is.na(dose$per_ml) &&
      str_detect(x, "\\b(?:PDR|PDRE|POUDRE|LYOPH[A-Z]*)\\s*(?:ET|\\+)\\s*(?:SOL|SOLV|SOLVANT)\\b")) {
    dose$per_ml <- NA_real_
  }

  # Suspension / sirop sans "/ML" dans le libellé : le dosage est une concentration
  # dont on ignore le volume de référence -> non calculable
  if (is.na(dose$per_ml) && str_detect(x, "\\b(?:SUSP|SIROP|SOL BUV)\\b")) {
    out$dose_unit <- dose$unit
    return(out)
  }

  if (!is.na(dose$per_ml)) {
    # Forme liquide : dose par contenant = concentration × volume
    out$dose_unit   <- dose$unit
    out$conc_per_ml <- dose$value / dose$per_ml
    out$unit_dose   <- if (!is.na(volume_ml)) out$conc_per_ml * volume_ml else NA_real_

    # "PDR FL 12" : sur un flacon, le nombre final est ambigu (souvent une masse de poudre)
    if (count_fin_libelle && coalesce(out$unit_count_per_box, 0) > 1) out$unit_count_per_box <- NA_real_
    # Volume explicite sans nombre de contenants ("60 ML EN FLACON") -> 1 flacon
    if (is.na(out$unit_count_per_box) && !is.na(volume_ml)) out$unit_count_per_box <- 1
  } else {
    out$dose_unit <- dose$unit
    out$unit_dose <- dose$value
  }

  out
}

#' Parse chaque libellé distinct de `data` (un CIP peut avoir plusieurs libellés selon les fichiers)
parse_labels_table <- function(data) {
  data %>%
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}")) %>%
    filter(!is.na(cip13_clean), !is.na(nom_court)) %>%
    distinct(cip13_clean, nom_court) %>%
    mutate(parsed = map(nom_court, parse_label)) %>%
    unnest_wider(parsed) %>%
    mutate(label_long = nchar(nom_court) >= 50)  # format long, probablement tronqué
}

# =============================================================================
# Références : BDPM et Thériaque
# =============================================================================

# `ref_cip` peut être le data.frame BDPM (historique) ou list(bdpm, theriaque)
.split_ref <- function(ref_cip) {
  if (is.data.frame(ref_cip)) return(list(bdpm = ref_cip, theriaque = NULL))
  if (is.list(ref_cip) && any(c("bdpm", "theriaque") %in% names(ref_cip))) {
    return(list(bdpm = ref_cip$bdpm, theriaque = ref_cip$theriaque))
  }
  stop("ref_cip doit être un data.frame BDPM ou une list(bdpm = ..., theriaque = ...)")
}

.empty_bdpm_dose <- function() {
  tibble(cip13_clean = character(), unit_count_per_box = numeric(), unit_dose = numeric(),
         dose_unit = character(), unit_dose_sa = numeric(), dose_unit_sa = character(),
         conc_per_ml = numeric(), conc_unit = character(), forme_pharmaceutique = character())
}

.empty_theriaque_dose <- function() {
  tibble(cip13_clean = character(), count_th = numeric(), dose_th = numeric(), unit_th = character(),
         ddd = numeric(), ddd_unit = character(), libelle_long = character(), atc_th = character())
}

#' Calcule le dosage par CIP13 depuis la référence BDPM
#'
#' - somme des substances par CIP, en 2 variantes :
#'   * FT : fraction thérapeutique (base) préférée à la SA correspondante
#'   * SA : substance active (souvent le sel) préférée à la FT quand elle est dosée
#'   `add_dosage_info()` retient la variante la plus proche du libellé (dosage nominal)
#' - formes liquides : dosage "pour X ml" × volume du contenant ;
#'   si le volume est inconnu (poudre "flacon de 12 g") -> NA plutôt qu'une valeur fausse
#' - semi-solides : dosage "pour X g" × masse du tube
#' - nombre d'unités recalculé depuis `presentation_libelle`
#'   ("3 plaquette(s) de 10 comprimé(s)" -> 30 ; "tube de 3,5 g" n'est pas un nombre d'unités)
#'
#' @param cips CIP13 à conserver (filtre précoce pour la performance)
build_ref_dosage <- function(ref_cip, cips = NULL) {

  if (is.null(ref_cip)) return(.empty_bdpm_dose())

  required <- c("cip13", "composition_dosage", "composition_reference_dosage",
                "composition_nature_composant", "composition_denomination_substance",
                "presentation_libelle")
  missing_cols <- setdiff(required, names(ref_cip))
  if (length(missing_cols) > 0) {
    stop("Colonnes absentes de ref_cip : ", paste(missing_cols, collapse = ", "))
  }
  if (!"forme_pharmaceutique" %in% names(ref_cip)) ref_cip$forme_pharmaceutique <- NA_character_
  if (!"nb_unites" %in% names(ref_cip))            ref_cip$nb_unites <- NA_real_

  ref <- ref_cip %>%
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}")) %>%
    filter(!is.na(cip13_clean))
  if (!is.null(cips)) ref <- ref %>% filter(cip13_clean %in% cips)
  if (nrow(ref) == 0) return(.empty_bdpm_dose())

  # Racines (6 lettres) d'un nom de substance, hors sels / hydrates
  racines_exclues <- c("sodium", "potass", "calciu", "magnes", "chlorh", "hydrat",
                       "trihyd", "monohy", "dihydr", "anhydr", "phosph", "acetat", "sodiqu")
  racines <- function(noms) {
    noms %>% str_to_lower() %>% str_extract_all("[a-zà-ÿ]{6,}") %>% unlist() %>%
      str_sub(1, 6) %>% setdiff(racines_exclues) %>% unique()
  }

  # Choix SA / FT pour chaque paire (SA, FT) d'une même substance
  # ex. "amoxicilline trihydratée" (SA) / "amoxicilline" (FT)
  garder_variant <- function(nature, denomination, value, variant) {
    keep  <- rep(TRUE, length(nature))
    nat   <- coalesce(nature, "")
    denom <- str_to_lower(coalesce(denomination, ""))
    for (i in which(nat == "FT")) {
      r <- racines(denomination[i])
      if (length(r) == 0) next
      sa <- which(nat == "SA" & map_lgl(denom, ~ any(str_detect(.x, fixed(r)))))
      if (length(sa) == 0) next
      if (variant == "sa" && any(!is.na(value[sa]))) {
        keep[i] <- FALSE
        keep[sa[is.na(value[sa])]] <- FALSE
      } else {
        keep[sa] <- FALSE
      }
    }
    keep
  }

  # 1. Une ligne par CIP × substance, dosage converti
  compo <- ref %>%
    distinct(cip13_clean, composition_denomination_substance, composition_dosage,
             composition_reference_dosage, composition_nature_composant) %>%
    mutate(
      # "350 000 UI" -> "350000 UI", puis même parser que les libellés
      dosage_norm = composition_dosage %>%
        str_replace_all("(?<=\\d)\\s(?=\\d{3}\\b)", "") %>%
        .normalise_label(),
      parsed = map(dosage_norm, ~ .parse_dose_chain(str_extract(.x, .regex_dose_chain))),
      value  = map_dbl(parsed, "value"),
      unit   = map_chr(parsed, "unit"),
      dosage_vide = is.na(composition_dosage) | str_squish(composition_dosage) == ""
    )

  # 2. Base de référence du dosage (par CIP)
  reference <- compo %>%
    mutate(ref_norm = str_to_lower(composition_reference_dosage)) %>%
    group_by(cip13_clean) %>%
    summarise(ref_norm = first(na.omit(na_if(ref_norm, ""))), .groups = "drop") %>%
    mutate(
      # "5 ml de suspension" -> 5 ; "1 ml" -> 1 ; "ml" seul -> 1
      ref_ml = case_when(
        str_detect(ref_norm, "\\d+(?:[.,]\\d+)?\\s*ml") ~
          as.numeric(str_replace(str_extract(ref_norm, "\\d+(?:[.,]\\d+)?(?=\\s*ml)"), ",", ".")),
        str_detect(ref_norm, "\\bml\\b") ~ 1,
        TRUE ~ NA_real_
      ),
      # "100 g de pommade" -> 100
      ref_g = as.numeric(str_replace(str_extract(ref_norm, "\\d+(?:[.,]\\d+)?(?=\\s*g\\b)"), ",", ".")),
      # Dosage exprimé par unité / contenant ("un comprimé", "une ampoule", "un sachet")
      # (la seringue n'est une unité que pour les injectables : voir `base`)
      ref_per_unit_mot = str_detect(coalesce(ref_norm, ""),
        "comprim|gélule|gelule|capsule|sachet|ampoule|flacon|poche|récipient|recipient|stylo|suppositoire|ovule|lyophilisat|dispositif|patch|unidose|pastille|tube") &
        !str_detect(coalesce(ref_norm, ""), "cuill|goutte|pulvéris|pulveris|mesure|pipette"),
      ref_seringue = str_detect(coalesce(ref_norm, ""), "seringue") &
        !str_detect(coalesce(ref_norm, ""), "cuill|goutte|mesure|pipette|graduée|graduee|orale")
    )

  # 3. Conditionnement (une ligne par CIP)
  mots_unites <- "comprimé|comprime|gélule|gelule|capsule|sachet|ampoule|suppositoire|ovule|lyophilisat|pastille|dispositif|patch|unidose|dose"
  mots_contenants <- "flacon|ampoule|seringue|poche|récipient|recipient|stylo|tube"
  num <- function(x) as.numeric(str_replace(x, ",", "."))

  presentation <- ref %>%
    distinct(cip13_clean, .keep_all = TRUE) %>%
    transmute(
      cip13_clean,
      forme_pharmaceutique,
      forme     = str_to_lower(coalesce(forme_pharmaceutique, "")),
      libelle   = str_to_lower(coalesce(presentation_libelle, "")),
      nb_unites_old = suppressWarnings(as.numeric(nb_unites)),
      # "3 plaquette(s) ... de 10 comprimé(s)" -> 3 × 10
      m_prod    = str_match(libelle, paste0(
        "(\\d+)\\s+(?:plaquette|tube|flacon|pilulier|boîte|boite|étui|etui|récipient|recipient|film)s?(?:\\(s\\))?",
        "[^\\d]*?\\bde\\s+(\\d+)\\s*(?:", mots_unites, ")")),
      n_produit = as.numeric(m_prod[, 2]) * as.numeric(m_prod[, 3]),
      # "30 comprimé(s)", "5 ampoule(s)"
      n_direct  = as.numeric(str_extract(libelle, paste0("\\d+(?=\\s*(?:", mots_unites, "))"))),
      n_contenants = as.numeric(str_extract(libelle, paste0("\\d+(?=\\s*(?:", mots_contenants, "))"))),
      # Volume du contenant : les volumes des dispositifs doseurs ne comptent pas
      # ("seringue pour administration orale 5 mL", "godet doseur gradué à 54 mL") ;
      # "12 cuillères-mesure de 5 ml" -> 12 × 5 = 60 ml
      m_cuill   = str_match(libelle, "(\\d+)\\s+cuill[eè]res?[- ]?mesures?\\s+de\\s+(\\d+(?:[.,]\\d+)?)\\s*ml\\b"),
      volume_cuilleres = as.numeric(m_cuill[, 2]) * num(m_cuill[, 3]),
      libelle_sans_doseur = str_remove_all(libelle,
        "(?:seringue|cuill[eè]re|godet|gobelet|pipette|compte-gouttes|doseur|dosette|mesure)[^,;+]{0,60}?\\d+(?:[.,]\\d+)?\\s*ml\\b"),
      volume_ml = coalesce(volume_cuilleres,
                           num(str_extract(libelle_sans_doseur, "\\d+(?:[.,]\\d+)?(?=\\s*ml\\b)"))),
      masse_g   = num(str_extract(libelle, "(?<=de\\s)\\d+(?:[.,]\\d+)?(?=\\s*g\\b)")),
      # l'ancien nb_unites n'est gardé que s'il ne vient pas d'un "de X g" / "de X ml"
      nb_unites_ok = if_else(
        !is.na(nb_unites_old) &
          str_detect(libelle, paste0("de\\s+", nb_unites_old, "(?:[.,]\\d+)?\\s*(?:g|ml|mg)\\b")),
        NA_real_, nb_unites_old)
    ) %>%
    select(-m_prod, -m_cuill, -libelle_sans_doseur)

  base <- presentation %>%
    left_join(reference, by = "cip13_clean") %>%
    mutate(
      semi_solide = !is.na(ref_g) & !is.na(masse_g) &
        str_detect(paste(forme, coalesce(ref_norm, "")), "pommade|crème|creme|gel\\b|pâte|pate|onguent"),
      forme_liquide = str_detect(forme, "suspension|solution|sirop|émulsion|emulsion|collyre|buvable|injectable|gouttes"),
      ref_per_unit  = ref_per_unit_mot | (ref_seringue & str_detect(forme, "inject|perfus")),
      # Garde-fou : un flacon buvable multidose contient plus d'une dose de référence
      volume_ml = if_else(!is.na(ref_ml) & !is.na(volume_ml) & volume_ml <= ref_ml &
                            str_detect(forme, "buvable"), NA_real_, volume_ml),
      base_contenant = !is.na(ref_ml) | semi_solide |
        str_detect(coalesce(ref_norm, ""), mots_contenants),
      unit_count_per_box = coalesce(
        n_produit, n_direct,
        if_else(base_contenant, coalesce(n_contenants, 1), NA_real_),
        nb_unites_ok
      )
    )

  # 4. Somme des substances, pour une variante (FT ou SA), puis dose par unité
  dose_variant <- function(variant) {
    compo %>%
      group_by(cip13_clean) %>%
      filter(garder_variant(composition_nature_composant, composition_denomination_substance, value, variant)) %>%
      # une SA/FT sans dosage renseigné n'empêche pas la somme des autres
      filter(!(is.na(value) & dosage_vide)) %>%
      summarise(
        tout_parse = all(!is.na(value)),
        dose = list(.combine_doses(value, unit)),
        .groups = "drop"
      ) %>%
      mutate(
        dose_sum  = if_else(tout_parse, map_dbl(dose, "value"), NA_real_),
        dose_unit = if_else(tout_parse, map_chr(dose, "unit"), NA_character_)
      ) %>%
      select(cip13_clean, dose_sum, dose_unit) %>%
      inner_join(base, by = "cip13_clean") %>%
      mutate(
        # pourcentage -> 10 mg/ml (ou mg/g)
        pct       = coalesce(dose_unit, "") == "%",
        conc_ml   = case_when(
          pct & !is.na(ref_ml)  ~ dose_sum * 10,
          !is.na(ref_ml)        ~ dose_sum / ref_ml,
          TRUE ~ NA_real_
        ),
        dose_unit = if_else(pct & (!is.na(ref_ml) | semi_solide), "mg", dose_unit),
        unit_dose = case_when(
          !is.na(ref_ml) & !is.na(volume_ml) ~ conc_ml * volume_ml,
          !is.na(ref_ml)                     ~ NA_real_,          # concentration sans volume
          semi_solide & pct                  ~ dose_sum * 10 * masse_g,
          semi_solide                        ~ dose_sum / ref_g * masse_g,
          ref_per_unit                       ~ dose_sum,
          forme_liquide                      ~ NA_real_,          # base de dosage inconnue
          TRUE                               ~ dose_sum
        )
      ) %>%
      select(cip13_clean, unit_count_per_box, unit_dose, dose_unit, conc_per_ml = conc_ml,
             forme_pharmaceutique)
  }

  ft <- dose_variant("ft")
  sa <- dose_variant("sa") %>% select(cip13_clean, unit_dose_sa = unit_dose, dose_unit_sa = dose_unit)

  ft %>%
    left_join(sa, by = "cip13_clean") %>%
    mutate(conc_unit = if_else(!is.na(conc_per_ml), dose_unit, NA_character_)) %>%
    select(cip13_clean, unit_count_per_box, unit_dose, dose_unit, unit_dose_sa, dose_unit_sa,
           conc_per_ml, conc_unit, forme_pharmaceutique)
}

#' Parse une DDD ("1,5 g", "1.5 G", "20 MG", "1 UD", "3 MU") -> valeur convertie + unité
#' (mg, ui, mmol ou "ud" = unités de prise). Sans unité : `unit_default`.
.parse_ddd <- function(x, unit_default = NA_character_) {
  xn  <- x %>% .ascii() %>% str_to_upper() %>% str_replace_all("(?<=\\d),(?=\\d)", ".") %>% str_squish()
  val <- suppressWarnings(as.numeric(str_extract(xn, "\\d+(?:\\.\\d+)?")))
  u   <- str_extract(xn, "(?<=\\d)\\s*[A-Z][A-Z. ]*") %>% str_remove_all("[\\s.]")
  u   <- if_else(is.na(u) | u == "", str_to_upper(unit_default), u)
  tibble(
    ddd = case_when(
      u == "G"                          ~ val * 1000,
      u == "MG"                         ~ val,
      u %in% c("MCG", "UG", "MICROG")   ~ val / 1000,
      u %in% c("U", "UI", "IU")         ~ val,
      u %in% c("TU", "KU", "KUI")       ~ val * 1e3,
      u %in% c("MU", "MUI", "MIU")      ~ val * 1e6,
      u == "MMOL"                       ~ val,
      u %in% c("UD", "TAB", "CP", "CPR", "COMPRIME", "DOSE", "UNITE") ~ val,
      TRUE ~ NA_real_
    ),
    ddd_unit = case_when(
      u %in% c("G", "MG", "MCG", "UG", "MICROG")            ~ "mg",
      u %in% c("U", "UI", "IU", "TU", "KU", "KUI", "MU", "MUI", "MIU") ~ "ui",
      u == "MMOL"                                           ~ "mmol",
      u %in% c("UD", "TAB", "CP", "CPR", "COMPRIME", "DOSE", "UNITE") ~ "ud",
      TRUE ~ NA_character_
    )
  )
}

#' Calcule le dosage par CIP13 depuis l'export Thériaque (voir `read_theriaque()`)
#'
#' dose par UCD = somme des substances de l'onglet "Dose totale UCD" (règle B si unités mélangées)
#' nombre d'unités = "Nombre d'unités" (UCD par boîte) de l'onglet "Données"
build_theriaque_dosage <- function(theriaque, cips = NULL, ddd_unit_default = NA_character_) {

  if (is.null(theriaque)) return(.empty_theriaque_dose())

  don  <- theriaque$donnees
  dose <- theriaque$dose
  ddd  <- theriaque$ddd
  if (!is.null(cips)) {
    don  <- don  %>% filter(cip13 %in% cips)
    dose <- dose %>% filter(cip13 %in% cips)
    ddd  <- ddd  %>% filter(cip13 %in% cips)
  }

  dose_cip <- dose %>%
    mutate(
      unite_norm = unite %>% .ascii() %>% str_to_upper() %>% str_remove_all("[\\s.]") %>%
        recode("U" = "UI", "IU" = "UI", "UNITE" = "UI", "UNITES" = "UI", "MIU" = "MUI",
               "MICROGRAMME" = "MCG", "MICROGRAMMES" = "MCG", "UG" = "MCG"),
      conv  = map2(dose, unite_norm, .to_mg),
      value = map_dbl(conv, 1),
      unit  = map_chr(conv, 2)
    ) %>%
    group_by(cip13) %>%
    summarise(d = list(.combine_doses(value, unit)), .groups = "drop") %>%
    transmute(cip13, dose_th = map_dbl(d, "value"), unit_th = map_chr(d, "unit"))

  # DDD : onglet "Données", à défaut onglet "DDD" (première ligne par CIP)
  ddd_cip <- bind_rows(
    don %>% select(cip13, ddd_raw) %>% filter(!is.na(ddd_raw)) %>% mutate(ordre = 1),
    ddd %>% select(cip13, ddd_raw) %>% mutate(ordre = 2)
  ) %>%
    arrange(cip13, ordre) %>%
    distinct(cip13, .keep_all = TRUE) %>%
    bind_cols(.parse_ddd(.$ddd_raw, ddd_unit_default)) %>%
    select(cip13, ddd, ddd_unit)

  n_ddd_sans_unite <- sum(is.na(ddd_cip$ddd_unit))
  if (n_ddd_sans_unite > 0) {
    message(sprintf("Thériaque : %d DDD sans unité reconnue (nb_ddd non calculé pour ces CIP). ",
                    n_ddd_sans_unite),
            "Utiliser `ddd_unit_default` si la DDD est toujours exprimée dans la même unité.")
  }

  don %>%
    select(cip13, count_th = nb_unites, libelle_long, atc_th = atc) %>%
    full_join(dose_cip, by = "cip13") %>%
    left_join(ddd_cip, by = "cip13") %>%
    rename(cip13_clean = cip13) %>%
    select(cip13_clean, count_th, dose_th, unit_th, ddd, ddd_unit, libelle_long, atc_th)
}

#' Lit la table de conversion des DDD (code_atc, unite_ddd, mg_par_unite)
.read_conversion_ddd <- function(path) {
  empty <- tibble(code_atc = character(), unite_ddd = character(), mg_par_unite = numeric())
  if (is.null(path) || !file.exists(path)) return(empty)
  read_csv(path, col_types = cols(.default = col_character()), comment = "#") %>%
    transmute(
      code_atc     = str_squish(code_atc),
      unite_ddd    = str_to_lower(str_squish(unite_ddd)),
      mg_par_unite = suppressWarnings(as.numeric(str_replace(mg_par_unite, ",", ".")))
    ) %>%
    filter(!is.na(code_atc), !is.na(mg_par_unite)) %>%
    distinct(code_atc, .keep_all = TRUE)
}

#' Lit la table de correction manuelle
.read_correction <- function(correction_path) {
  empty <- tibble(cip13_clean = character(), count_corr = numeric(), dose_corr = numeric(),
                  unit_corr = character(), volume_corr = numeric())
  if (is.null(correction_path) || !file.exists(correction_path)) return(empty)

  corr <- read_csv(correction_path, col_types = cols(.default = col_character()), comment = "#")
  for (col in c("cip13", "unit_count_per_box", "unit_dose", "dose_unit", "volume_ml")) {
    if (!col %in% names(corr)) corr[[col]] <- NA_character_
  }
  to_num <- function(x) suppressWarnings(as.numeric(str_replace(x, ",", ".")))

  corr %>%
    transmute(
      cip13_clean = str_extract(cip13, "\\d{13}"),
      count_corr  = to_num(unit_count_per_box),
      dose_corr   = to_num(unit_dose),
      unit_corr   = str_to_lower(na_if(str_squish(dose_unit), "")),
      volume_corr = to_num(volume_ml)
    ) %>%
    filter(!is.na(cip13_clean)) %>%
    distinct(cip13_clean, .keep_all = TRUE)
}

#' CIP13 de forme topique (BDPM, Thériaque, libellé) : tibble(cip13_clean, motif)
.cips_exclus <- function(data_clean, bdpm_dose, th_dose,
                         exclude_forme_pharma = .default_topiques,
                         exclude_label_regex = .default_topiques_label) {
  excluded <- tibble(cip13_clean = character(), motif = character())
  if (!is.null(exclude_forme_pharma)) {
    rx <- paste0("(?i)\\b(?:", paste(exclude_forme_pharma, collapse = "|"), ")")
    excluded <- bind_rows(
      bdpm_dose %>% filter(str_detect(.ascii(coalesce(forme_pharmaceutique, "")), rx)) %>%
        transmute(cip13_clean, motif = paste("bdpm :", forme_pharmaceutique)),
      th_dose %>% filter(str_detect(.ascii(coalesce(libelle_long, "")), rx)) %>%
        transmute(cip13_clean, motif = paste("theriaque :", libelle_long))
    )
  }
  if (!is.null(exclude_label_regex)) {
    excluded <- bind_rows(
      excluded,
      data_clean %>%
        distinct(cip13_clean, nom_court) %>%
        filter(str_detect(.ascii(str_to_upper(coalesce(nom_court, ""))), exclude_label_regex)) %>%
        transmute(cip13_clean, motif = paste("nom_court :", nom_court))
    )
  }
  excluded
}

# Valeur unique par ATC : renvoie la valeur si tous les CIP résolus de l'ATC
# (au moins `min_cip`) ont la même, sinon NA
.valeur_unique_atc <- function(df, value_col, unit_col, min_cip = 2) {
  df %>%
    filter(!is.na(.data[[value_col]]), !is.na(.data[[unit_col]])) %>%
    distinct(code_atc, cip13_clean, v = round(.data[[value_col]], 6), u = .data[[unit_col]]) %>%
    group_by(code_atc) %>%
    summarise(
      n_cip   = n_distinct(cip13_clean),
      n_val   = n_distinct(paste(v, u)),
      v_atc   = first(v),
      u_atc   = first(u),
      .groups = "drop"
    ) %>%
    filter(n_val == 1, n_cip >= min_cip) %>%
    select(code_atc, v_atc, u_atc)
}

# =============================================================================
# Fonction principale
# =============================================================================

#' Ajoute le nombre d'unités, le dosage, la dose totale dispensée et le nombre de DDD
#'
#' Étape 0 : les CIP de forme topique (`exclude_forme_pharma`) sont retirés.
#'
#' Ordre de priorité, champ par champ, pour chaque CIP13 :
#' 1. table de correction manuelle (`correction_path`) ; si seul `volume_ml` est
#'    renseigné, dose = concentration (BDPM ou libellé) × volume ("manuel_volume")
#' 2. BDPM ("bdpm") ; variante SA ou FT la plus proche du libellé (dosage nominal)
#' 3. Thériaque ("theriaque"), si `ref_cip` vient de `GET_dose_bdd(path_to_theriaque = ...)`
#' 4. parsing de `nom_court` ("nom_court"), tous les libellés connus du CIP
#' 5. dosage unique de l'ATC ("atc_unique"), si `impute_atc_unique = TRUE`
#'
#' La dose et son unité viennent toujours de la même source.
#'
#' @param ref_cip Sortie de `GET_dose_bdd()` : data.frame BDPM ou list(bdpm, theriaque).
#' @param strict "error" (stop si NA), "warn" (warning) ou "none". `TRUE`/`FALSE` acceptés.
#' @param exclude_forme_pharma Regex de formes à exclure (BDPM / Thériaque). `NULL` : aucune exclusion.
#' @param exclude_label_regex Regex appliquée à `nom_court` pour les mêmes formes. `NULL` : non utilisée.
#' @param impute_atc_unique Impute le dosage d'un ATC dont tous les CIP connus ont le même.
#' @param ddd_unit_default Unité supposée pour une DDD Thériaque sans unité (ex. "g").
#' @param conversion_ddd_path Table de conversion quand la DDD n'est pas dans l'unité de la dose
#'   (ex. lithium : DDD en mmol, dose en mg de carbonate). `NULL` : pas de conversion.
#' @return `data` enrichi. Attributs : `"unparsed"` (CIP non résolus), `"excluded"` (CIP retirés).
add_dosage_info <- function(data, ref_cip, strict = c("error", "warn", "none"),
                            correction_path = "data/correction_dosage.csv",
                            exclude_forme_pharma = .default_topiques,
                            exclude_label_regex = .default_topiques_label,
                            impute_atc_unique = TRUE,
                            ddd_unit_default = NA_character_,
                            conversion_ddd_path = "config/conversion_ddd.csv") {

  if (is.logical(strict)) strict <- if (isTRUE(strict)) "error" else "none"
  strict <- match.arg(strict)
  refs <- .split_ref(ref_cip)

  # 1. Normalisation stricte des CIP13
  data_clean <- data %>%
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}"))
  cips <- unique(na.omit(data_clean$cip13_clean))

  # 2. Références
  bdpm_dose <- build_ref_dosage(refs$bdpm, cips = cips)
  th_dose   <- build_theriaque_dosage(refs$theriaque, cips = cips, ddd_unit_default = ddd_unit_default)

  # 3. Exclusion des formes topiques
  excluded <- .cips_exclus(data_clean, bdpm_dose, th_dose, exclude_forme_pharma, exclude_label_regex)
  excluded <- data_clean %>%
    filter(cip13_clean %in% excluded$cip13_clean) %>%
    group_by(cip13, cip13_clean) %>%
    summarise(code_atc = first(code_atc), nb_boite_total = sum(nb_boite, na.rm = TRUE),
              libelles = paste(unique(nom_court), collapse = " | "), .groups = "drop") %>%
    left_join(excluded %>% group_by(cip13_clean) %>%
                summarise(motif = paste(unique(motif), collapse = " ; ")), by = "cip13_clean") %>%
    select(-cip13_clean) %>%
    arrange(desc(nb_boite_total))

  data_clean <- data_clean %>% filter(!cip13 %in% excluded$cip13)
  if (nrow(excluded) > 0) {
    message(sprintf("%d CIP13 exclus (formes topiques, %s boîtes). Voir attr(res, \"excluded\").",
                    nrow(excluded), format(sum(excluded$nb_boite_total), big.mark = " ")))
  }

  # 4. Fallback nom_court : on combine tous les libellés du CIP
  #    (libellés courts non tronqués d'abord, puis le premier qui donne chaque info)
  label_dose <- parse_labels_table(data_clean) %>%
    arrange(cip13_clean, label_long, nchar(nom_court)) %>%
    group_by(cip13_clean) %>%
    summarise(
      count_label = unit_count_per_box[which(!is.na(unit_count_per_box))[1]],
      i_dose      = which(!is.na(unit_dose) & !is.na(dose_unit))[1],
      dose_label  = unit_dose[i_dose],
      unit_label  = dose_unit[i_dose],
      i_conc      = which(!is.na(conc_per_ml) & !is.na(dose_unit))[1],
      conc_label  = conc_per_ml[i_conc],
      conc_unit_label = dose_unit[i_conc],
      .groups = "drop"
    ) %>%
    select(-i_dose, -i_conc)

  # 5. Correction manuelle
  correction <- .read_correction(correction_path)

  # 6. Assemblage
  bdpm_join <- bdpm_dose %>%
    select(cip13_clean, count_ref = unit_count_per_box, dose_ref_ft = unit_dose, unit_ref_ft = dose_unit,
           dose_ref_sa = unit_dose_sa, unit_ref_sa = dose_unit_sa, conc_ref = conc_per_ml, conc_unit_ref = conc_unit,
           forme_bdpm = forme_pharmaceutique)

  res <- data_clean %>%
    left_join(bdpm_join,  by = "cip13_clean") %>%
    left_join(th_dose %>% select(cip13_clean, count_th, dose_th, unit_th, ddd, ddd_unit), by = "cip13_clean") %>%
    left_join(label_dose, by = "cip13_clean") %>%
    left_join(correction, by = "cip13_clean") %>%
    mutate(
      # --- BDPM : dosage nominal (variante SA si plus proche du libellé, sinon FT) ---
      use_sa = coalesce(
        !is.na(dose_ref_sa) & !is.na(dose_label) & unit_ref_sa == unit_label &
          (is.na(dose_ref_ft) | abs(dose_ref_sa - dose_label) < abs(dose_ref_ft - dose_label)),
        FALSE),
      dose_ref = if_else(use_sa, dose_ref_sa, dose_ref_ft),
      unit_ref = if_else(use_sa, unit_ref_sa, unit_ref_ft),

      # --- Correction par volume : concentration (BDPM, sinon libellé) × volume ---
      conc_vol      = coalesce(conc_ref, conc_label),
      conc_vol_unit = if_else(!is.na(conc_ref), conc_unit_ref, conc_unit_label),
      dose_corr_vol = if_else(!is.na(volume_corr), conc_vol * volume_corr, NA_real_),

      # --- Nombre d'unités ---
      src_unit_count = case_when(
        !is.na(count_corr)  ~ "manuel",
        !is.na(count_ref)   ~ "bdpm",
        !is.na(count_th)    ~ "theriaque",
        !is.na(count_label) ~ "nom_court",
        TRUE ~ NA_character_
      ),
      unit_count_per_box = coalesce(count_corr, count_ref, count_th, count_label),

      # --- Dose et unité (toujours de la même source) ---
      src_dose = case_when(
        !is.na(dose_corr)     & !is.na(unit_corr)     ~ "manuel",
        !is.na(dose_corr_vol) & !is.na(conc_vol_unit) ~ "manuel_volume",
        !is.na(dose_ref)      & !is.na(unit_ref)      ~ "bdpm",
        !is.na(dose_th)       & !is.na(unit_th)       ~ "theriaque",
        !is.na(dose_label)    & !is.na(unit_label)    ~ "nom_court",
        TRUE ~ NA_character_
      ),
      unit_dose = case_when(
        src_dose == "manuel"        ~ dose_corr,
        src_dose == "manuel_volume" ~ dose_corr_vol,
        src_dose == "bdpm"          ~ dose_ref,
        src_dose == "theriaque"     ~ dose_th,
        src_dose == "nom_court"     ~ dose_label,
        TRUE ~ NA_real_
      ),
      dose_unit = case_when(
        src_dose == "manuel"        ~ unit_corr,
        src_dose == "manuel_volume" ~ conc_vol_unit,
        src_dose == "bdpm"          ~ unit_ref,
        src_dose == "theriaque"     ~ unit_th,
        src_dose == "nom_court"     ~ unit_label,
        TRUE ~ NA_character_
      ),
      src_ddd = if_else(!is.na(ddd) & !is.na(ddd_unit), "theriaque", NA_character_)
    )

  # 7. Imputation par ATC (dosage et DDD uniques dans l'ATC)
  if (impute_atc_unique) {
    dose_atc <- .valeur_unique_atc(res, "unit_dose", "dose_unit")
    res <- res %>%
      left_join(dose_atc, by = "code_atc") %>%
      mutate(
        impute    = is.na(src_dose) & !is.na(v_atc),
        src_dose  = if_else(impute, "atc_unique", src_dose),
        unit_dose = if_else(impute, v_atc, unit_dose),
        dose_unit = if_else(impute, u_atc, dose_unit)
      ) %>%
      select(-v_atc, -u_atc, -impute)
  }
  ddd_atc <- .valeur_unique_atc(res, "ddd", "ddd_unit")
  res <- res %>%
    left_join(ddd_atc, by = "code_atc") %>%
    mutate(
      impute   = is.na(src_ddd) & !is.na(v_atc),
      src_ddd  = if_else(impute, "atc_unique", src_ddd),
      ddd      = if_else(impute, v_atc, ddd),
      ddd_unit = if_else(impute, u_atc, ddd_unit)
    ) %>%
    select(-v_atc, -u_atc, -impute) %>%
    select(-c(count_ref, dose_ref_ft, unit_ref_ft, dose_ref_sa, unit_ref_sa, conc_ref, conc_unit_ref,
              use_sa, dose_ref, unit_ref, forme_bdpm, conc_vol, conc_vol_unit, dose_corr_vol,
              count_th, dose_th, unit_th, count_label, dose_label, unit_label, conc_label, conc_unit_label,
              count_corr, dose_corr, unit_corr, volume_corr))

  # 8. Contrôle de complétude
  unparsed <- res %>%
    filter(is.na(unit_count_per_box) | is.na(unit_dose) | is.na(dose_unit)) %>%
    group_by(cip13) %>%
    summarise(
      code_atc       = first(code_atc),
      classe_atc     = first(classe_atc),
      nb_boite_total = sum(nb_boite, na.rm = TRUE),
      libelles       = paste(unique(nom_court), collapse = " | "),
      unit_count_per_box = first(unit_count_per_box),
      unit_dose      = first(unit_dose),
      dose_unit      = first(dose_unit),
      src_unit_count = first(src_unit_count),
      src_dose       = first(src_dose),
      .groups = "drop"
    ) %>%
    arrange(desc(nb_boite_total))

  if (nrow(unparsed) > 0 && strict != "none") {
    msg <- sprintf(
      "Parsing incomplet : %d CIP13 avec au moins une valeur NA (%s boîtes). Exemples : %s. Voir attr(res, \"unparsed\").",
      nrow(unparsed),
      format(sum(unparsed$nb_boite_total), big.mark = " "),
      paste(head(unparsed$cip13, 5), collapse = ", ")
    )
    if (strict == "error") {
      print(unparsed, n = 500)
      stop(msg)
    }
    warning(msg)
  }

  # 9. Calcul des doses totales (dans dose_unit : mg si possible, sinon ui / mmol / %)
  #    et du nombre de DDD :
  #    - DDD en mg / ui / mmol : dose totale / DDD (si même unité que la dose)
  #    - DDD en unités de prise ("ud", associations) : nombre d'unités / DDD
  res <- res %>%
    mutate(
      dose_per_box         = unit_dose * unit_count_per_box,
      total_dispensed_dose = dose_per_box * nb_boite,
      nb_ddd = case_when(
        ddd_unit == "ud"                          ~ nb_boite * unit_count_per_box / ddd,
        !is.na(ddd_unit) & ddd_unit == dose_unit  ~ total_dispensed_dose / ddd,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-cip13_clean)

  # 10. Conversion de la DDD vers l'unité de la dose (ex. lithium : mmol -> mg)
  conversion <- .read_conversion_ddd(conversion_ddd_path)
  if (nrow(conversion) > 0) {
    res <- res %>%
      left_join(conversion, by = "code_atc") %>%
      mutate(
        nb_ddd = if_else(
          is.na(nb_ddd) & !is.na(mg_par_unite) & coalesce(ddd_unit == unite_ddd, FALSE) &
            coalesce(dose_unit == "mg", FALSE),
          total_dispensed_dose / (ddd * mg_par_unite),
          nb_ddd)
      ) %>%
      select(-unite_ddd, -mg_par_unite)
  }

  attr(res, "unparsed") <- unparsed
  attr(res, "excluded") <- excluded
  return(res)
}
