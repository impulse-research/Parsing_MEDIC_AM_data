library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)

# =============================================================================
# Helpers de parsing (partagés avec R/check_dosage.R)
# =============================================================================

# Unités de masse/quantité reconnues (ordre important : MCG avant MG avant G)
.regex_dose_unit <- "MICROGRAMMES?|MCG|µG|UG|MG|G|UI|MMOL|%"

# Un composant de dosage : "80", "12.5MG", "1 G", "5 ML" (pas de \\b : "12.5MG" doit matcher)
.regex_dose_comp <- paste0("\\d+(?:\\.\\d+)?\\s*(?:", .regex_dose_unit, "|ML)?(?![A-Z])")

# Une chaîne de composants séparés par "/" : "80/12.5MG", "250MG/5ML", "100MG/12.5MG/ML"
.regex_dose_chain <- paste0(.regex_dose_comp, "(?:\\s*/\\s*(?:", .regex_dose_comp, "|ML(?![A-Z])))*")

# Mise en forme commune : majuscules, virgule décimale -> point, "P. 100" -> "%"
.normalise_label <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("(?<=\\d),(?=\\d)", ".") %>%
    str_replace_all("(\\d+(?:\\.\\d+)?)\\s*(?:P\\.?\\s*100|POUR CENT)", "\\1 %") %>%
    str_squish()
}

# Conversion d'une valeur dans l'unité cible (mg quand c'est possible)
.to_mg <- function(value, unit) {
  if (unit == "G")                                      return(list(value * 1000, "mg"))
  if (unit == "MG")                                     return(list(value, "mg"))
  if (unit %in% c("MCG", "µG", "UG") || str_detect(unit, "^MICROGRAMME")) return(list(value / 1000, "mg"))
  if (unit == "UI")                                     return(list(value, "ui"))
  if (unit == "MMOL")                                   return(list(value, "mmol"))
  if (unit == "%")                                      return(list(value, "%"))
  list(NA_real_, NA_character_)
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

  conv  <- map2(num, unit, .to_mg)
  vals  <- map_dbl(conv, 1)
  units <- map_chr(conv, 2)

  # On ne somme que des composants de même unité (sinon non interprétable)
  if (any(is.na(units)) || n_distinct(units) > 1) return(empty)

  list(value = sum(vals), unit = units[1], per_ml = per_ml)
}

#' Parse un libellé Medic'AM (`nom_court`)
#'
#' Gère les 3 formats rencontrés :
#' - ancien format court   : "AMOXICILLINE MYLAN 250MG/5ML PDR FL 1/60 ML"
#' - format court récent    : "VALSARTAN/HCTZ AHCL 80/12,5MG  30"
#' - format long tronqué    : "ATENOLOL BIOGARAN 100 MG 1 BOITE DE 28, COMPRIMES"
#'
#' @return list(unit_count_per_box, unit_dose, dose_unit)
parse_label <- function(label) {
  out <- list(unit_count_per_box = NA_real_, unit_dose = NA_real_, dose_unit = NA_character_)
  if (is.na(label) || str_squish(label) == "") return(out)

  x <- .normalise_label(label)

  # Libellé tronqué au milieu du dosage ("… 500 MG/62,") -> somme non fiable
  dosage_tronque <- str_detect(x, "/\\s*\\d+(?:\\.\\d*)?,?$")
  volume_ml <- NA_real_

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
      x <- str_sub(x, 1, nchar(x) - nchar(m[1, 1]))
    }
  }

  # --- B. DOSAGE ---
  if (dosage_tronque) return(out)

  chains <- str_extract_all(x, .regex_dose_chain)[[1]]
  # Première chaîne contenant une vraie unité de dose (exclut "5ML" seul, "SACH 2", …)
  chains <- chains[str_detect(chains, paste0("(?:", .regex_dose_unit, ")(?![A-Z])"))]
  if (length(chains) == 0) return(out)

  dose <- .parse_dose_chain(chains[1])
  if (is.na(dose$value)) return(out)

  # Pourcentage -> concentration en mg/ml (1 % = 10 mg/ml) si un volume est connu
  if (dose$unit == "%" && !is.na(volume_ml)) {
    dose <- list(value = dose$value * 10, unit = "mg", per_ml = 1)
  }

  # Suspension / sirop sans "/ML" dans le libellé : le dosage est une concentration
  # dont on ignore le volume de référence -> non calculable
  if (is.na(dose$per_ml) && str_detect(x, "\\b(?:SUSP|SIROP|SOL BUV)\\b")) {
    out$dose_unit <- dose$unit
    return(out)
  }

  if (!is.na(dose$per_ml)) {
    # Forme liquide : dose par contenant = concentration × volume
    out$dose_unit <- dose$unit
    out$unit_dose <- if (!is.na(volume_ml)) dose$value / dose$per_ml * volume_ml else NA_real_
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

#' Calcule le dosage par CIP13 depuis la référence BDPM (`GET_dose_bdd`)
#'
#' - somme des substances (une ligne par substance après dédoublonnage SA/FT)
#' - formes liquides : dosage de référence "pour X ml" × volume du contenant
#'
#' @param cips CIP13 à conserver (filtre précoce pour la performance)
build_ref_dosage <- function(ref_cip, cips = NULL) {

  required <- c("cip13", "composition_dosage", "composition_reference_dosage",
                "composition_nature_composant", "composition_denomination_substance",
                "presentation_libelle", "nb_unites")
  missing_cols <- setdiff(required, names(ref_cip))
  if (length(missing_cols) > 0) {
    stop("Colonnes absentes de ref_cip : ", paste(missing_cols, collapse = ", "))
  }

  ref <- ref_cip %>%
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}")) %>%
    filter(!is.na(cip13_clean))
  if (!is.null(cips)) ref <- ref %>% filter(cip13_clean %in% cips)

  # Racines (6 lettres) d'un nom de substance, hors sels / hydrates
  racines_exclues <- c("sodium", "potass", "calciu", "magnes", "chlorh", "hydrat",
                       "trihyd", "monohy", "dihydr", "anhydr", "phosph", "acetat")
  racines <- function(noms) {
    noms %>% str_to_lower() %>% str_extract_all("[a-zà-ÿ]{6,}") %>% unlist() %>%
      str_sub(1, 6) %>% setdiff(racines_exclues) %>% unique()
  }

  # Garde la FT (fraction thérapeutique) et retire la SA correspondante
  # ex. "amoxicilline trihydratée" (SA) / "amoxicilline" (FT) -> on garde "amoxicilline"
  garder_sa_ft <- function(nature, denomination) {
    ft <- racines(denomination[!is.na(nature) & nature == "FT"])
    if (length(ft) == 0) return(rep(TRUE, length(nature)))
    is_sa  <- !is.na(nature) & nature == "SA"
    has_ft <- map_lgl(str_to_lower(denomination), ~ any(str_detect(.x, fixed(ft))))
    !(is_sa & has_ft)
  }

  # 1. Une ligne par CIP × substance
  compo <- ref %>%
    distinct(cip13_clean, composition_denomination_substance, composition_dosage,
             composition_reference_dosage, composition_nature_composant) %>%
    group_by(cip13_clean) %>%
    filter(garder_sa_ft(composition_nature_composant, composition_denomination_substance)) %>%
    ungroup() %>%
    mutate(
      # "1 000 000 UI" -> "1000000 UI", puis même parser que les libellés
      dosage_norm = composition_dosage %>%
        str_replace_all("(?<=\\d)\\s(?=\\d{3}\\b)", "") %>%
        .normalise_label(),
      parsed = map(dosage_norm, ~ .parse_dose_chain(str_extract(.x, .regex_dose_chain))),
      value  = map_dbl(parsed, "value"),
      unit   = map_chr(parsed, "unit"),
      # Dosage de référence : "5 ml" -> 5 ; "1 ml de suspension" -> 1 ; "un comprimé" -> NA
      ref_norm = str_to_lower(composition_reference_dosage),
      ref_ml   = case_when(
        str_detect(ref_norm, "\\d+(?:[.,]\\d+)?\\s*ml") ~
          as.numeric(str_replace(str_extract(ref_norm, "\\d+(?:[.,]\\d+)?(?=\\s*ml)"), ",", ".")),
        str_detect(ref_norm, "\\bml\\b") ~ 1,
        TRUE ~ NA_real_
      ),
      ref_contenant = str_detect(ref_norm, "flacon|ampoule|seringue|poche|récipient|recipient|stylo|tube")
    )

  # 2. Somme par CIP (unités homogènes uniquement)
  dose_cip <- compo %>%
    group_by(cip13_clean) %>%
    summarise(
      dose_ok       = all(!is.na(value)) && n_distinct(unit) == 1,
      dose_sum      = if (dose_ok[1]) sum(value) else NA_real_,
      dose_unit     = if (dose_ok[1]) first(unit) else NA_character_,
      ref_ml        = first(na.omit(ref_ml)),
      ref_contenant = any(ref_contenant, na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Conditionnement (une ligne par CIP)
  presentation <- ref %>%
    distinct(cip13_clean, .keep_all = TRUE) %>%
    transmute(
      cip13_clean,
      libelle    = str_to_lower(presentation_libelle),
      nb_unites  = suppressWarnings(as.numeric(nb_unites)),
      nb_contenants = as.numeric(str_extract(libelle, "\\d+(?=\\s+(?:flacon|ampoule|seringue|poche|récipient|stylo|tube))")),
      volume_ml  = as.numeric(str_replace(str_extract(libelle, "\\d+(?:[.,]\\d+)?(?=\\s*ml\\b)"), ",", "."))
    )

  dose_cip %>%
    left_join(presentation, by = "cip13_clean") %>%
    mutate(
      forme_liquide = !is.na(ref_ml),

      unit_count_per_box = case_when(
        forme_liquide | ref_contenant ~ coalesce(nb_contenants, 1),
        !is.na(nb_unites)             ~ nb_unites,
        str_detect(libelle, "\\d+\\s+(comprimé|gélule|pilule|sachet|capsule)") ~
          as.numeric(str_extract(libelle, "\\d+(?=\\s+(comprimé|gélule|pilule|sachet|capsule))")),
        TRUE ~ NA_real_
      ),

      # Pourcentage sur forme liquide -> mg/ml (1 % = 10 mg/ml)
      pct_liquide = forme_liquide & coalesce(dose_unit, "") == "%" & !is.na(volume_ml),
      dose_sum  = if_else(pct_liquide, dose_sum * 10, dose_sum),
      ref_ml    = if_else(pct_liquide, 1, ref_ml),
      dose_unit = if_else(pct_liquide, "mg", dose_unit),
      
      unit_dose = case_when(
        forme_liquide & !is.na(volume_ml) ~ dose_sum / ref_ml * volume_ml,
        forme_liquide                     ~ NA_real_,
        TRUE                              ~ dose_sum
      )
    ) %>%
    select(cip13_clean, unit_count_per_box, unit_dose, dose_unit)
}

# =============================================================================
# Fonction principale
# =============================================================================

#' Ajoute le nombre d'unités, le dosage et la dose totale dispensée
#'
#' Ordre de priorité pour chaque CIP13 :
#' 1. table de correction manuelle (`correction_path`) si une valeur y est renseignée
#' 2. référence BDPM (`ref_cip`)
#' 3. parsing de `nom_court` (tous les libellés connus du CIP, le plus fiable d'abord)
#'
#' Les colonnes `src_unit_count` et `src_dose` indiquent d'où vient chaque valeur
#' ("manuel", "ref", "nom_court"). La dose et son unité viennent toujours de la même source.
#'
#' @param strict "error" (stop si NA), "warn" (warning) ou "none". `TRUE`/`FALSE` acceptés.
#' @return `data` enrichi ; les CIP non résolus sont dans `attr(res, "unparsed")`
add_dosage_info <- function(data, ref_cip, strict = c("error", "warn", "none"),
                            correction_path = "data/correction_dosage.csv") {

  if (is.logical(strict)) strict <- if (isTRUE(strict)) "error" else "none"
  strict <- match.arg(strict)

  # 1. Normalisation stricte des CIP13
  data_clean <- data %>%
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}"))
  cips <- unique(na.omit(data_clean$cip13_clean))

  # 2. Référence BDPM
  ref_dose <- build_ref_dosage(ref_cip, cips = cips) %>%
    rename(count_ref = unit_count_per_box, dose_ref = unit_dose, unit_ref = dose_unit)

  # 3. Fallback nom_court : on combine tous les libellés du CIP
  #    (libellés courts non tronqués d'abord, puis le premier qui donne chaque info)
  label_dose <- parse_labels_table(data) %>%
    arrange(cip13_clean, label_long, nchar(nom_court)) %>%
    group_by(cip13_clean) %>%
    summarise(
      count_label = unit_count_per_box[which(!is.na(unit_count_per_box))[1]],
      i_dose      = which(!is.na(unit_dose) & !is.na(dose_unit))[1],
      dose_label  = unit_dose[i_dose],
      unit_label  = dose_unit[i_dose],
      .groups = "drop"
    ) %>%
    select(-i_dose)

  # 4. Correction manuelle (prioritaire quand renseignée)
  correction <- if (!is.null(correction_path) && file.exists(correction_path)) {
    read_csv(correction_path, col_types = cols(.default = col_character()), comment = "#") %>%
      transmute(
        cip13_clean = str_extract(cip13, "\\d{13}"),
        count_corr  = as.numeric(unit_count_per_box),
        dose_corr   = as.numeric(unit_dose),
        unit_corr   = str_to_lower(na_if(str_squish(dose_unit), ""))
      ) %>%
      filter(!is.na(cip13_clean)) %>%
      distinct(cip13_clean, .keep_all = TRUE)
  } else {
    tibble(cip13_clean = character(), count_corr = numeric(), dose_corr = numeric(), unit_corr = character())
  }

  # 5. Assemblage
  res <- data_clean %>%
    left_join(ref_dose,   by = "cip13_clean") %>%
    left_join(label_dose, by = "cip13_clean") %>%
    left_join(correction, by = "cip13_clean") %>%
    mutate(
      src_unit_count = case_when(
        !is.na(count_corr)  ~ "manuel",
        !is.na(count_ref)   ~ "ref",
        !is.na(count_label) ~ "nom_court",
        TRUE ~ NA_character_
      ),
      unit_count_per_box = coalesce(count_corr, count_ref, count_label),

      src_dose = case_when(
        !is.na(dose_corr)  & !is.na(unit_corr)  ~ "manuel",
        !is.na(dose_ref)   & !is.na(unit_ref)   ~ "ref",
        !is.na(dose_label) & !is.na(unit_label) ~ "nom_court",
        TRUE ~ NA_character_
      ),
      unit_dose = case_when(
        src_dose == "manuel"    ~ dose_corr,
        src_dose == "ref"       ~ dose_ref,
        src_dose == "nom_court" ~ dose_label,
        TRUE ~ NA_real_
      ),
      dose_unit = case_when(
        src_dose == "manuel"    ~ unit_corr,
        src_dose == "ref"       ~ unit_ref,
        src_dose == "nom_court" ~ unit_label,
        TRUE ~ NA_character_
      )
    ) %>%
    select(-c(count_ref, dose_ref, unit_ref, count_label, dose_label, unit_label,
              count_corr, dose_corr, unit_corr))

  # 6. Contrôle de complétude
  unparsed <- res %>%
    filter(is.na(unit_count_per_box) | is.na(unit_dose) | is.na(dose_unit)) %>%
    group_by(cip13, code_atc, classe_atc) %>%
    summarise(
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

  # 7. Calcul des doses totales (dans dose_unit : mg si possible, sinon ui / mmol / %)
  res <- res %>%
    mutate(
      dose_per_box         = unit_dose * unit_count_per_box,
      total_dispensed_dose = dose_per_box * nb_boite
    ) %>%
    select(-cip13_clean)

  attr(res, "unparsed") <- unparsed
  return(res)
}
