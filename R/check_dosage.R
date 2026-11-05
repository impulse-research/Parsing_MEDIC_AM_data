#' Mesure la fiabilité du parser de libellés sur les CIP présents dans la BDPM
#'
#' @description
#' Pour les CIP13 que l'on connaît par la référence BDPM, on parse aussi chaque
#' libellé `nom_court` et on compare. Le taux d'écart donne une idée de la qualité
#' du fallback utilisé pour les CIP absents de la BDPM, et les écarts listés
#' indiquent les motifs de libellé à corriger dans `parse_label()`.
#' Le dosage est jugé "ok" s'il correspond à la variante FT (base) ou SA (sel).
#'
#' @param data Sortie de `parse_medicam_files()`.
#' @param ref_cip Sortie de `GET_dose_bdd()` (data.frame BDPM ou list(bdpm, theriaque)).
#' @param tolerance Écart relatif toléré sur le dosage (défaut 1 %).
#' @param exclude_forme_pharma,exclude_label_regex Exclusion des topiques, comme `add_dosage_info()`.
#'
#' @return Un tibble des libellés en écart (un par CIP × libellé), trié par nombre
#'   de boîtes. Le résumé est affiché dans la console.
check_label_parser <- function(data, ref_cip, tolerance = 0.01,
                               exclude_forme_pharma = .default_topiques,
                               exclude_label_regex = .default_topiques_label) {

  refs <- .split_ref(ref_cip)
  data <- .data_sans_topiques(data, refs, exclude_forme_pharma, exclude_label_regex)
  cips <- unique(na.omit(str_extract(as.character(data$cip13), "\\d{13}")))

  ref_dose <- build_ref_dosage(refs$bdpm, cips = cips) %>%
    filter(!is.na(unit_count_per_box), !is.na(unit_dose), !is.na(dose_unit)) %>%
    select(cip13_clean, count_ref = unit_count_per_box, dose_ref = unit_dose, unit_ref = dose_unit,
           dose_ref_sa = unit_dose_sa, unit_ref_sa = dose_unit_sa)

  boites <- data %>%
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}")) %>%
    group_by(cip13_clean) %>%
    summarise(nb_boite_total = sum(nb_boite, na.rm = TRUE), .groups = "drop")

  proche <- function(a, b) !is.na(a) & !is.na(b) & abs(a - b) <= tolerance * pmax(abs(b), 1e-9)

  comparaison <- parse_labels_table(data) %>%
    inner_join(ref_dose, by = "cip13_clean") %>%
    left_join(boites, by = "cip13_clean") %>%
    mutate(
      ecart_count = case_when(
        is.na(unit_count_per_box)        ~ "NA",
        unit_count_per_box != count_ref  ~ "different",
        TRUE                             ~ "ok"
      ),
      ecart_dose = case_when(
        is.na(unit_dose) | is.na(dose_unit)                         ~ "NA",
        dose_unit == unit_ref & proche(unit_dose, dose_ref)         ~ "ok",
        coalesce(dose_unit == unit_ref_sa, FALSE) & proche(unit_dose, dose_ref_sa) ~ "ok",
        dose_unit != unit_ref                                       ~ "unite_differente",
        TRUE                                                        ~ "different"
      )
    )

  # Résumé
  resume <- comparaison %>%
    summarise(
      nb_libelles      = n(),
      count_ok_pct     = round(100 * mean(ecart_count == "ok"), 1),
      count_na_pct     = round(100 * mean(ecart_count == "NA"), 1),
      dose_ok_pct      = round(100 * mean(ecart_dose == "ok"), 1),
      dose_na_pct      = round(100 * mean(ecart_dose == "NA"), 1)
    )
  message("Comparaison parser nom_court vs BDPM (", resume$nb_libelles, " libellés) :")
  message("  nombre d'unités : ", resume$count_ok_pct, " % ok, ", resume$count_na_pct, " % NA")
  message("  dosage          : ", resume$dose_ok_pct,  " % ok, ", resume$dose_na_pct,  " % NA")

  comparaison %>%
    filter(ecart_count != "ok" | ecart_dose != "ok") %>%
    select(cip13 = cip13_clean, nom_court, nb_boite_total,
           ecart_count, count_label = unit_count_per_box, count_ref,
           ecart_dose,  dose_label = unit_dose, unit_label = dose_unit, dose_ref, unit_ref,
           dose_ref_sa) %>%
    arrange(desc(nb_boite_total))
}


#' Détecte les conflits entre sources de référence (BDPM, Thériaque, correction manuelle)
#'
#' @description
#' Pour chaque CIP13 de `data` présent dans au moins deux sources, compare le nombre
#' d'unités, l'unité de dose et la **dose par boîte** (unit_dose × unit_count_per_box).
#' La comparaison se fait sur la dose par boîte car l'"unité" diffère selon la source
#' (UCD Thériaque, comprimé ou flacon BDPM). Pour la BDPM, les variantes FT (base)
#' et SA (sel) sont testées : l'écart retenu est le plus faible des deux.
#'
#' Paires comparées : bdpm / theriaque, manuel / bdpm, manuel / theriaque.
#'
#' @param data Sortie de `parse_medicam_files()`.
#' @param ref_cip Sortie de `GET_dose_bdd()`.
#' @param correction_path Table de correction manuelle.
#' @param tolerance Écart relatif toléré sur la dose par boîte (défaut 5 %).
#' @param exclude_forme_pharma,exclude_label_regex Exclusion des topiques, comme `add_dosage_info()`.
#'
#' @return Un tibble des conflits (un par CIP × paire de sources), trié par nombre de boîtes.
check_ref_conflicts <- function(data, ref_cip, correction_path = "data/correction_dosage.csv",
                                tolerance = 0.05,
                                exclude_forme_pharma = .default_topiques,
                                exclude_label_regex = .default_topiques_label) {

  refs <- .split_ref(ref_cip)
  data <- .data_sans_topiques(data, refs, exclude_forme_pharma, exclude_label_regex)
  data_clean <- data %>% mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}"))
  cips <- unique(na.omit(data_clean$cip13_clean))

  infos <- data_clean %>%
    group_by(cip13_clean) %>%
    summarise(nom_court = first(nom_court), code_atc = first(code_atc),
              nb_boite_total = sum(nb_boite, na.rm = TRUE), .groups = "drop")

  # Une ligne par CIP et par source : count, unité, dose/boîte (+ variante SA pour la BDPM)
  bdpm_full <- build_ref_dosage(refs$bdpm, cips = cips)
  bdpm <- bdpm_full %>%
    transmute(cip13_clean, source = "bdpm", count = unit_count_per_box, unit = dose_unit,
              box = unit_dose * unit_count_per_box,
              box_alt = if_else(coalesce(dose_unit_sa == dose_unit, FALSE),
                                unit_dose_sa * unit_count_per_box, NA_real_))

  th <- build_theriaque_dosage(refs$theriaque, cips = cips) %>%
    transmute(cip13_clean, source = "theriaque", count = count_th, unit = unit_th,
              box = dose_th * count_th, box_alt = NA_real_)

  # Manuel : dose renseignée, ou concentration (BDPM / libellé) × volume_ml
  conc_label <- parse_labels_table(data_clean) %>%
    filter(!is.na(conc_per_ml)) %>%
    distinct(cip13_clean, .keep_all = TRUE) %>%
    select(cip13_clean, conc_label = conc_per_ml, conc_unit_label = dose_unit)
  conc_bdpm <- bdpm_full %>%
    select(cip13_clean, conc_ref = conc_per_ml, conc_unit_ref = conc_unit)

  manuel <- .read_correction(correction_path) %>%
    filter(cip13_clean %in% cips) %>%
    left_join(conc_bdpm, by = "cip13_clean") %>%
    left_join(conc_label, by = "cip13_clean") %>%
    left_join(bdpm %>% select(cip13_clean, count_b = count), by = "cip13_clean") %>%
    left_join(th %>% select(cip13_clean, count_t = count), by = "cip13_clean") %>%
    mutate(
      dose  = coalesce(dose_corr, volume_corr * coalesce(conc_ref, conc_label)),
      unit  = coalesce(unit_corr, if_else(!is.na(conc_ref), conc_unit_ref, conc_unit_label)),
      count = coalesce(count_corr, count_b, count_t)
    ) %>%
    transmute(cip13_clean, source = "manuel", count, unit, box = dose * count, box_alt = NA_real_)

  sources <- list(bdpm = bdpm, theriaque = th, manuel = manuel)
  paires  <- list(c("bdpm", "theriaque"), c("manuel", "bdpm"), c("manuel", "theriaque"))

  ecart_rel <- function(a, b) abs(a - b) / pmax(abs(a), abs(b), 1e-9)

  conflits <- map_dfr(paires, function(p) {
    a <- sources[[p[1]]]; b <- sources[[p[2]]]
    inner_join(a, b, by = "cip13_clean", suffix = c("_a", "_b")) %>%
      mutate(
        comparaison = paste(p[1], "vs", p[2]),
        ecart_box = pmin(ecart_rel(box_a, box_b), ecart_rel(box_alt_a, box_b),
                         ecart_rel(box_a, box_alt_b), na.rm = TRUE),
        ecart_box = if_else(is.infinite(ecart_box), NA_real_, ecart_box),
        type_ecart = case_when(
          !is.na(count_a) & !is.na(count_b) & count_a != count_b         ~ "nombre_unites",
          !is.na(unit_a) & !is.na(unit_b) & unit_a != unit_b             ~ "unite",
          !is.na(ecart_box) & ecart_box > tolerance                      ~ "dose_par_boite",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(type_ecart))
  })

  if (nrow(conflits) == 0) {
    message("Aucun conflit entre sources (tolérance ", tolerance * 100, " %).")
  } else {
    message("Conflits entre sources (tolérance ", tolerance * 100, " %) :")
    conflits %>% count(comparaison, type_ecart) %>%
      pwalk(~ message(sprintf("  %-22s %-15s %d", ..1, ..2, ..3)))
  }

  conflits %>%
    left_join(infos, by = "cip13_clean") %>%
    transmute(cip13 = cip13_clean, nom_court, code_atc, nb_boite_total, comparaison, type_ecart,
              count_a, count_b, unit_a, unit_b, dose_box_a = box_a, dose_box_b = box_b,
              ecart_pct = round(100 * ecart_box, 1)) %>%
    arrange(desc(nb_boite_total))
}


# Retire les CIP topiques de `data` (même règle que add_dosage_info())
.data_sans_topiques <- function(data, refs, exclude_forme_pharma, exclude_label_regex) {
  data_clean <- data %>% mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}"))
  cips <- unique(na.omit(data_clean$cip13_clean))
  excl <- .cips_exclus(data_clean,
                       build_ref_dosage(refs$bdpm, cips = cips),
                       build_theriaque_dosage(refs$theriaque, cips = cips),
                       exclude_forme_pharma, exclude_label_regex)
  data_clean %>% filter(!cip13_clean %in% excl$cip13_clean) %>% select(-cip13_clean)
}
