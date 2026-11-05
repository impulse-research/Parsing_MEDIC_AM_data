#' Mesure la fiabilité du parser de libellés sur les CIP présents dans la BDPM
#'
#' @description
#' Pour les CIP13 que l'on connaît par la référence BDPM, on parse aussi chaque
#' libellé `nom_court` et on compare. Le taux d'écart donne une idée de la qualité
#' du fallback utilisé pour les CIP absents de la BDPM, et les écarts listés
#' indiquent les motifs de libellé à corriger dans `parse_label()`.
#'
#' @param data Sortie de `parse_medicam_files()`.
#' @param ref_cip Sortie de `GET_dose_bdd()`.
#' @param tolerance Écart relatif toléré sur le dosage (défaut 1 %).
#'
#' @return Un tibble des libellés en écart (un par CIP × libellé), trié par nombre
#'   de boîtes. Le résumé est affiché dans la console.
check_label_parser <- function(data, ref_cip, tolerance = 0.01) {

  cips <- unique(na.omit(str_extract(as.character(data$cip13), "\\d{13}")))

  ref_dose <- build_ref_dosage(ref_cip, cips = cips) %>%
    filter(!is.na(unit_count_per_box), !is.na(unit_dose), !is.na(dose_unit)) %>%
    rename(count_ref = unit_count_per_box, dose_ref = unit_dose, unit_ref = dose_unit)

  boites <- data %>%
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}")) %>%
    group_by(cip13_clean) %>%
    summarise(nb_boite_total = sum(nb_boite, na.rm = TRUE), .groups = "drop")

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
        is.na(unit_dose) | is.na(dose_unit)               ~ "NA",
        dose_unit != unit_ref                             ~ "unite_differente",
        abs(unit_dose - dose_ref) > tolerance * dose_ref  ~ "different",
        TRUE                                              ~ "ok"
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
           ecart_dose,  dose_label = unit_dose, unit_label = dose_unit, dose_ref, unit_ref) %>%
    arrange(desc(nb_boite_total))
}
