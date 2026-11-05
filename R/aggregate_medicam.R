#' Agrège les données Medic'AM sur les dimensions temporelle et produit
#'
#' @description
#' Effectue un regroupement flexible des volumes remboursés issus de Medic'AM :
#' - Sur le plan temporel : conservation du mois, ou passage au trimestre, semestre ou année.
#' - Sur le plan produit : conservation de la granularité CIP13, ou agrégation au niveau ATC.
#'
#' Si `data` sort de `add_dosage_info()`, les doses (`total_dispensed_dose`, `nb_ddd`)
#' sont aussi sommées, avec la part des boîtes dont la valeur est connue.
#'
#' @param data `tibble` ou `data.frame`. La table produite par `parse_medicam_files()`
#'   ou `add_dosage_info()`, contenant au moins `cip13`, `nom_court`, `code_atc`,
#'   `classe_atc`, `annee`, `mois`, `nb_boite`.
#' @param freq `character(1)`. Niveau de réagrégation temporelle :
#'   - `"mois"` (défaut) : `"2023-01"`.
#'   - `"trimestre"` : `"2023-Q1"`.
#'   - `"semestre"` : `"2023-S1"`.
#'   - `"annee"` : `"2023"`.
#' @param by_cip `logical(1)`.
#'   - `TRUE` (défaut) : détail par `cip13` (+ `nom_court`).
#'   - `FALSE` : agrégation au niveau du `code_atc` (+ `classe_atc`).
#' @param mesures `character()` ou `NULL`. Mesures de dose à agréger parmi
#'   `"total_dispensed_dose"` et `"nb_ddd"`. `NULL` (défaut) : toutes celles présentes.
#'
#' @return Un `tibble` avec une ligne par identifiant (ATC ou CIP) et par période :
#'   \item{code_atc, classe_atc / cip13, nom_court}{Identifiant et libellé. Le libellé
#'     est le plus récent de l'identifiant : il ne change pas d'une période à l'autre
#'     (Medic'AM écrit par ex. "ET" ou "+" selon les années).}
#'   \item{periode}{Libellé de la période}
#'   \item{date}{Premier jour de la période (`Date`)}
#'   \item{nb_boite}{Somme des boîtes}
#'   \item{total_dispensed_dose, dose_unit, pct_boites_dose}{Si disponibles : dose totale
#'     dans l'unité majoritaire de l'identifiant, et part des boîtes de la période
#'     dont la dose est connue dans cette unité.}
#'   \item{nb_ddd, pct_boites_ddd}{Si disponibles : nombre de DDD et part des boîtes couvertes.}
#'
#' @importFrom dplyr %>% mutate summarize group_by arrange select all_of across
#' @importFrom stringr str_pad
#'
#' @export
aggregate_medicam <- function(data,
                              freq = c("mois", "trimestre", "semestre", "annee"),
                              by_cip = TRUE,
                              mesures = NULL) {

  # Validation des arguments
  freq <- match.arg(freq)
  mesures_dispo <- intersect(c("total_dispensed_dose", "nb_ddd"), names(data))
  mesures <- if (is.null(mesures)) mesures_dispo else intersect(mesures, mesures_dispo)

  # 1. Période et date de début de période
  df_prepped <- data %>%
    mutate(
      annee_num = as.integer(annee),
      mois_num  = as.integer(mois),
      periode   = .label_periode(annee_num, mois_num, freq),
      date      = .date_periode(annee_num, mois_num, freq),
      ordre     = annee_num * 100L + mois_num
    )

  # 2. Identifiant et libellé le plus récent
  id_cols  <- if (by_cip) c("code_atc", "cip13") else "code_atc"
  lib_cols <- if (by_cip) c("classe_atc", "nom_court") else "classe_atc"

  libelles <- df_prepped %>%
    filter(if_all(all_of(lib_cols), ~ !is.na(.x))) %>%
    group_by(across(all_of(id_cols))) %>%
    slice_max(ordre, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(all_of(c(id_cols, lib_cols)))

  # 3. Unité de dose majoritaire (en boîtes) par identifiant
  if ("total_dispensed_dose" %in% mesures) {
    unite <- df_prepped %>%
      filter(!is.na(dose_unit), !is.na(total_dispensed_dose)) %>%
      group_by(across(all_of(c(id_cols, "dose_unit")))) %>%
      summarise(n = sum(nb_boite, na.rm = TRUE), .groups = "drop") %>%
      group_by(across(all_of(id_cols))) %>%
      mutate(n_unites = n()) %>%
      slice_max(n, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(all_of(id_cols), unite_major = dose_unit, n_unites)

    multi <- unite %>% filter(n_unites > 1)
    if (nrow(multi) > 0) {
      warning(sprintf(
        "%d identifiant(s) avec plusieurs unités de dose : seule l'unité majoritaire est sommée (%s).",
        nrow(multi), paste(head(multi[[tail(id_cols, 1)]], 5), collapse = ", ")))
    }
    df_prepped <- df_prepped %>%
      left_join(unite %>% select(-n_unites), by = id_cols) %>%
      mutate(dose_ok = !is.na(total_dispensed_dose) & coalesce(dose_unit == unite_major, FALSE))
  }

  # 4. Agrégation finale
  part <- function(num, den) if_else(den > 0, num / den, NA_real_)

  res <- df_prepped %>%
    group_by(across(all_of(c(id_cols, "periode", "date")))) %>%
    summarize(
      nb_boite_tot = sum(nb_boite, na.rm = TRUE),
      dose_unit = if ("total_dispensed_dose" %in% mesures) first(unite_major) else NULL,
      boites_dose = if ("total_dispensed_dose" %in% mesures) sum(nb_boite[dose_ok], na.rm = TRUE) else NULL,
      total_dispensed_dose = if ("total_dispensed_dose" %in% mesures) {
        if (any(dose_ok)) sum(total_dispensed_dose[dose_ok]) else NA_real_
      } else NULL,
      boites_ddd = if ("nb_ddd" %in% mesures) sum(nb_boite[!is.na(nb_ddd)], na.rm = TRUE) else NULL,
      nb_ddd = if ("nb_ddd" %in% mesures) {
        if (any(!is.na(nb_ddd))) sum(nb_ddd, na.rm = TRUE) else NA_real_
      } else NULL,
      .groups = "drop"
    ) %>%
    rename(nb_boite = nb_boite_tot)

  if ("total_dispensed_dose" %in% mesures) {
    res <- res %>%
      mutate(pct_boites_dose = part(boites_dose, nb_boite),
             # aucune boîte : la dose est nulle, pas inconnue
             total_dispensed_dose = if_else(nb_boite == 0, 0, total_dispensed_dose)) %>%
      select(-boites_dose)
  }
  if ("nb_ddd" %in% mesures) {
    res <- res %>%
      mutate(pct_boites_ddd = part(boites_ddd, nb_boite),
             nb_ddd = if_else(nb_boite == 0, 0, nb_ddd)) %>%
      select(-boites_ddd)
  }

  res %>%
    left_join(libelles, by = id_cols) %>%
    relocate(all_of(c(id_cols, lib_cols, "periode", "date", "nb_boite"))) %>%
    arrange(across(all_of(c(id_cols, "date"))))
}

# Libellé de période : "2023-01", "2023-Q1", "2023-S1", "2023"
.label_periode <- function(annee, mois, freq) {
  switch(freq,
    mois      = paste0(annee, "-", str_pad(mois, width = 2, pad = "0")),
    trimestre = paste0(annee, "-Q", ceiling(mois / 3)),
    semestre  = paste0(annee, "-S", ceiling(mois / 6)),
    annee     = as.character(annee)
  )
}

# Premier jour de la période
.date_periode <- function(annee, mois, freq) {
  mois_debut <- switch(freq,
    mois      = mois,
    trimestre = (ceiling(mois / 3) - 1) * 3 + 1,
    semestre  = (ceiling(mois / 6) - 1) * 6 + 1,
    annee     = rep(1L, length(mois))
  )
  as.Date(sprintf("%04d-%02d-01", as.integer(annee), as.integer(mois_debut)))
}
