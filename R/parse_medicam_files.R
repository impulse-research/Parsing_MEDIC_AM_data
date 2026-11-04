#' Importe, filtre et restructure l'ensemble des fichiers Excel Medic'AM
#'
#' @description
#' Parcourt un dossier contenant des fichiers Excel Medic'AM (`.xls`), importe la
#' deuxième feuille de chacun, harmonise les noms de colonnes ATC, filtre sur une
#' liste de codes ATC d'intérêt, puis pivote les données au format long (année, mois, nombre de boîtes).
#'
#' @param folder_path `character(1)`. Chemin vers le dossier contenant les fichiers `.xls` Medic'AM.
#'   Valeur par défaut : `"data/medicam"`.
#' @param liste_atc `character()`. Vecteur contenant les codes ATC à conserver 
#'   (ex. `c("N02BE01", "C09AA02")`). Doit être disponible dans l'environnement.
#'
#' @return Un `tibble` au format long avec les colonnes :
#'   \item{cip13}{Code CIP13 sur 13 caractères (format texte)}
#'   \item{nom_court}{Nom commercial ou libellé court du médicament}
#'   \item{code_atc}{Code de la classification anatomique, thérapeutique et chimique}
#'   \item{classe_atc}{Libellé de la classe ATC}
#'   \item{annee}{Année de délivrance (format texte/numérique)}
#'   \item{mois}{Mois de délivrance (format texte/numérique)}
#'   \item{nb_boite}{Nombre de boîtes remboursées}
#'
#' @details
#' - L'argument `col_types = "text"` est passé à `read_xls` pour supprimer les warnings
#'   de parsing générés par les lignes de résumé ("Total", "Homéopathie") en bas de tableau.
#' - Les colonnes ATC (`code_atc` vs `code_6`, `classe_atc` vs `classe`) sont harmonisées 
#'   dynamiquement selon la structure du fichier traité.
#'
#' @importFrom readxl read_xls
#' @importFrom dplyr %>% mutate rename filter select starts_with bind_rows
#' @importFrom janitor clean_names
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr
#'
#' @export
parse_medicam_files <- function(folder_path = "data/medicam", liste_atc) {
  
  iteration_file <- list.files(path = folder_path, pattern = "\\.xls$", full.names = TRUE)
  
  result_medoc <- purrr::map_dfr(iteration_file, function(path_x) {
    
    # 1. Lecture en forçant le type texte pour éviter les warnings de parsing sur "Total" / "Homéopathie"
    df_fun <- readxl::read_xls(path_x, sheet = 2, col_types = "text") %>% 
      janitor::clean_names() %>% 
      # Filtrer d'emblée les lignes sans CIP valide (supprime "Total", "Homéopathie", etc.)
      filter(!is.na(cip13) & cip13 != "") %>% 
      mutate(
        cip13 = str_pad(as.character(cip13), width = 13, pad = "0")
      )
    
    # 2. Gestion dynamique de la dénomination des colonnes ATC
    col_atc <- if ("code_atc" %in% names(df_fun)) "code_atc" else "code_6"
    col_classe <- if ("classe_atc" %in% names(df_fun)) "classe_atc" else "classe"
    # browser()
    res <- df_fun %>% 
      rename(
        code_atc = all_of(col_atc),
        classe_atc = all_of(col_classe)
      ) %>% 
      # 3. Filtrage sur la liste des ATC cibles
      filter(code_atc %in% liste_atc) %>% 
      # 4. Sélection des colonnes d'intérêt
      select(
        cip13, 
        nom_court, 
        code_atc,
        classe_atc,
        starts_with("nombre_de_boites_remboursees_")
      ) %>% 
      # 5. Restructuration au format long
      pivot_longer(
        cols = matches("^nombre_de_boites_remboursees_\\d{4}_\\d{2}$"),
        names_to = c("annee", "mois"),
        names_pattern = "^nombre_de_boites_remboursees_(\\d{4})_(\\d{2})$",
        values_to = "nb_boite"
      ) %>% 
      mutate(
        nb_boite = as.numeric(nb_boite) # Conversion du nombre de boîtes en numérique
      )
    
    return(res)
  })
  
  return(result_medoc)
}

