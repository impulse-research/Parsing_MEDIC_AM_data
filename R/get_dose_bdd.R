#' Télécharge et prépare la base de données de dosage des médicaments (BDPM),
#' et optionnellement l'export Thériaque
#'
#' @description
#' Interroge l'API de la Base de Données Publique des Médicaments (BDPM) pour
#' récupérer l'ensemble du référentiel, extrait le conditionnement (nombre d'unités)
#' via des expressions régulières et associe les compositions chimiques par CIP13.
#' Met en cache le résultat au format CSV pour éviter de re-télécharger la base à chaque appel.
#'
#' Si `path_to_theriaque` est renseigné, l'export Thériaque (xlsx) est lu en plus :
#' il couvre les CIP retirés du marché, absents de la BDPM, et fournit la dose
#' totale par UCD (utile pour les suspensions à reconstituer) ainsi que la DDD.
#'
#' @param rerun `logical(1)`. Si `TRUE`, force le re-téléchargement et le retraitement
#'   des données depuis l'API, même si le fichier local existe déjà. Valeur par défaut : `TRUE`.
#' @param bdd_path `character(1)`. Chemin d'accès relatif ou absolu où sauvegarder/lire
#'   le fichier CSV de cache. Valeur par défaut : `"data/fichier_intermediaire/base_dosage.csv"`.
#' @param path_to_theriaque `character(1)` ou `NULL`. Chemin vers l'export Thériaque
#'   (ex. `"data/theriaque.xlsx"`). `NULL` (défaut) : Thériaque n'est pas utilisé.
#'
#' @return
#' - si `path_to_theriaque = NULL` : le `data.frame` BDPM (comportement historique) ;
#' - sinon : une `list(bdpm = <data.frame>, theriaque = <list>)` où `theriaque` contient
#'   `donnees`, `dose` et `ddd` (voir `read_theriaque()`).
#'   `add_dosage_info()` accepte les deux formats.
#'
#' @details
#' La fonction réalise un dépilage (`unnest`) des structures imbriquées `presentation`
#' et `composition`. Le nombre d'unités `nb_unites` calculé ici est conservé pour
#' compatibilité, mais `add_dosage_info()` le recalcule de façon plus fiable
#' depuis `presentation_libelle` (plaquettes × comprimés, "tube de 3,5 g", …).
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>% select mutate coalesce
#' @importFrom tidyr unnest
#' @importFrom janitor clean_names
#' @importFrom stringr str_pad str_extract
#' @importFrom utils write.csv read.csv
#'
#' @export
GET_dose_bdd <- function(rerun = TRUE,
                         bdd_path = "data/fichier_intermediaire/base_dosage.csv",
                         path_to_theriaque = NULL){

  if (rerun || !file.exists(bdd_path)) {

    # 1. Requête vers l'API d'export BDPM
    url_export <- "https://medicaments-api.giygas.dev/v1/medicaments/export"
    bdpm_complete <- jsonlite::fromJSON(url_export)

    # 2. Dépilage et nettoyage de la table de référence
    ref_cip_local <- bdpm_complete %>%
      select(cis, elementPharmaceutique, formePharmaceutique, composition, presentation) %>%
      unnest(presentation, names_sep = "_") %>%
      unnest(composition, names_sep = "_") %>%
      janitor::clean_names() %>%
      mutate(
        # Normalisation du CIP13 en chaîne de 13 caractères
        cip13 = str_pad(as.character(presentation_cip13), width = 13, pad = "0"),

        # Regex 1 : Extraction du nombre devant les mots-clés d'unités (ex: "30 comprimé(s)")
        nb_unites_str = str_extract(presentation_libelle, "(?i)(\\d+)\\s*(?=comprim|gélule|sachet|ampoule|flacon|poche|plaquette)"),

        # Regex 2 : Repli sur "de X" (ex: "boîte de 30")
        nb_unites_str = coalesce(
          nb_unites_str,
          str_extract(presentation_libelle, "(?i)(?<=de\\s)\\d+")
        ),

        # Conversion au format numérique
        nb_unites = as.numeric(nb_unites_str)
      )

    # 3. Sauvegarde sur disque (crée le dossier si inexistant)
    # UTF-8 explicite : sous Windows l'encodage natif abîme les accents ("comprimé")
    dir.create(dirname(bdd_path), showWarnings = FALSE, recursive = TRUE)
    write.csv(ref_cip_local, bdd_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  # 4. Lecture du fichier mis en cache
  # Les codes sont lus en texte : sinon read.csv les convertit en numérique
  # (zéros de tête perdus, risque d'écriture scientifique)
  ref_cip_local <- read.csv(
    bdd_path,
    fileEncoding = "UTF-8",
    colClasses = c(
      cip13              = "character",
      presentation_cip13 = "character",
      presentation_cip7  = "character",
      cis                = "character"
    )
  )

  # 5. Thériaque (optionnel)
  if (is.null(path_to_theriaque)) return(ref_cip_local)

  list(
    bdpm      = ref_cip_local,
    theriaque = read_theriaque(path_to_theriaque)
  )
}


# =============================================================================
# Thériaque
# =============================================================================

# Noms de colonnes en snake_case ASCII ("Nombre d'unités" -> "nombre_d_unites")
.clean_col_names <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# Retrouve une colonne par regex sur les noms nettoyés
.find_col <- function(df, pattern, sheet, required = TRUE) {
  hit <- names(df)[str_detect(names(df), pattern)][1]
  if (is.na(hit) && required) {
    stop(sprintf("Thériaque, onglet '%s' : colonne introuvable (motif '%s'). Colonnes : %s",
                 sheet, pattern, paste(names(df), collapse = ", ")))
  }
  hit
}

# Renvoie le contenu d'une colonne (ou NA si colonne facultative absente)
.get_col <- function(df, pattern, sheet, required = TRUE) {
  col <- .find_col(df, pattern, sheet, required)
  if (is.na(col)) return(rep(NA_character_, nrow(df)))
  df[[col]]
}

# CIP lus depuis Excel : "3400935251862", "3400935251862.0" ou "3.400935251862E12"
.clean_cip <- function(x, width = 13) {
  x <- str_squish(as.character(x))
  sci <- !is.na(x) & str_detect(x, "(?i)e\\+?\\d+$|\\.0+$")
  x[sci] <- format(as.numeric(x[sci]), scientific = FALSE, trim = TRUE)
  x <- str_remove_all(x, "\\D")
  x <- if_else(nchar(x) > 0 & nchar(x) < width, str_pad(x, width, pad = "0"), x)
  na_if(x, "")
}

#' Lit l'export Thériaque
#'
#' Onglets attendus (repérés par leur nom, insensible à la casse et aux accents) :
#' - "Données"          : une ligne par CIP (libellés, ATC, DDD, nombre et nature des unités)
#' - "Dose totale UCD"  : une ligne par CIP × substance (dose totale dans l'UCD + unité)
#' - "DDD"              : DDD par CIP et voie (facultatif, en complément de "Données")
#'
#' @return list(donnees, dose, ddd) de tibbles, CIP13 en texte
read_theriaque <- function(path) {
  if (!file.exists(path)) stop("Fichier Thériaque introuvable : ", path)

  sheets       <- readxl::excel_sheets(path)
  sheets_clean <- .clean_col_names(sheets)
  pick_sheet   <- function(pattern, required = TRUE) {
    s <- sheets[str_detect(sheets_clean, pattern)][1]
    if (is.na(s) && required) {
      stop(sprintf("Thériaque : onglet introuvable (motif '%s'). Onglets : %s",
                   pattern, paste(sheets, collapse = ", ")))
    }
    s
  }
  read_sheet <- function(sheet) {
    df <- readxl::read_xlsx(path, sheet = sheet, col_types = "text")
    names(df) <- .clean_col_names(names(df))
    df
  }

  # --- Onglet Données ---
  s_don <- pick_sheet("^donn")
  don   <- read_sheet(s_don)
  donnees <- tibble(
    cip13          = .clean_cip(.get_col(don, "^cip_?13$", s_don)),
    libelle_long   = .get_col(don, "libelle_long", s_don),
    libelle_court  = .get_col(don, "libelle_court", s_don, required = FALSE),
    atc            = str_squish(.get_col(don, "^atc$", s_don)),
    ddd_raw        = .get_col(don, "^ddd$", s_don, required = FALSE),
    voie_ddd       = .get_col(don, "^voie", s_don, required = FALSE),
    nb_unites      = suppressWarnings(as.numeric(str_replace(
                       .get_col(don, "^nb_unite", s_don), ",", "."))),
    nature_unite   = .get_col(don, "^nature", s_don, required = FALSE)
  ) %>%
    filter(!is.na(cip13)) %>%
    distinct(cip13, .keep_all = TRUE)

  # --- Onglet Dose totale UCD ---
  s_dose <- pick_sheet("dose_totale")
  dos    <- read_sheet(s_dose)
  dose <- tibble(
    cip13     = .clean_cip(.get_col(dos, "^cip_?13$", s_dose)),
    dose_raw  = .get_col(dos, "^dose_totale", s_dose),
    unite     = .get_col(dos, "^unite", s_dose),
    substance = .get_col(dos, "substance", s_dose, required = FALSE)
  ) %>%
    filter(!is.na(cip13)) %>%
    mutate(
      dose = suppressWarnings(as.numeric(
        dose_raw %>% str_remove_all("\\s") %>% str_replace(",", ".")
      ))
    ) %>%
    distinct()

  # --- Onglet DDD (facultatif) ---
  s_ddd <- pick_sheet("^ddd$", required = FALSE)
  ddd <- if (!is.na(s_ddd)) {
    dd <- read_sheet(s_ddd)
    tibble(
      cip13    = .clean_cip(.get_col(dd, "^cip_?13$", s_ddd)),
      ddd_raw  = .get_col(dd, "^ddd$", s_ddd),
      voie_ddd = .get_col(dd, "^voie", s_ddd, required = FALSE)
    ) %>%
      filter(!is.na(cip13), !is.na(ddd_raw)) %>%
      distinct()
  } else {
    tibble(cip13 = character(), ddd_raw = character(), voie_ddd = character())
  }

  message(sprintf("Thériaque : %d CIP (Données), %d lignes de dose, %d lignes DDD",
                  nrow(donnees), nrow(dose), nrow(ddd)))

  list(donnees = donnees, dose = dose, ddd = ddd)
}
