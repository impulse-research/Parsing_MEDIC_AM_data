#' Télécharge et prépare la base de données de dosage des médicaments (BDPM)
#'
#' @description
#' Interroge l'API de la Base de Données Publique des Médicaments (BDPM) pour 
#' récupérer l'ensemble du référentiel, extrait le conditionnement (nombre d'unités) 
#' via des expressions régulières et associe les compositions chimiques par CIP13. 
#' Met en cache le résultat au format CSV pour éviter de re-télécharger la base à chaque appel.
#'
#' @param rerun `logical(1)`. Si `TRUE`, force le re-téléchargement et le retraitement 
#'   des données depuis l'API, même si le fichier local existe déjà. Valeur par défaut : `TRUE`.
#' @param bdd_path `character(1)`. Chemin d'accès relatif ou absolu où sauvegarder/lire 
#'   le fichier CSV de cache. Valeur par défaut : `"data/base_dosage.csv"`.
#'
#' @return Un `data.frame` (ou `tibble`) contenant la base enrichie avec notamment :
#'   \item{cis}{Code Identifiant de Spécialité}
#'   \item{cip13}{Code Identifiant de Présentation sur 13 caractères (format texte)}
#'   \item{nb_unites}{Nombre d'unités (comprimés, gélules, etc.) extrait du libellé de présentation}
#'   \item{composition_...}{Informations nettoyées sur la substance active et le dosage}
#'   \item{presentation_...}{Informations sur le conditionnement et le statut commercial}
#'
#' @details
#' La fonction réalise un dépilage (`unnest`) des structures imbriquées `presentation` 
#' et `composition`. L'extraction du nombre d'unités repose sur une double stratégie Regex :
#' 1. Recherche d'un nombre immédiatement suivi par une forme galénique (comprimé, gélule, etc.).
#' 2. À défaut, recherche d'un nombre précédé par la préposition "de ".
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>% select mutate coalesce
#' @importFrom tidyr unnest
#' @importFrom janitor clean_names
#' @importFrom stringr str_pad str_extract
#' @importFrom utils write.csv read.csv
#' 
#' @export
GET_dose_bdd <- function(rerun = TRUE, bdd_path = "data/fichier_intermediaire/base_dosage.csv"){
  
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
    dir.create(dirname(bdd_path), showWarnings = FALSE, recursive = TRUE)
    write.csv(ref_cip_local, bdd_path, row.names = FALSE)
  }
  
  # 4. Lecture du fichier mis en cache
  # Les codes sont lus en texte : sinon read.csv les convertit en numérique
  # (zéros de tête perdus, risque d'écriture scientifique)
  ref_cip_local <- read.csv(
    bdd_path,
    colClasses = c(
      cip13              = "character",
      presentation_cip13 = "character",
      presentation_cip7  = "character",
      cis                = "character"
    )
  )
  return(ref_cip_local)
}