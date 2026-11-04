library(dplyr)
library(stringr)
library(readr)
library(purrr)

add_dosage_info <- function(data, ref_cip, strict = TRUE) {
  
  # 1. Normalisation stricte des CIP13 (extraction de la séquence de 13 chiffres)
  data_clean <- data %>% 
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}"))
  
  col_dosage_name <- names(ref_cip)[names(ref_cip) %in% c("composition_dosage_substance", "dosage_substance", "composition_dosage")][1]
  col_spec_name   <- names(ref_cip)[names(ref_cip) %in% c("element_pharmaceutique", "nom_specialite", "denomination")][1]
  
  regex_units <- "(?i)(microgramme[s]?/ml|microgramme[s]?|mcg/ml|mcg|µg/ml|µg|mg/g|mg/ml|mg|g/l|g|mmol/ml|mmol|ui/ml|ui|unité[s]?|unite[s]?|\\bu\\b|sq-hdm|sq|ch|dh|\\bk\\b|\\btm\\b|\\b\\(tm\\)\\b|pour cent|%|ml|l)"
  
  # Fonction pour sommer les composants d'une combinaison (ex: "80 MG/12.5 MG" -> 92.5)
  parse_and_sum_dosages <- function(str_val) {
    if (is.na(str_val) || str_val == "") return(NA_real_)
    
    # Si concentration (ex: mg/ml), extraire uniquement le premier chiffre
    if (str_detect(str_val, "(?i)mg/ml|mg/g|µg/ml|mcg/ml|mmol/ml|g/l")) {
      return(suppressWarnings(readr::parse_number(str_replace(str_val, "(\\d+),(\\d+)", "\\1.\\2"))))
    }
    
    str_clean <- str_replace_all(str_val, "(\\d+),(\\d+)", "\\1.\\2")
    
    # Extraire les nombres associés aux dosages (ignore les nombres isolés comme CPR 30 en fin de chaîne si possible)
    # Recherche des motifs de dosage : nombres suivis optionnellement de mg/g/etc. ou séparés par /
    doses <- str_extract_all(str_clean, "\\d+(\\.\\d+)?(?=\\s*(mg|g|mcg|µg|ui|%|/|$))")[[1]]
    
    if (length(doses) == 0) {
      doses <- str_extract_all(str_clean, "\\d+(\\.\\d+)?")[[1]]
    }
    
    if (length(doses) == 0) return(NA_real_)
    sum(as.numeric(doses), na.rm = TRUE)
  }
  
  # 2. Nettoyage de la table de référence
  ref_clean <- ref_cip %>% 
    mutate(cip13_clean = str_extract(as.character(cip13), "\\d{13}")) %>% 
    filter(!is.na(cip13_clean)) %>% 
    distinct(cip13_clean, .keep_all = TRUE) %>% 
    mutate(
      raw_comp = if (!is.na(col_dosage_name)) as.character(.data[[col_dosage_name]]) else NA_character_,
      raw_spec = if (!is.na(col_spec_name)) as.character(.data[[col_spec_name]]) else NA_character_,
      
      raw_comp_clean = na_if(str_squish(raw_comp), ""),
      raw_spec_clean = na_if(str_squish(raw_spec), ""),
      
      nb_unites_ref = case_when(
        !is.na(nb_unites) ~ as.numeric(nb_unites),
        str_detect(presentation_libelle, "(?i)\\b(\\d+)\\s+(comprimé|gélule|pilule|sachet|ampoule|capsule)s?\\b") ~ 
          as.numeric(str_extract(presentation_libelle, "(?i)(?<=\\b)\\d+(?=\\s+(comprimé|gélule|pilule|sachet|ampoule|capsule))")),
        str_detect(presentation_libelle, "(?i)\\b(1|un|une)\\s+(seringue|stylo|récipient|poche|flacon|flacon pressurisé)\\b") ~ 1,
        TRUE ~ NA_real_
      ),
      
      dosage_str_ref = coalesce(raw_comp_clean, raw_spec_clean),
      
      dose_unit_ref = str_to_lower(str_extract(dosage_str_ref, regex_units)),
      unit_dose_ref = map_dbl(dosage_str_ref, parse_and_sum_dosages)
    )
  
  # 3. Jointure + Fallback direct sur `nom_court` pour les CIP manquants dans ref_cip
  # Motif strict pour les unités médicales
  regex_units_strict <- "(?i)\\b(mg|mcg|µg|g|ml|l|ui|%)\\b"
  
  res <- data_clean %>% 
    left_join(
      ref_clean %>% select(cip13_clean, unit_count_per_box = nb_unites_ref, unit_dose = unit_dose_ref, dose_unit = dose_unit_ref),
      by = "cip13_clean"
    ) %>% 
    mutate(
      # --- 1. FALLBACK UNIT_COUNT_PER_BOX ---
      unit_count_per_box = case_when(
        !is.na(unit_count_per_box) ~ unit_count_per_box,
        str_detect(nom_court, "(?i)\\b(cpr|gél|cp|gelule|sachet|flacon|ampoule)?\\s*\\d+\\s*$") ~ 
          as.numeric(str_extract(nom_court, "\\d+\\s*$")),
        TRUE ~ NA_real_
      ),
      
      # --- 2. FALLBACK DOSE_UNIT ---
      dose_unit = case_when(
        !is.na(dose_unit) ~ dose_unit,
        
        # Si une unité est explicitement présente (ex: MG, G, ML, UI, %)
        str_detect(nom_court, "(?i)\\b(mg|mcg|µg|g|ml|l|ui|%)\\b") ~ 
          str_to_lower(str_extract(nom_court, "(?i)\\b(mg|mcg|µg|g|ml|l|ui|%)\\b")),
        
        # Déduction par défaut si l'unité est absente mais que c'est un comprimé / gélule / sachet
        str_detect(nom_court, "(?i)\\b(cpr|cp|gél|gelule|sachet)\\b") ~ "mg",
        
        TRUE ~ NA_character_
      ),
      
      # --- 3. FALLBACK UNIT_DOSE ---
      unit_dose = if_else(
        is.na(unit_dose),
        map_dbl(nom_court, function(str_nc) {
          # Normalisation des virgules décimales en points (ex: "12,5" -> "12.5")
          str_nc_norm <- str_replace_all(str_nc, "(?<=\\d),(?=\\d)", ".")
          
          # Gestion des suspensions/sirops du type "250 MG/5 ML" ou "100 MG/12.5 MG/ML"
          # On extrait la partie dosage avant le /5 ML ou /ML s'il s'agit d'une concentration
          if (str_detect(str_nc_norm, "(?i)/\\s*\\d*\\s*ml")) {
            str_nc_norm <- str_remove(str_nc_norm, "(?i)/\\s*\\d*\\s*ml.*$")
          }
          
          # Extraction du segment de dosage (ex: "80/12.5", "160/12.5", "500/62.5")
          dosage_part <- str_extract(
            str_nc_norm, 
            "(?i)\\b\\d+([.]\\d+)?(\\s*/\\s*\\d+([.]\\d+)?)*(\\s*(mg|g|µg|mcg|ui|%))?\\b"
          )
          
          if (is.na(dosage_part)) dosage_part <- str_nc_norm
          
          # Calcul de la somme via ta fonction personnalisée
          parse_and_sum_dosages(dosage_part)
        }),
        unit_dose
      )
    ) %>% 
    select(-cip13_clean)
  
  # 4. Contrôle de complétude
  if (strict) {
    unparsed <- res %>% 
      filter(is.na(unit_count_per_box) | is.na(unit_dose) | is.na(dose_unit)) %>% 
      distinct(cip13,.keep_all = T)
    
    print(unparsed,n = 500)
    if (nrow(unparsed) > 0) {
      nb_missing <- nrow(unparsed)
      cips_missing <- paste(head(unique(unparsed$cip13), 5), collapse = ", ")
      stop(
        sprintf(
          "Erreur de parsing : %d ligne(s) contiennent des valeurs NA. CIP13 concernés (exemples) : %s",
          nb_missing,
          cips_missing
        )
      )
    }
  }
  
  # 5. Calcul des doses totales
  res <- res %>% 
    mutate(
      dose_per_box = case_when(
        dose_unit == "%" ~ (unit_dose / 100) * unit_count_per_box,
        TRUE ~ unit_dose * unit_count_per_box
      ),
      total_dispensed_dose = dose_per_box * nb_boite
    )
  
  return(res)
}
# a <- ref_clean %>% 
#   filter(is.na(nb_unites)| is.na(unit_dose)| is.na(dose_unit))
  


