library(tidyverse)

# Chargement de toutes les fonctions du pipeline
list.files("R", pattern = "\\.R$", full.names = TRUE) %>%
  walk(source, encoding = "UTF-8")

# -----------------------------------------------------------------------------
# 1. Base enrichie (relue depuis le cache si aucune entrée n'a changé)
#    ATC d'intérêt : config/atc_interet.csv
# -----------------------------------------------------------------------------
base <- build_medicam_base(
  liste_atc         = get_liste_atc(),
  path_to_theriaque = if (file.exists("data/theriaque.xlsx")) "data/theriaque.xlsx" else NULL,
  rerun             = FALSE,   # TRUE : reconstruit même si rien n'a changé
  rerun_bdpm        = FALSE    # TRUE : retélécharge la BDPM
)
# Journaux de qualité : outpout/logs/ (unparsed, excluded, conflits, couverture_atc)

# -----------------------------------------------------------------------------
# 2. Séries pour les modèles
# -----------------------------------------------------------------------------
# Séries ATC au mois, au trimestre et au semestre (lues par le dépôt de modélisation)
# Chaque série contient aussi nb_jours, periode_complete et <mesure>_par_jour
series_atc <- c("mois", "trimestre", "semestre") %>%
  set_names() %>%
  map(~ get_medicam_series(base, niveau = "atc", freq = .x))
walk(series_atc, write_medicam_series)   # outpout/series/atc_<freq>.{rds,csv,_metadata.json}
series_atc_mois <- series_atc$mois

series_cip_trim <- get_medicam_series(base, niveau = "cip", freq = "trimestre")
write_medicam_series(series_cip_trim)   # outpout/series/cip_trimestre.{rds,csv,_metadata.json}

# Export historique (même colonnes qu'avant : code_atc, classe_atc, periode, nb_boite)
series_atc_mois %>%
  select(code_atc, classe_atc = libelle, periode, nb_boite) %>%
  write.csv("outpout/donne_detect_penurie_lola.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. Graphiques
# -----------------------------------------------------------------------------
plot_medicam_series(series_atc_mois, mesure = "nb_boite", dir = "outpout/plots_atc")
