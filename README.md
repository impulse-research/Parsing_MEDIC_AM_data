# 📦 MedicAM Analysis Pipeline

Ce projet R importe, nettoie, enrichit et agrège les données mensuelles de délivrance de médicaments en ville issues de la base **Medic'AM** (Assurance Maladie). Il produit des **séries temporelles prêtes pour la comparaison de modèles** : nombre de boîtes, dose totale dispensée et nombre de DDD, par ATC ou par CIP, au mois, au trimestre, au semestre ou à l'année.

---

## 🛠️ Structure du projet

```text
.
├── R/
│   ├── parse_medicam_files.R   # Lecture des .xls Medic'AM, format long
│   ├── get_dose_bdd.R          # Référentiels : BDPM (API, cache CSV) + Thériaque (optionnel)
│   ├── add_dosage_info.R       # Nombre d'unités, dosage, dose dispensée, DDD
│   ├── check_dosage.R          # Contrôles : parser de libellés, conflits entre sources
│   ├── aggregate_medicam.R     # Agrégation temporelle et produit
│   └── pipeline.R              # Construction de la base, séries, covariables, sorties
├── config/
│   ├── atc_interet.csv         # ATC suivis (seul endroit à modifier pour en ajouter)
│   ├── substitution.csv        # ATC substituts (covariables de substitution)
│   └── conversion_ddd.csv      # Conversion DDD ↔ unité de dose (ex. lithium)
├── data/
│   ├── medicam/                # Fichiers .xls Medic'AM (Ameli)
│   ├── theriaque.xlsx          # Export Thériaque (optionnel, non versionné)
│   ├── correction_dosage.csv   # Corrections manuelles de dosage
│   └── fichier_intermediaire/  # Caches : base_dosage.csv (BDPM), medicam_base.rds
├── outpout/
│   ├── logs/                   # Journaux de qualité de la dernière construction
│   ├── series/                 # Séries écrites (RDS + CSV + métadonnées)
│   └── plots_atc/              # Graphiques PDF
├── main.R                      # Script principal
└── test_modele.R               # Essais de modèles
```

---

## 📋 Prérequis

```r
install.packages(c("tidyverse", "readxl", "janitor", "jsonlite", "stringi", "writexl"))
```

`writexl` ne sert qu'aux tests. Les fichiers R sont en UTF-8 : chargez-les avec `source(..., encoding = "UTF-8")`, comme le fait `main.R`.

---

## 🚀 Utilisation rapide

```r
library(tidyverse)
list.files("R", pattern = "\\.R$", full.names = TRUE) %>% walk(source, encoding = "UTF-8")

# 1. Base enrichie, une ligne par CIP × mois (relue depuis le cache si rien n'a changé)
base <- build_medicam_base(path_to_theriaque = "data/theriaque.xlsx")

# 2. Séries pour les modèles
series <- get_medicam_series(base, niveau = "atc", freq = "trimestre")

# 3. Découpage apprentissage / test, identique pour tous les modèles
split <- split_temporel(series, debut_test = "2024-01-01")
split$train ; split$test
```

---

## 1. Configuration

### ATC d'intérêt : `config/atc_interet.csv`

```text
code_atc,molecule,actif,commentaire
N05AN01,lithium,TRUE,ajout 2026-11
S01CA01,dexamethasone et anti-infectieux,TRUE,uniquement formes ophtalmiques
```

* **Ajouter un ATC :** ajouter une ligne. Aucun code n'est à modifier, et la base est reconstruite automatiquement au prochain `build_medicam_base()`.
* **Retirer un ATC sans perdre la trace :** passer `actif` à `FALSE`.
* `get_liste_atc()` renvoie les codes actifs. La colonne `molecule` est seulement informative : les sorties utilisent les libellés de Medic'AM.

### Substituts : `config/substitution.csv`

```text
code_atc,code_atc_substitut,commentaire
J01CA04,J01CR02,amoxicilline -> amoxicilline + acide clavulanique
```

* Les lignes commençant par `#` sont ignorées.
* L'ATC substitut doit lui aussi être actif dans `atc_interet.csv`.
* Plusieurs substituts pour un même ATC sont sommés.

### Conversion des DDD : `config/conversion_ddd.csv`

Elle sert quand la DDD n'est pas exprimée dans l'unité de la dose. C'est le cas du lithium : DDD OMS en mmol de Li⁺, dose en mg de carbonate de lithium.

```text
code_atc,unite_ddd,mg_par_unite,commentaire
N05AN01,mmol,36.95,1 mmol Li+ = 36,95 mg de Li2CO3 (à vérifier)
```

### Corrections manuelles : `data/correction_dosage.csv`

```text
cip13,unit_count_per_box,unit_dose,dose_unit,volume_ml,commentaire
3400935793027,1,,,30,amox/clav NR : 30 ml après reconstitution
```

* La correction manuelle est **prioritaire** sur toutes les autres sources.
* **`volume_ml` seul :** dose = concentration (BDPM ou libellé) × volume.
* **`unit_dose` + `dose_unit` :** la valeur est imposée.
* Les lignes commençant par `#` sont ignorées. Le fichier contient des modèles commentés, à compléter après vérification.

---

## 2. Construire la base : `build_medicam_base()`

```r
base <- build_medicam_base(
  liste_atc           = get_liste_atc(),
  medicam_dir         = "data/medicam",
  path_to_theriaque   = "data/theriaque.xlsx",   # NULL : sans Thériaque
  correction_path     = "data/correction_dosage.csv",
  conversion_ddd_path = "config/conversion_ddd.csv",
  rerun               = FALSE,   # TRUE : reconstruit même si le cache est à jour
  rerun_bdpm          = FALSE,   # TRUE : retélécharge la BDPM
  strict              = "warn"   # "error" : stop si un CIP n'a pas de dose
)
```

**Étapes :** `GET_dose_bdd()` → `parse_medicam_files()` → `add_dosage_info()` → `check_ref_conflicts()`.

**Cache.** La base est enregistrée dans `data/fichier_intermediaire/medicam_base.rds`. Elle est relue directement tant que rien ne change parmi :
* les fichiers Medic'AM ;
* l'export Thériaque ;
* les tables de correction et de conversion ;
* le cache BDPM ;
* la liste d'ATC ;
* le code dans `R/`.

Ce mécanisme ne dépend d'aucun outil externe : il compare les empreintes MD5 des fichiers.

**Journaux de qualité.** Ils sont réécrits à chaque construction dans `outpout/logs/` :

| Fichier | Contenu |
|---|---|
| `unparsed.csv` | CIP sans nombre d'unités ou sans dose, triés par nombre de boîtes |
| `excluded.csv` | CIP exclus (formes topiques) et motif de l'exclusion |
| `conflits.csv` | Désaccords BDPM / Thériaque / correction manuelle (> 5 % sur la dose par boîte) |
| `couverture_atc.csv` | Par ATC : part des boîtes avec une dose et avec une DDD, unités, sources |

**Colonnes ajoutées à chaque ligne CIP × mois :**

| Colonne | Description |
|---|---|
| `unit_count_per_box` | Nombre d'unités par boîte (comprimés, flacons…) |
| `unit_dose`, `dose_unit` | Dose par unité, en mg si possible (sinon ui, mmol, %) |
| `dose_per_box`, `total_dispensed_dose` | Dose par boîte, et dose × nombre de boîtes |
| `ddd`, `ddd_unit`, `nb_ddd` | DDD (Thériaque) et nombre de DDD dispensées |
| `src_unit_count`, `src_dose`, `src_ddd` | Source de chaque valeur |

**Sources, par ordre de priorité et champ par champ :**
1. `manuel`, puis `manuel_volume` ;
2. `bdpm` ;
3. `theriaque` ;
4. `nom_court` (lecture du libellé Medic'AM) ;
5. `atc_unique` (imputation quand tous les CIP connus de l'ATC ont la même valeur).

**Exclusions.** Les formes topiques sont exclues par défaut : collyres, pommades, crèmes, gels cutanés, formes nasales, auriculaires et vaginales. Les paramètres `exclude_forme_pharma` et `exclude_label_regex` d'`add_dosage_info()`, transmis via `...`, permettent d'en changer. `NULL` désactive l'exclusion.

---

## 3. Obtenir des séries : `get_medicam_series()`

C'est **la fonction à appeler depuis les modèles**.

```r
get_medicam_series(
  base,
  niveau       = "atc",          # "atc" ou "cip"
  freq         = "trimestre",    # "mois", "trimestre", "semestre", "annee"
  mesures      = c("nb_boite", "total_dispensed_dose", "nb_ddd"),
  atc          = NULL,           # ex. c("J01CA04", "J01CR02")
  debut        = NULL,           # ex. "2018-01-01" (inclus)
  fin          = NULL,
  complete     = TRUE,           # séries régulières (voir ci-dessous)
  substitution = FALSE,          # colonnes subst_* depuis config/substitution.csv
  covariables  = NULL,           # voir section 4
  calendrier   = FALSE,          # ajoute annee, mois, trimestre, semestre
  format       = "long"          # ou "wide"
)
```

### Exemples

```r
# Boîtes mensuelles par ATC
get_medicam_series(base, niveau = "atc", freq = "mois", mesures = "nb_boite")

# DDD trimestrielles par CIP pour deux ATC, depuis 2019
get_medicam_series(base, niveau = "cip", freq = "trimestre", mesures = "nb_ddd",
                   atc = c("J01CA04", "J01CR02"), debut = "2019-01-01")

# Avec substituts et calendrier (ex. pour un modèle de régression)
get_medicam_series(base, niveau = "atc", freq = "mois",
                   substitution = TRUE, calendrier = TRUE)

# Format large (une ligne par période) pour un modèle multivarié
get_medicam_series(base, niveau = "atc", freq = "mois", mesures = "nb_boite", format = "wide")
```

### Colonnes de sortie (format long)

| Colonne | Description |
|---|---|
| `id` | Code ATC (`niveau = "atc"`) ou CIP13 (`niveau = "cip"`) |
| `libelle` | Libellé le plus récent de la série. Il ne change pas d'une période à l'autre, même si Medic'AM change de libellé (« ET » ou « + »). |
| `code_atc` | Code ATC, aussi au niveau CIP |
| `periode` | `"2023-01"`, `"2023-Q1"`, `"2023-S1"`, `"2023"` |
| `date` | Premier jour de la période (`Date`) |
| `nb_boite` | Boîtes remboursées |
| `total_dispensed_dose`, `dose_unit` | Dose totale, dans l'unité majoritaire de la série |
| `nb_ddd` | Nombre de DDD |
| `pct_boites_dose`, `pct_boites_ddd` | Part des boîtes de la période dont la dose (ou la DDD) est connue. **À vérifier avant d'utiliser une mesure de dose :** une couverture inférieure à 1 signifie que la somme est sous-estimée. |
| `subst_atc`, `subst_<mesure>` | Si `substitution = TRUE` : codes des substituts et somme de leurs mesures |

Attributs : `attr(series, "niveau")`, `"freq"` et `"mesures"`.

### Séries régulières (`complete = TRUE`)

* **Niveau ATC :** toutes les périodes de la plage sont présentes. Une période sans dispensation vaut **0**.
* **Niveau CIP :** **0** entre la première et la dernière période où le CIP figure dans Medic'AM, **NA** en dehors. Ainsi, « pas encore commercialisé » ou « retiré » ne se confond pas avec une pénurie.
* Une dose inconnue (NA) dans une période existante reste NA : seules les périodes absentes sont mises à 0.

### Format large (`format = "wide"`)

* Une ligne par période, avec une colonne `<mesure>__<id>` par série (ex. `nb_boite__J01CA04`).
* Les covariables communes à toutes les séries (clé `date` seule) et le calendrier restent en colonnes simples.
* Les covariables propres à un ATC ou à un CIP ne sont gardées qu'en format long.

---

## 4. Covariables : `join_covariables()` ou paramètre `covariables`

```r
series <- get_medicam_series(
  base, niveau = "atc", freq = "trimestre",
  covariables = list(
    grippe  = "data/covariables/grippe.csv",  # chemin CSV
    penurie = df_penurie                       # ou data.frame
  ),
  covariables_agg = list(grippe = "sum", penurie = "max")
)

# Équivalent, en deux temps
series <- get_medicam_series(base, niveau = "atc", freq = "trimestre")
series <- join_covariables(series, list(grippe = "data/covariables/grippe.csv"),
                           agg = list(grippe = "sum"))
```

**La jointure est déduite des colonnes de la covariable :**

| Colonnes de la covariable | Exemple | Jointure |
|---|---|---|
| `date` + valeurs | épidémie de grippe, confinement | toutes les séries |
| `code_atc` + `date` + valeurs | période de pénurie ANSM | par ATC |
| `cip13` + `date` + valeurs | arrêt de commercialisation | par CIP, ou agrégée par ATC si `niveau = "atc"` |
| `code_atc` (ou `cip13`) + valeurs | classe thérapeutique | statique |

**Règles :**
* **Dates :** elles sont ramenées au début de la période de la série. Une covariable mensuelle sur des séries trimestrielles est donc agrégée.
* **Agrégation :** règle `"mean"` par défaut, `"max"` pour les colonnes logiques. Autres règles possibles : `"sum"`, `"min"`, `"last"`. On choisit la règle par covariable (`list(grippe = "sum")`) ou par colonne.
* **Nom des colonnes :** une covariable à une seule colonne de valeur prend le nom de la liste (`grippe`). Avec plusieurs colonnes, chacune devient `<nom>_<colonne>`.
* **Valeurs absentes :** elles restent **NA**. Par exemple, une indicatrice de pénurie vaut `TRUE` ou NA, et c'est au modèle de remplacer NA par `FALSE` si besoin.

---

## 5. Sorties et évaluation

```r
# RDS (types conservés) + CSV (lisible) + métadonnées JSON
write_medicam_series(series, dir = "outpout/series")   # -> atc_trimestre.rds / .csv / _metadata.json

# Relire
series <- readRDS("outpout/series/atc_trimestre.rds")

# Découpage temporel commun à tous les modèles
split <- split_temporel(series, debut_test = "2024-01-01", fin_test = NULL)

# Graphiques PDF, un par série
plot_medicam_series(series, mesure = "nb_ddd", dir = "outpout/plots_atc")
```

Le fichier `_metadata.json` indique la date d'écriture, le niveau, la fréquence, les mesures, le nombre de séries, la plage de dates et les colonnes.

### Brancher un modèle (ex. `test_modele.R`)

```r
series <- get_medicam_series(base, niveau = "atc", freq = "mois", mesures = "nb_boite")
split  <- split_temporel(series, debut_test = "2024-01-01")

df_mp <- calculer_matrix_profile(split$train, col_group = id, col_valeur = nb_boite, col_periode = date)
```

---

## 6. Contrôles qualité

```r
ref_cip      <- GET_dose_bdd(rerun = FALSE, path_to_theriaque = "data/theriaque.xlsx")
result_medoc <- parse_medicam_files(liste_atc = get_liste_atc())

check_ref_conflicts(result_medoc, ref_cip)   # désaccords entre BDPM, Thériaque et correction manuelle
check_label_parser(result_medoc, ref_cip)    # fiabilité de la lecture des libellés (comparée à la BDPM)

attr(base, "unparsed") ; attr(base, "excluded")
couverture_medicam(base)
```

---

## ⚙️ Fonctions de bas niveau

Ces fonctions sont utilisées par le pipeline. Elles restent appelables directement.

| Fonction | Rôle |
|---|---|
| `parse_medicam_files(folder_path, liste_atc)` | Lit les `.xls` Medic'AM et passe au format long (`cip13`, `nom_court`, `code_atc`, `classe_atc`, `annee`, `mois`, `nb_boite`) |
| `GET_dose_bdd(rerun, bdd_path, path_to_theriaque)` | Référentiel BDPM (API + cache CSV) et, optionnellement, Thériaque. Renvoie un data.frame, ou une `list(bdpm, theriaque)` avec Thériaque. |
| `add_dosage_info(data, ref_cip, ...)` | Exclusion des topiques, nombre d'unités, dosage nominal, dose dispensée, DDD |
| `aggregate_medicam(data, freq, by_cip, mesures)` | Agrégation par ATC ou CIP et par période, libellé le plus récent, couverture des doses |

### Points d'attention

* **Associations** (valsartan/HCTZ, amox/clav…) : les doses des substances sont **sommées**. Le résultat est comparable dans le temps à l'intérieur d'un ATC, mais n'a pas de sens pharmacologique.
* **`nb_ddd` des associations à DDD exprimée en masse** (amox/clav) : il inclut l'inhibiteur, ce qui donne une surestimation d'environ 12 %, stable dans le temps. Les associations à DDD en unités de prise (« UD ») ne sont pas concernées.
* **Suspensions à reconstituer :** le volume vient de la BDPM (« 12 cuillères-mesure de 5 ml » = 60 ml), sinon de Thériaque. Les volumes des dispositifs doseurs (seringue 5 mL, godet 54 mL) sont ignorés.
* **Sel ou base :** pour la BDPM, c'est la variante la plus proche du libellé qui est retenue, c'est-à-dire le dosage affiché sur la boîte.
