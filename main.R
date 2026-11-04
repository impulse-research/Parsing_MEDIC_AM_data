library(tidyverse)
library(httr2)
liste_atc <- c(
  "N05AH04","N06AB06","N06AX16","N07CA04","C10AX13","S01CA01","L01BC02","H02AB06",
  "H02AB07","N06BA04","C03DA01","C03EA04","C07AB03","C07FB03","C09CA03","C09DB01","C09DA03","N03AA03","N03AG06",
  "N04BA02","N03AG01","N04BA03","J01CA04","J01CR02"
  )

# ajouter parametre pour atc/cip complémentaire
# Quetiapine , prednisolone , divazlproate de sodium

# normalisation boite par moi
# plot avec période pénurie



list.files("R", pattern = "\\.R$", full.names = TRUE) %>% 
  walk(source)


ref_cip <- GET_dose_bdd(rerun = TRUE, bdd_path = "data/fichier_intermediaire/base_dosage.csv")

# path_x <- iteration_file[1]#"data/M�dic'AM mensuel 2017 - 2eme semestre_tous r�gimes.xls"

result_medoc <- parse_medicam_files(liste_atc = liste_atc)


result_medoc_dosage <- add_dosage_info(
  data = result_medoc, 
  ref_cip = ref_cip
)



res_trimestre_cip <- aggregate_medicam(
  data = result_medoc, 
  freq = "mois",#"trimestre", 
  by_cip = FALSE
)



write.csv(res_trimestre_cip,"outpout/donne_detect_penurie_lola.csv",row.names = FALSE)


# 1. Préparation des données (création d'une vraie date pour l'axe des x)
df_plot <- result_medoc %>% 
  mutate(
    date = ym(paste(annee, mois, sep = "-")),
    nb_boite = as.numeric(nb_boite)
  ) %>% 
  # Agrégation : somme des boîtes par code ATC, nom de produit et date
  group_by(code_atc, classe_atc, date) %>% 
  summarise(total_boites = sum(nb_boite, na.rm = TRUE), .groups = "drop")
# 2. Création du dossier de destination s'il n'existe pas
output_dir <- "outpout/plots_atc"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 3. Génération et sauvegarde des graphiques en PDF
liste_atc %>% 
  walk(function(atc_code) {
    
    # Filtrage sur le code ATC courant
    df_sub <- df_plot %>% filter(code_atc == atc_code)
    
    if (nrow(df_sub) == 0) return(NULL)
    
    # Récupération du/des nom(s) de produit associés à cet ATC pour le titre
    nom_produit <- paste(unique(df_sub$classe_atc), collapse = " / ")
    
    # Création du graphique
    p <- ggplot(df_sub, aes(x = date, y = total_boites)) +
      geom_line(color = "#2b5c8f", linewidth = 1) +
      geom_point(color = "#2b5c8f", size = 2) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
      labs(
        title = paste("Évolution —", nom_produit),
        subtitle = paste("Code ATC :", atc_code),
        x = "Date",
        y = "Nombre total de boîtes remboursées"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Sauvegarde en PDF
    file_path <- file.path(output_dir, paste0("plot_", atc_code, ".pdf"))
    ggsave(
      filename = file_path, 
      plot = p, 
      width = 9, 
      height = 5, 
      device = "pdf"
    )
  })
