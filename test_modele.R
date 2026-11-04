library(tidyverse)
library(tsmp)
liste_atc <- c(
  "N05AH04","N06AB06","N06AX16","N07CA04","C10AX13","S01CA01","L01BC02","H02AB06",
  "H02AB07","N06BA04","C03DA01","C03EA04","C07AB03","C07FB03","C09CA03","C09DB01","C09DA03","N03AA03","N03AG06",
  "N04BA02","N03AG01","N04BA03","J01CA04","J01CR02"
)


df_base <- read.csv("outpout/donne_detect_penurie_lola.csv")


calculer_matrix_profile <- function(df, 
                                    col_group = code_atc, 
                                    col_valeur = nb_boite, 
                                    col_periode = periode, 
                                    window_sizes = c(#3,
                                                     6, 12)) {
  
  col_group_var  <- enquo(col_group)
  col_valeur_var <- enquo(col_valeur)
  col_periode_var <- enquo(col_periode)
  
  df %>%
    # Tri chronologique par groupe
    arrange(!!col_group_var, !!col_periode_var) %>%
    group_by(!!col_group_var) %>%
    group_split() %>%
    map_dfr(function(sub_df) {
      group_val <- unique(pull(sub_df, !!col_group_var))
      ts_vec <- as.numeric(pull(sub_df, !!col_valeur_var))
      n <- length(ts_vec)
      
      map_dfr(window_sizes, function(m) {
        if (n >= m + 1) {
          mp_obj <- tsmp(ts_vec, window_size = m, verbose = 0)
          
          tibble(
            identifiant = group_val,
            m = m,
            index_sous_sequence = seq_along(mp_obj$mp),
            matrix_profile = mp_obj$mp,
            nn_index = mp_obj$pi
          ) %>%
            # Renomme dynamiquement la colonne de groupe avec le nom d'origine
            rename(!!as_label(col_group_var) := identifiant)
          
        } else {
          message(sprintf("Groupe %s (%s) : série trop courte (n = %d) pour m = %d", 
                          as_label(col_group_var), group_val, n, m))
          tibble()
        }
      })
    })
}

df_mp_cip  <- calculer_matrix_profile(df_base, col_group = code_atc)
