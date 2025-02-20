#' @title Résumé du département
#' @name summary_departement
#' @param x Un objet de type "département".
#' @description
#' La fonction affiche les informations d’un département donné (nom, nombre de communes, nombre d’élu·e·s,
#' distribution des âges) et identifie l’élu·e le/la plus âgé·e et le/la plus jeune.
#' Elle calcule également la commune avec la moyenne d’âge la plus faible et la plus élevée.
#' @return Retourne les informations sur le département.
#' @importFrom dplyr mutate filter slice group_by summarise pull
#' @importFrom lubridate dmy interval years
#' @export





summary_departement <- function(x) {
  library(dplyr)
  library(lubridate)

  if (!inherits(x, "département")) {
    stop("L'objet doit être de type 'département'.")
  }

  cat("Nom du département :", unique(x$Libellé.du.département), "\n")
  cat("Nombre de communes :", length(unique(x$Libellé.de.la.commune)), "\n")
  cat("Nombre d'élu.e.s :", nrow(x), "\n")

  cat("Distribution des âges des élu.e.s :", "\n")

  # 🔹 Étape 2 : Nettoyer la colonne avant conversion
  x <- x |>
    mutate(Date.de.naissance = as.character(Date.de.naissance),  # S'assurer que c'est bien du texte
           Date.de.naissance = ifelse(Date.de.naissance %in% c("", "NA", "Inconnu", "-"), NA, Date.de.naissance),
           Date.de.naissance = dmy(Date.de.naissance),  # Conversion propre
           Âge = ifelse(is.na(Date.de.naissance), NA, as.integer(interval(Date.de.naissance, Sys.Date()) / years(1))))

  if (all(is.na(x$Date.de.naissance))) {
    cat("Impossible de calculer la distribution des âges : toutes les dates sont invalides.\n")
  } else {
    print(summary(x$Âge, na.rm = TRUE))
  }

  # Trouver l'élu le plus âgé
  trouver_l_elu_le_plus_age <- function(df) {
    df |>
      filter(!is.na(Date.de.naissance)) |>
      slice(which.min(Date.de.naissance)) |>
      mutate(Âge = as.integer(interval(Date.de.naissance, Sys.Date()) / years(1)))
  }

  elu_max <- trouver_l_elu_le_plus_age(x)
  if (nrow(elu_max) > 0) {
    cat("Élu.e le/la plus âgé.e :", elu_max$Nom.de.l.élu,
        ", Âge :", elu_max$Âge,
        ", Commune :", elu_max$Libellé.de.la.commune, "\n")
  } else {
    cat("Impossible de déterminer l'élu.e le/la plus âgé.e.\n")
  }

  # Trouver l'élu le plus jeune
  trouver_l_elu_le_plus_jeune <- function(df) {
    df |>
      filter(!is.na(Date.de.naissance)) |>
      slice(which.max(Date.de.naissance)) |>
      mutate(Âge = as.integer(interval(Date.de.naissance, Sys.Date()) / years(1)))
  }

  elu_min <- trouver_l_elu_le_plus_jeune(x)
  if (nrow(elu_min) > 0) {
    cat("Élu.e le/la plus jeune :", elu_min$Nom.de.l.élu,
        ", Âge :", elu_min$Âge,
        ", Commune :", elu_min$Libellé.de.la.commune, "\n")
  } else {
    cat("Impossible de déterminer l'élu.e le/la plus jeune.\n")
  }

  # Commune avec la moyenne d’âge la plus faible
  age_par_commune <- x |>
    group_by(Libellé.de.la.commune) |>
    summarise(Age.Moyen = mean(Âge, na.rm = TRUE)) |>
    filter(!is.na(Age.Moyen))

  if (nrow(age_par_commune) > 0) {
    commune_min_age <- age_par_commune |> filter(Age.Moyen == min(Age.Moyen)) |> pull(Libellé.de.la.commune)
    cat("Commune avec la moyenne d’âge la plus faible :", commune_min_age, "\n")
    print(summary(x$Âge[x$Libellé.de.la.commune == commune_min_age], na.rm = TRUE))

    commune_max_age <- age_par_commune |> filter(Age.Moyen == max(Age.Moyen)) |> pull(Libellé.de.la.commune)
    cat("Commune avec la moyenne d’âge la plus élevée :", commune_max_age, "\n")
    print(summary(x$Âge[x$Libellé.de.la.commune == commune_max_age], na.rm = TRUE))
  } else {
    cat("Impossible de calculer les moyennes d'âge par commune.\n")
  }
}
