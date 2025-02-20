#' @title Calcul de la distribution des âges des élus
#' @name calcul_distribution_age
#' @param df Un dataframe
#' @description
#' La fonction calcule les quantiles 0, 25, 50, 75 et 100 de l'âge des élus
#' @return Un vecteur contenant les quantiles de l'âge.
#' @export
#' @import dplyr
#' @importFrom lubridate today dmy
#' @importFrom stats quantile

calcul_distribution_age <- function(df) {
  # Convertir la colonne 'Date.de.naissance' en date et calculer l'âge
  df <- df |>
    mutate(age = as.numeric(difftime(today(), dmy(Date.de.naissance), units = "days")) / 365)

  # Exclure les NA de la colonne 'age' pour calculer les quantiles
  df_clean <- df |> filter(!is.na(age))  # Filtrer les lignes avec NA dans 'age'

  # Calculer les quantiles
  quantiles <- quantile(df_clean$age, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)

  return(quantiles)
}



