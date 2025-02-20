#' @title Résumé de la commune
#' @name summary_commune
#' @param x un objet de type commune choisit pour avoir des informations
#' @description
#' La fonction affiche les informations d’une commune donnée (nom, nombre d’élu·e·s, distribution des âges)
#' et identifie l’élu·e le/la plus âgé·e
#' @return Des inforformations sur la commune.
#' @export





summary_commune <- function(x) {

  if (!inherits(x, "commune")) {
    stop("L'objet doit être de type 'commune'.")
  }

  elu_plus_age <- trouver_l_elu_le_plus_age(x)

  cat("Nom de la commune :", unique(x$Libellé.de.la.commune), "\n")
  cat("Nombre d'élu.e.s :",  compter_nombre_d_elus(x), "\n")

  cat("Distribution des âges des élu.e.s :", "\n")
  print(calcul_distribution_age(x))

  cat("L'élu.e le ou la plus âgé.e :", "\n")
  cat("Nom :", elu_plus_age$Nom.de.l.élu, "\n")
  cat("Prénom :", elu_plus_age$Prénom.de.l.élu, "\n")
  cat("Âge :", elu_plus_age$Date.de.naissance, "\n")

}


