#' @title Création d'un objet représentant une commune
#' @name creer_commune
#' @param df Un data.frame contenant les données des élus. Il doit inclure les colonnes suivantes :
#'   - `Code.de.la.commune` : Code de la commune
#'   - `Libellé.de.la.commune` : Nom de la commune
#'   - `Nom.de.l.élu` : Nom de l’élu
#'   - `Prénom.de.l.élu` : Prénom de l’élu
#'   - `Code.sexe` : Code du sexe de l’élu
#'   - `Date.de.naissance` : Date de naissance de l’élu
#'   - `Libellé.de.la.fonction` : Fonction de l’élu
#'   - `Date.de.début.de.la.fonction` : Date de début de la fonction
#'   - `Code.nationalité` : Code de la nationalité de l’élu
#' @description
#' Cette fonction crée une liste représentant une commune à partir des données des élus.
#' @return Une liste de classe `"commune"` contenant les informations uniques sur les élus d'une commune.
#' @export







creer_commune <- function(df) {


  # Colonnes requises
  required_columns <- c(
    "Code.de.la.commune",
    "Libellé.de.la.commune",
    "Nom.de.l.élu",
    "Prénom.de.l.élu",
    "Code.sexe",
    "Date.de.naissance",
    "Libellé.de.la.fonction",
    "Date.de.début.de.la.fonction",
    "Code.nationalité"
  )

  # Vérification des colonnes
  if (!all(required_columns %in% colnames(df))) {
    stop("Le data.frame doit contenir les colonnes nécessaires pour la commune.")
  }

  # Nettoyage des doublons exacts
  df <- df[!duplicated(df), ]

  # Création de la liste pour la commune
  commune <- list(
    Code.de.la.commune = unique(df$Code.de.la.commune),
    Libellé.de.la.commune = unique(df$Libellé.de.la.commune),
    Nom.de.l.élu = unique(df$Nom.de.l.élu),
    Prénom.de.l.élu = unique(df$Prénom.de.l.élu),
    Code.sexe = unique(df$Code.sexe),
    Date.de.naissance = unique(df$Date.de.naissance),
    Libellé.de.la.fonction = unique(df$Libellé.de.la.fonction),
    Date.de.début.de.la.fonction = unique(df$Date.de.début.de.la.fonction),
    Code.nationalité = unique(df$Code.nationalité))

  # Attribution de la classe 'commune'
  class(commune) <- "commune"

  return(commune)
}
