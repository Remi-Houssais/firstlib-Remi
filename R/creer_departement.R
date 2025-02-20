#' @title Création d'un objet département
#' @name creer_departement
#'
#' @description
#' Cette fonction génère un objet de classe `departement` à partir d'un `data.frame` contenant des informations
#' sur les communes d'un département.
#' @param df Un `data.frame` contenant les données des communes d'un département. Il doit contenir au minimum les colonnes :
#'   - `Code.du.département` : code du département
#'   - `Libellé.du.département` : nom du département
#'   - `Code.de.la.commune` : code de la commune
#'   - `Libellé.de.la.commune` : nom de la commune
#' @return Une liste de classe `"departement"` contenant :
#'   - `Code.du.département` : le code du département
#'   - `Libellé.du.département` : le nom du département
#'   - `Collectivite` : (si applicable) les informations sur la collectivité à statut particulier
#'   - `Communes` : une liste d'objets `commune`, un par commune du département
#' @import dplyr
#' @export




creer_departement <- function(df) {
  # Colonnes obligatoires
  required_columns <- c(
    "Code.du.département",
    "Libellé.du.département",
    "Code.de.la.commune",
    "Libellé.de.la.commune"
  )

  # Vérification des colonnes
  if (!all(required_columns %in% colnames(df))) {
    stop("Le data.frame doit contenir les colonnes nécessaires pour le département.")
  }

  # Gestion des collectivités à statut particulier (optionnel)
  has_collectivite <- all(c("Code.de.la.collectivité.à.statut.particulier",
                            "Libellé.de.la.collectivité.à.statut.particulier") %in% colnames(df))

  # Création de la liste pour le département
  departement <- list(
    Code.du.département = unique(df$Code.du.département),
    Libellé.du.département = unique(df$Libellé.du.département),

    # Intégration des collectivités si présentes
    Collectivite = if (has_collectivite) {
      unique(df[, c("Code.de.la.collectivité.à.statut.particulier",
                    "Libellé.de.la.collectivité.à.statut.particulier")])
    } else {
      NULL
    },

    # Liste des communes associées au département
    Communes = lapply(split(df, df$Code.de.la.commune), creer_commune)
  )

  # Attribution de la classe 'departement'
  class(departement) <- "departement"

  return(departement)
}

