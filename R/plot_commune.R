#' @title Visualisation des professions des élus d'une commune
#' @name plot_commune
#' @param df Un dataframe contenant les données des élus d'une commune. Il doit contenir les colonnes :
#'   - `Libellé.de.la.commune` : le nom de la commune
#'   - `Libellé.du.département` : le nom du département
#'   - `Code.de.la.catégorie.socio.professionnelle` : le code de la catégorie socio-professionnelle
#' @description
#' Cette fonction crée un graphique à barres représentant la répartition des professions des élus d'une commune.
#' Le graphique affiche les codes professionnels des élus, leur fréquence et est filtré pour une seule commune.
#' @return Un graphique à barres généré par `ggplot2` affichant les catégories socio-professionnelles des élus d'une commune.
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme
#' @import dplyr
#' @export



plot_commune <- function(df) {

  commune_name <- unique(df$Libellé.de.la.commune)
  departement_name <- unique(df$Libellé.du.département)

  if (length(commune_name) > 1) {
    stop("Le data.frame contient plusieurs communes. Veuillez filtrer pour une seule commune.")
  }

  professions_code <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(desc(n))

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste(commune_name, "-", departement_name),
      x = paste("Libellés des codes professionnels pour les élus (", compter_nombre_d_elus(df), " élus)", sep = ""),
      y = "Code Professionnel"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}
