#' @title Visualisation des codes professionnels par département
#' @name plot_departement
#' @param df Data frame contenant les colonnes `Libellé.du.département`, `Libellé.de.la.commune`, et `Code.de.la.catégorie.socio.professionnelle`.
#' @description Affiche un graphique des 10 codes professionnels les plus fréquents pour un département.
#' @importFrom dplyr filter group_by summarise arrange slice_max
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme
#' @export





plot_departement <- function(df) {

  df <- df |>
    filter(!is.na(Libellé.du.département))

  departement_name <- unique(df$Libellé.du.département)
  nombre_communes <- length(unique(df$Libellé.de.la.commune))

  if (length(departement_name) > 1) {
    stop("Le data.frame contient plusieurs départements. Veuillez filtrer pour un seul département.")
  }

  # Filtrer les NA dans 'Libellé.du.département'
  professions_code <- df |>
    filter(!is.na(Libellé.du.département)) |>  # Exclure les NA dans 'Libellé.du.département'
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(desc(n)) |>
    slice_max(n, n = 10)  # Garder uniquement les 10 premiers codes professionnels

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste(departement_name, "-", nombre_communes, "communes"),
      x = paste("Libellés des 10 codes professionnels les plus représentés pour le département", departement_name),
      y = "Code Professionnel"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}
