#' @title plot du code de la profession
#' @name plot_code_professions
#' @param df Un dataframe
#' @description
#' la fonction comptera, pour chaque code professionnel, le nombre d’élus ayant ce code. Un bar chart
#' horizontal est utiliser pour représenter ces informations.
#' @return un histogramme horizontal.
#' @export
#' @import ggplot2

plot_code_professions <- function(df) {


  # Compter les occurrences de chaque code socio professionnels
  professions_code <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n()) |>
    # On compte le nombre d'élus par catégories
    arrange(desc(n))
  # On arrange par ordre décroissant

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    # stat identidy affiche directement les valeurs
    labs(title = paste("Nombre d'élus par catégorie socio professionnelle"),
         x = "Nombre d'élus", y = "Code Professionnel") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}
