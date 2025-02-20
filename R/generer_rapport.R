#' @title Générer un rapport Quarto pour une commune et un département
#'
#' @description Cette fonction génère un rapport HTML à partir d'un fichier Quarto.
#' Elle utilise les codes de la commune et du département donnée, et sauvegarde le
#' rapport dans un dossier html.
#'
#' @param commune Un code de commune sous forme de chaîne de caractères (ex : "44109").
#' @param departement Un code de département sous forme de chaîne de caractères (ex : "44").
#' @param output Chemin de sortie du fichier HTML généré, sous forme de chaîne de caractères.
#'
#' @return un fichier html
#'
#' @importFrom quarto quarto_render
#' @import ggplot2
#' @examples
#' \dontrun{
#' generer_rapport(
#'   commune = "44109",
#'   departement = "44",
#'   output = "rapport_nantes.html"
#' )
#' }
#'
#' @export







generer_rapport <- function(commune, departement, output) {
  quarto::quarto_render(
    input = file.path("C:", "Users", "Remih", "OneDrive", "Bureau",
                      "cours M1", "R avancé et GIT", "firstlib.Remi",
                      "inst", "rapport.qmd"),

    output_file = output,
    execute_params = list(
      code_commune = commune,
      code_departement = departement
    )
  )
}
