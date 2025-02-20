#' @title Compter le nombres d'adjoints
#' @name compter_nombres_d_adjoints
#' @param df = un dataframe
#' @description
#' La fonction permet de donner le nombre d'adjoints unique en fonction de son nom, prénom et date de
#' naissance présente dans le dataframe
#' @return Le nombre d'adjoint unique
#' @export
#' @import dplyr

compter_nombre_d_adjoints <- function(df){

    df |>
      filter(grepl("adjoint", Libellé.de.la.fonction, ignore.case = TRUE)) |>
      nrow()
}





