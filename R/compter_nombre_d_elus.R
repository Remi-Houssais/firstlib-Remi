#' @title Compter le nombre d'élus
#' @name compter_nombre_d_elus
#' @param df un dataframe
#' @description
#' Elle compte le nombre unique d’élus basés sur le nom/prénom/date de naissance
#' @return le nombre d'élus
#' @export
#' @import dplyr




compter_nombre_d_elus <- function(df) {
  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    nrow()
}


