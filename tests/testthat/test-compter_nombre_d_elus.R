# tests/testthat/test-compter_nombre_d_elus.R
library(testthat)
library(dplyr)

# Exemple de jeu de données pour tester la fonction
df <- tibble::tibble(
  Nom.de.l.élu = c("Dupont", "Durand", "Jacques", "Leclerc", "Martin"),
  Prénom.de.l.élu = c("Jean", "Marie", "Jean", "Paul", "Luc"),
  Date.de.naissance = c("01-01-1970", "02-02-1980", "01-01-1970", "03-03-1990", "04-04-1985")
)

test_that("compter_nombre_d_elus retourne un nombre correct", {

  result <- compter_nombre_d_elus(df)

  # Le nombre d'élus distincts devrait être 5 (après avoir retiré les doublons)
  expect_equal(result, 5)
})



test_that("compter_nombre_d_elus sur un data frame vide", {
  # Test avec un DataFrame vide
  df_empty <- tibble::tibble(Nom.de.l.élu = character(0),
                             Prénom.de.l.élu = character(0),
                             Date.de.naissance = character(0))

  result <- compter_nombre_d_elus(df_empty)

  # La fonction devrait retourner 0 car il n'y a pas d'élus
  expect_equal(result, 0)
})
