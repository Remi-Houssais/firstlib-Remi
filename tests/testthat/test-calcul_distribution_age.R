# Test de la fonction calcul_distribution_age
library(testthat)
library(dplyr)
library(lubridate)

# Jeu de données pour tester la fonction
df <- tibble::tibble(
  Nom.de.l.élu = c("Dupont", "Durand", "Leclerc", "Martin", "Benoit"),
  Prénom.de.l.élu = c("Jean", "Marie", "Paul", "Luc", "Sophie"),
  Date.de.naissance = c("01-01-1980", "02-02-1990", "01-01-2000", "03-03-1985", "04-04-1995")
)

# Test 1: Vérifier que la fonction calcule correctement les quantiles d'âge
test_that("calcul_distribution_age calcule les quantiles d'âge correctement", {
  result <- calcul_distribution_age(df)

  expect_true(all(names(result) == c("0%", "25%", "50%", "75%", "100%")))
  expect_true(all(!is.na(result)))  # Vérifier que les quantiles ne sont pas NA
 # Le quantile à 100% doit correspondre a l'age le plus grand
})

# Test 2: Vérifier que la fonction qu'il y ait bien 5 valeurs qui apparaissent mêmê avec des valeurs manquantes
df_na <- tibble::tibble(
  Nom.de.l.élu = c("Dupont", "Durand", "Leclerc", "Martin", "Benoit"),
  Prénom.de.l.élu = c("Jean", "Marie", "Paul", "Luc", "Sophie"),
  Date.de.naissance = c("01-01-1980", "", "01-01-2000", "03-03-1985", "04-04-1995")
)

test_that("calcul_distribution_age gère les valeurs NA correctement", {
  result <- calcul_distribution_age(df_na)

  expect_true(all(!is.na(result)))
  expect_equal(length(result), 5)  # Vérifier qu'il y a 5 valeurs pour les quantiles (0%, 25%, 50%, 75%, 100%)
})

