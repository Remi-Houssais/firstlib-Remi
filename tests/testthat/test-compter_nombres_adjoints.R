library(testthat)
library(dplyr)
library(tibble)

# Jeu de données simplifié pour tester la fonction, avec uniquement la colonne Libellé.de.la.fonction
df <- tibble(
  Libellé.de.la.fonction = c("Adjoint", "Maire", "Secrétaire", "Adjoint")
)

# Test 1: Vérifier que la fonction compte bien les adjoints
test_that("compter_nombre_d_adjoints compte correctement les adjoints", {
  result <- compter_nombre_d_adjoints(df)
  expect_equal(result, 2)
})

# Test 2: Vérifier que la fonction ignore les majuscules
test_that("compter_nombre_d_adjoints ignore le fait que ca soit en majuscule pour 'adjoint'", {
  df <- df |>
    mutate(Libellé.de.la.fonction = c("Adjoint", "Maire", "Secrétaire", "ADJOINT"))

  result <- compter_nombre_d_adjoints(df)
  expect_equal(result, 2)
})
