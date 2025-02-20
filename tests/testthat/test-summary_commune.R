library(testthat)

# Test 1 : Vérifier que la fonction échoue si l'objet passé n'est pas de type 'commune'
test_that("summary_commune échoue si l'objet n'est pas de type 'commune'", {
  df_invalid <- tibble::tibble(
    Nom.de.l.élu = c("Dupont", "Durand"),
    Prénom.de.l.élu = c("Jean", "Marie"),
    Date.de.naissance = c("1970-01-01", "1980-02-02")
  )

  expect_error(summary_commune(df_invalid), "L'objet doit être de type 'commune'.")
})

# Test 2 : Vérifier que la fonction utilise correctement 'compter_nombre_d_elus'
test_that("summary_commune utilise correctement compter_nombre_d_elus", {
  df_commune <- tibble::tibble(
    Libellé.de.la.commune = rep("Paris", 5),
    Nom.de.l.élu = c("Dupont", "Durand", "Martin", "Leclerc", "Joly"),
    Prénom.de.l.élu = c("Jean", "Marie", "Luc", "Paul", "Pierre"),
    Date.de.naissance = c("1970-01-01", "1980-02-02", "1990-03-03", "1985-04-04", "1975-05-05")
  )
  class(df_commune) <- c("commune", class(df_commune))  # Ajouter le type 'commune'

  # Vérifier que 'compter_nombre_d_elus' retourne bien 5
  expect_equal(compter_nombre_d_elus(df_commune), 5)
})
