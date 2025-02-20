
#Test 1: on vérifie s'il y a bien une erreur s'il l'objet n'est pas un département
test_that("summary_departement échoue si l'objet n'est pas de type 'département'", {
  df_invalid <- tibble::tibble(
    Nom.de.l.élu = c("Dupont", "Durand"),
    Prénom.de.l.élu = c("Jean", "Marie"),
    Date.de.naissance = c("1970-01-01", "1980-02-02")
  )

  expect_error(summary_departement(df_invalid), "L'objet doit être de type 'département'.")
})


# Test 2 pour vérifier si la fonction gère correctement les valeurs manquantes
test_that("summary_departement gère les âges manquants ou invalides", {
  df_invalid_age <- tibble::tibble(
    Libellé.du.département = rep("Paris", 3),
    Libellé.de.la.commune = c("Commune1", "Commune2", "Commune3"),
    Nom.de.l.élu = c("Dupont", "Durand", "Martin"),
    Prénom.de.l.élu = c("Jean", "Marie", "Luc"),
    Date.de.naissance = c("NA", "Inconnu", "-")
  )

  class(df_invalid_age) <- c("département", class(df_invalid_age))  # Ajouter le type 'département'

  # Vérifier que le calcul des âges échoue correctement
  expect_output(summary_departement(df_invalid_age), "Impossible de calculer la distribution des âges : toutes les dates sont invalides.")
})
