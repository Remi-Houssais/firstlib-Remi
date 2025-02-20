library(testthat)


#Test 1 : pour vérifier si la colonne supprime bien les doublons
test_that("La fonction supprime bien les doublons", {
  df_test <- data.frame(
    Code.de.la.commune = c("12345", "12345"),
    Libellé.de.la.commune = c("Exempleville", "Exempleville"),
    Nom.de.l.élu = c("Dupont", "Dupont"),
    Prénom.de.l.élu = c("Jean", "Jean"),
    Code.sexe = c("M", "M"),
    Date.de.naissance = as.Date(c("1970-05-10", "1970-05-10")),
    Libellé.de.la.fonction = c("Maire", "Maire"),
    Date.de.début.de.la.fonction = as.Date(c("2020-01-01", "2020-01-01")),
    Code.nationalité = c("FR", "FR"),
    stringsAsFactors = FALSE
  )

  result <- creer_commune(df_test)

  expect_length(result$Nom.de.l.élu, 1)  # Devrait être unique
})

#Test 2: ne marche pas s'il manque des données
test_that("La fonction génère une erreur si une colonne est manquante", {
  df_incomplet <- data.frame(
    Code.de.la.commune = c("12345"),
    Libellé.de.la.commune = c("Exempleville"),
    Nom.de.l.élu = c("Dupont"),
    Prénom.de.l.élu = c("Jean")
  )

  expect_error(creer_commune(df_incomplet), "Le data.frame doit contenir les colonnes nécessaires pour la commune.")
})


