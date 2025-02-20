library(testthat)


#Test 1: Permet de voir s'il cela indique une erreur si il manque des colonnes au départ
test_that("La fonction génère une erreur si une colonne est manquante", {
  df_incomplet <- data.frame(
    Code.du.département = c("75"),
    Libellé.du.département = c("Paris")
  )

  expect_error(creer_departement(df_incomplet), "Le data.frame doit contenir les colonnes nécessaires pour le département.")
})



# Test 2: permet de voir si le code marche aussi avec les departement d'outre-mer
test_that("Les collectivités à statut particulier sont bien gérées", {
  df_collectivite <- data.frame(
    Code.du.département = c("976", "976"),
    Libellé.du.département = c("Mayotte", "Mayotte"),
    Code.de.la.commune = c("97601", "97602"),
    Libellé.de.la.commune = c("Mamoudzou", "Dzaoudzi"),
    Code.de.la.collectivité.à.statut.particulier = c("976", "976"),
    Libellé.de.la.collectivité.à.statut.particulier = c("Mayotte", "Mayotte"),

    # Colonnes obligatoires pour creer_commune()
    Nom.de.l.élu = c("Dupont", "Durand"),
    Prénom.de.l.élu = c("Jean", "Sophie"),
    Code.sexe = c("M", "F"),
    Date.de.naissance = c("1980-01-01", "1975-06-15"),
    Libellé.de.la.fonction = c("Maire", "Adjoint"),
    Date.de.début.de.la.fonction = c("2020-03-15", "2020-03-15"),
    Code.nationalité = c("FR", "FR"),

    stringsAsFactors = FALSE
  )

  result <- creer_departement(df_collectivite)

  expect_false(is.null(result$Collectivite))  # La collectivité doit être présente
  expect_equal(result$Collectivite$Code.de.la.collectivité.à.statut.particulier, "976")
})
