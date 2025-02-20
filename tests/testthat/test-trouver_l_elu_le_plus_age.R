
test_that("trouver_l_elu_le_plus_age retourne l'élu le plus âgé", {
  # Exemple de jeu de données avec des élus
  df <- tibble::tibble(
    Nom.de.l.élu = c("Dupont", "Durand", "Leclerc", "Martin"),
    Prénom.de.l.élu = c("Jean", "Marie", "Paul", "Luc"),
    Date.de.naissance = c("01-01-1970", "02-02-1980", "03-03-1990", "04-04-1985")
  )

  # Appeler la fonction
  result <- trouver_l_elu_le_plus_age(df)

  # Le plus âgé doit être Dupont Jean avec une date de naissance en 1970
  expect_equal(result$Nom.de.l.élu, "Dupont")
  expect_equal(result$Prénom.de.l.élu, "Jean")
  expect_equal(result$Âge, as.integer(as.period(interval(dmy("01-01-1970"), Sys.Date()))$year))
})



#Second test
test_that("trouver_l_elu_le_plus_age ignore les valeurs manquantes et invalides", {
  # Exemple de jeu de données avec des valeurs invalides dans les dates de naissance
  df <- tibble::tibble(
    Nom.de.l.élu = c("Dupont", "Durand", "Leclerc", "Martin"),
    Prénom.de.l.élu = c("Jean", "Marie", "Paul", "Luc"),
    Date.de.naissance = c("01-01-1970", "Inconnu", "NA", "04-04-1985")
  )

  # Appeler la fonction
  result <- trouver_l_elu_le_plus_age(df)

  # Le plus âgé doit être Dupont Jean avec une date de naissance en 1970
  expect_equal(result$Nom.de.l.élu, "Dupont")
  expect_equal(result$Prénom.de.l.élu, "Jean")
  expect_equal(result$Âge, as.integer(as.period(interval(dmy("01-01-1970"), Sys.Date()))$year))

# Vérifier que les valeurs invalides ont été ignorées
expect_true(all(!is.na(result$Date.de.naissance)))
})
