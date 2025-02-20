test_that("plot_departement échoue si plusieurs départements sont présents", {
  # Créer un jeu de données avec plusieurs départements
  df_multiple_departements <- tibble::tibble(
    Libellé.du.département = c("Département1", "Département2", "Département1", "Département2", "Département1"),
    Libellé.de.la.commune = c("Commune1", "Commune2", "Commune3", "Commune4", "Commune5"),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "A", "C", "B"),
    Nom.de.l.élu = c("Dupont", "Durand", "Martin", "Leclerc", "Joly")
  )

  # Tester la fonction
  expect_error(plot_departement(df_multiple_departements),
               "Le data.frame contient plusieurs départements. Veuillez filtrer pour un seul département.")
})

#Test 2: le titre correspond à ce qui a été demandé
test_that("plot_departement affiche correctement le titre avec le nom du département et le nombre de communes", {
  df_single_departement <- tibble::tibble(
    Libellé.du.département = rep("Département1", 5),
    Libellé.de.la.commune = c("Commune1", "Commune2", "Commune3", "Commune4", "Commune5"),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "A", "B", "C"),
    Nom.de.l.élu = c("Dupont", "Durand", "Martin", "Leclerc", "Joly")
  )

  # Générer le graphique
  plot_obj <- plot_departement(df_single_departement)

  # Extraire le titre du graphique
  plot_data <- ggplot_build(plot_obj)
  plot_title <- plot_data$plot$labels$title

  # Vérifier que le titre contient le nom du département et le nombre de communes
  expect_true(grepl("Département1 - 5 communes", plot_title))
})
