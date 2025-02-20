library(testthat)
library(ggplot2)

# Test 1 : Vérifier que la fonction génère une erreur si plusieurs communes sont présentes
test_that("plot_commune échoue si plusieurs communes sont présentes", {
  df_multiple_communes <- tibble::tibble(
    Libellé.de.la.commune = c("Commune1", "Commune2"),
    Libellé.du.département = rep("Département1", 2),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B"),
    Nom.de.l.élu = c("Dupont", "Durand")
  )

  expect_error(plot_commune(df_multiple_communes),
               "Le data.frame contient plusieurs communes. Veuillez filtrer pour une seule commune.")
})


#Test 2 Vérifie si le titre est réalisé comme demandé
test_that("plot_commune affiche correctement le titre avec le nom de la commune et du département", {
  df_single_commune <- tibble::tibble(
    Libellé.de.la.commune = rep("Commune1", 5),
    Libellé.du.département = rep("Département1", 5),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "A", "B", "C"),
    Nom.de.l.élu = c("Dupont", "Durand", "Martin", "Leclerc", "Joly"),
    Prénom.de.l.élu = c("Jean", "Paul", "Michel", "Sophie", "Pierre"),
    Date.de.naissance = as.Date(c("1980-01-01", "1975-05-12", "1990-07-10", "1982-09-23", "1984-11-11"))
  )

  # Générer le graphique
  plot_obj <- plot_commune(df_single_commune)

  # Extraire le titre du graphique
  plot_data <- ggplot_build(plot_obj)
  plot_title <- plot_data$plot$labels$title

  # Vérifier que le titre contient la commune et le département
  expect_true(grepl("Commune1 - Département1", plot_title))
})
