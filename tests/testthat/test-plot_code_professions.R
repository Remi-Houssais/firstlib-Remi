# Exemple de jeu de données avec différentes catégories socio-professionnelles
df_professions <- tibble::tibble(
  Code.de.la.catégorie.socio.professionnelle = c("A", "B", "A", "C", "B", "A"),
  Nom.de.l.élu = c("Dupont", "Durand", "Martin", "Leclerc", "Joly", "Tanguy")
)

test_that("plot_code_professions calcule correctement les catégories socio-professionnelles", {
  # Créer un graphique sans l'afficher
  result <- plot_code_professions(df_professions)

  # Vérifier que le graphique a bien été généré (ici, juste un test basique pour vérifier que le résultat n'est pas NULL)
  expect_s3_class(result, "gg")  # vérifier que l'objet est un ggplot

  # Vérifier les catégories et le nombre d'élus calculé (en fonction de notre jeu de données)
  expect_equal(result$data$n[1], 3)  # "A" devrait avoir 3 élus
  expect_equal(result$data$n[2], 2)  # "B" devrait avoir 2 élus
  expect_equal(result$data$n[3], 1)  # "C" devrait avoir 1 élu
})


#Second test pour vérifier s'il fait un graph quand il y a pas de df
test_that("plot_code_professions ne génère pas de graphique pour un dataframe vide", {
  # Créer un jeu de données vide
  df_empty <- tibble::tibble(
    Code.de.la.catégorie.socio.professionnelle = character(0),
    Nom.de.l.élu = character(0)
  )

  # Appliquer la fonction avec ce jeu de données vide
  result_empty <- plot_code_professions(df_empty)

  # Vérifier qu'aucune donnée n'est présente dans le résultat
  expect_equal(nrow(result_empty$data), 0)  # Pas de données à afficher
})

