library(firstlib.Remi)
library(readr)
elus_conseillers_municipaux_cm_1_ <- read_delim("data/elus-conseillers-municipaux-cm(1).csv",
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)



colnames(elus_conseillers_municipaux_cm_1_) <- c(
  "Code.du.département",
  "Libellé.du.département",
  "Code.de.la.collectivité.à.statut.particulier",
  "Libellé.de.la.collectivité.à.statut.particulier",
  "Code.de.la.commune",
  "Libellé.de.la.commune",
  "Nom.de.l.élu",
  "Prénom.de.l.élu",
  "Code.sexe",
  "Date.de.naissance",
  "Code.de.la.catégorie.socio.professionnelle",
  "Libellé.de.la.catégorie.socio.professionnelle",
  "Date.de.début.du.mandat",
  "Libellé.de.la.fonction",
  "Date.de.début.de.la.fonction",
  "Code.nationalité"
)

df_nantes <- elus_conseillers_municipaux_cm_1_[elus_conseillers_municipaux_cm_1_$Libellé.de.la.commune == "Nantes", ]
df_Loire_atlantique <- elus_conseillers_municipaux_cm_1_[elus_conseillers_municipaux_cm_1_$Libellé.du.département == "Loire-Atlantique", ]
df_Faverelles <- elus_conseillers_municipaux_cm_1_[elus_conseillers_municipaux_cm_1_$Libellé.de.la.commune == "Faverelles", ]
df_Gers <- elus_conseillers_municipaux_cm_1_[elus_conseillers_municipaux_cm_1_$Libellé.du.département == "Gers", ]


compter_nombre_d_elus(df_nantes)

compter_nombre_d_adjoints(df_nantes)

trouver_l_elu_le_plus_age(df_nantes)

calcul_distribution_age(df_nantes)

plot_code_professions(df_nantes)

summary_commune(df_nantes)
# affectation de la classe 'commune' aux dataframes
class(df_nantes) <- c("commune", class(df_nantes))
#enlever la classe 'comune'
class(df_nantes) <- setdiff(class(df_nantes), "commune")


class(df_Loire_atlantique) <- c("département", class(df_Loire_atlantique))
summary_departement(df_Loire_atlantique)

plot_commune(df_nantes)
plot_departement(df_Gers)


creer_commune(df_nantes)
creer_departement(df_Loire_atlantique)
