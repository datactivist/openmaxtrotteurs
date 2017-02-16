library(googlesheets)
library(tidyverse)
library(rex)
reponses <- gs_title("Open Maxtrotteurs (réponses)")
reponses_df <- reponses %>% gs_read()
names(reponses_df) <- c("Horodatage", "trajets", "genre", "age", "residence", "accord")

rex_mode()

nb_trajet <- rex(regex("(?s)"),
                 maybe(
                    anything,
                    space,
                    capture(name = "nb_trajets",
                            one_or_more(number)),
                    " trajets réalisés")
                 )

re_matches(reponses_df$trajets, nb_trajet)  


dates <- rex(
  capture(name = "date",
          letters,
          space,
          n_times(number, 2),
          space,
          non_numbers,
          space,
          n_times(number, 4))
)
re_matches(reponses_df[["trajets"]], dates, global = TRUE)

train <- rex(
  newline,
  capture(
    name = "train",
    n_times(number, 4)
  )
)
re_matches(reponses_df[["trajets"]], train, global = TRUE)

duree <- rex(
  regex("(?s)"),
  "Temps du trajet :",
  capture(
    name = "duree",
    n_times(number, 2),
    "h",
    n_times(number, 2)
  )
)

heure_depart <- rex(
  newline,
  n_times(number, 4),
  newline,
  capture(
    name = "heure_depart",
    n_times(number, 2),
    "h",
    n_times(number, 2)
  )
)

heure_arrivee <- rex(
  newline,
  n_times(number, 2),
  "h",
  n_times(number, 2),
  newline %or% space,
  capture(
    name = "heure_arrivee",
    n_times(number, 2),
    "h",
    n_times(number, 2)
  )
)
re_matches(reponses_df[["trajets"]], heure_arrivee, global = TRUE)

gares <- list("Agen", "Aix-en-Provence TGV", "Albertville", "Annemasse", "Auray", "Avignon", "Avignon TGV", "Bayonne", "Belle Plagne", "Bellegarde", "Béziers", "Biarritz", "Bordeaux St Jean", "Bourg-Saint-Maurice", "Brest", "Cannes", "Chambéry", "Chamrousse", "Cluses", "Courchevel 1300", "Courchevel 1550", "Courchevel 1650", "Courchevel 1850", "Dax", "Flaine", "Grenoble", "Hendaye", "La Rochelle", "Landerneau", "Landry", "Le Grand Tourmalet", "Les 2 Alpes", "Les Carroz", "Les Gets", "Les Ménuires", "Lorient", "Lyon", "L’Alpe d’Huez", "Marseille Gare Saint-Charles", "Méribel Centre", "Méribel Mottaret", "Méribel Village", "Montpellier", "Morlaix", "Morzine", "Morzine les Prodains", "Moûtiers", "Nice ville", "Nimes", "Niort", "Paris Gare de Lyon", "Paris Montparnasse 1 et 2", "Perpignan", "Peyragudes", "Plagne Aime 2000", "Plagne Bellecôte", "Plagne Centre", "Plagne Soleil", "Plagne Village", "Quimper", "Quimperlé", "Redon", "Rennes", "Rosporden", "Saint-Brieuc", "Saint-Gervais", "Saint-Jean-de-Luz", "Saint-Raphaël", "Sallanches", "Tignes 1800", "Tignes Le Lac", "Tignes Les Brévières", "Tignes Val Claret", "Toulon", "Toulouse", "Val d’Isère Gare routière", "Val d’Isère la Daille", "Val Thorens", "Vannes")

gare_depart <- rex(
  n_times(number, 2),
  "h",
  n_times(number, 2),
  newline %or% space,
  n_times(number, 2),
  "h",
  n_times(number, 2),
  newline,
  capture(
    name = "gare_depart",
    or(gares)
  ),
  maybe(newline)
)
re_matches(reponses_df[["trajets"]], gare_depart, global = TRUE)

gare_arrivee <- rex(
  n_times(number, 2),
  "h",
  n_times(number, 2),
  newline %or% space,
  n_times(number, 2),
  "h",
  n_times(number, 2),
  newline,
  or(gares),
  maybe(space %or% newline),
  capture(
    name = "gare_arrivee",
    or(gares)
  )
)
re_matches(reponses_df[["trajets"]], gare_arrivee, global = TRUE)




trajets <- reponses_df %>% 
  tibble::rownames_to_column() %>% 
  rename(ID = rowname) %>% 
  select(-Horodatage) %>% 
  filter(!is.na(accord)) %>% 
  select(-accord) %>% 
  mutate(nb_trajets = re_matches(trajets, nb_trajet)$nb_trajets, 
         dates = map(re_matches(trajets, dates, global = TRUE), "date"),
         train = map(re_matches(trajets, train, global = TRUE), "train"),
         duree = map(re_matches(trajets, duree, global = TRUE), "duree"),
         heure_depart = map(re_matches(trajets, heure_depart, global = TRUE), "heure_depart"),
         heure_arrivee = map(re_matches(trajets, heure_arrivee, global = TRUE), "heure_arrivee"),
         gare_depart = map(re_matches(trajets, gare_depart, global = TRUE), "gare_depart"),
         gare_arrivee = map(re_matches(trajets, gare_arrivee, global = TRUE), "gare_arrivee")) %>% 
  select(-trajets) %>% 
  unnest() %>% 
  glimpse()

tmpfile <- paste0(tempfile(), ".csv")

write_excel_csv(trajets, path = tmpfile)

gs_upload(tmpfile, sheet_title = "Trajets OpenMaxtrotteurs", overwrite = TRUE)
         