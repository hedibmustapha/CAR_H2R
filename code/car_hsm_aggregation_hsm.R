##### ADJUST ONLY THE CODE HERE ####
##### UNLESS THE TOOL HAS CHANGED ##
CLEANED_DATASET <- "input/car_hsm_data.xlsx"
AGGREGATED_DATASET <- "output/car_hsm_aggregated.csv"
# install.packages("readxl")
####################################
library(tidyverse)
library(readxl)

# Function to aggregate results for questions where one response is preferred over other responses

hsm_preferred_response <- function(x, type, preferred_response) {
  x <- x[!is.na(x)]
  if (preferred_response %in% x[type %in% c("habite_localite", "visite_localite")]) {
    return(preferred_response)
  } else if (any(type %in% c("habite_localite", "visite_localite")) | sum(x %in% preferred_response) == 0) {
    ux <- unique(x)
    table <- tabulate(match(x, ux))
    max_appearance <- max(tabulate(match(x, ux)))
    num_matches <- length(which(tabulate(match(x, ux)) == max_appearance))
    if (num_matches > 1) {
      if (sum(x %in% preferred_response) == max_appearance) {
        return(preferred_response)
      } else {
        return("AC")
      }
    } else {
      ux[which.max(tabulate(match(x, ux)))]
    }
  } else {
    return(preferred_response)
  }
}

# Generic mode function that takes into account KI type

hsm_mode <- function(x, type) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  table <- tabulate(match(x, ux))
  max_appearance <- max(table)
  num_matches <- length(which(table == max_appearance))
  if (num_matches > 1) {
    tied <- ux[which(table == max_appearance)]
    tied_visited <- integer(0)
    for (i in 1:length(tied)) {
      tied_visited[i] <- sum(type[x %in% tied[i]] %in% c("habite_localite", "visite_localite"))
    }
    if (sum(tied_visited == max(tied_visited)) == 1) {
      return (tied[which.max(tied_visited)])
    } else {
      return("AC")
    }
  } else {
    ux[which.max(table)]
  }
}

#Reading data and form
# data <- read_csv(CLEANED_DATASET, col_types = cols(.default = "c"))
data <- read_xlsx(CLEANED_DATASET)

# Template dataset to merge at the end
template_data <- data[0,]

# Remove other questions
data <- data %>%
  select(everything(), -ends_with("_autre"))

# Delete select_multiple text columns (not the binary/logical columns)
data <- data %>%
  select(everything(), -strategie_survie)

# Questions that are always oui
ques_oui <- c("incident_mort", "incident_pillage","mineurs_separes" )

# Questions that are always non
ques_non <- c("sentiment_securite")

# Aggregating all questions
oui_localite_data <- data %>%
  group_by(admin1_localite, admin2_localite, admin3_localite, nom_localite_renseigne) %>%
  summarize_at(vars(ques_oui), ~hsm_preferred_response(., .data[["source_info_localite"]], "oui"))

non_localite_data <- data %>%
  group_by(admin1_localite, admin2_localite, admin3_localite, nom_localite_renseigne) %>%
  summarize_at(vars(ques_non), ~hsm_preferred_response(., .data[["source_info_localite"]], "non"))

autre_localite_data <- data %>%
  group_by(admin1_localite, admin2_localite, admin3_localite, nom_localite_renseigne) %>%
  select(large_deplacement_population:priorite_trois, -one_of(ques_oui, ques_non)) %>%
  summarize_all(hsm_mode, .data[["source_info_localite"]])

# Aggregating all data
localite_data <- oui_localite_data %>%
  left_join(non_localite_data, by = c("admin1_localite", "admin2_localite", "admin3_localite", "nom_localite_renseigne")) %>%
  left_join(autre_localite_data, by = c("admin1_localite", "admin2_localite", "admin3_localite", "nom_localite_renseigne"))

### NOW WE WANT TO CORRECT FOR SKIP LOGIC ###

localite_data <- localite_data %>%
  mutate(raison_deplacement = ifelse(large_deplacement_population == "oui", raison_deplacement, NA),
         proportion_deplaces = ifelse(idp_site == "oui" | idp_famille_accueil == "oui", proportion_deplaces, NA),
         date_dernier_idp = ifelse(idp_site == "oui" | idp_famille_accueil == "oui", date_dernier_idp, NA),
         provenance_dernier_idp = ifelse(idp_site == "oui" | idp_famille_accueil == "oui", provenance_dernier_idp, NA),
         provenance_dernier_idp_prefecture = ifelse(provenance_dernier_idp == "oui",provenance_dernier_idp_prefecture, NA),
         provenance_dernier_idp_sous_prefecture = ifelse(provenance_dernier_idp == "oui",provenance_dernier_idp_sous_prefecture, NA),
         raison_presence = ifelse(idp_site == "oui" | idp_famille_accueil == "oui",raison_presence, NA),
         condition_retour = ifelse(idp_site == "oui" | idp_famille_accueil == "oui",condition_retour, NA),
         date_retour = ifelse(retourne_dernier_mois == "oui",date_retour, NA),
         provenance_dernier_retourne = ifelse(retourne_dernier_mois == "oui",provenance_dernier_retourne, NA),
         provenance_dernier_retourne_prefecture = ifelse(retourne_dernier_mois == "oui",provenance_dernier_retourne_prefecture, NA),
         provenance_dernier_retourne_sous_prefecture = ifelse(retourne_dernier_mois == "oui", provenance_dernier_retourne_sous_prefecture, NA),
         date_retour_rapatries = ifelse(presence_rapatries == "oui", date_retour_rapatries, NA),
         provenance_rapatries = ifelse(presence_rapatries == "oui", provenance_rapatries, NA),
         provenance_rapatries_pays = ifelse(presence_rapatries == "oui", provenance_rapatries_pays, NA),
         raison_rapatriement = ifelse(retourne_dernier_mois == "oui" | presence_rapatries == "oui", raison_rapatriement, NA),
         raison_inaccessible = ifelse(acces_nourriture == "non",raison_inaccessible, NA),
         marche_fonctionnel_acces = ifelse(marche_fonctionnel == "oui", marche_fonctionnel_acces, NA),
         marche_fonctionnel_prix = ifelse(marche_fonctionnel == "oui", marche_fonctionnel_prix, NA),
         type_betail = ifelse(possession_betail == "oui", type_betail, NA),
         epidemie_betail = ifelse(possession_betail == "oui", epidemie_betail, NA),
         raison_accouchement_maison = ifelse(lieu_accouchement == "maison", raison_accouchement_maison, NA),
         lieu_soin_choisi = ifelse(service_sante_distance_marche == "oui",lieu_soin_choisi, NA),
         service_sante_duree_marche = ifelse(service_sante_distance_marche == "oui", service_sante_duree_marche, NA),
         service_sante_barriere = ifelse(service_sante_acces == "pas_suffisant" | service_sante_acces == "insuffisant", service_sante_barriere, NA),
         mineurs_separes_raison = ifelse(mineurs_separes == "oui", mineurs_separes_raison, NA),
         type_abris_refugie_idp = ifelse(idp_site == "oui" | idp_famille_accueil == "oui", type_abris_refugie_idp, NA),
         type_abris_retourne_rapatrie = ifelse(retourne_dernier_mois == "oui" | presence_rapatries == "oui", type_abris_retourne_rapatrie, NA),
         relation_idp_hote = ifelse(idp_site == "oui" | idp_famille_accueil == "oui" | retourne_dernier_mois == "oui" | presence_rapatries == "oui", relation_idp_hote, NA),
         acces_eau_barriere = ifelse(acces_eau == "pas_suffisant" | acces_eau == "insuffisant", acces_eau_barriere, NA),
         service_education_dispo = ifelse(service_education_fonctionnel == "oui", service_education_dispo, NA),
         service_education_indispo = ifelse(service_education_fonctionnel == "non", service_education_indispo, NA),
         service_education_indispo_insecurite = ifelse(service_education_indispo == "insecurite", service_education_indispo_insecurite, NA),
         participation_fille = ifelse(service_education_fonctionnel == "oui", participation_fille, NA),
         participation_garcon = ifelse(service_education_fonctionnel == "oui", participation_garcon, NA),
         participation_fille_ado = ifelse(service_education_fonctionnel == "oui", participation_fille_ado, NA),
         participation_garcon_ado = ifelse(service_education_fonctionnel == "oui", participation_garcon_ado, NA),
         acces_education_barriere = ifelse(service_education_fonctionnel == "oui", acces_education_barriere, NA),
         reseau_tel_fonctionnel_rupture = ifelse(reseau_tel_fonctionnel == "oui", reseau_tel_fonctionnel_rupture, NA)) 


# SAVING FINAL DATA

final_data <- bind_rows(template_data, localite_data)

# write_csv(final_data, AGGREGATED_DATASET, na = "")
write.csv(final_data, AGGREGATED_DATASET, na = "")

