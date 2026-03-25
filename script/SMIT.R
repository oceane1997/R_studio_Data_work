#----- PACKAGES NECESSAIRES----

library(readxl)
library(questionr)
library(tidyverse)
library(gtsummary)
library(labelled)
library(forcats)
#library(broom)
library(stringr)
library(tidyr)
library(stringi)
library(lubridate)
library(gt)
library(glue)
library(purrr)
library(GGally)
library(MASS)
library(forestmodel)


###################################
SMIT <- read_excel("Data/Mortality_2023-2024.xlsx")
View(SMIT)

str(SMIT)
glimpse(SMIT)

names(SMIT)
###################################################################
#-------------------CONCORDANCE DICTIONNAIRE EXCEL ET VALUE LABEL


# 0) Lire les feuilles XLSForm
path_xlsform <- "Data/MortalityXLS.xlsx"
survey  <- read_excel(path_xlsform, sheet = "survey")  |> rename_with(tolower)
choices <- read_excel(path_xlsform, sheet = "choices") |> rename_with(tolower)

# 1) variable -> list_name à partir de survey (select_one / select_multiple)
survey_lists <- survey |>
  mutate(
    type = str_squish(tolower(type)),
    is_select = str_detect(type, "^select_(one|multiple)\\s+"),
    list_name = ifelse(is_select, str_replace(type, "^select_(one|multiple)\\s+(.+)$", "\\2"), NA_character_)
  ) |>
  filter(is_select, !is.na(list_name)) |>
  mutate(list_name = str_squish(tolower(list_name))) |>
  distinct(variable = name, list_name)



# ---------- RÉORDONNAGE BINAIRE (impose 0/1) ----------
# Définis ici toutes les listes binaires à forcer (dans l’ordre souhaité)
binary_orders <- tibble::tribble(
  ~list_name, ~labels_in_order,
  "sexe",     c("Féminin","Masculin"),  # => Féminin -> 0 ; Masculin -> 1
  "ouinon",   c("Non","Oui")            # => Non -> 0 ; Oui -> 1
)

apply_binary_order <- function(choices, binary_orders) {
  out <- choices |> mutate(list_name = str_squish(tolower(list_name)))
  for (i in seq_len(nrow(binary_orders))) {
    ln   <- binary_orders$list_name[i]
    labs <- binary_orders$labels_in_order[[i]]
    out <- out |>
      mutate(order_key = ifelse(list_name == ln, match(label, labs), NA_real_)) |>
      arrange(list_name, order_key, name) |>
      select(-order_key)
  }
  out
}




choices_reord <- apply_binary_order(choices, binary_orders) |>
  transmute(list_name = str_squish(tolower(list_name)),
            code  = as.character(name),
            label = as.character(label)) |>
  distinct()

# ---------- DICTIONNAIRE DES VALEURS ----------
dict_vals_base <- survey_lists |>
  left_join(choices_reord, by = "list_name") |>
  distinct(variable, list_name, code, label) |>
  group_by(variable) |>
  mutate(binaire = as.integer(n_distinct(code) == 2)) |>
  ungroup()

# 4) Mapper les noms courts du dict vers les noms COMPLETS de SMIT
#    - base name = ce qui vient après le dernier '/'
df_names        <- names(SMIT)
df_base_names   <- sub(".*/", "", df_names)                   # "Epidemio/sexe" -> "sexe"
map_base_to_col <- tibble::tibble(base = df_base_names, col = df_names) |> distinct()

dict_vals <- dict_vals_base |>
  mutate(base = sub(".*/", "", variable)) |>
  left_join(map_base_to_col, by = c("base" = "base")) |>
  mutate(variable = coalesce(col, variable)) |>
  select(variable, list_name, code, label, binaire) |>
  distinct()


##########fonction
# besoin: install.packages("stringi") si pas déjà


encode_from_dict <- function(df, dict,
                             suffix_factor = "_f",
                             suffix_num    = "_num",
                             binary_convention = c("first_is_0","explicit"),
                             warn_unseen = TRUE,
                             return_unseen = FALSE) {
  binary_convention <- match.arg(binary_convention)
  vars <- unique(dict$variable)
  unseen_list <- list()
  
  normalize <- function(z) stri_trans_general(tolower(trimws(as.character(z))), "Latin-ASCII")
  
  for (v in vars) {
    if (!v %in% names(df)) next
    
    sub    <- dict[dict$variable == v, , drop = FALSE]
    codes  <- as.character(sub$code)
    labels <- as.character(sub$label)
    
    x_chr <- as.character(df[[v]])
    
    # --- Harmonisation: si la valeur ressemble à un label, on la remplace par le code correspondant
    lab2code <- setNames(codes, normalize(labels))   # "oui" -> "2", "non" -> "1", etc.
    idx_lab  <- normalize(x_chr) %in% names(lab2code)
    x_chr[idx_lab] <- lab2code[ normalize(x_chr[idx_lab]) ]
    
    # --- Alerte sur codes restants non vus
    if (warn_unseen) {
      unseen <- sort(setdiff(unique(na.omit(x_chr)), codes))
      if (length(unseen)) {
        message(sprintf("[encode_from_dict] Variable '%s': %d code(s) non mappé(s): %s",
                        v, length(unseen), paste(unseen, collapse = ", ")))
        unseen_list[[v]] <- unseen
      }
    }
    
    # --- FACTOR lisible (toujours)
    f <- factor(x_chr, levels = codes, labels = labels, ordered = FALSE)
    df[[paste0(v, suffix_factor)]] <- f
    
    # --- NUM 0/1 uniquement si binaire (2 modalités dans le dict)
    if (isTRUE(any(sub$binaire == 1)) && length(unique(codes)) == 2) {
      if (binary_convention == "first_is_0") {
        num <- ifelse(x_chr == codes[2], 1L,
                      ifelse(x_chr == codes[1], 0L, NA_integer_))
      } else {
        num <- NA_integer_
      }
      df[[paste0(v, suffix_num)]] <- num
    }
  }
  
  if (return_unseen) list(df = df, unseen = unseen_list) else df
}



#---------------NEW SMIT----
#####################

res  <- encode_from_dict(
  df = SMIT,
  dict = dict_vals,
  suffix_factor = "_f",
  suffix_num    = "_num",
  binary_convention = "first_is_0",  # 1re modalité -> 0 ; 2e -> 1
  warn_unseen = TRUE,
  return_unseen = TRUE
)

SMIT  <- res$df
unseen_codes <- res$unseen   # liste des codes présents dans SMIT mais absents du dictionnaire

# Contrôles rapides
str(SMIT[, c("Epidemio/sexe", "Epidemio/sexe_f", "Epidemio/sexe_num")])
table(SMIT$`Epidemio/sexe_f`,  useNA = "ifany")   # "Féminin", "Masculin"
table(SMIT$`Epidemio/sexe_num`, useNA = "ifany")  # 0/1/NA

names(SMIT)















#########################################
# Labels de variables (attributs) si tu veux
apply_var_labels <- function(df, dict_vars) {
  if (!requireNamespace("labelled", quietly = TRUE)) return(df)
  for (i in seq_len(nrow(dict_vars))) {
    v <- dict_vars$variable[i]
    if (v %in% names(df)) {
      labelled::var_label(df[[v]]) <- dict_vars$var_label[i]
    }
    # Si tu veux aussi copier le label vers *_f :
    vf <- paste0(v, "_f")
    if (vf %in% names(df)) {
      labelled::var_label(df[[vf]]) <- dict_vars$var_label[i]
    }
  }
  df
}
#-##############################################-
#SMITold: chargement et préparation ----


#selection sous base patient ages >= 65

SMITold <- SMIT |> filter(`Epidemio/age` >= 65)
##############nettoyage prealable SMITold

# 1) Identifiant unique & doublons : ok
SMITold |> count(`Epidemio/numq`) |> filter(n > 1) # si >0, investiguer/choisir règle

# 2) format date et heure et duree de sejour (DDS)


SMITold <- SMITold |>
  dplyr::mutate(
    dt_adm   = suppressWarnings(ymd(`Hospi/date_h`)),
    hr_adm   = suppressWarnings(hm(`Hospi/heure_h`)),
    dt_sort  = suppressWarnings(ymd(`Evolu/date_sortie`)),
    dt_deces = suppressWarnings(ymd(`Evolu/date_deces`)),
    # Priorité à la date de sortie pour LOS, sinon décès, sinon NA
    LOS = case_when(
      !is.na(dt_sort)  & !is.na(dt_adm) ~ as.numeric(as_date(dt_sort)  - as_date(dt_adm)),
      is.na(dt_sort)   & !is.na(dt_deces) & !is.na(dt_adm) ~ as.numeric(as_date(dt_deces) - as_date(dt_adm)),
      TRUE ~ NA_real_
    ))

# LOS négatives : doivent être 0 ou positives
SMITold |> dplyr::filter(!is.na(LOS) & LOS < 0) |> nrow()


#Indicateur décès , creation de la variable

# Harmonise la casse/espaces dans l’étiquette
normalize <- function(z) stringi::stri_trans_general(tolower(trimws(as.character(z))), "Latin-ASCII")

SMITold <- SMITold |>
  mutate(
    evol_norm = normalize(`Evolu/evolution_f`),
    deces = case_when(
      evol_norm %in% c("deces","décès") ~ 1L,
      evol_norm %in% c("guerison","guérison","decharge","transfert","evasion") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Petits contrôles non bloquants (messages uniquement)
neg_los_n <- SMITold %>% filter(!is.na(LOS) & LOS < 0) %>% nrow()
if (neg_los_n > 0) {
  message(sprintf("[LOS] %d enregistrement(s) avec LOS négative. Vérifie les dates adm/sortie/décès.", neg_los_n))
}

if (all(is.na(SMITold$LOS))) {
  message("[LOS] Toutes les LOS sont NA : vérifie les colonnes de dates/heure et leurs formats.")
}
# Aperçu rapide de ce qu'on a construit
SMITold %>%
  select(`Epidemio/numq`, dt_adm, dt_sort, dt_deces, LOS, deces) %>%
  head(10)

#Profil des valeurs manquantes

missing_rate <- sapply(SMITold, function(x) mean(is.na(x))) |> sort(decreasing = TRUE)
head(missing_rate, 20)


###########################################################
#decoupage en sous bases
#################################################
# ------------------- EPIDEMIO-----

EPIDEMIO <- SMITold |>
  select(starts_with("Epidemio"))
  
dim(EPIDEMIO) #157-17

names(EPIDEMIO)


# A) Vérifier que toutes les colonnes essentielles sont présentes
required <- c(
  "Epidemio/numq",
  "Epidemio/age",
  "Epidemio/sexe_f", "Epidemio/sexe_num",
  "Epidemio/sitmat_f",
  "Epidemio/regmat_f",
  "Epidemio/nivinst_f"
)
setdiff(required, names(EPIDEMIO))  # doit afficher character(0) si tout est là

# B) Contrôle des types pour 3 variables clés
str(EPIDEMIO[c("Epidemio/age","Epidemio/sexe_f","Epidemio/sexe_num")])

# C) Coup d’œil sur la distribution de l’âge et du sexe
summary(EPIDEMIO$`Epidemio/age`)
table(EPIDEMIO$`Epidemio/sexe_f`, useNA = "ifany")

look_for(EPIDEMIO)


#Fixation reference et ordre 
?fct_na_if

EPIDEMIO <- EPIDEMIO %>%
  mutate(
    # (1) Recode "Inconnu" -> NA (par variable)
    `Epidemio/sitmat_f`    = fct_recode(`Epidemio/sitmat_f`,    NULL = "Inconnu"),
    `Epidemio/regmat_f`    = fct_recode(`Epidemio/regmat_f`,    NULL = "Inconnu"),
    `Epidemio/nivinst_f`   = fct_recode(`Epidemio/nivinst_f`,   NULL = "inconnu"),
    `Epidemio/profession_f`= fct_recode(`Epidemio/profession_f`,NULL = "Inconnu"),
    
    # 2.2 — Fixer références / ordres (utile pour tableaux et régressions)
    `Epidemio/sexe_f`    = fct_relevel(`Epidemio/sexe_f`, "Féminin"),
    `Epidemio/sitmat_f`  = fct_relevel(`Epidemio/sitmat_f`, "Célibataire"),
    `Epidemio/regmat_f`  = fct_relevel(`Epidemio/regmat_f`, "Monogame"),
    `Epidemio/nivinst_f` = fct_relevel(`Epidemio/nivinst_f`,
                                       c("Non instruit","Primaire","Secondaire","Universitaire"))
  )





#correction fautes 


EPIDEMIO <- EPIDEMIO %>%
  mutate(
    # Profession
    `Epidemio/profession_f` = fct_recode(`Epidemio/profession_f`,
                                         "Salarié"   = "Salarie",
                                         "Chômeur"   = "Chomeiur",
                                         "Commerçant"= "Commercant",
                                         "Ouvrier"   = "Ouvier",
                                         "Étudiant"  = "Etudiant",
                                         # "Retraité" déjà correct ?
                                         # "Autres"   déjà correct ?
                                         NULL        = "Inconnu"   # au cas où il resterait, sinon ignoré
    ),
    # Origine géographique
    `Epidemio/originegeo_f` = fct_recode(`Epidemio/originegeo_f`,
                                         "Région" = "Region"
    ),
    # (Optionnel) SitMat / RegMat / NivInst si tu veux forcer une orthographe
    `Epidemio/sitmat_f`  = fct_recode(`Epidemio/sitmat_f`,
                                      "Marié(e)" = "Marié"    # si tu préfères l’affichage Marié(e)
    )
  )



#verification 

# Vérifier qu'il n'y a plus "Inconnu" comme modalité
lapply(EPIDEMIO[c("Epidemio/sitmat_f","Epidemio/regmat_f",
                  "Epidemio/nivinst_f","Epidemio/profession_f")], levels)

# Compter les NA apparus après recodage
sapply(EPIDEMIO[c("Epidemio/sitmat_f","Epidemio/regmat_f",
                  "Epidemio/nivinst_f","Epidemio/profession_f")],
       function(f) sum(is.na(f)))





###ANALYSE DESCRIPTIVE EPIDEMIO ----



# 3.1 — Tableau principal (âge + variables catégorielles), NAs exclus du dénominateur
tab_epi_main <- EPIDEMIO %>%
  select(
    `Epidemio/age`,
    `Epidemio/sexe_f`,
    `Epidemio/sitmat_f`,
    `Epidemio/regmat_f`,
    `Epidemio/nivinst_f`,
    `Epidemio/profession_f`,
    `Epidemio/originegeo_f`
  ) %>%
  tbl_summary(
      # médiane [Q1, Q3] pour les continues ; n (%) pour les catégorielles
      statistic = list(
      all_continuous()  ~ "Moy.sd : {mean} ({sd}) | Med.IQR : {median}[{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
         ),
      missing = "no",          # << exclut les NA du dénominateur
      digits = all_continuous() ~ 1,

    label = list(
    `Epidemio/age`           ~ "Âge (ans)",
    `Epidemio/sexe_f`        ~ "Sexe",
    `Epidemio/sitmat_f`      ~ "Situation matrimoniale",
    `Epidemio/regmat_f`      ~ "Régime matrimonial",
    `Epidemio/nivinst_f`     ~ "Niveau d'instruction",
    `Epidemio/profession_f`  ~ "Profession",
    `Epidemio/originegeo_f`  ~ "Origine géographique"
  )
 ) %>%
  modify_header(label ~ "**Caractéristiques socio-démographiques (≥65 ans)**")|>
  add_n()|> 
  bold_labels()

# 3.2 — Tableau conditionnel : régime matrimonial *parmi les marié(e)s*
n_maries <- sum(EPIDEMIO$`Epidemio/sitmat_f` == "Marié(e)", na.rm = TRUE)

tab_regmat <- EPIDEMIO %>%
  filter(`Epidemio/sitmat_f` == "Marié(e)") %>%
  select(`Epidemio/regmat_f`) %>%
  tbl_summary(
    statistic = ~ "{n} ({p}%)",
    missing = "no"           # << exclut les NA (s'il y en a chez les mariés)
  ) %>%
  modify_header(label ~ glue::glue("**Régime matrimonial** (parmi Marié·e·s ; n = {n_maries})"))

# 3.3 — Afficher les deux tableaux (séparés)
tab_epi_main
tab_regmat



#AJOUT DES LABELS DE VARIABLE




var_label(EPIDEMIO$`Epidemio/age`)           <- "Âge (ans)"
var_label(EPIDEMIO$`Epidemio/sexe_f`)        <- "Sexe"
var_label(EPIDEMIO$`Epidemio/sitmat_f`)      <- "Situation matrimoniale"
var_label(EPIDEMIO$`Epidemio/regmat_f`)      <- "Régime matrimonial"
var_label(EPIDEMIO$`Epidemio/nivinst_f`)     <- "Niveau d'instruction"
var_label(EPIDEMIO$`Epidemio/profession_f`)  <- "Profession"
var_label(EPIDEMIO$`Epidemio/originegeo_f`)  <- "Origine géographique"



#DESCRIPTION AVEC LABEL 
# Puis un tbl_summary sans préciser 'label =' (ils seront lus depuis var_label)
EPIDEMIO %>%
  select(`Epidemio/age`, `Epidemio/sexe_f`, `Epidemio/sitmat_f`, `Epidemio/regmat_f`,
         `Epidemio/nivinst_f`, `Epidemio/profession_f`, `Epidemio/originegeo_f`) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  )










###-######################################
#-----------------HOSPI----

HOSPI <- SMITold %>%
  select(
    `Epidemio/numq`,
    starts_with("Hospi"),
    LOS, deces
  )


look_for(HOSPI)

##----------------------------------------------------Recodage et nettoyage----

# 1) Corriger orthographe/accents et enlever "Inconnu" (=> NA)
HOSPI <- HOSPI %>%
  mutate(
    # provenance (semble déjà OK: domicile/mutation/transfert)
    `Hospi/provenance_f` = fct_relevel(`Hospi/provenance_f`,
                                       "domicile","mutation","transfert"),
    
    # structure d'origine : corriger accents + enlever "Inconnu"
    `Hospi/struc_origine_f` = fct_recode(`Hospi/struc_origine_f`,
                                         "Hôpital"          = "Hopital",
                                         "Centre de Santé"  = "Centre de Sante",
                                         "Privé"            = "Prive",
                                         NULL               = "Inconnu"   # -> drop la modalité Inconnu => NA
    ),
    
    # site : corriger la faute "SALLLE" -> "Salle"
    `Hospi/site_f` = fct_recode(`Hospi/site_f`,
                                "Salle" = "SALLLE"
    ) %>% fct_relevel("Salle","USI")
  )

# 2) Poser des labels PERSISTANTS (utiles pour tous les tableaux)
var_label(HOSPI$`Hospi/date_h`)           <- "Date d'admission"
var_label(HOSPI$`Hospi/heure_h`)          <- "Heure d'admission"
var_label(HOSPI$`Hospi/provenance_f`)     <- "Provenance"
var_label(HOSPI$`Hospi/reference_f`)      <- "Admission référée"
var_label(HOSPI$`Hospi/reference_num`)    <- "Admission référée (0/1)"
var_label(HOSPI$`Hospi/struc_origine_f`)  <- "Structure d'origine"
var_label(HOSPI$`Hospi/site_f`)           <- "Site d'hospitalisation"
var_label(HOSPI$`Hospi/site_num`)         <- "Site d'hospitalisation (0/1)"
var_label(HOSPI$LOS)                      <- "Durée d'hospitalisation (jours)"
var_label(HOSPI$deces)                    <- "Décès (0/1)"

# 3) Petits checks (non destructifs)
lapply(HOSPI[c("Hospi/provenance_f","Hospi/struc_origine_f","Hospi/site_f")], levels)
sapply(HOSPI[c("Hospi/provenance_f","Hospi/struc_origine_f","Hospi/site_f")],
       function(f) mean(is.na(f)))

##----------------------Description HOSPI----


# S'assurer d'un ordre logique pour 'Admission référée' (Non d'abord, puis Oui)
#HOSPI <- HOSPI %>%
  #mutate(`Hospi/reference_f` = fct_relevel(`Hospi/reference_f`, "Non", "Oui"))

# 1) Tableau principal (Provenance, Admission référée, Site, LOS)
tab_hospi_main <- HOSPI %>%
  select(`Hospi/provenance_f`,
         `Hospi/reference_f`,
         `Hospi/struc_origine_f`,
         `Hospi/site_f`,
         LOS) %>%
  tbl_summary(
    statistic = list(
      LOS ~ "Moy.sd : {mean} ({sd}) | Med.IQR : {median}[{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"  # NAs exclus des dénominateurs
  ) %>%
  modify_header(label ~ "**Admission (≥65 ans)**")|>
  add_n()|> 
  bold_labels()

# 2) Tableau conditionnel : Structure d'origine parmi les référés = Oui
n_ref <- sum(HOSPI$`Hospi/reference_f` == "Oui", na.rm = TRUE)

tab_struc_ref <- HOSPI %>%
  filter(`Hospi/reference_f` == "Oui") %>%
  select(`Hospi/struc_origine_f`) %>%
  tbl_summary(
    statistic = ~ "{n} ({p}%)",
    missing = "no"  # exclut les NA structurels
  ) %>%
  modify_header(label ~ glue::glue("**Structure d'origine** (parmi référés ; n = {n_ref})"))|>
  add_n()|> 
  bold_labels()

# Afficher
tab_hospi_main
tab_struc_ref




##-#################
#------------------ATCDS----

ATCD <- SMITold %>%
  select(
    `Epidemio/numq`,
    starts_with("ATCDS")
  )

look_for(ATCD)

#ATCD <- ATCD %>%
  #mutate(
    #autre_tare_flag = if_else(!is.na(`ATCDS/autre_tare`) & `ATCDS/autre_tare` != "", 0L, 1L)
  #)

ATCD <- ATCD %>%
  mutate(
    ATCDS_autre_tare_clean = na_if(trimws(`ATCDS/autre_tare`), ""),   # "" -> NA
    autre_tare_flag = if_else(!is.na(ATCDS_autre_tare_clean), 1L, 0L)
  )

var_label(ATCD$autre_tare_flag) <- "Autre tare (mentionnée oui/non)"


##labellisation----

var_label(ATCD$`ATCDS/Tare_f`)                <- "Comorbidité (tare) présente"
var_label(ATCD$`ATCDS/Diabete_f`)             <- "Diabète"
var_label(ATCD$`ATCDS/HTA_f`)                 <- "Hypertension artérielle"
var_label(ATCD$`ATCDS/MRC_f`)                 <- "Maladie rénale chronique"
var_label(ATCD$`ATCDS/HIV_f`)                 <- "Infection VIH"
var_label(ATCD$`ATCDS/TB_f`)                  <- "Tuberculose"
var_label(ATCD$`ATCDS/BMR_f`)                 <- "Bactérie multirésistante (BMR)"
var_label(ATCD$`ATCDS/Cardio-vasculaire_f`)   <- "Antécédent cardio-vasculaire"
var_label(ATCD$`ATCDS/Hospi_ant_f`)           <- "Hospitalisation antérieure"


var_label(ATCD$autre_tare_flag) <- "Autre tare (mentionnée oui/non)"


##--------------------------Tableau descriptif HOSPI----

tab_atcd <- ATCD %>%
  select(`ATCDS/Diabete_f`, `ATCDS/HTA_f`, `ATCDS/MRC_f`,
         `ATCDS/HIV_f`,autre_tare_flag, `ATCDS/TB_f`, `ATCDS/BMR_f`,
         `ATCDS/Cardio-vasculaire_f`, `ATCDS/Hospi_ant_f`) %>%
  tbl_summary(
    statistic = ~ "{n} ({p}%)",
    missing = "no"   # exclut les NA du dénominateur
  ) %>%
  modify_header(label ~ "**Antécédents médicaux (≥65 ans)**")|>
  add_n()|> 
  bold_labels()

tab_atcd


##Autre tableau ----



atcd_vars <- c("ATCDS/Diabete_f", "ATCDS/HTA_f", "ATCDS/MRC_f",
               "ATCDS/HIV_f", "ATCDS/TB_f", "ATCDS/BMR_f",
               "ATCDS/Cardio-vasculaire_f", "ATCDS/Hospi_ant_f")

tab_atcd_oui <- ATCD %>%
  summarise(across(all_of(atcd_vars), ~ mean(. == "Oui", na.rm = TRUE))) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "Antécédent",
                      values_to = "Proportion_oui") %>%
  mutate(Proportion_oui = round(100 * Proportion_oui, 1))

tab_atcd_oui




##-###################
#------------------------CLINIQUE----
CLINIQUE <- SMITold %>%
  select(
    `Epidemio/numq`,
    starts_with("Clinique"))

look_for(CLINIQUE)

##labellisation des variables----



# Variables quantitatives
var_label(CLINIQUE$`Clinique/temp`)    <- "Température (°C)"
var_label(CLINIQUE$`Clinique/pouls`)   <- "Pouls (bpm)"
var_label(CLINIQUE$`Clinique/PAS`)     <- "Pression artérielle systolique (mmHg)"
var_label(CLINIQUE$`Clinique/PAD`)     <- "Pression artérielle diastolique (mmHg)"
var_label(CLINIQUE$`Clinique/glycemie`)<- "Glycémie (g/L)"
var_label(CLINIQUE$`Clinique/glasgow`) <- "Score de Glasgow"

# Signes cliniques principaux (Oui/Non)
var_label(CLINIQUE$`Clinique/fievre_f`)         <- "Fièvre"
var_label(CLINIQUE$`Clinique/deshydratation_f`)<- "Déshydratation"
var_label(CLINIQUE$`Clinique/AEG_f`)            <- "Altération de l'état général"
var_label(CLINIQUE$`Clinique/CCV_f`)            <- "Collapsus cardio-vasculaire"
var_label(CLINIQUE$`Clinique/choc_f`)           <- "Choc"
var_label(CLINIQUE$`Clinique/respi_f`)          <- "Syndrome respiratoire"
var_label(CLINIQUE$`Clinique/toux_f`)           <- "Toux"
var_label(CLINIQUE$`Clinique/dyspnee_f`)        <- "Dyspnée"
var_label(CLINIQUE$`Clinique/dleur_thor_f`)     <- "Douleur thoracique"
var_label(CLINIQUE$`Clinique/Hemoptysie_f`)     <- "Hémoptysie"
var_label(CLINIQUE$`Clinique/digestif_f`)       <- "Syndrome digestif"
var_label(CLINIQUE$`Clinique/dleur_abdo_f`)     <- "Douleur abdominale"
var_label(CLINIQUE$`Clinique/vomissement_f`)    <- "Vomissements"
var_label(CLINIQUE$`Clinique/diarrhee_f`)       <- "Diarrhée"
var_label(CLINIQUE$`Clinique/arret_matiere_gaz_f`) <- "Arrêt matière/gaz"
var_label(CLINIQUE$`Clinique/neuro_f`)          <- "Syndrome neurologique"
var_label(CLINIQUE$`Clinique/Convulsion_f`)     <- "Convulsions"
var_label(CLINIQUE$`Clinique/NC_f`)             <- "Névrite crânienne"
var_label(CLINIQUE$`Clinique/sd_encephalitique_f`)<- "Syndrome encéphalitique"
var_label(CLINIQUE$`Clinique/sd_meninge_f`)     <- "Syndrome méningé"
var_label(CLINIQUE$`Clinique/sd_focal_f`)       <- "Syndrome focal"
var_label(CLINIQUE$`Clinique/sd_peripherique_f`)<- "Syndrome périphérique"
var_label(CLINIQUE$`Clinique/splgg_f`)          <- "Splénomégalie"
var_label(CLINIQUE$`Clinique/SPM_f`)            <- "Syndrome polyadénopathique (SPM)"
var_label(CLINIQUE$`Clinique/ADP_f`)            <- "Adénopathies"
var_label(CLINIQUE$`Clinique/cutane_f`)         <- "Atteinte cutanée"
var_label(CLINIQUE$`Clinique/urinaire_f`)       <- "Syndrome urinaire"
var_label(CLINIQUE$`Clinique/osteo_f`)          <- "Syndrome ostéo-articulaire"


# Vérif rapide : combien de "Oui" au global
table(CLINIQUE$`Clinique/respi_f`, useNA = "ifany")
table(CLINIQUE$`Clinique/digestif_f`, useNA = "ifany")
table(CLINIQUE$`Clinique/neuro_f`, useNA = "ifany")
table(CLINIQUE$`Clinique/splgg_f`, useNA = "ifany")
table(CLINIQUE$`Clinique/cutane_f`, useNA = "ifany")
table(CLINIQUE$`Clinique/urinaire_f`, useNA = "ifany")
table(CLINIQUE$`Clinique/osteo_f`, useNA = "ifany")

##tableau descriptif global CLINIQUE----



tab_clinique <- CLINIQUE %>%
  select(
    # Variables quantitatives
    `Clinique/temp`, `Clinique/pouls`, `Clinique/PAS`, `Clinique/PAD`,
    `Clinique/glycemie`, `Clinique/glasgow`,
    
    # Signes généraux
    `Clinique/fievre_f`, `Clinique/deshydratation_f`, `Clinique/AEG_f`,
    
    # Syndromes cardio-vasculaires
    `Clinique/CCV_f`, `Clinique/choc_f`,
    
    # Syndrome respiratoire global + détails
    `Clinique/respi_f`, `Clinique/toux_f`, `Clinique/dyspnee_f`,
    `Clinique/dleur_thor_f`, `Clinique/Hemoptysie_f`,
    
    # Syndrome digestif global + détails
    `Clinique/digestif_f`, `Clinique/dleur_abdo_f`, `Clinique/vomissement_f`,
    `Clinique/diarrhee_f`, `Clinique/arret_matiere_gaz_f`,
    
    # Syndrome neurologique global + détails
    `Clinique/neuro_f`, `Clinique/Convulsion_f`, `Clinique/NC_f`,
    `Clinique/sd_encephalitique_f`, `Clinique/sd_meninge_f`,
    `Clinique/sd_focal_f`, `Clinique/sd_peripherique_f`,
    
    # Syndromes spléniques/lymphatiques
    `Clinique/splgg_f`, `Clinique/SPM_f`, `Clinique/ADP_f`,
    
    # Syndromes cutané, urinaire, ostéo
    `Clinique/cutane_f`, `Clinique/urinaire_f`, `Clinique/osteo_f`
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "Moy.sd : {mean} ({sd}) | Med.IQR : {median}[{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",  # exclut les NA structurels du dénominateur
    digits = all_continuous() ~ 1
  ) %>%
  modify_header(label ~ "**Signes cliniques (≥65 ans)**")|>
  add_n()|> 
  bold_labels()

tab_clinique


look_for(CLINIQUE, "glasg")

summary(CLINIQUE$`Clinique/glasgow`)

class(CLINIQUE$`Clinique/glasgow`)

look_for(CLINIQUE$`Clinique/glasgow`)

####### BLOC struCturé -----


## -------- Sélections par section --------
vars_qt   <- c("Clinique/temp","Clinique/pouls","Clinique/PAS","Clinique/PAD",
               "Clinique/glycemie")

vars_gen  <- c("Clinique/fievre_f","Clinique/deshydratation_f","Clinique/AEG_f","Clinique/glasgow")

vars_cardi<- c("Clinique/CCV_f","Clinique/choc_f")

vars_respi<- c("Clinique/respi_f","Clinique/toux_f","Clinique/dyspnee_f",
               "Clinique/dleur_thor_f","Clinique/Hemoptysie_f")

vars_diges<- c("Clinique/digestif_f","Clinique/dleur_abdo_f","Clinique/vomissement_f",
               "Clinique/diarrhee_f","Clinique/arret_matiere_gaz_f")

vars_neuro<- c("Clinique/neuro_f","Clinique/Convulsion_f","Clinique/NC_f",
               "Clinique/sd_encephalitique_f","Clinique/sd_meninge_f",
               "Clinique/sd_focal_f","Clinique/sd_peripherique_f")

vars_splgg<- c("Clinique/splgg_f","Clinique/SPM_f","Clinique/ADP_f")

vars_other<- c("Clinique/cutane_f","Clinique/urinaire_f","Clinique/osteo_f")

## -------- Un tbl_summary par section --------
tt_qt <- CLINIQUE %>%
  select(all_of(vars_qt)) %>%
  tbl_summary(
    statistic = all_continuous() ~ "Moy.sd : {mean} ({sd}) | Med.IQR : {median}[{p25}, {p75}]",
    missing   = "no",
    digits    = all_continuous() ~ 1
  )|>  add_n()

tt_gen <- CLINIQUE %>%
  select(all_of(vars_gen)) %>%
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)", missing = "no")|>  add_n()

tt_cardi <- CLINIQUE %>%
  select(all_of(vars_cardi)) %>%
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)", missing = "no")|>  add_n()

tt_respi <- CLINIQUE %>%
  select(all_of(vars_respi)) %>%
  tbl_summary(
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  )|>  add_n()

tt_diges <- CLINIQUE %>% 
  select(all_of(vars_diges)) %>%
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)", missing = "no")|>  add_n()

tt_neuro <- CLINIQUE %>%
  select(all_of(vars_neuro)) %>%
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)", missing = "no")|>  add_n()

tt_splgg <- CLINIQUE %>%
  select(all_of(vars_splgg)) %>%
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)", missing = "no")|>  add_n()

tt_other <- CLINIQUE %>%
  select(all_of(vars_other)) %>%
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)", missing = "no")|>  add_n()

## -------- Empiler avec des entêtes de section --------
tab_clinique_struct <- tbl_stack(
  tbls = list(tt_qt, tt_gen, tt_cardi, tt_respi, tt_diges, tt_neuro, tt_splgg, tt_other),
  group_header = c(
    "Paramètres vitaux",
    "Signes généraux",
    "Cardio-vasculaire",
    "Respiratoire",
    "Digestif",
    "Neurologique",
    "Spléno-ganglionnairee",
    "Cutané / Urinaire / Ostéo-articulaire"
  )
) %>%
  modify_header(label ~ "**Signes cliniques (≥65 ans)**")

  

  
  # -> convertir en gt et styler les "row group headers" en gras
tab_clinique_struct_gt <- as_gt(tab_clinique_struct) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

tab_clinique_struct_gt


##------------BLOC 2 revisité----

# Fonction auxiliaire : top 5 modalités non vides/non-NA
top5 <- function(df, var){
  df %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    count(.data[[var]], sort = TRUE) %>%
    mutate(Prop = round(100 * n / sum(n), 1)) %>%  # ajoute %
    rename(Modalité = 1, Effectif = n) %>%
    head(5)
}

# Appliquer sur chaque champ texte
list(
  cutane        = top5(CLINIQUE, "Clinique/typ_cutane"),
  urinaire      = top5(CLINIQUE, "Clinique/typ_urinaire"),
  osteo         = top5(CLINIQUE, "Clinique/typ_osteo"),
  autre_respi   = top5(CLINIQUE, "Clinique/autre_respi"),
  autre_digestif= top5(CLINIQUE, "Clinique/autre_digestif"),
  autre_neuro   = top5(CLINIQUE, "Clinique/autre_neuro"),
  autre_splgg   = top5(CLINIQUE, "Clinique/autre_splgg")
)



##-------BLOC 3 autre ----


# 1) Fonction top5 + ajout du nom de champ
top5 <- function(df, var, champ_label){
  df %>%
    filter(!is.na(.data[[var]]), str_trim(.data[[var]]) != "") %>%   # enlève NA & vides
    count(Modalité = .data[[var]], sort = TRUE) %>%
    mutate(
      Prop = round(100 * n / sum(n), 1),
      Champ = champ_label
    ) %>%
    transmute(
      Champ,
      Modalité,
      Effectif = n,
      `%` = Prop
    ) %>%
    slice_head(n = 5)
}

# 2) Champs texte à résumer (nom de colonne -> étiquette lisible)
champs_txt <- c(
  "Clinique/typ_cutane"     = "Cutané (types)",
  "Clinique/typ_urinaire"   = "Urinaire (types)",
  "Clinique/typ_osteo"      = "Ostéo-articulaire (types)",
  "Clinique/autre_respi"    = "Respiratoire (autres précisions)",
  "Clinique/autre_digestif" = "Digestif (autres précisions)",
  "Clinique/autre_neuro"    = "Neurologique (autres précisions)",
  "Clinique/autre_splgg"    = "Spléno/lymphatique (autres précisions)"
)

# 3) Construire le tableau combiné
tab_txt_clinique <- map2_dfr(
  .x = names(champs_txt),
  .y = unname(champs_txt),
  ~ top5(CLINIQUE, .x, .y)
) %>%
  arrange(Champ, desc(Effectif))

# 4) Afficher
tab_txt_clinique

tab_txt_clinique %>%
  gt() %>%
  tab_header(title = "Résumé qualitatif des champs texte (Top 5 par champ)") 



#######-----------BLOC 2 PRO ----


### ---------- Utilitaires ----------

# Facultatif : normaliser les textes pour réduire les doublons (décommente si besoin)
normalize_text <- function(x) {
  x %>%
    stringr::str_trim() %>%                 # retire espaces
    {ifelse(. == "", NA_character_, .)} %>%
    # stringr::str_to_lower() %>%
    # stringr::str_replace_all("[ ]+", " ") %>%
    identity()
}

# Fabrique un tableau gt "pro" top K pour un champ texte, avec condition (facultative)
topk_gt <- function(df, value_col, condition_flag = NULL, k = 5,
                    title = "Top modalités", subtitle = NULL) {
  # Appliquer condition (ex: clin/cutane_f == "Oui")
  d <- df
  if (!is.null(condition_flag)) {
    d <- d %>% filter(.data[[condition_flag]] == "Oui")
  }
  # Garder non manquants / non vides
  d <- d %>%
    mutate(val = normalize_text(.data[[value_col]])) %>%
    filter(!is.na(val))
  
  denom <- nrow(d) # dénominateur: non manquants après condition
  out <- d %>%
    count(Modalité = val, sort = TRUE) %>%
    mutate(`%` = round(100 * n / sum(n), 1)) %>%
    rename(Effectif = n) %>%
    slice_head(n = k)
  
  # Construire titre/subtitre informatifs
  title_txt <- title
  if (!is.null(condition_flag)) {
    subtitle_txt <- subtitle %||% glue("Calculé parmi les patients avec {condition_flag} = Oui. Dénominateur (non manquants) = {denom}.")
  } else {
    subtitle_txt <- subtitle %||% glue("Dénominateur (non manquants) = {denom}.")
  }
  
  # Rendu gt "pro"
  gt_tbl <- out %>%
    gt() %>%
    tab_header(
      title = md(glue("**{title_txt}**")),
      subtitle = subtitle_txt
    ) %>%
    cols_align(align = "left", columns = "Modalité") %>%
    cols_align(align = "right", columns = c("Effectif", "%")) %>%
    fmt_number(columns = "Effectif", decimals = 0, use_seps = TRUE) %>%
    fmt_number(columns = "%", decimals = 1) %>%
    tab_options(
      table.font.size = px(14),
      data_row.padding = px(6)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    opt_row_striping()
  
  gt_tbl
}

#### ---------- RECOd VAR ----------









#### ---------- Tables séparées (un par champ) ----------

tab_typ_cutane <- topk_gt(
  CLINIQUE,
  value_col = "Clinique/typ_cutane",
  condition_flag = "Clinique/cutane_f",
  k = 5,
  title = "Atteintes cutanées — Top 5",
  subtitle = NULL # auto: précisera le dénominateur
)

tab_typ_urinaire <- topk_gt(
  CLINIQUE,
  value_col = "Clinique/typ_urinaire",
  condition_flag = "Clinique/urinaire_f",
  k = 5,
  title = "Signes urinaire — Top 5"
)

tab_typ_osteo <- topk_gt(
  CLINIQUE,
  value_col = "Clinique/typ_osteo",
  condition_flag = "Clinique/osteo_f",
  k = 5,
  title = "Signes ostéo-articulaire — Top 5"
)

tab_autre_respi <- topk_gt(
  CLINIQUE,
  value_col = "Clinique/autre_respi",
  condition_flag = "Clinique/respi_f",
  k = 5,
  title = "Respiratoire — autres précisions (Top 5)"
)

tab_autre_digestif <- topk_gt(
  CLINIQUE,
  value_col = "Clinique/autre_digestif",
  condition_flag = "Clinique/digestif_f",
  k = 5,
  title = "Digestif — autres précisions (Top 5)"
)

tab_autre_neuro <- topk_gt(
  CLINIQUE,
  value_col = "Clinique/autre_neuro",
  condition_flag = "Clinique/neuro_f",
  k = 5,
  title = "Neurologique — autres précisions (Top 5)"
)

tab_autre_splgg <- topk_gt(
  CLINIQUE,
  value_col = "Clinique/autre_splgg",
  condition_flag = "Clinique/splgg_f",
  k = 5,
  title = "Spléno-ganglionnaire — autres précisions (Top 5)"
)

# Afficher chaque tableau
tab_typ_cutane
tab_typ_urinaire
tab_typ_osteo
tab_autre_respi
tab_autre_digestif
tab_autre_neuro
tab_autre_splgg




##-#########################
#--------PARACLINIQUE----


PARA <- SMITold %>%
  select(
    `Epidemio/numq`,
    starts_with("paraclinique")
  )
look_for(PARA)

##---------Labelisation variables----



### --- Bioquanti ----
var_label(PARA$`paraclinique/Hb`)       <- "Hémoglobine (g/dL)"
var_label(PARA$`paraclinique/GB`)       <- "Leucocytes (G/L)"
var_label(PARA$`paraclinique/PNN`)      <- "Neutrophiles (G/L)"
var_label(PARA$`paraclinique/EO`)       <- "Éosinophiles (G/L)"
var_label(PARA$`paraclinique/Monocytes`)<- "Monocytes (G/L)"
var_label(PARA$`paraclinique/PLT`)      <- "Plaquettes (G/L)"
var_label(PARA$`paraclinique/CRP`)      <- "CRP (mg/L)"
var_label(PARA$`paraclinique/PCT`)      <- "Procalcitonine (ng/mL)"
var_label(PARA$`paraclinique/DDIMERES`)<- "D-Dimères"
var_label(PARA$`paraclinique/uree`)     <- "Urée"
var_label(PARA$`paraclinique/Creat`)    <- "Créatinine"
var_label(PARA$`paraclinique/NA`)       <- "Sodium"
var_label(PARA$`paraclinique/K`)        <- "Potassium"
var_label(PARA$`paraclinique/ASAT`)     <- "ASAT"
var_label(PARA$`paraclinique/ALAT`)     <- "ALAT"
var_label(PARA$`paraclinique/TP`)       <- "TP"

### --- Microbiologie ----
var_label(PARA$`paraclinique/bacterio_f`)   <- "Prélèvement bactériologique réalisé"
var_label(PARA$`paraclinique/hemoc_f`)      <- "Hémoculture"
var_label(PARA$`paraclinique/urine_f`)      <- "Examen cytobactériologique des urines"
var_label(PARA$`paraclinique/selles_f`)     <- "Coproculture"
var_label(PARA$`paraclinique/pus_f`)        <- "Prélèvement pus"
var_label(PARA$`paraclinique/LCS_f`)        <- "LCR (Ponction lombaire)"
var_label(PARA$`paraclinique/respiratoire_f`)<- "Prélèvement respiratoire"
var_label(PARA$`paraclinique/PV_f`)         <- "Prélèvement vaginal"
var_label(PARA$`paraclinique/isol_germe_f`) <- "Germe isolé"
var_label(PARA$`paraclinique/nbrebact_f`)   <- "Nombre de bactéries (mono vs multi)"
var_label(PARA$`paraclinique/bacterie1`)    <- "Bactérie n°1"
var_label(PARA$`paraclinique/bacterie2`)    <- "Bactérie n°2"
var_label(PARA$`paraclinique/bacterie3`)    <- "Bactérie n°3"
var_label(PARA$`paraclinique/BMR_f`)        <- "Bactérie multirésistante (BMR)"
var_label(PARA$`paraclinique/KAOP_f`)       <- "Selles KAOP"
var_label(PARA$`paraclinique/isol_kaop`)    <- "Germe KAOP (texte)"
var_label(PARA$`paraclinique/autre`)        <- "Autre type de prélèvement (texte)"

### --- Tests rapides ----
var_label(PARA$`paraclinique/GE_f`)         <- "Goutte Epaisse (GE)"
var_label(PARA$`paraclinique/Resulats_GE`)  <- "Résultat GE (texte)"
var_label(PARA$`paraclinique/TDR_palu_f`)   <- "TDR paludisme réalisé"
var_label(PARA$`paraclinique/Resulats_TDR`) <- "Résultat TDR palu (texte)"

# --- Imagerie ---
var_label(PARA$`paraclinique/Im_thx_f`)     <- "Rx thoracique"
var_label(PARA$`paraclinique/result_imthx`)<- "Résultat imagerie thoracique (texte)"
var_label(PARA$`paraclinique/Im_cere_f`)    <- "Imagerie cérébrale"
var_label(PARA$`paraclinique/result_imcere`)<- "Résultat imagerie cérébrale (texte)"
var_label(PARA$`paraclinique/Im_diges_f`)   <- "Imagerie digestive"
var_label(PARA$`paraclinique/result_imdiges`)<- "Résultat imagerie digestive (texte)"
var_label(PARA$`paraclinique/autre_im_f`)   <- "Autre imagerie"
var_label(PARA$`paraclinique/typ_autre_im`)<- "Type d'imagerie autre (texte)"
var_label(PARA$`paraclinique/resul_autre_im`)<- "Résultat autre imagerie (texte)"


##------------------ Analyse descriptive----
###-----1.BLOC Bio----


# Liste des variables bioquanti
vars_bio <- c(
  "paraclinique/Hb", "paraclinique/GB", "paraclinique/PNN",
  "paraclinique/EO", "paraclinique/Monocytes", "paraclinique/PLT",
  "paraclinique/CRP", "paraclinique/PCT", "paraclinique/DDIMERES",
  "paraclinique/uree", "paraclinique/Creat",
  "paraclinique/NA", "paraclinique/K",
  "paraclinique/ASAT", "paraclinique/ALAT", "paraclinique/TP"
)

# Tableau descriptif : médiane [IQR]
tab_para_bio <- PARA %>%
  select(all_of(vars_bio)) %>%
  tbl_summary(
    statistic = all_continuous() ~ "Moy.sd : {mean} ({sd}) | Med.IQR : {median}[{p25}, {p75}]",
    digits    = all_continuous() ~ 1,
    missing   = "no"
  ) %>%
  modify_header(label ~ "**Examens biologiques (≥65 ans)**")|>
  add_n()|> 
  bold_labels()

tab_para_bio



tab_para_bio1 <- PARA %>%
  select(all_of(vars_bio)) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{N_nonmiss} / {median} [{p25}, {p75}]",
    digits    = all_continuous() ~ c(0, 1, 1, 1), # N=entier, autres=1 décimale
    missing   = "ifany"
  ) %>%
  modify_header(
    label ~ "**Examens biologiques (≥65 ans)**",
    stat_0 ~ "**N valides / Médiane [IQR]**"
  )|> add_n()|> 
  bold_labels()

tab_para_bio1

###----2.BLOC Microbio/bacterio----


# Variables "globales" de prélèvements réalisés (Oui/Non)
vars_micro_global <- c(
  "paraclinique/bacterio_f",     # Au moins un prélèvement bactério
  "paraclinique/hemoc_f",        # Hémoculture
  "paraclinique/urine_f",        # ECBU
  "paraclinique/selles_f",       # Coproculture
  "paraclinique/pus_f",          # Pus
  "paraclinique/LCS_f",          # LCR
  "paraclinique/respiratoire_f", # Prélèvement respiratoire
  "paraclinique/PV_f"            # Prélèvement vaginal
)

tab_para_micro_global <- PARA %>%
  select(all_of(vars_micro_global)) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"   # NA exclus du dénominateur
  ) %>%
  modify_header(
    label  ~ "**Microbiologie — prélèvements réalisés (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )|> add_n()|> 
  bold_labels()

tab_para_micro_global



###------3.pos Microbiologie ----


# --- 1. Sous-échantillon : uniquement les patients avec germe isolé
PARA_pos <- PARA %>% filter(`paraclinique/isol_germe_f` == "Oui")

# --- 2. Tableau résumé (nbrebact_f et BMR_f)
tab_para_micro_pos <- PARA_pos %>%
  select(`paraclinique/nbrebact_f`, `paraclinique/BMR_f`) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ "**Microbiologie positive (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )|>
add_n()|> 
  bold_labels()

# --- 3. Top 15 des bactéries isolées (en combinant bacterie1-3)

#BACT RECODAGE----


## Recoding PARA$`paraclinique/bacterie1` into PARA$`paraclinique/type_bacterie1`
PARA$`paraclinique/type_bacterie1_rec` <- PARA$`paraclinique/bacterie1` %>%
  fct_recode(
    "Candida albicans" = "Candida albican",
    "E.Coli" = "Echerichia Coli",
    "E.Coli" = "Echérichia Coli",
    "E.Coli" = "Echerichia Coli ( ECBU)",
    "Enterobacter Cloacae" = "Enterobacter Cloacae (ECBE)",
    "E.Coli" = "Escherichia coli",
    "E.Coli" = "Escherichia Coli ( ECBU et Hémoculture)",
    "E.Coli" = "Escherichia Coli ( ECBU et Hémoculture).",
    "Mycobacterium tuberculosis" = "Mucobactérium tuberculosis",
    "Proteus mirabilis" = "Porteus mirabilis",
    "Pseudomonas aeroguninosa" = "Speudomonas aeroginosa ( Expectoration)",
    "Staphyloccocus  haemolyticus" = "Staphyloccocus  haemolyticus(ECBU)",
    "Staphyloccocus aureus" = "Staphyloccocus aureus ( ECBU)",
    "Staphyloccocus  haemolyticus" = "Staphyloccocus haemolyticus",
    "Streptoccocus pneumoniae" = "Streptoccocus pneumoniae( ECBE)"
  )


## Recoding PARA$`paraclinique/bacterie2` into PARA$`paraclinique/bacterie2_rec`
PARA$`paraclinique/type_bacterie2_rec` <- PARA$`paraclinique/bacterie2` %>%
  fct_recode(
    NULL = "N'est",
    NULL = "Néant",
    "Staphyloccocus aureus" = "Staphylococcus aureus",
    "Stenotrophomonas maltophilia" = "Stenotrophomonas maltophilia (ECBE)"
  )


## Recoding PARA$`paraclinique/bacterie3` into PARA$`paraclinique/bacterie3_rec`
PARA$`paraclinique/bacterie3_rec` <- PARA$`paraclinique/bacterie3` %>%
  fct_recode(
    NULL = "Néant"
  )

top_bacteries <- PARA_pos %>%
  select(`paraclinique/type_bacterie1_rec`, `paraclinique/type_bacterie2_rec`, `paraclinique/bacterie3_rec`) %>%
  pivot_longer(everything(), values_to = "Germe") %>%
  filter(!is.na(Germe), Germe != "") %>%
  count(Germe, sort = TRUE) %>%
  mutate(Prop = round(100 * n / sum(n), 1)) %>%
  slice_head(n = 30)

tab_top_bacteries <- top_bacteries %>%
  gt() %>%
  tab_header(
    title = md("**Germes isolés (chez les positifs)**"),
    subtitle = md(glue::glue("n = {nrow(PARA_pos)} patients positifs"))
  ) %>%
  cols_label(
    Germe ~ "Germe",
    n     ~ "Effectif",
    Prop  ~ "%"
  ) %>%
  fmt_number(columns = c("n", "Prop"), decimals = 1) %>%
  opt_row_striping()

# --- Résultats
tab_para_micro_pos
tab_top_bacteries


####-------FLUX PATIENT BACTERIO----

# --- Sous-échantillons ---
N_total <- nrow(PARA)
N_bacterio <- PARA %>% filter(`paraclinique/bacterio_f` == "Oui") %>% nrow()
N_pos <- PARA %>% filter(`paraclinique/isol_germe_f` == "Oui") %>% nrow()

# --- 3a. Flux des patients ---
flux_micro <- tibble(
  Etape = c(
    "Patients avec prélèvement bactériologique",
    "Patients avec germe isolé",
    "Mono-infection",
    "Multi-infection"
  ),
  Effectif = c(
    N_bacterio,
    N_pos,
    PARA %>% filter(`paraclinique/isol_germe_f` == "Oui", `paraclinique/nbrebact_f` == "mono") %>% nrow(),
    PARA %>% filter(`paraclinique/isol_germe_f` == "Oui", `paraclinique/nbrebact_f` == "multi") %>% nrow()
  )
) %>%
  mutate(Prop = round(100 * Effectif / N_total, 1))

tab_flux_micro <- flux_micro %>%
  gt() %>%
  tab_header(title = md("**Flux des patients — Microbiologie**"),
             subtitle = md(glue::glue("Population totale = {N_total} patients"))) %>%
  cols_label(
    Etape ~ "Variables",
    Effectif ~ "n",
    Prop ~ "% du total"
  ) %>%
  fmt_number(columns = c("Effectif", "Prop"), decimals = 1 ) %>%
  opt_row_striping()

# --- 3b. Résistance (BMR) chez les positifs ---
tab_bmr <- PARA %>%
  filter(`paraclinique/isol_germe_f` == "Oui") %>%
  select(`paraclinique/BMR_f`) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  modify_header(
    label ~ "**Résistance bactérienne (BMR) — positifs**",
    stat_0 ~ "**n (%)**"
  )

# --- 3c. Spectre bactérien (Top 5 germes isolés) ---
top_bacteries <- PARA %>%
  filter(`paraclinique/isol_germe_f` == "Oui") %>%
  select(`paraclinique/bacterie1`, `paraclinique/bacterie2`, `paraclinique/bacterie3`) %>%
  pivot_longer(everything(), values_to = "Germe") %>%
  filter(!is.na(Germe), Germe != "") %>%
  count(Germe, sort = TRUE) %>%
  mutate(Prop = round(100 * n / sum(n), 1)) %>%
  slice_head(n = 5)

tab_top_bacteries <- top_bacteries %>%
  gt() %>%
  tab_header(title = md("**Top 5 germes isolés**"),
             subtitle = md(glue::glue("Chez {N_pos} patients positifs"))) %>%
  cols_label(
    Germe ~ "Germe",
    n ~ "Effectif",
    Prop ~ "%"
  ) %>%
  fmt_number(columns = c("n", "Prop"), decimals = 1) %>%
  opt_row_striping()

# --- Résultats à afficher ---
tab_flux_micro
tab_bmr
tab_top_bacteries


###------------- 4. PARASITOLOGIE ----

#### ---------- 4a. KAOP réalisé (Oui/Non) ----------
tab_kaop_realise <- PARA %>%
  select(`paraclinique/KAOP_f`) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"  # NA exclus du dénominateur
  ) %>%
  modify_header(
    label  ~ "**Parasitologie — KAOP (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )

#### ---------- 4b. Top 5 parasites isolés (chez KAOP = Oui) ----------
# (1) Sous-échantillon: uniquement si KAOP réalisé = Oui
PARA_kaop_oui <- PARA %>% filter(`paraclinique/KAOP_f` == "Oui")

# (2) Nettoyage minimal du texte: trim + enlever vides
top_kaop <- PARA_kaop_oui %>%
  transmute(Parasite = str_trim(`paraclinique/isol_kaop`)) %>%
  filter(!is.na(Parasite), Parasite != "") %>%
  count(Parasite, sort = TRUE) %>%
  mutate(Prop = round(100 * n / sum(n), 1)) %>%
  slice_head(n = 5)

# (3) Rendu gt "pro"
tab_kaop_top <- top_kaop %>%
  gt() %>%
  tab_header(
    title    = md("**Résultats selles (KAOP)**"),
    subtitle = md(glue("Calculé chez les patients avec KAOP = Oui (n = {nrow(PARA_kaop_oui)})"))
  ) %>%
  cols_label(
    Parasite ~ "Résultat",
    n        ~ "Effectif",
    Prop     ~ "%"
  ) %>%
  fmt_number(columns = c("n","Prop"), decimals = 1) %>%
  opt_row_striping() %>%
  tab_options(table.font.size = px(14), data_row.padding = px(6)) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

#### ---------- Afficher ----------
tab_kaop_realise
tab_kaop_top


###----------5.Tests rapides----

#### ---------- 5a. Tests réalisés ----------
vars_tests <- c("paraclinique/GE_f", "paraclinique/TDR_palu_f")

tab_tests_realise <- PARA %>%
  select(all_of(vars_tests)) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ "**Tests rapides (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )|>
  add_n()|> bold_labels()

#### ---------- 5b. Top 5 résultats ----------
# GE
PARA_ge_oui <- PARA %>% filter(`paraclinique/GE_f` == "Oui")
top_GE <- PARA_ge_oui %>%
  transmute(Resultat = str_trim(`paraclinique/Resulats_GE`)) %>%
  filter(!is.na(Resultat), Resultat != "") %>%
  count(Resultat, sort = TRUE) %>%
  mutate(Prop = round(100 * n / sum(n), 1)) %>%
  slice_head(n = 5)

tab_top_GE <- top_GE %>%
  gt() %>%
  tab_header(
    title    = md("**Top 5 résultats — Test GE**"),
    subtitle = md(glue("Chez {nrow(PARA_ge_oui)} patients avec test GE = Oui"))
  ) %>%
  cols_label(Resultat ~ "Résultat", n ~ "Effectif", Prop ~ "%") %>%
  fmt_number(columns = c("n","Prop"), decimals = 1) %>%
  opt_row_striping()

# TDR palu
PARA_tdr_oui <- PARA %>% filter(`paraclinique/TDR_palu_f` == "Oui")
top_TDR <- PARA_tdr_oui %>%
  transmute(Resultat = str_trim(`paraclinique/Resulats_TDR`)) %>%
  filter(!is.na(Resultat), Resultat != "") %>%
  count(Resultat, sort = TRUE) %>%
  mutate(Prop = round(100 * n / sum(n), 1)) %>%
  slice_head(n = 5)

tab_top_TDR <- top_TDR %>%
  gt() %>%
  tab_header(
    title    = md("**Résultats — TDR Paludisme**"),
    subtitle = md(glue("Chez {nrow(PARA_tdr_oui)} patients avec TDR palu = Oui"))
  ) %>%
  cols_label(Resultat ~ "Résultat", n ~ "Effectif", Prop ~ "%") %>%
  fmt_number(columns = c("n","Prop"), decimals = 1) %>%
  opt_row_striping()

#### ---------- Afficher ----------
tab_tests_realise
tab_top_GE
tab_top_TDR


###---------6.IMAGERIE----

#### ---------- 6a. Imagerie réalisée (Oui/Non) ----------
vars_imagerie <- c(
  "paraclinique/Im_thx_f",   # thorax
  "paraclinique/Im_cere_f",  # cérébral
  "paraclinique/Im_diges_f", # digestif
  "paraclinique/autre_im_f"  # autre
)

tab_imagerie_realise <- PARA %>%
  select(all_of(vars_imagerie)) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ "**Imagerie réalisée (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )|>
  add_n()|> bold_labels()


#### ---------- 6b. Résultats (Top 5), conditionnés par Im_*_f == "Oui" ----------
# Utilitaire pour produire un gt "pro" Top 5 textes
top5_im <- function(data, flag_col, text_col, titre){
  d_flag <- data %>% filter(.data[[flag_col]] == "Oui")
  top_df <- d_flag %>%
    transmute(Res = str_trim(.data[[text_col]])) %>%
    filter(!is.na(Res), Res != "") %>%
    count(Res, sort = TRUE) %>%
    mutate(Prop = round(100 * n / sum(n), 1)) %>%
    slice_head(n = 10)
  
  gt_tbl <- top_df %>%
    gt() %>%
    tab_header(
      title    = md(glue("**{titre} — Top 5 résultats**")),
      subtitle = md(glue("Chez {nrow(d_flag)} patients avec l'examen = Oui"))
    ) %>%
    cols_label(
      Res ~ "Résultat",
      n   ~ "Effectif",
      Prop~ "%"
    ) %>%
    fmt_number(columns = c("n","Prop"), decimals = 1) %>%
    opt_row_striping() %>%
    tab_options(table.font.size = px(14), data_row.padding = px(6)) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    )
  
  gt_tbl
}

# Thorax
tab_im_thx_top <- top5_im(
  PARA,
  flag_col = "paraclinique/Im_thx_f",
  text_col = "paraclinique/result_imthx",
  titre    = "Imagerie thoracique"
)

# Cérébral
tab_im_cere_top <- top5_im(
  PARA,
  flag_col = "paraclinique/Im_cere_f",
  text_col = "paraclinique/result_imcere",
  titre    = "Imagerie cérébrale"
)

# Digestif
tab_im_diges_top <- top5_im(
  PARA,
  flag_col = "paraclinique/Im_diges_f",
  text_col = "paraclinique/result_imdiges",
  titre    = "Imagerie digestive"
)

# Autre (on peut afficher le type + résultat si tu veux, ici on met le résultat)
tab_im_autre_top <- top5_im(
  PARA,
  flag_col = "paraclinique/autre_im_f",
  text_col = "paraclinique/resul_autre_im",  # tu peux aussi résumer 'typ_autre_im'
  titre    = "Autre imagerie"
)

#### ---------- Afficher ----------
tab_imagerie_realise
tab_im_thx_top
tab_im_cere_top
tab_im_diges_top
tab_im_autre_top





#-###########################################

#---------------DIAGNOSTIC----


DIAGNOSTIC <- SMITold %>%
  select(
    `Epidemio/numq`,
    starts_with("Diagnostic")
  )


look_for(DIAGNOSTIC)

#----------Labelisation


# -- diagnostics ciblés
var_label(DIAGNOSTIC$`Diagnostic/palu_f`)          <- "Paludisme grave"
var_label(DIAGNOSTIC$`Diagnostic/tetanos_f`)       <- "Tétanos"
var_label(DIAGNOSTIC$`Diagnostic/tb_f`)            <- "Tuberculose"
var_label(DIAGNOSTIC$`Diagnostic/pulmonaire_f`)    <- "TB pulmonaire"
var_label(DIAGNOSTIC$`Diagnostic/pleurale_f`)      <- "TB pleurale"
var_label(DIAGNOSTIC$`Diagnostic/ganglionnaire_f`) <- "TB ganglionnaire"
var_label(DIAGNOSTIC$`Diagnostic/tbdigestif_f`)    <- "TB digestive"
var_label(DIAGNOSTIC$`Diagnostic/neulogique_f`)    <- "TB neurologique"
var_label(DIAGNOSTIC$`Diagnostic/osseuse_f`)       <- "TB osseuse"
var_label(DIAGNOSTIC$`Diagnostic/pericardique_f`)  <- "TB péricardique"
var_label(DIAGNOSTIC$`Diagnostic/septicemie_f`)    <- "Septicémie"
var_label(DIAGNOSTIC$`Diagnostic/meningite_f`)     <- "Méningo-Encephalite"

# -- étiologies (catégories globales)
var_label(DIAGNOSTIC$`Diagnostic/bacterienne_f`)       <- "ME bactérienne"
var_label(DIAGNOSTIC$`Diagnostic/virale_f`)            <- "ME virale"
var_label(DIAGNOSTIC$`Diagnostic/parasitaire_f`)       <- "ME parasitaire"
var_label(DIAGNOSTIC$`Diagnostic/fongique_f`)          <- "ME fongique"
var_label(DIAGNOSTIC$`Diagnostic/non_infectieuse_f`)   <- "ME non infectieuse"

# -- divers
var_label(DIAGNOSTIC$`Diagnostic/DHBNN_f`) <- "Dermohypodermite(DHBNN)"
var_label(DIAGNOSTIC$`Diagnostic/IO_f`)    <- "Infection opportuniste (IO)"
var_label(DIAGNOSTIC$`Diagnostic/IO_pulm_f`)  <- "IO pulmonaire"
var_label(DIAGNOSTIC$`Diagnostic/IO_diges_f`) <- "IO digestive"
var_label(DIAGNOSTIC$`Diagnostic/IO_neuro_f`) <- "IO neurologique"

# -- champs texte (typ_*, autre*)
# (ces colonnes sont pour les résumés qualitatifs Top 5 plus tard)
# Si tu veux des labels, on peut aussi poser:
# var_label(DIAGNOSTIC$`Diagnostic/typ_Iopulm`)  <- "IO pulmonaire (précisions)"
# var_label(DIAGNOSTIC$`Diagnostic/typ_Iodiges`) <- "IO digestive (précisions)"
# var_label(DIAGNOSTIC$`Diagnostic/typ_Ioneuro`) <- "IO neurologique (précisions)"
# var_label(DIAGNOSTIC$`Diagnostic/autreIO`)     <- "Autre IO (précisions)"
# var_label(DIAGNOSTIC$`Diagnostic/autrediagnostic`) <- "Autre diagnostic (texte libre)"


##-------BLOC General ----

tab_diag_main <- DIAGNOSTIC %>%
  select(
    `Diagnostic/palu_f`, `Diagnostic/tetanos_f`,
    `Diagnostic/tb_f`, `Diagnostic/septicemie_f`,
    `Diagnostic/meningite_f`, `Diagnostic/DHBNN_f`,`Diagnostic/IO_f`
  ) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ "**Diagnostics principaux (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )|> add_n() |> bold_labels()

tab_diag_main

##-------BLOC TB----

DIAG_tb <- DIAGNOSTIC %>% filter(`Diagnostic/tb_f` == "Oui")
n_tb <- nrow(DIAG_tb)

tab_tb_loc <- DIAG_tb %>%
  select(
    `Diagnostic/pulmonaire_f`, `Diagnostic/pleurale_f`,
    `Diagnostic/ganglionnaire_f`, `Diagnostic/tbdigestif_f`,
    `Diagnostic/neulogique_f`, `Diagnostic/osseuse_f`,
    `Diagnostic/pericardique_f`
  ) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ glue("**Tuberculose — localisations** (parmi TB = Oui ; n = {n_tb})"),
    stat_0 ~ "**n (%)**"
  )|> add_n() |> bold_labels()

tab_tb_loc

##--------------BLOC MEningite----

DIAG_men <- DIAGNOSTIC %>% filter(`Diagnostic/meningite_f` == "Oui")
n_men <- nrow(DIAG_men)

tab_meningite_etiol <- DIAG_men %>%
  select(
    `Diagnostic/bacterienne_f`, `Diagnostic/virale_f`,
    `Diagnostic/parasitaire_f`, `Diagnostic/fongique_f`,
    `Diagnostic/non_infectieuse_f`
  ) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ glue("**Méningite — étiologie** (parmi méningite = Oui ; n = {n_men})"),
    stat_0 ~ "**n (%)**"
  ) |> bold_labels()

tab_meningite_etiol

##----------------BLOC IO ----

DIAG_io <- DIAGNOSTIC %>% filter(`Diagnostic/IO_f` == "Oui")
n_io <- nrow(DIAG_io)


top5_txt <- function(df, flag_col, text_col, titre){
  d <- df %>% filter(.data[[flag_col]] == "Oui")
  out <- d %>%
    transmute(Res = str_trim(.data[[text_col]])) %>%
    filter(!is.na(Res), Res != "") %>%
    count(Res, sort = TRUE) %>%
    mutate(Prop = round(100 * n / sum(n), 1)) %>%
    slice_head(n = 5)
  out %>%
    gt() %>%
    tab_header(
      title    = md(paste0("**", titre, " — Top 5**")),
      subtitle = md(glue("Parmi IO = Oui (n = {nrow(d)})"))
    ) %>%
    cols_label(Res ~ "Précision", n ~ "Effectif", Prop ~ "%") %>%
    fmt_number(columns = c(n, Prop), decimals = 1) %>%
    opt_row_striping()
}


tab_io_pulm_top  <- top5_txt(DIAG_io, "Diagnostic/IO_pulm_f",  "Diagnostic/typ_Iopulm",  "IO pulmonaires")
tab_io_diges_top <- top5_txt(DIAG_io, "Diagnostic/IO_diges_f", "Diagnostic/typ_Iodiges", "IO digestives")
tab_io_neuro_top <- top5_txt(DIAG_io, "Diagnostic/IO_neuro_f", "Diagnostic/typ_Ioneuro", "IO neurologiques")
tab_io_autre_top <- top5_txt(DIAG_io, "Diagnostic/autreIO",    "Diagnostic/typ_autreIO", "Autres IO") # si 'autreIO' est binaire Oui/Non

tab_io_pulm_top
tab_io_diges_top
tab_io_neuro_top
tab_io_autre_top

##AUTRE DIAGNOS----

###RECOd autre diag----





tab_autre_diag <- DIAGNOSTIC %>%
  transmute(Autre = str_trim(`Diagnostic/autrediagnostic`)) %>%
  filter(!is.na(Autre), Autre != "") %>%
  count(Autre, sort = TRUE) %>%
  mutate(Prop = round(100 * n / sum(n), 1)) %>%
  slice_head(n = 100) %>%
  gt() %>%
  tab_header(
    title = md("**Autre diagnostic — Top 5**"),
    subtitle = "Parmi les non manquants"
  ) %>%
  cols_label(Autre ~ "Diagnostic", n ~ "Effectif", Prop ~ "%") %>%
  fmt_number(columns = c(n, Prop), decimals = 1) %>%
  opt_row_striping()

tab_autre_diag


#-###########################
#------------TRAITEMENT----


TRAITEMENT <- SMITold %>%
  select(
    `Epidemio/numq`,
    starts_with("Traitement")
  )


look_for(TRAITEMENT)



##-------Labelisation----


# Antibiotiques
var_label(TRAITEMENT$`Traitement/ATBpie_f`) <- "Antibiothérapie "
var_label(TRAITEMENT$`Traitement/ATB1`)     <- "Antibiotique 1"
var_label(TRAITEMENT$`Traitement/ATB2`)     <- "Antibiotique 2"
var_label(TRAITEMENT$`Traitement/ATB3`)     <- "Antibiotique 3"

# Antiviraux
var_label(TRAITEMENT$`Traitement/Anti_viraux_f`) <- "Antiviraux"
var_label(TRAITEMENT$`Traitement/antiviral1`)    <- "Antiviral 1"
var_label(TRAITEMENT$`Traitement/antiviral2`)    <- "Antiviral 2"

# ARV
var_label(TRAITEMENT$`Traitement/ARV_f`)        <- "Antiretroviraux (ARV)"
var_label(TRAITEMENT$`Traitement/schemasARV`)   <- "Schéma ARV"

# Antiparasitaires / Palu
var_label(TRAITEMENT$`Traitement/Anti_parasitaire_f`) <- "Antiparasitaires"
var_label(TRAITEMENT$`Traitement/typ_antiparasitaire`) <- "Type d'antiparasitaire"
var_label(TRAITEMENT$`Traitement/Anti_Palu_f`)   <- "Antipaludiques"
var_label(TRAITEMENT$`Traitement/typ_antipalu`)  <- "Type d'antipaludique"

# Antifongiques
var_label(TRAITEMENT$`Traitement/Anti_fongique_f`) <- "Antifongiques"

# Anti-TB
var_label(TRAITEMENT$`Traitement/Anti_TB_f`) <- "Anti-Tuberculeux"

# Autres classes thérapeutiques
var_label(TRAITEMENT$`Traitement/Anti_pyretique_f`) <- "Antipyrétiques"
var_label(TRAITEMENT$`Traitement/Corticoides_f`)    <- "Corticoïdes"
var_label(TRAITEMENT$`Traitement/Typ_cortico`)      <- "Type de corticoïde"
var_label(TRAITEMENT$`Traitement/Sedatifs_f`)       <- "Sédatifs"
var_label(TRAITEMENT$`Traitement/typ_sedatifs`)     <- "Type de sédatif"

# Réanimation / support
var_label(TRAITEMENT$`Traitement/Oxygenation_f`) <- "Oxygénation"
var_label(TRAITEMENT$`Traitement/IOT_f`)         <- "Intubation orotrachéale"
var_label(TRAITEMENT$`Traitement/Tracheotomie_f`)<- "Trachéotomie"
var_label(TRAITEMENT$`Traitement/EER_f`)         <- "Épuration extra-rénale"
var_label(TRAITEMENT$`Traitement/Remplissage_f`) <- "Remplissage vasculaire"
var_label(TRAITEMENT$`Traitement/Autre_rea`)     <- "Autre geste de réanimation"

# Divers
var_label(TRAITEMENT$`Traitement/autretraitement`) <- "Autres traitements"



##-------TTT Classe----

vars_traitements <- c(
  "Traitement/ATBpie_f", "Traitement/Anti_viraux_f", "Traitement/ARV_f",
  "Traitement/Anti_parasitaire_f", "Traitement/Anti_fongique_f",
  "Traitement/Anti_TB_f", "Traitement/Anti_Palu_f",
  "Traitement/Anti_pyretique_f", "Traitement/Corticoides_f",
  "Traitement/Sedatifs_f", "Traitement/Oxygenation_f",
  "Traitement/IOT_f", "Traitement/Tracheotomie_f",
  "Traitement/EER_f", "Traitement/Remplissage_f",
  "Traitement/Autre_rea"  # texte mais sert de binaire (si rempli)
)




tab_traitements <- TRAITEMENT %>%
  mutate(`Traitement/Autre_rea_f` = ifelse(is.na(`Traitement/Autre_rea`) | `Traitement/Autre_rea`=="", "Non", "Oui")) %>%
  select(all_of(vars_traitements), `Traitement/Autre_rea_f`) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ "**Traitements administrés (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )

tab_traitements


tab_traitements1 <- TRAITEMENT %>%
  select(all_of(vars_traitements)) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ "**Traitements administrés (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )|> bold_labels()|>add_n()

tab_traitements1

##---Type TTT---------
### RECO TRAITEMENT$`Traitement/ATB1` into TRAITEMENT$`Traitement/ATB1_rec`----
TRAITEMENT$`Traitement/ATB1_rec` <- TRAITEMENT$`Traitement/ATB1` %>%
  fct_recode(
    "Amoxicilline-acide clavulanique" = "Amoxicilline acide clavulanique",
    "Amoxicilline-acide clavulanique" = "Amoxicilline- acide clavulanique",
    "Cotrimoxazole" = "Cotrimoxazol",
    "Imipeneme" = "Imipénéme",
    "Metronidazole" = "Métronidazole"
  )

## Recoding TRAITEMENT$`Traitement/ATB2` into TRAITEMENT$`Traitement/ATB2_rec`
TRAITEMENT$`Traitement/ATB2_rec` <- TRAITEMENT$`Traitement/ATB2` %>%
  fct_recode(
    "Amoxicilline-acide clavulanique" = "Amoxicilline acide clavulanique",
    "Amoxicilline-acide clavulanique" = "Fleming",
    "Imipeneme" = "Imipénéme",
    "Metronidazole" = "Métronidazole",
    NULL = "Néant"
  )


## Recoding TRAITEMENT$`Traitement/ATB3` into TRAITEMENT$`Traitement/ATB3_rec`
TRAITEMENT$`Traitement/ATB3_rec` <- TRAITEMENT$`Traitement/ATB3` %>%
  fct_recode(
    "Clarithromycine" = "Amoxicilline et clarithromycine",
    "Cotrimoxazole" = "Cotrimoxazol",
    "Metronidazole" = "Métronidazole",
    NULL = "Neant",
    NULL = "Néant"
  )

## Recoding TRAITEMENT$`Traitement/Typ_cortico` into TRAITEMENT$`Traitement/Typ_cortico_rec`
TRAITEMENT$`Traitement/Typ_cortico_rec` <- TRAITEMENT$`Traitement/Typ_cortico` %>%
  fct_recode(
    "Betamethasone" = "Béthamétasone",
    "Prednisone" = "Cortancyl",
    "Dexamethasone" = "Dexamétasone",
    "Dexamethasone" = "Dexaméthasone",
    "Methylprednisolone" = "Méthylprednisolone",
    "Prednisone" = "Predenisone"
  )

## Recoding TRAITEMENT$`Traitement/typ_sedatifs` into TRAITEMENT$`Traitement/typ_sedatifs_rec`
TRAITEMENT$`Traitement/typ_sedatifs_rec` <- TRAITEMENT$`Traitement/typ_sedatifs` %>%
  fct_recode(
    "Diazepam" = "Diazépam",
    "Diazepam + Phenobarbital" = "Diazépam \nPhénobarbital",
    "Phenobarbital" = "Phénobarbital",
    "Diazepam" = "Valium"
  )



top5_txt <- function(data, col, titre){
  data %>%
    transmute(val = str_trim(.data[[col]])) %>%
    filter(!is.na(val), val != "") %>%
    count(val, sort = TRUE) %>%
    mutate(Prop = round(100 * n / sum(n), 1)) %>%
    slice_head(n = 50) %>%
    gt() %>%
    tab_header(title = md(glue::glue("**{titre} — molécules**"))) %>%
    cols_label(val ~ "Molécule", n ~ "n", Prop ~ "%") %>%
    fmt_number(columns = c("n","Prop"), decimals = 1) %>%
    opt_row_striping()
}

tab_ATB   <- top5_txt(TRAITEMENT, "Traitement/ATB1_rec", "Antibiotiques (1ère ligne)")
tab_ATB2  <- top5_txt(TRAITEMENT, "Traitement/ATB2_rec", "Antibiotiques (2e ligne)")
tab_ATB3  <- top5_txt(TRAITEMENT, "Traitement/ATB3_rec", "Antibiotiques (3e ligne)")
tab_AV    <- top5_txt(TRAITEMENT, "Traitement/antiviral1", "Antiviraux")
tab_ARV   <- top5_txt(TRAITEMENT, "Traitement/schemasARV", "Schémas ARV")
tab_AP    <- top5_txt(TRAITEMENT, "Traitement/typ_antiparasitaire", "Antiparasitaires")
tab_APalu <- top5_txt(TRAITEMENT, "Traitement/typ_antipalu", "Antipaludiques")
tab_Cort  <- top5_txt(TRAITEMENT, "Traitement/Typ_cortico_rec", "Corticoïdes")
tab_Sed   <- top5_txt(TRAITEMENT, "Traitement/typ_sedatifs_rec", "Sédatifs")
tab_Autre <- top5_txt(TRAITEMENT, "Traitement/autretraitement", "Autres traitements")

# Tu affiches ensuite celui qui t'intéresse :
# tab_ATB ; tab_AV ; tab_ARV ; etc.


#####################################

#-----------EVOLUTION----


EVOLUTION <- SMITold %>%
  select(
    `Epidemio/numq`,
    starts_with("Evolu")
  )


look_for(EVOLUTION)

##-----------Labellisation----



# Labels persistants (affichés par gtsummary/gt)
var_label(EVOLUTION$`Evolu/evolution_f`)            <- "Issue de l'hospitalisation"
var_label(EVOLUTION$`Evolu/complic_f`)              <- "Complication"
var_label(EVOLUTION$`Evolu/IAS_f`)                  <- "Infection associée aux soins (IAS)"
var_label(EVOLUTION$`Evolu/choc_septique_f`)        <- "Choc septique"
var_label(EVOLUTION$`Evolu/defaillance_neuro_f`)    <- "Défaillance neurologique"
var_label(EVOLUTION$`Evolu/insuffisance_renale_f`)  <- "Insuffisance rénale aiguë"
var_label(EVOLUTION$`Evolu/hypoglycemie_f`)         <- "Hypoglycémie"
var_label(EVOLUTION$`Evolu/blocage_thoracique_f`)   <- "Blocage thoracique"
var_label(EVOLUTION$`Evolu/Hypoxie_f`)              <- "Hypoxie"
var_label(EVOLUTION$`Evolu/IHC_f`)                  <- "Insuffisance hepatocellulaire (IHC)"


##----ISSU HOSPI------


tab_evol_issue <- EVOLUTION %>%
  select(`Evolu/evolution_f`) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"  # NA exclus du dénominateur
  ) %>%
  modify_header(
    label  ~ "**Issue de l'hospitalisation (≥65 ans)**",
    stat_0 ~ "**n (%)**"
  )|> bold_labels()|>add_n()

tab_evol_issue

##------COMPLICATIONS----

vars_comp_all <- c(
  "Evolu/complic_f","Evolu/IAS_f","Evolu/choc_septique_f",
  "Evolu/defaillance_neuro_f","Evolu/insuffisance_renale_f",
  "Evolu/hypoglycemie_f","Evolu/blocage_thoracique_f",
  "Evolu/Hypoxie_f","Evolu/IHC_f"
)

tab_comp_all <- EVOLUTION %>%
  select(all_of(vars_comp_all)) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"  # exclut les NA structurels du dénominateur
  ) %>%
  modify_header(
    label  ~ "**Complications — prévalence globale**",
    stat_0 ~ "**n (%)**"
  )|> bold_labels()|>add_n()

tab_comp_all


###-----Profil Complications


EVOL_comp_yes <- EVOLUTION %>% filter(`Evolu/complic_f` == "Oui")
n_comp <- nrow(EVOL_comp_yes)

tab_comp_among_yes <- EVOL_comp_yes %>%
  select(all_of(setdiff(vars_comp_all, "Evolu/complic_f","Evolu/autre_complic" ))) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  modify_header(
    label  ~ glue::glue("**Complications (profil)** — parmi 'complication = Oui' (n = {n_comp})"),
    stat_0 ~ "**n (%)**"
  )

tab_comp_among_yes








SMITold %>%
  select(`Epidemio/numq`, LOS) %>%
  left_join(EVOLUTION, by = "Epidemio/numq") %>%
  select(LOS, `Evolu/evolution_f`) %>%
  tbl_summary(
    by = `Evolu/evolution_f`,
    statistic = all_continuous() ~ "{median} [{p25}, {p75}]",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Durée de séjour (jours) par issue**")





PARA |> select(`paraclinique/type_bacterie1_rec`)|>
  tbl_summary(missing = "no")


#-######################FACTEURS MORTALITE ----

#SAVE DATA----
#save(trial, file = "Data/trial.RData")


#---------------BASE ANALYSE----

##Var base----

#df <- merge(EPIDEMIO, HOSPI,ATCD, CLINIQUE, PARA, DIAGNOSTIC, TRAITEMENT, EVOLUTION, by ="`Epidemio/numq`")


df <- merge(EPIDEMIO, HOSPI, by = "Epidemio/numq")
df <- merge(df, ATCD, by = "Epidemio/numq")
df <- merge(df, CLINIQUE, by = "Epidemio/numq")
df <- merge(df, PARA, by = "Epidemio/numq")
df <- merge(df, DIAGNOSTIC, by = "Epidemio/numq")
df <- merge(df, TRAITEMENT, by = "Epidemio/numq")
df <- merge(df, EVOLUTION, by = "Epidemio/numq")



df <- copy_labels(df, EPIDEMIO)
df <- copy_labels(df, HOSPI)
df <- copy_labels(df, ATCD)
df <- copy_labels(df, CLINIQUE)
df <- copy_labels(df, PARA)
df <- copy_labels(df, DIAGNOSTIC)
df <- copy_labels(df, TRAITEMENT)
df <- copy_labels(df, EVOLUTION)

look_for(df)






# --- Démographie

?names
?dput

dput(names(EPIDEMIO))
dput(names(HOSPI))
dput(names(CLINIQUE))
dput(names(EVOLUTION))




# --- Démographie
preds_demo <- c("Epidemio/age", "Epidemio/sexe_f", "Epidemio/sitmat_f")

# --- Antécédents
preds_atcd <- c(
  "ATCDS/Diabete_f", "ATCDS/HTA_f", "ATCDS/MRC_f",
  "ATCDS/HIV_f", "ATCDS/Cardio-vasculaire_f",
  "ATCDS/TB_f", "ATCDS/Hospi_ant_f"
)

# --- Clinique à l’admission
preds_clin <- c(
  "Clinique/temp", "Clinique/pouls", "Clinique/PAS", "Clinique/PAD", "Clinique/glasgow",
  "Clinique/choc_f", "Clinique/CCV_f", "Clinique/respi_f", "Clinique/neuro_f",
  "Clinique/digestif_f", "Clinique/splgg_f", "Clinique/cutane_f",
  "Clinique/urinaire_f", "Clinique/osteo_f"
)

# --- Biologie initiale
preds_bio <- c(
  "paraclinique/Hb", "paraclinique/GB", "paraclinique/PNN", "paraclinique/EO", "paraclinique/Monocytes",
  "paraclinique/PLT", "paraclinique/CRP", "paraclinique/PCT", "paraclinique/DDIMERES",
  "paraclinique/uree", "paraclinique/Creat", "paraclinique/NA", "paraclinique/K"
)

# --- Bloc diagnostics (préparé pour usage ultérieur)
preds_diag <- c(
  "Diagnostic/palu_f", "Diagnostic/tetanos_f", "Diagnostic/tb_f",
  "Diagnostic/meningite_f", "Diagnostic/septicemie_f", "Diagnostic/DHBNN_f", "Diagnostic/IO_f"
)


preds_hospi <- c("Hospi/date_h", "Hospi/provenance_f", 
                 "Hospi/reference_f", "Hospi/struc_origine_f", 
                 "Hospi/site_f", "LOS", "deces")






df_new <- df |> select(
  "Epidemio/age", "Epidemio/sexe_f", "Epidemio/sitmat_f",
  "Hospi/date_h", "Hospi/provenance_f", 
  "Hospi/reference_f", "Hospi/struc_origine_f", "Hospi/site_f",
  "ATCDS/Diabete_f", "ATCDS/HTA_f", "ATCDS/MRC_f",
  "ATCDS/HIV_f", "ATCDS/Cardio-vasculaire_f",
  "ATCDS/TB_f", "ATCDS/Hospi_ant_f",
  "Clinique/temp", "Clinique/pouls", "Clinique/PAS", "Clinique/PAD", "Clinique/glasgow",
  "Clinique/choc_f", "Clinique/CCV_f", "Clinique/respi_f", "Clinique/neuro_f",
  "Clinique/digestif_f", "Clinique/splgg_f", "Clinique/cutane_f",
  "Clinique/urinaire_f", "Clinique/osteo_f","Clinique/fievre_f","Clinique/deshydratation_f",
  "Clinique/AEG_f","Clinique/toux_f","Clinique/dyspnee_f", "Clinique/dleur_thor_f", "Clinique/dleur_abdo_f",
  "Clinique/vomissement_f", "Clinique/diarrhee_f", "Clinique/sd_encephalitique_f", "Clinique/sd_meninge_f", "Clinique/sd_focal_f",
  "paraclinique/Hb", "paraclinique/GB", "paraclinique/PNN", "paraclinique/EO", "paraclinique/Monocytes",
  "paraclinique/PLT", "paraclinique/CRP", "paraclinique/PCT", "paraclinique/DDIMERES",
  "paraclinique/uree", "paraclinique/Creat", "paraclinique/NA", "paraclinique/K",
  "Diagnostic/palu_f", "Diagnostic/tetanos_f", "Diagnostic/tb_f",
  "Diagnostic/meningite_f", "Diagnostic/septicemie_f", "Diagnostic/DHBNN_f", "Diagnostic/IO_f",
  "LOS", "deces","Evolu/complic_f", "Evolu/evolution_f"
  ) 

look_for(df_new)

# recodage 

## Cutting df_new$`Epidemio/age` into df_new$Classe_age
df_new$CLASSE_AGE <- df_new$`Epidemio/age` |> 
  cut(
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(60, 75, 106)
) %>%
  fct_recode(
    "< 75 ans" = "[60,75)",
    "75 ans et plus" = "[75,106]"
  )


df$CLASSE_AGE <- df$`Epidemio/age` |> 
  cut(
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(60, 75, 106)
  ) %>%
  fct_recode(
    "< 75 ans" = "[60,75)",
    "75 ans et plus" = "[75,106]"
  )



## Recoding df_new$`Epidemio/sitmat_f` into df_new$`Epidemio/sitmat_f_rec`
df_new$MARIE <- df_new$`Epidemio/sitmat_f` %>%
  fct_recode(
    "Non Marié(e)" = "Célibataire",
    "Non Marié(e)" = "Divorcé",
    "Non Marié(e)" = "Veuf"
  ) %>%
  fct_explicit_na("Manquant")


freq(df_new$MARIE)

df$MARIE <- df$`Epidemio/sitmat_f` %>%
  fct_recode(
    "Non Marié(e)" = "Célibataire",
    "Non Marié(e)" = "Divorcé",
    "Non Marié(e)" = "Veuf"
  ) %>%
  fct_explicit_na("Manquant")


## Recoding df_new$MARIE into df_new$MARIE_rec
df_new$MARIE_rec <- df_new$MARIE %>%
  fct_recode(
    "Non" = "Non Marié(e)",
    "Oui" = "Marié(e)"
  )


df$MARIE_rec <- df$MARIE %>%
  fct_recode(
    "Non" = "Non Marié(e)",
    "Oui" = "Marié(e)"
  )

## Recoding df_new$`Hospi/provenance_f` into df_new$PROVENANCE_DOMICILE
df_new$PROVENANCE_DOMICILE <- df_new$`Hospi/provenance_f` %>%
  fct_recode(
    "Oui" = "domicile",
    "Non" = "mutation",
    "Non" = "transfert"
  )


df$PROVENANCE_DOMICILE <- df$`Hospi/provenance_f` %>%
  fct_recode(
    "Oui" = "domicile",
    "Non" = "mutation",
    "Non" = "transfert"
  )

## Cutting df_new$`Clinique/glasgow` into df_new$GLASGOW_SCORE
df_new$GLASGOW_SCORE <- df_new$`Clinique/glasgow`|>
  cut(
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(5, 15, 20)
)|>
  fct_recode(
    "< 15" = "[5,15)",
    "> 15" = "[15,20]"
  ) %>%
  fct_explicit_na("Manquant")


df$GLASGOW_SCORE <- df$`Clinique/glasgow`|>
  cut(
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(5, 15, 20)
  )|>
  fct_recode(
    "< 15" = "[5,15)",
    "> 15" = "[15,20]"
  ) %>%
  fct_explicit_na("Manquant")


## Cutting df_new$`paraclinique/NA` into df_new$NATREMIE
df_new$NATREMIE <- df_new$`paraclinique/NA`|>
  cut(
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(110, 130, 170)
) |>
  fct_recode(
    "Hyponatremie" = "[110,130)",
    "Natremie Normale" = "[130,170]"
  ) %>%
  fct_explicit_na("Manquant")


df$NATREMIE <- df$`paraclinique/NA`|>
  cut(
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(110, 130, 170)
  ) |>
  fct_recode(
    "Hyponatremie" = "[110,130)",
    "Natremie Normale" = "[130,170]"
  ) %>%
  fct_explicit_na("Manquant")


## Cutting df_new$`paraclinique/K` into df_new$KALIEMIE
df_new$KALIEMIE <- df_new$`paraclinique/K`|>
  cut(
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(1, 3.5, 7)
)
## Recoding df_new$KALIEMIE into df_new$KALIEMIE_rec
df_new$KALIEMIE_rec <- df_new$KALIEMIE %>%
  fct_recode(
    "Hypokaliemie" = "[1,3.5)",
    "Kaliemie normale" = "[3.5,7]"
  ) %>%
  fct_explicit_na("Manquant")



df$KALIEMIE <- df$`paraclinique/K`|>
  cut(
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(1, 3.5, 7)
  )
## Recoding df_new$KALIEMIE into df_new$KALIEMIE_rec
df$KALIEMIE_rec <- df$KALIEMIE %>%
  fct_recode(
    "Hypokaliemie" = "[1,3.5)",
    "Kaliemie normale" = "[3.5,7]"
  ) %>%
  fct_explicit_na("Manquant")

## Recoding df_new$`Evolu/evolution_f` into df_new$DECES
df_new$DECES <- df_new$`Evolu/evolution_f` %>%
  fct_recode(
    "Non" = "guerison",
    "Oui" = "deces",
    "Non" = "transfert",
    "Non" = "decharge",
    "Non" = "evasion"
  )

df$DECES <- df$`Evolu/evolution_f` %>%
  fct_recode(
    "Non" = "guerison",
    "Oui" = "deces",
    "Non" = "transfert",
    "Non" = "decharge",
    "Non" = "evasion"
  )

freq(df_new$DECES)

look_for(df_new)

var_label(df_new$DECES) <- "DECES"

var_label(df_new$PROVENANCE_DOMICILE) <- "PROVENANCE DOMICILE"

var_label(df_new$MARIE_rec) <- "Marie(e)"

var_label(df_new$`ATCDS/TB_f`) <- "Antécédent de Tuberculose"


look_for(df_new)

names(df_new)



var_label(df$DECES) <- "DECES"

var_label(df$PROVENANCE_DOMICILE) <- "PROVENANCE DOMICILE"

var_label(df$MARIE_rec) <- "Marie(e)"

var_label(df$`ATCDS/TB_f`) <- "Antécédent de Tuberculose"




##------UNIVARIE----

#theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")

theme_gtsummary_language(language = "fr", big.mark = " ")
theme_gtsummary_mean_sd()

df_new |> 
  select( any_of(
    c("CLASSE_AGE" , "Epidemio/sexe_f", "MARIE_rec"   , "PROVENANCE_DOMICILE","Hospi/site_f",
    "ATCDS/Diabete_f" , "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/Cardio-vasculaire_f", "ATCDS/TB_f" , "ATCDS/Hospi_ant_f",
    "GLASGOW_SCORE" ,  "Clinique/fievre_f" ,"Clinique/deshydratation_f" , "Clinique/AEG_f"  , 
    "Clinique/respi_f" , "Clinique/digestif_f" ,  "Clinique/neuro_f" , "Clinique/urinaire_f" , "Clinique/osteo_f" ,
    "Clinique/dyspnee_f", "Clinique/toux_f" , "Clinique/dleur_thor_f"  , "Clinique/dleur_abdo_f" ,
    "Clinique/vomissement_f" ,  "Clinique/diarrhee_f" , "Clinique/sd_encephalitique_f", "Clinique/sd_meninge_f" , "Clinique/sd_focal_f" , 
    "paraclinique/Hb" ,  "paraclinique/GB"  ,  "KALIEMIE_rec",
    "Diagnostic/palu_f", "Diagnostic/tetanos_f" , "Diagnostic/tb_f" , "Diagnostic/meningite_f", "Diagnostic/septicemie_f" ,
    "Diagnostic/DHBNN_f" , "Diagnostic/IO_f" , "LOS" , "Evolu/complic_f"  , "DECES" ))
    
  ) |>
  tbl_summary(
    digits = all_categorical() ~ c(0,1)
  ) |>
  add_overall(last = T) |>
  bold_labels()

##------BIVARIE----



df_new |> 
  select( any_of(
    c("CLASSE_AGE" , "Epidemio/sexe_f", "MARIE_rec"   , "PROVENANCE_DOMICILE","Hospi/site_f",
      "ATCDS/Diabete_f" , "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/Cardio-vasculaire_f", "ATCDS/TB_f" , "ATCDS/Hospi_ant_f",
      "GLASGOW_SCORE" ,  "Clinique/fievre_f" ,"Clinique/deshydratation_f" , "Clinique/AEG_f"  , 
      "Clinique/respi_f" , "Clinique/digestif_f" ,  "Clinique/neuro_f" , "Clinique/urinaire_f" , "Clinique/osteo_f" ,
      "Clinique/dyspnee_f", "Clinique/toux_f" , "Clinique/dleur_thor_f"  , "Clinique/dleur_abdo_f" ,
      "Clinique/vomissement_f" ,  "Clinique/diarrhee_f" , "Clinique/sd_encephalitique_f", "Clinique/sd_meninge_f" , "Clinique/sd_focal_f" , 
      "paraclinique/Hb" ,  "paraclinique/GB"  ,  "KALIEMIE_rec",
      "Diagnostic/palu_f", "Diagnostic/tetanos_f" , "Diagnostic/tb_f" , "Diagnostic/meningite_f", "Diagnostic/septicemie_f" ,
      "Diagnostic/DHBNN_f" , "Diagnostic/IO_f" , "LOS" , "Evolu/complic_f"  , "DECES" ))
    
  ) |>
  tbl_summary( by = DECES,
               percent = "row",
               digits = all_categorical() ~ c(0,1)
  ) |>
  add_overall(last = T) |>
  bold_labels()


###------GGbivariate----

df_new |> 
  ggbivariate(
    outcome = "DECES",
    explanatory = c("CLASSE_AGE" , "Epidemio/sexe_f", "MARIE_rec"   , "PROVENANCE_DOMICILE","Hospi/site_f",
                    "ATCDS/Diabete_f" , "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/Cardio-vasculaire_f", "ATCDS/TB_f" , "ATCDS/Hospi_ant_f",
                    "GLASGOW_SCORE" ,  "Clinique/fievre_f" ,"Clinique/deshydratation_f" , "Clinique/AEG_f"  , 
                    "Clinique/respi_f" , "Clinique/digestif_f" ,  "Clinique/neuro_f" , "Clinique/urinaire_f" , "Clinique/osteo_f" ,
                    "Clinique/dyspnee_f", "Clinique/toux_f" , "Clinique/dleur_thor_f"  , "Clinique/dleur_abdo_f" ,
                    "Clinique/vomissement_f" ,  "Clinique/diarrhee_f" , "Clinique/sd_encephalitique_f", "Clinique/sd_meninge_f" , "Clinique/sd_focal_f" , 
                    "paraclinique/Hb" ,  "paraclinique/GB"  ,  "KALIEMIE_rec",
                    "Diagnostic/palu_f", "Diagnostic/tetanos_f" , "Diagnostic/tb_f" , "Diagnostic/meningite_f", "Diagnostic/septicemie_f" ,
                    "Diagnostic/DHBNN_f" , "Diagnostic/IO_f" , "LOS" , "Evolu/complic_f"  , "DECES" )

)





df_new |> 
  ggbivariate(
    outcome = "DECES",
    explanatory = c("CLASSE_AGE" , "Epidemio/sexe_f", "MARIE_rec"   , "PROVENANCE_DOMICILE","Hospi/site_f",
                    "ATCDS/Diabete_f" , "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/Cardio-vasculaire_f", "ATCDS/TB_f" , "ATCDS/Hospi_ant_f",
                    "GLASGOW_SCORE" ,  
                    "Clinique/dyspnee_f", "KALIEMIE_rec",
                    "Diagnostic/palu_f", "Diagnostic/tetanos_f" , "Diagnostic/tb_f" , "Diagnostic/meningite_f", "Diagnostic/septicemie_f" ,
                    "Diagnostic/DHBNN_f" , "Diagnostic/IO_f" , "Evolu/complic_f" )
    
  )


df_new |> 
  ggbivariate(
    outcome = "DECES",
    explanatory = c("CLASSE_AGE" , "Epidemio/sexe_f", "MARIE_rec"   , "PROVENANCE_DOMICILE","Hospi/site_f",
                    "ATCDS/Diabete_f" , "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/Cardio-vasculaire_f", "ATCDS/TB_f" , "ATCDS/Hospi_ant_f",
                    "GLASGOW_SCORE" ,  "Clinique/dyspnee_f", "KALIEMIE_rec" ) )

##---- RELEVEL














BIVAR_TAB <- df_new |> 
  select( any_of(
    c("CLASSE_AGE" , "Epidemio/sexe_f", "MARIE_rec"   , "PROVENANCE_DOMICILE","Hospi/site_f",
      "ATCDS/Diabete_f" , "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/Cardio-vasculaire_f", "ATCDS/TB_f" , "ATCDS/Hospi_ant_f",
      "GLASGOW_SCORE" ,  "Clinique/fievre_f" ,"Clinique/deshydratation_f" , "Clinique/AEG_f"  , 
      "Clinique/respi_f" , "Clinique/digestif_f" ,  "Clinique/neuro_f" , "Clinique/urinaire_f" , "Clinique/osteo_f" ,
      "Clinique/dyspnee_f", "Clinique/toux_f" , "Clinique/dleur_thor_f"  , "Clinique/dleur_abdo_f" ,
      "Clinique/vomissement_f" ,  "Clinique/diarrhee_f" , "Clinique/sd_encephalitique_f", "Clinique/sd_meninge_f" , "Clinique/sd_focal_f" , 
      "paraclinique/Hb" ,  "paraclinique/GB"  ,  "KALIEMIE_rec",
      "Diagnostic/palu_f", "Diagnostic/tetanos_f" , "Diagnostic/tb_f" , "Diagnostic/meningite_f", "Diagnostic/septicemie_f" ,
      "Diagnostic/DHBNN_f" , "Diagnostic/IO_f" , "LOS" , "Evolu/complic_f"  , "DECES" ))
    
  ) |>
  tbl_summary( by = DECES,
    percent = "row",
    digits = all_categorical() ~ c(0,1)
  ) |>
  add_overall(last = T) |>
  add_p() |>
  bold_labels() |>
modify_spanning_header(all_stat_cols() ~ "**DECES**") |> add_n()

BIVAR_TAB


##---ANALYSE MULTIVAR ----

var_multi <- c("Epidemio/sexe_f" , "PROVENANCE_DOMICILE","Hospi/site_f",
               "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/Cardio-vasculaire_f", "ATCDS/Hospi_ant_f",
               "GLASGOW_SCORE" ,  "Clinique/fievre_f" , "Clinique/AEG_f"  , 
               "Clinique/respi_f" , "Clinique/digestif_f" ,  "Clinique/neuro_f" , 
               "Clinique/dleur_abdo_f" ,
               "Diagnostic/tetanos_f" , "Diagnostic/septicemie_f" ,
               "Diagnostic/DHBNN_f" , "Diagnostic/IO_f" , "LOS" , "Evolu/complic_f")


#MODELE 1----

mod <- glm(
  DECES ~ 
    `Epidemio/sexe_f` + PROVENANCE_DOMICILE + `Hospi/site_f` +
    `ATCDS/HTA_f` + `ATCDS/HIV_f`  +
    GLASGOW_SCORE + `Clinique/fievre_f` + `Clinique/AEG_f` +
    `Clinique/respi_f` + `Clinique/digestif_f` + `Clinique/neuro_f` +
    `Diagnostic/tetanos_f` + `Diagnostic/septicemie_f` + `Diagnostic/IO_f` +
    LOS + `Evolu/complic_f`,
  family = binomial(),
  data = df_new
)

summary(mod)


mod |> tbl_regression(intercept = T, exponentiate = T)


stepAIC(mod)

mod_fin <- glm(formula = DECES ~ PROVENANCE_DOMICILE + `ATCDS/HTA_f` + `Clinique/AEG_f` + 
                 `Clinique/digestif_f` + `Diagnostic/IO_f` + `Evolu/complic_f`, 
               family = binomial(), data = df_new)



mod_fin |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(mod_fin, exponentiate = TRUE)



forest_model(mod_fin)


#Model 2----

## Recoding df_new$KALIEMIE_rec into df_new$KALIEMIE_rec_rec
df_new$KALIEMIE_rec_rec <- df_new$KALIEMIE_rec %>%
  fct_recode(
    NULL = "Manquant"
  )


mod2 <- glm(
  DECES ~ CLASSE_AGE +
    `Epidemio/sexe_f` + PROVENANCE_DOMICILE + `Hospi/site_f` + `ATCDS/Diabete_f`+
    `ATCDS/HTA_f` + `ATCDS/HIV_f`  + `ATCDS/Hospi_ant_f`+  `ATCDS/Cardio-vasculaire_f`+ `ATCDS/TB_f`+
    GLASGOW_SCORE + `Clinique/dyspnee_f` +
    `Diagnostic/tetanos_f` + `Diagnostic/septicemie_f` + `Diagnostic/tb_f`  + `Diagnostic/DHBNN_f` +
    `Diagnostic/meningite_f` +`Diagnostic/IO_f` +
   `Evolu/complic_f`,
  family = binomial(),
  data = df_new
)

names(df_new)




mod3 <- glm(
  DECES ~ CLASSE_AGE +
    `Epidemio/sexe_f` + PROVENANCE_DOMICILE + `Hospi/site_f`+ `ATCDS/Diabete_f`+
    `ATCDS/HTA_f` + `ATCDS/HIV_f`+ `ATCDS/Hospi_ant_f`+  `ATCDS/Cardio-vasculaire_f`+
    `ATCDS/TB_f`+ GLASGOW_SCORE + `Diagnostic/tetanos_f` + `Diagnostic/septicemie_f` + 
    `Diagnostic/tb_f`+
    `Diagnostic/DHBNN_f`+ `Diagnostic/meningite_f` +`Diagnostic/IO_f` + `Evolu/complic_f`,
  family = binomial(),
  data = df_new
)






freq(df_new$`Clinique/dyspnee_f`)

freq(df_new$MARIE)

freq(df_new$KALIEMIE_rec)


stepAIC(mod3)

# multi base ----

## Recoding df$`Clinique/dyspnee_f` into df$`Clinique/dyspnee_f_rec`
df$`Clinique/dyspnee_f_rec` <- df$`Clinique/dyspnee_f` %>%
  fct_explicit_na("Non")



names(df)


#Model forcé ----


multi <- df |> dplyr:: select(
   "CLASSE_AGE","Epidemio/sexe_f" , "MARIE_rec" ,"PROVENANCE_DOMICILE","Hospi/site_f", "ATCDS/Diabete_f" ,
         "ATCDS/HTA_f"  , "ATCDS/HIV_f" , "ATCDS/TB_f" ,"ATCDS/Cardio-vasculaire_f", "ATCDS/Hospi_ant_f",
         "GLASGOW_SCORE" ,  "NATREMIE",  "KALIEMIE_rec" ,           "Diagnostic/palu_f", "Diagnostic/meningite_f",
         "Diagnostic/tetanos_f" , "Diagnostic/septicemie_f" , "Diagnostic/tb_f",
         "Diagnostic/DHBNN_f" , "Diagnostic/IO_f", "Evolu/complic_f",
"Evolu/IAS_f", "Evolu/choc_septique_f",  "Evolu/defaillance_neuro_f"  ,
"Evolu/insuffisance_renale_f",  "Evolu/hypoglycemie_f",     
"Evolu/blocage_thoracique_f" ,   "Evolu/Hypoxie_f" ,   
"Evolu/IHC_f" ,    "Evolu/evolution_f"  ,"DECES")


multi |> tbl_summary(by = "DECES") |> add_n()

## Reordering multi$NATREMIE
multi$NATREMIE <- multi$NATREMIE %>%
  fct_relevel(
    "Natremie Normale", "Hyponatremie", "Manquant"
  )


## Reordering multi$KALIEMIE_rec
multi$KALIEMIE_rec <- multi$KALIEMIE_rec %>%
  fct_relevel(
    "Kaliemie normale", "Hypokaliemie", "Manquant"
  )


## Recoding multi$GLASGOW_SCORE
multi$GLASGOW_SCORE <- multi$GLASGOW_SCORE %>%
  fct_recode(
    "= 15" = "> 15"
  )

## Reordering multi$GLASGOW_SCORE
multi$GLASGOW_SCORE <- multi$GLASGOW_SCORE %>%
  fct_relevel(
    "= 15", "< 15", "Manquant"
  )

## Recoding multi$`Diagnostic/DHBNN_f` into multi$`Diagnostic/DHBNN_f_rec`
multi$`Diagnostic/DHBNN_f_rec` <- multi$`Diagnostic/DHBNN_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`ATCDS/Hospi_ant_f` into multi$`ATCDS/Hospi_ant_f_rec`
multi$`ATCDS/Hospi_ant_f_rec` <- multi$`ATCDS/Hospi_ant_f` %>%
  fct_explicit_na("Non")

## Recoding multi$DECES into multi$DECES_rec


multi$DECES_rec <- multi$DECES %>%
  fct_explicit_na("Non")

## Recoding multi$`Evolu/IAS_f` into multi$`Evolu/IAS_f_rec`
multi$`Evolu/IAS_f_rec` <- multi$`Evolu/IAS_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`Evolu/choc_septique_f` into multi$`Evolu/choc_septique_f_rec`
multi$`Evolu/choc_septique_f_rec` <- multi$`Evolu/choc_septique_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`Evolu/defaillance_neuro_f` into multi$`Evolu/defaillance_neuro_f_rec`
multi$`Evolu/defaillance_neuro_f_rec` <- multi$`Evolu/defaillance_neuro_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`Evolu/insuffisance_renale_f` into multi$`Evolu/insuffisance_renale_f_rec`
multi$`Evolu/insuffisance_renale_f_rec` <- multi$`Evolu/insuffisance_renale_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`Evolu/hypoglycemie_f` into multi$`Evolu/hypoglycemie_f_rec`
multi$`Evolu/hypoglycemie_f_rec` <- multi$`Evolu/hypoglycemie_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`Evolu/blocage_thoracique_f` into multi$`Evolu/blocage_thoracique_f_rec`
multi$`Evolu/blocage_thoracique_f_rec` <- multi$`Evolu/blocage_thoracique_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`Evolu/Hypoxie_f` into multi$`Evolu/Hypoxie_f_rec`
multi$`Evolu/Hypoxie_f_rec` <- multi$`Evolu/Hypoxie_f` %>%
  fct_explicit_na("Non")


## Recoding multi$`Evolu/IHC_f` into multi$`Evolu/IHC_f_rec`
multi$`Evolu/IHC_f_rec` <- multi$`Evolu/IHC_f` %>%
  fct_explicit_na("Non")











names(multi)



mod3 <- glm(
  DECES_rec ~ CLASSE_AGE +
    `Epidemio/sexe_f`+ `MARIE_rec` + PROVENANCE_DOMICILE + `Hospi/site_f`+ `ATCDS/Diabete_f`+
    `ATCDS/HTA_f` + `ATCDS/HIV_f`+`ATCDS/TB_f` + `ATCDS/Hospi_ant_f_rec`+  `ATCDS/Cardio-vasculaire_f`+
     GLASGOW_SCORE + `Diagnostic/palu_f` +`Diagnostic/tetanos_f` + `Diagnostic/septicemie_f` + 
    `Diagnostic/tb_f`+
    `Diagnostic/DHBNN_f_rec`+ `Diagnostic/meningite_f` +`Diagnostic/IO_f` + `Evolu/complic_f`,
  family = binomial(),
  data = multi
)


stepAIC(mod3)


modff <- glm(formula = DECES_rec ~ CLASSE_AGE + PROVENANCE_DOMICILE + 
               `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` + 
               `ATCDS/Cardio-vasculaire_f` + `Diagnostic/IO_f` + `Evolu/complic_f`, 
             family = binomial(), data = multi)



modff |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modff, exponentiate = TRUE)



forest_model(modff)





##modele avec complication----


###Bivar ----



multi |>
  tbl_summary( by = DECES,
               percent = "row",
               digits = all_categorical() ~ c(0,1)
  ) |>
  add_overall(last = T) |>
  add_p() |>
  bold_labels() |>
  modify_spanning_header(all_stat_cols() ~ "**DECES**") |> add_n()



###Multivar avec complications

mod4 <- glm(formula = DECES_rec ~ CLASSE_AGE + PROVENANCE_DOMICILE + 
              `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` + 
              `ATCDS/Cardio-vasculaire_f` + `Diagnostic/IO_f` + 
              `Evolu/IAS_f_rec` +      `Evolu/choc_septique_f_rec`  +
              `Evolu/defaillance_neuro_f_rec`  + `Evolu/insuffisance_renale_f_rec` + 
              `Evolu/hypoglycemie_f_rec` + `Evolu/blocage_thoracique_f_rec` +
             `Evolu/Hypoxie_f_rec`  +   `Evolu/IHC_f_rec`  , 
            family = binomial(), data = multi)


summary(mod4)


stepAIC(mod4)

#modele COMPLICATIONS ----
modf_compli <- glm(formula = DECES_rec ~ `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` + 
                     `ATCDS/Cardio-vasculaire_f` + `Diagnostic/IO_f` + `Evolu/IAS_f_rec` + 
                     `Evolu/choc_septique_f_rec` + `Evolu/insuffisance_renale_f_rec` + 
                     `Evolu/blocage_thoracique_f_rec`, family = binomial(), data = multi)



modf_compli |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modf_compli, exponentiate = TRUE)



forest_model(modf_compli)

summary(modf_compli)     #AIC: 95.129


#enlever ir et bloc thoraci ajout 3 maladies : Modele 1 CHOISI----


modf1 <- glm(formula = DECES_rec ~ `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` + 
                     `ATCDS/Cardio-vasculaire_f`+ `Diagnostic/IO_f` +`Diagnostic/tetanos_f`  + 
                      `Diagnostic/tb_f`+
                      `Diagnostic/DHBNN_f_rec`+
                      + `Evolu/IAS_f_rec` + 
                     `Evolu/choc_septique_f_rec` 
                     , family = binomial(), data = multi)



modf1 |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modf1, exponentiate = TRUE)


summary(modf1)


car::vif(modf1)


#enlever choc septique et IAS ajouter et ajout 3 pathologies


modf2 <- glm(formula = DECES_rec ~ `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` + 
               `ATCDS/Cardio-vasculaire_f` + `Diagnostic/IO_f` +`Diagnostic/tetanos_f`  + 
               `Diagnostic/tb_f`+
               `Diagnostic/DHBNN_f_rec`
             , family = binomial(), data = multi)



modf2 |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modf2, exponentiate = TRUE)


summary(modf2)


# prendre la liste de depart sociodemo et principaux diagnostiques sans stepAIC


modf3 <- glm(
  DECES_rec ~ CLASSE_AGE +
    `Epidemio/sexe_f`+ `MARIE_rec` + PROVENANCE_DOMICILE + `Hospi/site_f`+ `ATCDS/Diabete_f`+
    `ATCDS/HTA_f` + `ATCDS/HIV_f`+`ATCDS/TB_f` + `ATCDS/Hospi_ant_f_rec`+  `ATCDS/Cardio-vasculaire_f`+
    GLASGOW_SCORE + `NATREMIE` + `KALIEMIE_rec` +  `Diagnostic/IO_f` + `Diagnostic/tetanos_f` + 
    `Diagnostic/tb_f`+
    `Diagnostic/DHBNN_f_rec`,
  family = binomial(),
  data = multi
)


modf3 |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modf3, exponentiate = TRUE)


summary(modf3)



#enlever choc septique de modf1


modf1_ <- glm(formula = DECES_rec ~ `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` + 
               `ATCDS/Cardio-vasculaire_f` +`Diagnostic/tetanos_f`  + 
               `Diagnostic/tb_f`+
               `Diagnostic/DHBNN_f_rec`+
               + `Evolu/IAS_f_rec`  
             , family = binomial(), data = multi)



modf1_ |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modf1_, exponentiate = TRUE)


summary(modf1_)


#enlever IAS de modf1

modf1__ <- glm(formula = DECES_rec ~ `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` + 
               `ATCDS/Cardio-vasculaire_f` +`Diagnostic/tetanos_f`  + 
               `Diagnostic/tb_f`+
               `Diagnostic/DHBNN_f_rec`+
               `Evolu/choc_septique_f_rec` 
             , family = binomial(), data = multi)



modf1__ |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modf1__ , exponentiate = TRUE)


summary(modf1__)

#MODELE1 choisi modif - enlever ATCD CARDIOV


modf10 <- glm(formula = DECES_rec ~ `Hospi/site_f` + `ATCDS/HTA_f` + `ATCDS/Hospi_ant_f_rec` +
                `Diagnostic/IO_f` +`Diagnostic/tetanos_f`  + 
               `Diagnostic/tb_f`+
               `Diagnostic/DHBNN_f_rec`+
               + `Evolu/IAS_f_rec` + 
               `Evolu/choc_septique_f_rec` 
             , family = binomial(), data = multi)



modf10 |> tbl_regression(exponentiate = T) |> bold_labels()

ggcoef_model(modf10, exponentiate = TRUE)


summary(modf10)     #AIC: 107.56

# NEW FOREST PLOT ----
sjPlot::plot_model(modf10)

plot_model (modf10, axis.labels = c("Choc septique", "IAS","DHBNN","Tuberculose","Tetanos","Infection opportuniste(IO)",
                                    "Hospitalisation anterieure", "Hypertension arterielle", "USI"),
            axis.title = "Odds Ratio(95%)",
            colors="turquoise4",vline.color = "black", line.size=1) + theme_sjplot()+
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))












