# =========================================================
# NSFC — EXTRACTION DES AUTHOR IDs
# Objectif :
# - lire les fichiers annuels un par un
# - extraire une table publication-auteur en format long
# - sauvegarder une version légère par année
# - assembler un fichier final long
# - créer aussi une version agrégée par publication
# - joindre avec nsfc_final_clean.rds
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(data.table)

# ==============================
# 1) PARAMÈTRES
# ==============================

data_dir <- "data_nsfc"
years <- 1997:2025

author_dir <- file.path(data_dir, "author_links_years")
dir.create(author_dir, showWarnings = FALSE, recursive = TRUE)

author_chunk_dir <- file.path(data_dir, "author_links_chunks")
dir.create(author_chunk_dir, showWarnings = FALSE, recursive = TRUE)

final_long_file <- file.path(data_dir, "works_nsfc_1997_2025_author_links_long.rds")
final_publevel_file <- file.path(data_dir, "works_nsfc_1997_2025_author_links_publevel.rds")
final_joined_file <- file.path(data_dir, "nsfc_final_clean_with_authors.rds")

log_file <- file.path(data_dir, "nsfc_author_links_log.csv")

years_per_chunk <- 3

# ==============================
# 2) HELPERS
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

year_file <- function(year, data_dir = "data_nsfc") {
  file.path(data_dir, paste0("works_nsfc_", year, "_final.rds"))
}

author_year_file <- function(year, author_dir) {
  file.path(author_dir, paste0("works_nsfc_", year, "_author_links.rds"))
}

vec_to_string <- function(x, sep = ", ") {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = sep)
}

normalize_authorships <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  
  # cas enveloppé dans une couche supplémentaire
  if (is.list(x) && length(x) == 1 && is.list(x[[1]]) && is.null(names(x))) {
    if (length(x[[1]]) > 0 && is.list(x[[1]][[1]])) {
      x <- x[[1]]
    }
  }
  
  x
}

extract_author_rows <- function(pub_id, publication_year, authorships) {
  authorships <- normalize_authorships(authorships)
  
  if (is.null(authorships) || length(authorships) == 0) {
    return(data.table(
      id = character(0),
      publication_year = integer(0),
      author_id = character(0),
      author_display_name = character(0),
      author_position = character(0),
      is_corresponding = logical(0),
      raw_author_name = character(0),
      author_rank = integer(0)
    ))
  }
  
  out <- vector("list", length(authorships))
  
  for (i in seq_along(authorships)) {
    au <- authorships[[i]]
    
    author_id <- au$author$id %||% NA_character_
    author_display_name <- au$author$display_name %||% NA_character_
    author_position <- au$author_position %||% NA_character_
    is_corresponding <- au$is_corresponding %||% NA
    raw_author_name <- au$raw_author_name %||% NA_character_
    
    out[[i]] <- data.table(
      id = as.character(pub_id),
      publication_year = as.integer(publication_year),
      author_id = as.character(author_id),
      author_display_name = as.character(author_display_name),
      author_position = as.character(author_position),
      is_corresponding = as.logical(is_corresponding),
      raw_author_name = as.character(raw_author_name),
      author_rank = as.integer(i)
    )
  }
  
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

# ==============================
# 3) FONCTION DE TRAITEMENT D'UNE ANNÉE
# ==============================

process_one_year_author_links <- function(year, data_dir, author_dir) {
  
  in_file <- year_file(year, data_dir)
  out_file <- author_year_file(year, author_dir)
  
  if (!file.exists(in_file)) {
    stop("Fichier introuvable : ", in_file)
  }
  
  if (file.exists(out_file)) {
    message("✔ Fichier author_links déjà présent pour ", year, " : ", out_file)
    dt0 <- readRDS(out_file)
    n0 <- nrow(dt0)
    rm(dt0); gc()
    return(data.frame(
      year = year,
      status = "skipped_exists",
      n_obs = n0,
      file_in = in_file,
      file_out = out_file,
      stringsAsFactors = FALSE
    ))
  }
  
  message("\n====================================")
  message("Traitement année : ", year)
  message("Lecture : ", in_file)
  
  t0 <- Sys.time()
  
  dt <- as.data.table(readRDS(in_file))
  
  message("  - n lignes publications : ", nrow(dt))
  message("  - n colonnes : ", ncol(dt))
  
  needed_cols <- c("id", "publication_year", "authorships")
  missing_cols <- setdiff(needed_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes dans ", in_file, " : ", paste(missing_cols, collapse = ", "))
  }
  
  message("  - Extraction des auteurs...")
  
  author_links_list <- vector("list", nrow(dt))
  
  for (i in seq_len(nrow(dt))) {
    author_links_list[[i]] <- extract_author_rows(
      pub_id = dt$id[[i]],
      publication_year = dt$publication_year[[i]],
      authorships = dt$authorships[[i]]
    )
    
    if (i %% 50000 == 0) {
      message("    ... ", i, " publications traitées")
      gc()
    }
  }
  
  author_links_dt <- rbindlist(author_links_list, use.names = TRUE, fill = TRUE)
  
  # enlever lignes totalement vides côté auteur si jamais
  author_links_dt <- author_links_dt[
    !is.na(author_id) | !is.na(author_display_name) | !is.na(raw_author_name)
  ]
  
  saveRDS(author_links_dt, out_file, compress = "xz")
  
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  
  message("  - OK")
  message("  - Fichier author_links : ", out_file)
  message("  - n lignes auteurs : ", nrow(author_links_dt))
  message("  - Temps : ", elapsed, " sec")
  
  n_obs <- nrow(author_links_dt)
  
  rm(dt, author_links_list, author_links_dt)
  gc()
  
  data.frame(
    year = year,
    status = "success",
    n_obs = n_obs,
    file_in = in_file,
    file_out = out_file,
    stringsAsFactors = FALSE
  )
}

# ==============================
# 4) LANCER LE TRAITEMENT ANNUEL
# ==============================

log_list <- vector("list", length(years))
names(log_list) <- as.character(years)

for (yy in years) {
  res <- tryCatch(
    process_one_year_author_links(
      year = yy,
      data_dir = data_dir,
      author_dir = author_dir
    ),
    error = function(e) {
      data.frame(
        year = yy,
        status = paste0("error: ", conditionMessage(e)),
        n_obs = NA,
        file_in = year_file(yy, data_dir),
        file_out = author_year_file(yy, author_dir),
        stringsAsFactors = FALSE
      )
    }
  )
  
  log_list[[as.character(yy)]] <- res
  gc()
}

log_dt <- rbindlist(log_list, fill = TRUE)
fwrite(log_dt, log_file)

print(log_dt)

# ==============================
# 5) ASSEMBLAGE EN CHUNKS
# ==============================

author_year_files <- list.files(
  author_dir,
  pattern = "_author_links\\.rds$",
  full.names = TRUE
)

author_year_files <- author_year_files[order(author_year_files)]

message("\n====================================")
message("Assemblage en chunks")

chunk_starts <- seq(1, length(years), by = years_per_chunk)
chunk_files <- character(0)

for (k in seq_along(chunk_starts)) {
  
  idx_start <- chunk_starts[k]
  idx_end <- min(idx_start + years_per_chunk - 1, length(years))
  yrs <- years[idx_start:idx_end]
  
  out_chunk <- file.path(
    author_chunk_dir,
    paste0(
      "author_links_chunk_",
      sprintf("%03d", k),
      "_",
      min(yrs),
      "_",
      max(yrs),
      ".rds"
    )
  )
  
  if (file.exists(out_chunk)) {
    message("✔ Chunk déjà présent : ", out_chunk)
    chunk_files <- c(chunk_files, out_chunk)
    next
  }
  
  message("  - Chunk ", k, " : ", min(yrs), "-", max(yrs))
  
  files_k <- file.path(author_dir, paste0("works_nsfc_", yrs, "_author_links.rds"))
  files_k <- files_k[file.exists(files_k)]
  
  if (length(files_k) == 0) next
  
  dt_list <- lapply(files_k, readRDS)
  dt_chunk <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  
  saveRDS(dt_chunk, out_chunk, compress = "xz")
  chunk_files <- c(chunk_files, out_chunk)
  
  rm(dt_list, dt_chunk)
  gc()
}

# ==============================
# 6) ASSEMBLAGE FINAL LONG
# ==============================

message("\n====================================")
message("Assemblage final long")

if (!file.exists(final_long_file)) {
  chunk_files <- list.files(
    author_chunk_dir,
    pattern = "^author_links_chunk_.*\\.rds$",
    full.names = TRUE
  )
  chunk_files <- chunk_files[order(chunk_files)]
  
  dt_list <- lapply(chunk_files, readRDS)
  author_links_long <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  
  # sécuriser types
  author_links_long[, id := as.character(id)]
  author_links_long[, publication_year := as.integer(publication_year)]
  author_links_long[, author_id := as.character(author_id)]
  author_links_long[, author_display_name := as.character(author_display_name)]
  author_links_long[, author_position := as.character(author_position)]
  author_links_long[, is_corresponding := as.logical(is_corresponding)]
  author_links_long[, raw_author_name := as.character(raw_author_name)]
  author_links_long[, author_rank := as.integer(author_rank)]
  
  saveRDS(author_links_long, final_long_file, compress = "xz")
  
  rm(dt_list, author_links_long)
  gc()
}

author_links_long <- readRDS(final_long_file)

message("✔ Fichier final long : ", final_long_file)
message("✔ n lignes : ", nrow(author_links_long))
message("✔ n colonnes : ", ncol(author_links_long))

# ==============================
# 7) VERSION AGRÉGÉE PAR PUBLICATION
# ==============================

message("\n====================================")
message("Construction version agrégée par publication")

if (!file.exists(final_publevel_file)) {
  
  author_links_publevel <- author_links_long[
    ,
    .(
      publication_year = first(publication_year),
      n_authors_from_ids = .N,
      author_ids = list(author_id[!is.na(author_id) & nzchar(author_id)]),
      author_ids_string = vec_to_string(author_id[!is.na(author_id) & nzchar(author_id)]),
      corresponding_author_ids = list(author_id[isTRUE(is_corresponding) & !is.na(author_id) & nzchar(author_id)]),
      corresponding_author_ids_string = vec_to_string(author_id[isTRUE(is_corresponding) & !is.na(author_id) & nzchar(author_id)]),
      first_author_id = {
        x <- author_id[author_rank == min(author_rank, na.rm = TRUE)]
        x <- x[!is.na(x) & nzchar(x)]
        if (length(x) == 0) NA_character_ else x[1]
      },
      last_author_id = {
        x <- author_id[author_rank == max(author_rank, na.rm = TRUE)]
        x <- x[!is.na(x) & nzchar(x)]
        if (length(x) == 0) NA_character_ else x[1]
      }
    ),
    by = id
  ]
  
  saveRDS(author_links_publevel, final_publevel_file, compress = "xz")
  rm(author_links_publevel)
  gc()
}

author_links_publevel <- readRDS(final_publevel_file)

message("✔ Fichier agrégé : ", final_publevel_file)
message("✔ n lignes : ", nrow(author_links_publevel))
message("✔ n colonnes : ", ncol(author_links_publevel))

# ==============================
# 8) JOINTURE AVEC NSFC FINAL CLEAN
# ==============================

message("\n====================================")
message("Jointure avec nsfc_final_clean.rds")

nsfc_clean_file <- file.path(data_dir, "nsfc_final_clean.rds")

if (!file.exists(nsfc_clean_file)) {
  stop("Fichier introuvable : ", nsfc_clean_file)
}

nsfc_clean <- as.data.table(readRDS(nsfc_clean_file))
author_links_publevel <- as.data.table(author_links_publevel)

nsfc_with_authors <- merge(
  nsfc_clean,
  author_links_publevel,
  by = "id",
  all.x = TRUE
)

saveRDS(nsfc_with_authors, final_joined_file, compress = "xz")

message("✔ Fichier joint : ", final_joined_file)
message("✔ n lignes : ", nrow(nsfc_with_authors))
message("✔ n colonnes : ", ncol(nsfc_with_authors))

# ==============================
# 9) CONTRÔLES
# ==============================

message("\n====================================")
message("Contrôles")

cat("Long author-links:\n")
print(dim(author_links_long))
print(head(author_links_long))

cat("\nPublevel author-links:\n")
print(dim(author_links_publevel))
print(head(author_links_publevel))

cat("\nJointure finale:\n")
print(dim(nsfc_with_authors))
print(names(nsfc_with_authors))

cat("\nPart des publications avec au moins un author_id :\n")
print(mean(!is.na(nsfc_with_authors$n_authors_from_ids)))

cat("\nDistribution n_authors_from_ids :\n")
print(summary(nsfc_with_authors$n_authors_from_ids))

# ==============================
# 10) FICHIERS PRODUITS
# ==============================

message("\nFichiers produits :")
message(" - ", final_long_file)
message(" - ", final_publevel_file)
message(" - ", final_joined_file)
message(" - ", log_file)


#################

# ============================================================
# DiD complet pour analyser le "reputation trap" et les spillovers
# sur les revues / éditeurs à partir du corpus NSFC + CAS warning list
# ============================================================
#
# Ce script suppose que vous avez déjà en mémoire (ou sauvegardé en .rds/.csv)
# les objets suivants :
#   - nsfc_early_warning_full_analysis
#   - nsfc_with_authors
#   - wos
#   - scopus
#
# Il reconstruit un panel auteur × revue × année, estime :
#   1) un DiD de base (revues listées vs non listées)
#   2) un event-study (pré-tendances + dynamique post)
#   3) un modèle d'hétérogénéité par type d'éditeur
#   4) un test de spillover au niveau éditeur / portefeuille
#   5) des variantes de robustesse
#
# Le code est écrit pour être lisible, commenté, et modifiable.
#
# ============================================================
# 0. Packages
# ============================================================
library(tidyverse)
library(data.table)
library(fixest)
library(lubridate)
library(stringr)
library(janitor)
library(broom)
library(modelsummary)
library(ggplot2)
library(patchwork)
library(furrr)
library(scales)


#################
nsfc_early_warning_full_analysis <- readRDS("data_nsfc/nsfc_early_warning_full_analysis.rds")
nsfc_with_authors <- readRDS("data_nsfc/nsfc_final_clean_with_authors.rds")
wos <- readRDS("D:/wos_issn.rds")
scopus <- readRDS("D:/scopus_issn.rds")


setFixest_nthreads(max(1, parallel::detectCores() - 1))
options(scipen = 999)

# ============================================================
# 1. Paramètres généraux
# ============================================================

YEAR_MIN <- 2016
YEAR_MAX <- 2025
TREATMENT_YEAR <- 2019
REFERENCE_YEAR <- 2020

# Choix de la variable d'année dans chaque base.
# Ici, on privilégie publication_year si elle existe.
YEAR_VAR_MAIN <- "publication_year"
YEAR_VAR_AUTH <- "publication_year.x"

# Répertoire de sortie
out_dir <- "outputs_did_reputation_trap"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "data"), showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 2. Vérifications minimales des objets
# ============================================================

objects_needed <- c(
  "nsfc_early_warning_full_analysis",
  "nsfc_with_authors",
  "wos",
  "scopus"
)

missing_objects <- objects_needed[!sapply(objects_needed, exists)]
if (length(missing_objects) > 0) {
  stop("Objets manquants dans l'environnement : ", paste(missing_objects, collapse = ", "))
}

# Conversion en data.table pour gros volumes
nsfc_main <- as.data.table(nsfc_early_warning_full_analysis)
nsfc_auth <- as.data.table(nsfc_with_authors)
wos_dt <- as.data.table(wos)
scopus_dt <- as.data.table(scopus)

# ============================================================
# 3. Préparation des métadonnées journaux
# ============================================================

# On part de la base principale, qui contient les variables liées au CAS early warning list.
# On garde une ligne par revue (source_id) avec ses caractéristiques structurelles.

journal_meta <- nsfc_main[
  get(YEAR_VAR_MAIN) >= YEAR_MIN & get(YEAR_VAR_MAIN) <= YEAR_MAX,
  .(
    source_display_name = first(na.omit(source_display_name)),
    issn_l = first(na.omit(issn_l)),
    # issn_l_norm = first(na.omit(issn_l_norm)),
    host_organization = first(na.omit(host_organization)),
    host_organization_name = first(na.omit(host_organization_name)),
    source_country_code = first(na.omit(source_country_code)),
    publisher_name = first(na.omit(publisher_name)),
    publisher_group = first(na.omit(publisher_group)),
    publisher_family = first(na.omit(publisher_family)),
    source_is_oa = first(na.omit(source_is_oa)),
    impact_2yr = suppressWarnings(mean(impact_2yr, na.rm = TRUE)),
    main_domain = first(na.omit(main_domain)),
    in_early_warning = max(as.integer(in_early_warning), na.rm = TRUE),
    ewl_year = suppressWarnings(min(ewl_year[in_early_warning == TRUE], na.rm = TRUE)),
    warning_reason = first(na.omit(warning_reason)),
    is_big5 = max(as.integer(is_big5), na.rm = TRUE),
    is_grey = max(as.integer(is_grey), na.rm = TRUE),
    is_chinese_publisher = max(as.integer(is_chinese_publisher), na.rm = TRUE)
  ),
  by = .(source_id)
]

# Corriger les NaN produits par mean(..., na.rm=TRUE)
journal_meta[is.nan(impact_2yr), impact_2yr := NA_real_]
journal_meta[is.infinite(ewl_year), ewl_year := NA_real_]

# On définit la variable de traitement sur la base de la liste CAS.
# Important : pour l'analyse principale correspondant au papier,
# le traitement est "ever listed" selon la liste identifiée dans les données.
journal_meta[, listed := as.integer(in_early_warning == 1)]

# ============================================================
# 4. Ajout du statut d'indexation WoS / Scopus
# ============================================================

wos_dt <- wos_dt %>% clean_names() %>% as.data.table()
scopus_dt <- scopus_dt %>% clean_names() %>% as.data.table()

if (!"wos_issn" %in% names(wos_dt)) stop("La base wos doit contenir la colonne wos_issn")
if (!"scopus_issn" %in% names(scopus_dt)) stop("La base scopus doit contenir la colonne scopus_issn")

wos_dt[, issn_l := str_replace_all(wos_issn, "[^A-Za-z0-9]", "")]
scopus_dt[, issn_l := str_replace_all(scopus_issn, "[^A-Za-z0-9]", "")]

journal_meta[, in_wos := as.integer(issn_l %in% unique(wos_dt$issn_l))]
journal_meta[, in_scopus := as.integer(issn_l %in% unique(scopus_dt$issn_l))]
journal_meta[, indexed_wos_scopus := as.integer(in_wos == 1 | in_scopus == 1)]

# ============================================================
# 5. Harmonisation de la typologie éditeurs
# ============================================================

# On reconstruit une variable simple et explicite de groupe d'éditeur.
journal_meta[, publisher_group_simple := case_when(
  publisher_group == "Grey publishers" ~ "Grey",
  publisher_group == "Big 5 publishers" ~ "Big 5",
  publisher_group == "Major society / university / non-profit publishers" ~ "Societies / Univ.",
  publisher_group == "Chinese publishers" ~ "Chinese",
  publisher_group == "Other international commercial publishers" ~ "Other intl.",
  TRUE ~ NA_character_
)]

# ============================================================
# 6. Préparation de la base auteurs-publications
# ============================================================

# Cette base fournit les author_ids et permet de passer au niveau auteur × revue × année.
# On harmonise quelques noms de variables puis on déplie author_ids.

nsfc_auth_small <- nsfc_auth[
  get(YEAR_VAR_AUTH) >= YEAR_MIN & get(YEAR_VAR_AUTH) <= YEAR_MAX,
  .(
    id,
    year = get(YEAR_VAR_AUTH),
    source_display_name,
    issn_l,
    fwci,
    oa_status,
    is_oa,
    n_authors,
    country_string,
    main_domain,
    author_ids,
    first_author_id,
    last_author_id
  )
]

# On fusionne avec la base principale pour récupérer source_id et toutes les infos CAS.
# On utilise id comme identifiant publication OpenAlex.
pub_join <- unique(
  nsfc_main[
    get(YEAR_VAR_MAIN) >= YEAR_MIN & get(YEAR_VAR_MAIN) <= YEAR_MAX,
    .(
      id,
      source_id,
      source_display_name_main = source_display_name,
      host_organization_name,
      publisher_name,
      publisher_family,
      source_country_code,
      source_is_oa,
      impact_2yr,
      main_domain_main = main_domain,
      in_early_warning,
      ewl_year,
      warning_reason,
      before_after,
      event_time,
      is_big5,
      is_grey,
      is_chinese_publisher,
      issn_l
    )
  ],
  by = "id"
)

nsfc_auth_small <- merge(
  nsfc_auth_small,
  pub_join,
  by = "id",
  all.x = TRUE,
  all.y = FALSE
)

# On enlève les publications sans source_id car elles ne permettent pas le niveau revue.
nsfc_auth_small <- nsfc_auth_small[!is.na(source_id)]

# Sécurisation du format list-col des author_ids
if (!is.list(nsfc_auth_small$author_ids)) {
  stop("La colonne author_ids doit être une list-column contenant les identifiants auteurs.")
}

# Dépliage publication × auteur
pub_author <- nsfc_auth_small[
  lengths(author_ids) > 0
][
  , .(author_id = unlist(author_ids)),
  by = .(
    id, year, source_id, source_display_name, fwci, oa_status, is_oa,
    n_authors, country_string, main_domain, host_organization_name,
    publisher_name, publisher_family, source_country_code, source_is_oa,
    impact_2yr, in_early_warning, ewl_year, warning_reason,
    before_after, event_time, is_big5, is_grey, is_chinese_publisher
  )
]

# Une ligne = une publication x auteur.
pub_author <- unique(pub_author, by = c("id", "author_id"))

# ============================================================
# 7. Construction du panel auteur × revue × année
# ============================================================

# On réduit pub_author au strict minimum avant agrégation
pub_author_small <- unique(
  pub_author[, .(id, author_id, source_id, year)]
)

# Comptage des publications par auteur × revue × année
author_journal_year <- pub_author_small[
  , .(n_pub = .N),
  by = .(author_id, source_id, year)
]

# Ajout métadonnées revues
panel <- merge(
  author_journal_year,
  journal_meta,
  by = "source_id",
  all.x = TRUE
)

# On retire les groupes éditeurs inconnus si souhaité
panel <- panel[!is.na(publisher_group_simple)]

# ============================================================
# 8. Variables de traitement et variables utiles
# ============================================================

panel[, post := as.integer(year >= TREATMENT_YEAR)]
panel[, rel_year := year - REFERENCE_YEAR]
panel[, year_factor := factor(year)]
panel[, author_year_fe := interaction(author_id, year, drop = TRUE)]

# Outcome alternatif binaire
panel[, any_pub := as.integer(n_pub > 0)]

# Traitement direct
panel[, treated_journal := listed]

# Spillover : revue non listée mais appartenant à un éditeur qui possède au moins une revue listée
publisher_exposure <- journal_meta[
  !is.na(publisher_group_simple),
  .(
    publisher_has_listed = max(listed, na.rm = TRUE)
  ),
  by = .(host_organization_name, publisher_family, publisher_group_simple)
]

panel <- merge(
  panel,
  publisher_exposure,
  by = c("host_organization_name", "publisher_family", "publisher_group_simple"),
  all.x = TRUE
)

panel[is.na(publisher_has_listed), publisher_has_listed := 0L]
panel[, spillover_journal := as.integer(listed == 0 & publisher_has_listed == 1)]
panel[, exposed_publisher := as.integer(publisher_has_listed == 1)]

# Contrôles
panel[, source_is_oa := as.integer(source_is_oa == TRUE)]
panel[, log_impact_2yr := log1p(impact_2yr)]
panel[, main_domain := fct_explicit_na(as.factor(main_domain), na_level = "Unknown")]

# ============================================================
# 9. Restriction d'identification DiD
# ============================================================

# Ici, sur panel non équilibré observé
author_support <- panel[
  , .(
    has_listed = max(treated_journal == 1),
    has_nonlisted = max(treated_journal == 0),
    has_pre = max(year < TREATMENT_YEAR),
    has_post = max(year >= TREATMENT_YEAR)
  ),
  by = author_id
]

eligible_authors <- author_support[
  has_listed == 1 & has_nonlisted == 1 & has_pre == 1 & has_post == 1,
  author_id
]

panel_did <- panel[author_id %in% eligible_authors]

# Sauvegarde intermédiaire
#fwrite(panel_did, file.path(out_dir, "data", "panel_did.csv"))
#saveRDS(panel_did, "data_nsfc/panel_did.rds")
#panel_did <- readRDS("data_nsfc/panel_did.rds")

panel <- panel[year <= 2025]

panel_did <- panel_did %>%
  group_by(author_id, year) %>%
  mutate(total_pub_author_year = sum(n_pub, na.rm = TRUE)) %>%
  ungroup()

panel_did <- panel_did %>%
  mutate(share_pub = n_pub / total_pub_author_year)

# ============================================================
# 10. Descriptifs utiles
# ============================================================
library(data.table)
setDT(panel_did)

message("Nombre d'observations panel_did : ", format(nrow(panel_did), big.mark = " "))
message("Nombre d'auteurs : ", format(uniqueN(panel_did$author_id), big.mark = " "))
message("Nombre de revues : ", format(uniqueN(panel_did$source_id), big.mark = " "))

n_revues_listees <- panel_did[
  treated_journal == 1,
  uniqueN(source_id)
]
message(
  "Nombre de revues listées : ",
  format(n_revues_listees, big.mark = " ")
)

# ============================================================
# 11. Figure descriptive : trajectoires moyennes listées vs non listées
# ============================================================
YEAR_MIN <- 2016
YEAR_MAX <- 2025
TREATMENT_YEAR <- 2022
REFERENCE_YEAR <- 2021


plot_desc <- panel_did[
  , .(avg_pub = mean(n_pub, na.rm = TRUE)),
  by = .(year, group = ifelse(treated_journal == 1, "Listed journals", "Non-listed journals"))
] %>%
  filter(year <= 2025) %>%
  ggplot(aes(x = year, y = avg_pub, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = 2) +
  labs(
    title = "Average trajectories before/after the CAS list",
    x = "Year",
    y = "Average number of publications per author × journal"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(out_dir, "figures", "desc_listed_vs_nonlisted.png"),
  plot = plot_desc,
  width = 9,
  height = 6,
  dpi = 300
)

# ============================================================
# 12. Spécification DiD de base
# ============================================================

# Modèle linéaire avec FE auteur × année, cluster auteur.
# On ajoute des contrôles au niveau revue.

m_did_base <- feols(
  n_pub ~ treated_journal * post + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe,
  cluster = ~ author_id,
  data = panel_did
)

# Variante avec FE additionnelles revue si vous voulez isoler encore plus les différences inobservées fixes.
# ATTENTION : avec author_year_fe + source_id FE, le terme treated_journal seul est absorbé,
# ce qui est normal et même souhaitable.

m_did_base_jfe <- feols(
  n_pub ~ treated_journal:post + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)


# ============================================================
# 12B. Robustness to diversification / dilution of publication intensity
# Clean version ready to paste
# ============================================================

# ------------------------------------------------------------
# 12B.1 Build author-year scale variables directly in data.table
# ------------------------------------------------------------

# Make sure panel_did is a data.table
# setDT(panel_did)

# Total number of publications by author-year in the estimation sample
panel_did[, total_pub_author_year := sum(n_pub, na.rm = TRUE), by = .(author_id, year)]

# Share of an author's yearly publications going to a given journal
panel_did[, share_pub := fifelse(
  total_pub_author_year > 0,
  n_pub / total_pub_author_year,
  NA_real_
)]

# Log of total author-year publications
panel_did[, log_total_pub_author_year := log1p(total_pub_author_year)]

# Author and year FE for the scale-controlled specification
panel_did[, author_fe := factor(author_id)]
panel_did[, year_fe   := factor(year)]

# Optional quick checks
summary(panel_did$total_pub_author_year)
summary(panel_did$share_pub)
summary(panel_did$log_total_pub_author_year)

# ------------------------------------------------------------
# 12B.2 Main robustness models
# ------------------------------------------------------------

# (1) Baseline DiD with journal fixed effects
m_did_base_jfe <- feols(
  n_pub ~ treated_journal:post + source_is_oa + log_impact_2yr +
    indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# (2) Allocation model: share of publications within author-year
m_did_share <- feols(
  share_pub ~ treated_journal:post + source_is_oa + log_impact_2yr +
    indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did[!is.na(share_pub)]
)

# (3) Scale-controlled DiD
# We do NOT use author_year_fe here because log_total_pub_author_year
# is defined at the author-year level and would be absorbed.
# We use author FE + year FE + journal FE instead.
m_did_control_scale <- feols(
  n_pub ~ treated_journal:post + log_total_pub_author_year |
    author_fe + year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# (4) Poisson FE model for counts
m_did_pois_scale <- fepois(
  n_pub ~ treated_journal:post + source_is_oa + log_impact_2yr +
    indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# ------------------------------------------------------------
# 12B.3 Event-study on publication shares
# ------------------------------------------------------------

m_event_study_share <- feols(
  share_pub ~ i(year, treated_journal, ref = REFERENCE_YEAR) +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did[!is.na(share_pub)]
)

# Extract coefficients for plotting
es_share_df <- broom::tidy(m_event_study_share, conf.int = TRUE) %>%
  filter(str_detect(term, "year::")) %>%
  mutate(
    year = str_extract(term, "(?<=year::)\\d{4}") %>% as.integer(),
    period = ifelse(year < TREATMENT_YEAR, "Pre", "Post")
  ) %>%
  filter(year <= 2025)

plot_es_share <- ggplot(
  es_share_df,
  aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = period)
) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = 2) +
  geom_pointrange() +
  scale_x_continuous(breaks = YEAR_MIN:YEAR_MAX) +
  labs(
    x = "Year",
    y = "Difference-in-differences estimate (share outcome)",
    color = NULL,
    title = "Event-study analysis using publication shares"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(out_dir, "figures", "event_study_share.png"),
  plot = plot_es_share,
  width = 9,
  height = 6,
  dpi = 300
)

# Joint pre-trend test for the share event-study
pretrend_terms_share <- grep(
  "year::2016:treated_journal|year::2017:treated_journal|year::2018:treated_journal",
  names(coef(m_event_study_share)),
  value = TRUE
)

pretrend_test_share <- if (length(pretrend_terms_share) > 0) {
  wald(m_event_study_share, pretrend_terms_share)
} else {
  NULL
}

# ------------------------------------------------------------
# 12B.4 Conference-friendly descriptive plots
# ------------------------------------------------------------

# A. GAP plot
gap_df <- panel_did %>%
  as_tibble() %>%
  group_by(year) %>%
  summarise(
    listed = mean(n_pub[treated_journal == 1], na.rm = TRUE),
    non_listed = mean(n_pub[treated_journal == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(gap = listed - non_listed) %>%
  filter(year <= 2025)

plot_gap <- ggplot(gap_df, aes(x = year, y = gap)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Difference (listed - non-listed)",
    title = "Gap in publication intensity between listed and non-listed journals"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "figures", "gap_listed_vs_nonlisted.png"),
  plot_gap, width = 8, height = 5, dpi = 300
)

# B. Index plot (2020 = 100)
index_df <- panel_did %>%
  as_tibble() %>%
  mutate(group = ifelse(treated_journal == 1, "Listed", "Non-listed")) %>%
  group_by(group, year) %>%
  summarise(mean_pub = mean(n_pub, na.rm = TRUE), .groups = "drop") %>%
  filter(year <= 2025) %>%
  group_by(group) %>%
  mutate(index = mean_pub / mean_pub[year == TREATMENT_YEAR] * 100) %>%
  ungroup()

plot_index <- ggplot(index_df, aes(x = year, y = index, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Index (2022 = 100)",
    color = NULL,
    title = "Relative evolution of publication intensity"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "figures", "index_listed_vs_nonlisted.png"),
  plot_index, width = 8, height = 5, dpi = 300
)

# C. Share plot
share_df <- panel_did %>%
  as_tibble() %>%
  mutate(group = ifelse(treated_journal == 1, "Listed", "Non-listed")) %>%
  group_by(year, group) %>%
  summarise(total = sum(n_pub, na.rm = TRUE), .groups = "drop") %>%
  filter(year <= 2024) %>%
  group_by(year) %>%
  mutate(share = total / sum(total)) %>%
  ungroup()

plot_share <- ggplot(share_df, aes(x = year, y = share, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Share of publications",
    color = NULL,
    title = "Reallocation of publications across journal types"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "figures", "share_listed_vs_nonlisted.png"),
  plot_share, width = 8, height = 5, dpi = 300
)

# ------------------------------------------------------------
# 12B.5 Clean robustness table
# ------------------------------------------------------------

modelsummary(
  list(
    "Baseline DiD + Journal FE" = m_did_base_jfe,
    "Share outcome" = m_did_share,
    "Scale-controlled DiD" = m_did_control_scale,
    "Poisson FE" = m_did_pois_scale
  ),
  coef_rename = c(
    "treated_journal:post" = "Listed journal × Post",
    "post:treated_journal" = "Listed journal × Post",
    "log_total_pub_author_year" = "Log total author-year publications"
  ),
  #coef_omit = "source_is_oa|log_impact_2yr|indexed_wos_scopus|main_domain",
  coef_omit = "main_domain",
  output = file.path(out_dir, "tables", "did_scale_robustness_clean.html"),
  stars = TRUE,
  statistic = "({std.error})"
)

# ------------------------------------------------------------
# 12B.6 Quick interpretation file
# ------------------------------------------------------------

interpret_coef <- function(model, term_candidates) {
  ct <- broom::tidy(model, conf.int = TRUE)
  
  for (tm in term_candidates) {
    row <- ct[ct$term == tm, ]
    if (nrow(row) > 0) {
      return(
        paste0(
          tm, " = ", round(row$estimate, 4),
          " [", round(row$conf.low, 4), "; ", round(row$conf.high, 4), "]",
          ", p = ", signif(row$p.value, 3)
        )
      )
    }
  }
  
  return(NA_character_)
}

robustness_lines <- c(
  paste(
    "Baseline DiD + Journal FE:",
    interpret_coef(m_did_base_jfe, c("treated_journal:post", "post:treated_journal"))
  ),
  paste(
    "Share outcome:",
    interpret_coef(m_did_share, c("treated_journal:post", "post:treated_journal"))
  ),
  paste(
    "Scale-controlled DiD:",
    interpret_coef(m_did_control_scale, c("treated_journal:post", "post:treated_journal"))
  ),
  paste(
    "Poisson FE:",
    interpret_coef(m_did_pois_scale, c("treated_journal:post", "post:treated_journal"))
  )
)

writeLines(
  robustness_lines,
  file.path(out_dir, "tables", "did_scale_robustness_interpretation.txt")
)

# ------------------------------------------------------------
# 12B.7 Console message
# ------------------------------------------------------------

message("Diversification / dilution robustness block completed.")
message("Saved outputs:")
message("- Table: ", file.path(out_dir, "tables", "did_scale_robustness_clean.html"))
message("- Interpretation: ", file.path(out_dir, "tables", "did_scale_robustness_interpretation.txt"))
message("- Figures: event_study_share.png, gap_listed_vs_nonlisted.png, index_listed_vs_nonlisted.png, share_listed_vs_nonlisted.png")


# ============================================================
# 13. Heterogeneity: publisher-group and publisher-specific effects
# ============================================================

# -------------------------------
# 13.1 Hétérogénéité agrégée
# -------------------------------
broom::tidy(m_did_hetero_group, conf.int = TRUE) %>%
  dplyr::select(term) %>%
  print(n = Inf)


panel_did[, publisher_group_simple := factor(
  publisher_group_simple,
  levels = c("Other intl.", "Grey", "Big 5", "Societies / Univ.", "Chinese")
)]

m_did_hetero_group <- feols(
  n_pub ~ i(publisher_group_simple, treated_journal * post, ref = "Other intl.") +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# Variante équivalente, plus explicite
m_did_hetero_group_3way <- feols(
  n_pub ~ treated_journal * post * publisher_group_simple +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# -------------------------------
# 13.2 Hétérogénéité fine : éditeurs spécifiques
# -------------------------------

panel_did[, publisher_top := fcase(
  str_detect(publisher_family, regex("^MDPI$", ignore_case = TRUE)), "MDPI",
  str_detect(publisher_family, regex("Frontiers", ignore_case = TRUE)), "Frontiers",
  str_detect(publisher_family, regex("Hindawi", ignore_case = TRUE)), "Hindawi",
  str_detect(publisher_family, regex("Elsevier", ignore_case = TRUE)), "Elsevier",
  str_detect(publisher_family, regex("Springer", ignore_case = TRUE)), "Springer Nature",
  str_detect(publisher_family, regex("Wiley", ignore_case = TRUE)), "Wiley",
  str_detect(publisher_family, regex("Taylor", ignore_case = TRUE)), "Taylor & Francis",
  str_detect(publisher_family, regex("SAGE", ignore_case = TRUE)), "SAGE",
  default = "Other"
)]

panel_did[, publisher_top := factor(
  publisher_top,
  levels = c(
    "Other",
    "MDPI",
    "Frontiers",
    "Hindawi",
    "Elsevier",
    "Springer Nature",
    "Wiley",
    "Taylor & Francis",
    "SAGE"
  )
)]

m_did_hetero_top <- feols(
  n_pub ~ i(publisher_top, treated_journal * post, ref = "Other") +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# Variante 3-way
m_did_hetero_top_3way <- feols(
  n_pub ~ treated_journal * post * publisher_top +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)
# ============================================================
# 14. Event-study
# ============================================================

# Référence = 2021.
# On met des FE source_id pour que le niveau structurel des revues ne pollue pas la dynamique.

m_event_study <- feols(
  n_pub ~ i(year, treated_journal, ref = REFERENCE_YEAR) +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# Extraire les coefficients pour le graphe
es_df <- broom::tidy(m_event_study, conf.int = TRUE) %>%
  filter(str_detect(term, "year::")) %>%
  mutate(
    year = str_extract(term, "(?<=year::)\\d{4}") %>% as.integer(),
    period = ifelse(year < TREATMENT_YEAR, "Pre", "Post")
  ) %>%
  filter(year < 2025)

plot_es <- ggplot(es_df, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = period)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = 2) +
  geom_pointrange() +
  scale_x_continuous(breaks = YEAR_MIN:YEAR_MAX) +
  labs(
    x = "Year",
    y = "Difference-in-differences estimate (vs. 2020)",
    color = NULL,
    title = "Event-study analysis of the CAS early-warning list"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(out_dir, "figures", "event_study_listed.png"),
  plot = plot_es,
  width = 9,
  height = 6,
  dpi = 300
)

# Test conjoint des pré-tendances
pretrend_terms <- grep("year::2016:treated_journal|year::2017:treated_journal|year::2018:treated_journal", names(coef(m_event_study)), value = TRUE)
pretrend_test <- if (length(pretrend_terms) > 0) {
  wald(m_event_study, pretrend_terms)
} else {
  NULL
}

# ============================================================
# 15. Spillover 1 : au niveau des revues non listées d'éditeurs exposés
# ============================================================

# Idée : l'effet de réputation peut contaminer le portefeuille de l'éditeur.
# On retire les revues directement listées, puis on compare les revues NON listées
# chez les éditeurs exposés à des revues NON listées chez les éditeurs non exposés.

panel_spill <- panel_did[treated_journal == 0]

m_spillover_portfolio <- feols(
  n_pub ~ spillover_journal * post +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_spill
)

# Hétérogénéité du spillover par groupe d'éditeur
m_spillover_hetero <- feols(
  n_pub ~ spillover_journal * post * publisher_group_simple +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_spill
)

# ============================================================
# 16. Spillover 2 : effet portefeuille total éditeur exposé
# ============================================================

# Ici on mesure si les éditeurs qui possèdent des titres listés perdent de l'attractivité,
# même au-delà des seules revues listées.

m_publisher_exposure <- feols(
  n_pub ~ exposed_publisher * post +
    treated_journal * post +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# ============================================================
# 17. Redistribution des soumissions : qui récupère les flux ?
# ============================================================

# Analyse simple des gagnants/perdants après 2020 par type d'éditeur.
# On se restreint aux revues non listées pour voir où vont les soumissions déplacées.

m_reallocation <- feols(
  n_pub ~ post * publisher_group_simple +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did[treated_journal == 0]
)

# ============================================================
# 18. Robustesses
# ============================================================

# 18.1 Outcome binaire : publication oui/non
# m_did_binary <- feols(
#   any_pub ~ treated_journal * post + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
#     author_year_fe + source_id,
#   cluster = ~ author_id,
#   data = panel_did
# )

# 18.2 Poisson pseudo-ML pour des comptes très asymétriques / beaucoup de zéros
m_did_pois <- fepois(
  n_pub ~ treated_journal * post + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did
)

# 18.3 Restreindre aux auteurs ayant au moins une publication chaque côté du choc
# déjà fait ; ici une variante plus stricte : auteurs avec activité avant ET après dans des revues traitées

strict_author_support <- panel_did[
  , .(
    treated_pre = max(year < TREATMENT_YEAR & treated_journal == 1 & n_pub > 0),
    treated_post = max(year >= TREATMENT_YEAR & treated_journal == 1 & n_pub > 0),
    control_pre = max(year < TREATMENT_YEAR & treated_journal == 0 & n_pub > 0),
    control_post = max(year >= TREATMENT_YEAR & treated_journal == 0 & n_pub > 0)
  ),
  by = author_id
]

strict_authors <- strict_author_support[
  treated_pre == 1 & control_pre == 1 & control_post == 1,
  author_id
]

panel_did_strict <- panel_did[author_id %in% strict_authors]

m_did_strict <- feols(
  n_pub ~ treated_journal * post + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_strict
)

# ============================================================
# 19. Tableaux de résultats
# ============================================================

# modelsummary(
#   list(
#     "Baseline DiD" = m_did_base,
#     "DiD + Journal FE" = m_did_base_jfe,
#     "Event study" = m_event_study,
#     "Portfolio spillover" = m_spillover_portfolio,
#     "Publisher exposure" = m_publisher_exposure
#   ),
#   output = file.path(out_dir, "tables", "models_main.html"),
#   stars = TRUE,
#   statistic = "({std.error})"
# )

# modelsummary(
#   list(
#     "Baseline DiD" = m_did_base,
#     "DiD + Journal FE" = m_did_base_jfe,
#     "Event study" = m_event_study,
#     "Portfolio spillover" = m_spillover_portfolio,
#     "Publisher exposure" = m_publisher_exposure
#   ),
#   coef_map = c(
#     "treated_journal:post" = "Listed journal × Post",
#     "spillover_journal:post" = "Spillover × Post",
#     "exposed_publisher:post" = "Exposed publisher × Post"
#   ),
#   output = file.path(out_dir, "tables", "models_main.html"),
#   stars = TRUE,
#   statistic = "({std.error})"
# )

modelsummary(
  list(
    "Baseline DiD" = m_did_base,
    "DiD + Journal FE" = m_did_base_jfe,
    "Event study" = m_event_study,
    "Portfolio spillover" = m_spillover_portfolio,
    "Publisher exposure" = m_publisher_exposure
  ),
  coef_rename = c(
    "treated_journal:post" = "Listed journal × Post",
    "post:treated_journal" = "Listed journal × Post",
    "spillover_journal:post" = "Spillover × Post",
    "exposed_publisher:post" = "Exposed publisher × Post"
  ),
  output = file.path(out_dir, "tables", "models_main.html"),
  stars = TRUE,
  statistic = "({std.error})"
)

modelsummary(
  list(
    #"Binary outcome" = m_did_binary,
    "Poisson model" = m_did_pois,
    "Strict sample" = m_did_strict,
    "Reallocation" = m_reallocation,
    "Heterogeneous spillover" = m_spillover_hetero
  ),
  output = file.path(out_dir, "tables", "models_robustness.html"),
  stars = TRUE,
  statistic = "({std.error})"
)

# ============================================================
# 20. Table de synthèse lisible des coefficients clés
# ============================================================

extract_key_terms <- function(model, model_name) {
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(model = model_name)
}

coef_table <- bind_rows(
  extract_key_terms(m_did_base, "did_base"),
  extract_key_terms(m_did_base_jfe, "did_base_jfe"),
  extract_key_terms(m_spillover_portfolio, "spillover_portfolio"),
  extract_key_terms(m_publisher_exposure, "publisher_exposure"),
  #extract_key_terms(m_did_binary, "did_binary"),
  extract_key_terms(m_did_pois, "did_pois")
)

fwrite(coef_table, file.path(out_dir, "tables", "key_coefficients.csv"))

# ============================================================
# 21. Heterogeneity plots
# ============================================================

# -------------------------------
# 21.1 By publisher group
# -------------------------------

hetero_group_df <- broom::tidy(m_did_hetero_group, conf.int = TRUE) %>%
  filter(str_detect(term, "publisher_group_simple::")) %>%
  mutate(
    group = term %>%
      str_remove("publisher_group_simple::") %>%
      str_remove(":treated_journal \\* post")
  )

if (nrow(hetero_group_df) > 0) {
  plot_hetero_group <- ggplot(
    hetero_group_df,
    aes(x = reorder(group, estimate), y = estimate, ymin = conf.low, ymax = conf.high)
  ) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange() +
    coord_flip() +
    labs(
      x = NULL,
      y = "Differential treatment effect",
      title = "Heterogeneous effects of the CAS early-warning list across publisher groups"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(
    filename = file.path(out_dir, "figures", "heterogeneity_by_publisher_group.png"),
    plot = plot_hetero_group,
    width = 9,
    height = 6,
    dpi = 300
  )
}

# -------------------------------
# 21.2 By specific publisher
# -------------------------------

hetero_top_df <- broom::tidy(m_did_hetero_top, conf.int = TRUE) %>%
  filter(str_detect(term, "publisher_top::")) %>%
  mutate(
    publisher = term %>%
      str_remove("publisher_top::") %>%
      str_remove(":treated_journal \\* post"),
    publisher = factor(
      publisher,
      levels = c(
        "MDPI",
        "Frontiers",
        "Hindawi",
        "Elsevier",
        "Springer Nature",
        "Wiley",
        "Taylor & Francis"
      )
    )
  ) %>%
  filter(!is.na(publisher))

if (nrow(hetero_top_df) > 0) {
  plot_hetero_top <- ggplot(
    hetero_top_df,
    aes(x = reorder(publisher, estimate), y = estimate, ymin = conf.low, ymax = conf.high)
  ) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange() +
    coord_flip() +
    labs(
      x = NULL,
      y = "Differential treatment effect",
      title = "Heterogeneous effects of the CAS early-warning list across publishers"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(
    filename = file.path(out_dir, "figures", "heterogeneity_by_publisher_top.png"),
    plot = plot_hetero_top,
    width = 9,
    height = 6,
    dpi = 300
  )
}

# ============================================================
# 22. Figure spillover : revues non listées chez éditeurs exposés
# ============================================================

spill_desc <- panel_spill[
  , .(avg_pub = mean(n_pub, na.rm = TRUE)),
  by = .(year, group = ifelse(spillover_journal == 1, "Non-listed in exposed publishers", "Non-listed in non-exposed publishers"))
] %>%
  filter(year <= 2025) %>%
  ggplot(aes(x = year, y = avg_pub, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = 2) +
  labs(
    x = "Year",
    y = "Average number of publications per author × journal",
    color = NULL,
    title = "Descriptive evidence of spillover effects at the publisher level"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(out_dir, "figures", "spillover_descriptive.png"),
  plot = spill_desc,
  width = 9,
  height = 6,
  dpi = 300
)

# ============================================================
# 23. Lecture interprétative automatique minimale
# ============================================================

interpret_coef <- function(model, term) {
  ct <- broom::tidy(model, conf.int = TRUE)
  row <- ct[ct$term == term, ]
  if (nrow(row) == 0) return(NA_character_)
  
  paste0(
    term, " = ", round(row$estimate, 4),
    " [", round(row$conf.low, 4), "; ", round(row$conf.high, 4), "]",
    ", p = ", signif(row$p.value, 3)
  )
}

summary_lines <- c(
  paste("Baseline difference-in-differences:", interpret_coef(m_did_base, "treated_journal:post")),
  paste("DiD with journal fixed effects:", interpret_coef(m_did_base_jfe, "treated_journal:post")),
  paste("Spillover (publisher portfolio):", interpret_coef(m_spillover_portfolio, "spillover_journal:post")),
  paste("Publisher-level exposure effect:", interpret_coef(m_publisher_exposure, "exposed_publisher:post"))
)

writeLines(summary_lines, file.path(out_dir, "tables", "interpretation_rapide.txt"))

# ============================================================
# 24. Conseils d'interprétation
# ============================================================

cat(
  "\n================ INTERPRETATION =================\n",
  "1) Si treated_journal:post < 0 et significatif :\n",
  "   les revues listées subissent bien une baisse relative après 2020.\n\n",
  "2) Si spillover_journal:post < 0 et significatif :\n",
  "   il y a un spillover négatif sur les revues NON listées appartenant\n",
  "   à des éditeurs touchés par la liste.\n\n",
  "3) Si exposed_publisher:post < 0 après contrôle de treated_journal:post :\n",
  "   l'effet dépasse les seules revues listées et touche le portefeuille éditeur.\n\n",
  "4) Dans l'event-study, les coefficients avant 2020 doivent être proches de 0\n",
  "   (ou au moins non systématiquement significatifs) pour soutenir l'hypothèse\n",
  "   de tendances parallèles.\n\n",
  "5) Si les interactions avec publisher_group_simple montrent des effets plus négatifs\n",
  "   pour les grey publishers, cela va dans le sens d'une vulnérabilité réputationnelle\n",
  "   plus forte de ces portefeuilles.\n",
  file = file.path(out_dir, "tables", "guide_interpretation.txt")
)

# ============================================================
# 25. Extensions possibles
# ============================================================
# - Remplacer la variable listed par une variable dépendant de l'année de listing
#   si vous reconstruisez la liste CAS annuelle par cohorte.
# - Faire un stacked DiD par cohorte de listing (beaucoup plus propre si vous voulez
#   tenir compte de l'évolution des revues listées au fil du temps).
# - Estimer des modèles séparés par domaine disciplinaire.
# - Ajouter une analyse au niveau auteur ou publication.
# - Ajouter une pondération par taille initiale des revues.
# ============================================================

saveRDS(
  list(
    panel_did = panel_did,
    models = list(
      m_did_base = m_did_base,
      m_did_base_jfe = m_did_base_jfe,
      #m_did_hetero = m_did_hetero,
      #m_did_hetero_3way = m_did_hetero_3way,
      m_event_study = m_event_study,
      m_spillover_portfolio = m_spillover_portfolio,
      m_spillover_hetero = m_spillover_hetero,
      m_publisher_exposure = m_publisher_exposure,
      m_reallocation = m_reallocation,
      #m_did_binary = m_did_binary,
      m_did_pois = m_did_pois,
      m_did_strict = m_did_strict,
      pretrend_test = pretrend_test
    )
  ),
  file.path(out_dir, "data", "did_analysis_objects.rds")
)

message("Script terminé. Résultats enregistrés dans : ", normalizePath(out_dir))


#####
# ============================================================
# 22. Publisher-level reallocation analysis
# Full R block
# ============================================================

# ------------------------------------------------------------
# 22.1 Safety checks
# ------------------------------------------------------------

library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(fixest)
library(broom)
library(modelsummary)
library(scales)

setDT(panel_did)

# Make sure the usual folders exist
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "data"), showWarnings = FALSE, recursive = TRUE)

# Keep only years up to 2024
panel_did <- panel_did[year <= 2024]

# ------------------------------------------------------------
# 22.2 Ensure author-year total publications exist
# ------------------------------------------------------------

if (!"total_pub_author_year" %in% names(panel_did)) {
  panel_did[, total_pub_author_year := sum(n_pub, na.rm = TRUE), by = .(author_id, year)]
}

# Safety
panel_did[is.na(total_pub_author_year), total_pub_author_year := 0L]

# ------------------------------------------------------------
# 22.3 Clean publisher-group variable
# ------------------------------------------------------------

panel_did[, publisher_group_simple := factor(
  publisher_group_simple,
  levels = c("Other intl.", "Grey", "Big 5", "Societies / Univ.", "Chinese")
)]

# Drop missing publisher groups if any
panel_did_group <- panel_did[!is.na(publisher_group_simple)]

# ------------------------------------------------------------
# 22.4 Build publisher-specific variable
# ------------------------------------------------------------

panel_did_group[, publisher_top := fcase(
  str_detect(publisher_family, regex("^MDPI$", ignore_case = TRUE)), "MDPI",
  str_detect(publisher_family, regex("Frontiers", ignore_case = TRUE)), "Frontiers",
  str_detect(publisher_family, regex("Hindawi", ignore_case = TRUE)), "Hindawi",
  str_detect(publisher_family, regex("Elsevier", ignore_case = TRUE)), "Elsevier",
  str_detect(publisher_family, regex("Springer", ignore_case = TRUE)), "Springer Nature",
  str_detect(publisher_family, regex("Wiley", ignore_case = TRUE)), "Wiley",
  str_detect(publisher_family, regex("Taylor", ignore_case = TRUE)), "Taylor & Francis",
  str_detect(publisher_family, regex("SAGE", ignore_case = TRUE)), "SAGE",
  default = "Other"
)]

panel_did_group[, publisher_top := factor(
  publisher_top,
  levels = c(
    "Other",
    "MDPI",
    "Frontiers",
    "Hindawi",
    "Elsevier",
    "Springer Nature",
    "Wiley",
    "Taylor & Francis",
    "SAGE"
  )
)]

# ------------------------------------------------------------
# 22.5 Outcome for reallocation: share within author-year
# ------------------------------------------------------------

panel_did_group[, share_pub := fifelse(
  total_pub_author_year > 0,
  n_pub / total_pub_author_year,
  NA_real_
)]

# Non-listed journals only: where do publications go after CAS?
panel_nonlisted_group <- panel_did_group[
  treated_journal == 0 & !is.na(share_pub)
]

# ------------------------------------------------------------
# 22.6 Reallocation across publisher groups
# ------------------------------------------------------------

# Main model: among non-listed journals, does allocation shift toward some publisher groups?
m_realloc_group_share <- feols(
  share_pub ~ post * publisher_group_simple |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_nonlisted_group
)

# Alternative count-based model
m_realloc_group_count <- feols(
  n_pub ~ post * publisher_group_simple |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_nonlisted_group
)

# ------------------------------------------------------------
# 22.7 Reallocation across specific publishers
# ------------------------------------------------------------

panel_nonlisted_top <- panel_nonlisted_group[publisher_top != "Other"]

m_realloc_top_share <- feols(
  share_pub ~ post * publisher_top |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_nonlisted_top
)

m_realloc_top_count <- feols(
  n_pub ~ post * publisher_top |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_nonlisted_top
)

# ------------------------------------------------------------
# 22.8 Exposure-based publisher-level analysis
# ------------------------------------------------------------

# Publisher exposure already exists in your main script:
# exposed_publisher = 1 if publisher has at least one listed journal

m_publisher_exposure_share <- feols(
  share_pub ~ exposed_publisher * post + treated_journal:post |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_group[!is.na(share_pub)]
)

m_publisher_exposure_count <- feols(
  n_pub ~ exposed_publisher * post + treated_journal:post |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_group
)

# ------------------------------------------------------------
# 22.9 Descriptive shares by publisher group over time
# ------------------------------------------------------------

share_group_year <- panel_nonlisted_group %>%
  as_tibble() %>%
  group_by(year, publisher_group_simple) %>%
  summarise(total_pub = sum(n_pub, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = total_pub / sum(total_pub)) %>%
  ungroup()

plot_share_group_year <- ggplot(
  share_group_year,
  aes(x = year, y = share, color = publisher_group_simple)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = "dashed") +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Year",
    y = "Share of non-listed publications",
    color = NULL,
    title = "Reallocation across publisher groups among non-listed journals"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "figures", "publisher_group_reallocation_share.png"),
  plot_share_group_year,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 22.10 Descriptive shares by specific publisher over time
# ------------------------------------------------------------

share_top_year <- panel_nonlisted_top %>%
  as_tibble() %>%
  group_by(year, publisher_top) %>%
  summarise(total_pub = sum(n_pub, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = total_pub / sum(total_pub)) %>%
  ungroup()

# plot_share_top_year <- ggplot(
#   share_top_year,
#   aes(x = year, y = share, color = publisher_top)
# ) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   geom_vline(xintercept = TREATMENT_YEAR, linetype = "dashed") +
#   scale_y_continuous(labels = percent) +
#   labs(
#     x = "Year",
#     y = "Share of non-listed publications",
#     color = NULL,
#     title = "Reallocation across publishers among non-listed journals"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(legend.position = "bottom")
publisher_colors <- c(
  "MDPI" = "#1b9e77",
  "Frontiers" = "#d95f02",
  "Hindawi" = "#7570b3",
  "Elsevier" = "#e7298a",
  "Springer Nature" = "#66a61e",
  "Wiley" = "#e6ab02",
  "Taylor & Francis" = "#a6761d",
  "SAGE" = "#666666"
)

plot_share_top_year <- ggplot(
  share_top_year,
  aes(x = year, y = share, color = publisher_top)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR, linetype = "dashed") +
  scale_color_manual(values = publisher_colors) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Year",
    y = "Share of non-listed publications",
    color = NULL,
    title = "Reallocation across publishers among non-listed journals"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")


ggsave(
  file.path(out_dir, "figures", "publisher_top_reallocation_share.png"),
  plot_share_top_year,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 22.11 Extract interaction coefficients for publisher groups
# ------------------------------------------------------------

tidy_group_share <- broom::tidy(m_realloc_group_share, conf.int = TRUE) %>%
  filter(str_detect(term, "post:publisher_group_simple|publisher_group_simple.*:post")) %>%
  mutate(
    publisher_group = case_when(
      str_detect(term, "Grey") ~ "Grey",
      str_detect(term, "Big 5") ~ "Big 5",
      str_detect(term, "Societies / Univ.") ~ "Societies / Univ.",
      str_detect(term, "Chinese") ~ "Chinese",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(publisher_group))

plot_group_effects <- ggplot(
  tidy_group_share,
  aes(x = reorder(publisher_group, estimate), y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Post-period differential effect",
    title = "Publisher-group reallocation effects"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  file.path(out_dir, "figures", "publisher_group_reallocation_effects.png"),
  plot_group_effects,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 22.12 Extract interaction coefficients for specific publishers
# ------------------------------------------------------------

tidy_top_share <- broom::tidy(m_realloc_top_share, conf.int = TRUE) %>%
  filter(str_detect(term, "post:publisher_top|publisher_top.*:post")) %>%
  mutate(
    publisher = case_when(
      str_detect(term, "MDPI") ~ "MDPI",
      str_detect(term, "Frontiers") ~ "Frontiers",
      str_detect(term, "Hindawi") ~ "Hindawi",
      str_detect(term, "Elsevier") ~ "Elsevier",
      str_detect(term, "Springer Nature") ~ "Springer Nature",
      str_detect(term, "Wiley") ~ "Wiley",
      str_detect(term, "Taylor & Francis") ~ "Taylor & Francis",
      str_detect(term, "SAGE") ~ "SAGE",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(publisher))

plot_top_effects <- ggplot(
  tidy_top_share,
  aes(x = reorder(publisher, estimate), y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Post-period differential effect",
    title = "Publisher-level reallocation effects"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  file.path(out_dir, "figures", "publisher_top_reallocation_effects.png"),
  plot_top_effects,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 22.13 Save regression tables
# ------------------------------------------------------------

modelsummary(
  list(
    "Group reallocation (share)" = m_realloc_group_share,
    "Group reallocation (count)" = m_realloc_group_count,
    "Publisher reallocation (share)" = m_realloc_top_share,
    "Publisher reallocation (count)" = m_realloc_top_count,
    "Publisher exposure (share)" = m_publisher_exposure_share,
    "Publisher exposure (count)" = m_publisher_exposure_count
  ),
  output = file.path(out_dir, "tables", "publisher_level_reallocation.html"),
  stars = TRUE,
  statistic = "({std.error})"
)

# ------------------------------------------------------------
# 22.14 Quick interpretation file
# ------------------------------------------------------------

interpret_coef <- function(model, pattern) {
  ct <- broom::tidy(model, conf.int = TRUE)
  row <- ct[str_detect(ct$term, pattern), ]
  if (nrow(row) == 0) return(NA_character_)
  paste0(
    row$term[1], " = ", round(row$estimate[1], 4),
    " [", round(row$conf.low[1], 4), "; ", round(row$conf.high[1], 4), "]",
    ", p = ", signif(row$p.value[1], 3)
  )
}

publisher_lines <- c(
  paste("Publisher exposure (share):", interpret_coef(m_publisher_exposure_share, "exposed_publisher:post|post:exposed_publisher")),
  paste("Publisher exposure (count):", interpret_coef(m_publisher_exposure_count, "exposed_publisher:post|post:exposed_publisher")),
  paste("Group reallocation to Grey:", interpret_coef(m_realloc_group_share, "Grey")),
  paste("Group reallocation to Big 5:", interpret_coef(m_realloc_group_share, "Big 5")),
  paste("Publisher reallocation to MDPI:", interpret_coef(m_realloc_top_share, "MDPI")),
  paste("Publisher reallocation to Frontiers:", interpret_coef(m_realloc_top_share, "Frontiers")),
  paste("Publisher reallocation to Elsevier:", interpret_coef(m_realloc_top_share, "Elsevier")),
  paste("Publisher reallocation to Springer Nature:", interpret_coef(m_realloc_top_share, "Springer Nature"))
)

writeLines(
  publisher_lines,
  file.path(out_dir, "tables", "publisher_level_reallocation_interpretation.txt")
)

# ------------------------------------------------------------
# 22.15 Save intermediate datasets
# ------------------------------------------------------------

fwrite(
  as.data.table(share_group_year),
  file.path(out_dir, "data", "publisher_group_reallocation_shares.csv")
)

fwrite(
  as.data.table(share_top_year),
  file.path(out_dir, "data", "publisher_top_reallocation_shares.csv")
)

# ------------------------------------------------------------
# 22.16 Console message
# ------------------------------------------------------------

message("Publisher-level reallocation analysis completed.")
message("Saved outputs:")
message("- Figures:")
message("  * publisher_group_reallocation_share.png")
message("  * publisher_top_reallocation_share.png")
message("  * publisher_group_reallocation_effects.png")
message("  * publisher_top_reallocation_effects.png")
message("- Tables:")
message("  * publisher_level_reallocation.html")
message("  * publisher_level_reallocation_interpretation.txt")
message("- Data:")
message("  * publisher_group_reallocation_shares.csv")
message("  * publisher_top_reallocation_shares.csv")


obs <- panel_did[
  treated_journal == 1 & post == 1,
  sum(n_pub)
]

panel_cf <- copy(panel_did)
panel_cf[treated_journal == 1 & post == 1, post := 0]

panel_cf[, pred_cf := predict(m_did_base_jfe, newdata = panel_cf)]

cf <- panel_cf[
  treated_journal == 1 & post == 1,
  sum(pred_cf)
]

effect <- obs - cf
