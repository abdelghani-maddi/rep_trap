# =========================================================
# OPENALEX NSFC PIPELINE — VERSION ROBUSTE AVEC REPRISE
# =========================================================
# - reprise automatique sur batchs déjà présents
# - sauvegarde par batch
# - assemblage final
# - logs
# - extraction des colonnes utiles
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(openalexR)
library(data.table)
library(coro)
library(tibble)
library(dplyr)

# ==============================
# 1) HELPERS
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

extract_needed_fields <- function(rec) {
  
  source_display_name <- NA_character_
  issn_l <- NA_character_
  
  if (!is.null(rec$primary_location)) {
    src <- rec$primary_location$source
    
    if (!is.null(src)) {
      source_display_name <- src$display_name %||% NA_character_
      
      issn_l <- src$issn_l %||% NA_character_
      
      if (is.na(issn_l) && !is.null(src$issn)) {
        if (length(src$issn) > 0) {
          issn_l <- src$issn[[1]]
        }
      }
    }
  }
  
  tibble::tibble(
    id = rec$id %||% NA_character_,
    title = rec$title %||% NA_character_,
    display_name = rec$display_name %||% NA_character_,
    doi = rec$doi %||% NA_character_,
    publication_year = rec$publication_year %||% NA_integer_,
    fwci = rec$fwci %||% NA_real_,
    cited_by_count = rec$cited_by_count %||% NA_integer_,
    counts_by_year = list(rec$counts_by_year %||% NULL),
    ids = list(rec$ids %||% NULL),
    type = rec$type %||% NA_character_,
    open_access = list(rec$open_access %||% NULL),
    primary_location = list(rec$primary_location %||% NULL),
    source_display_name = source_display_name,
    issn_l = issn_l,
    topics = list(rec$topics %||% NULL),
    is_retracted = rec$is_retracted %||% NA,
    language = rec$language %||% NA_character_,
    grants = list(rec$grants %||% NULL),
    authorships = list(rec$authorships %||% NULL)
  )
}

# ==============================
# 2) FONCTION DE TÉLÉCHARGEMENT
#    AVEC REPRISE SUR BATCHS EXISTANTS
# ==============================

fetch_nsfc_year_batched <- function(
    year,
    out_dir,
    batch_size = 1000
) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  year_dir <- file.path(out_dir, paste0("year_", year))
  dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)
  
  done_flag <- file.path(year_dir, "_DONE")
  
  if (file.exists(done_flag)) {
    message("✔ Année ", year, " déjà terminée")
    return(invisible(list(
      year = year,
      n_existing = NA_integer_,
      n_saved_this_run = 0L,
      n_total = NA_integer_,
      status = "already_done"
    )))
  }
  
  # --------------------------------
  # Détection des batchs déjà présents
  # --------------------------------
  existing_batches <- list.files(
    year_dir,
    pattern = "^batch_\\d{5}\\.rds$",
    full.names = TRUE
  )
  existing_batches <- sort(existing_batches)
  
  n_existing <- 0L
  next_batch_id <- 1L
  
  if (length(existing_batches) > 0) {
    message("Reprise détectée pour l'année ", year)
    message("Batchs existants : ", length(existing_batches))
    
    for (f in existing_batches) {
      tmp <- readRDS(f)
      n_existing <- n_existing + nrow(tmp)
      rm(tmp)
      gc()
    }
    
    last_batch_num <- sub(
      "^batch_(\\d{5})\\.rds$",
      "\\1",
      basename(tail(existing_batches, 1))
    )
    next_batch_id <- as.integer(last_batch_num) + 1L
    
    message("Notices déjà sauvegardées : ", n_existing)
    message("Prochain batch : ", next_batch_id)
  }
  
  # --------------------------------
  # Requête OpenAlex
  # --------------------------------
  query_url <- oa_query(
    entity = "works",
    funders.id = "f4320321001",
    publication_year = year
  )
  
  message("\n=============================")
  message("Année : ", year)
  message("Query : ", query_url)
  
  gen <- oa_generate(query_url, verbose = TRUE)
  
  batch_list <- vector("list", batch_size)
  n_total_seen <- 0L
  n_total_saved_this_run <- 0L
  batch_id <- next_batch_id
  
  t0 <- Sys.time()
  
  coro::loop(for (rec in gen) {
    
    n_total_seen <- n_total_seen + 1L
    
    # Skip des notices déjà présentes
    if (n_total_seen <= n_existing) {
      if (n_total_seen %% 10000 == 0) {
        message("Skip reprise : ", n_total_seen, " notices déjà présentes")
      }
      next
    }
    
    j <- (n_total_saved_this_run %% batch_size) + 1L
    batch_list[[j]] <- extract_needed_fields(rec)
    n_total_saved_this_run <- n_total_saved_this_run + 1L
    
    if (j == batch_size) {
      batch_dt <- data.table::rbindlist(batch_list, fill = TRUE)
      
      file_batch <- file.path(
        year_dir,
        sprintf("batch_%05d.rds", batch_id)
      )
      
      saveRDS(batch_dt, file_batch, compress = "xz")
      
      message(
        "✔ Batch ", batch_id,
        " | n=", nrow(batch_dt),
        " | total seen=", n_total_seen,
        " | new saved this run=", n_total_saved_this_run
      )
      
      batch_list <- vector("list", batch_size)
      batch_id <- batch_id + 1L
      gc()
    }
  })
  
  # Dernier batch incomplet
  remainder <- n_total_saved_this_run %% batch_size
  
  if (remainder > 0L) {
    batch_dt <- data.table::rbindlist(
      batch_list[1:remainder],
      fill = TRUE
    )
    
    file_batch <- file.path(
      year_dir,
      sprintf("batch_%05d.rds", batch_id)
    )
    
    saveRDS(batch_dt, file_batch, compress = "xz")
    
    message(
      "✔ Dernier batch | n=", nrow(batch_dt),
      " | total seen=", n_total_seen,
      " | new saved this run=", n_total_saved_this_run
    )
  }
  
  total_final <- n_existing + n_total_saved_this_run
  
  elapsed <- round(
    as.numeric(difftime(Sys.time(), t0, units = "secs")),
    2
  )
  
  writeLines(
    c(
      paste("year:", year),
      paste("n_existing:", n_existing),
      paste("n_saved_this_run:", n_total_saved_this_run),
      paste("n_total:", total_final),
      paste("elapsed_sec:", elapsed)
    ),
    con = done_flag
  )
  
  message(
    "🎉 FIN année ", year,
    " | total=", total_final,
    " | temps=", elapsed, " sec"
  )
  
  invisible(list(
    year = year,
    n_existing = n_existing,
    n_saved_this_run = n_total_saved_this_run,
    n_total = total_final,
    status = "success"
  ))
}

# ==============================
# 3) ASSEMBLAGE FINAL PAR ANNÉE
# ==============================

assemble_nsfc_year <- function(year, out_dir) {
  
  year_dir <- file.path(out_dir, paste0("year_", year))
  
  files <- list.files(
    year_dir,
    pattern = "^batch_\\d+\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("❌ Aucun batch pour ", year)
  }
  
  files <- sort(files)
  
  message("Assemblage ", year, " | nb fichiers : ", length(files))
  
  dt_list <- lapply(files, readRDS)
  final_dt <- data.table::rbindlist(dt_list, fill = TRUE)
  
  file_final <- file.path(
    out_dir,
    paste0("works_nsfc_", year, "_final.rds")
  )
  
  saveRDS(final_dt, file_final, compress = "xz")
  
  message("✔ Fichier final : ", file_final, " | n=", nrow(final_dt))
  
  invisible(final_dt)
}

# ==============================
# 4) PIPELINE GLOBAL
# ==============================

run_nsfc_pipeline <- function(
    years,
    out_dir = "data_nsfc",
    batch_size = 1000,
    assemble = TRUE
) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  log_list <- list()
  
  for (year in years) {
    
    t0 <- Sys.time()
    
    res <- tryCatch({
      
      fetch_res <- fetch_nsfc_year_batched(
        year = year,
        out_dir = out_dir,
        batch_size = batch_size
      )
      
      n_obs <- NA_integer_
      final_file <- NA_character_
      
      if (assemble && !identical(fetch_res$status, "already_done")) {
        final_dt <- assemble_nsfc_year(
          year = year,
          out_dir = out_dir
        )
        n_obs <- nrow(final_dt)
        final_file <- file.path(out_dir, paste0("works_nsfc_", year, "_final.rds"))
        rm(final_dt)
        gc()
      } else if (assemble && identical(fetch_res$status, "already_done")) {
        final_file <- file.path(out_dir, paste0("works_nsfc_", year, "_final.rds"))
        if (file.exists(final_file)) {
          tmp <- readRDS(final_file)
          n_obs <- nrow(tmp)
          rm(tmp)
          gc()
        }
      }
      
      elapsed <- as.numeric(
        difftime(Sys.time(), t0, units = "secs")
      )
      
      data.frame(
        year = year,
        status = fetch_res$status,
        n_existing = fetch_res$n_existing,
        n_saved_this_run = fetch_res$n_saved_this_run,
        n_total = fetch_res$n_total,
        n_obs = n_obs,
        time_sec = elapsed,
        file = final_file,
        stringsAsFactors = FALSE
      )
      
    }, error = function(e) {
      
      elapsed <- as.numeric(
        difftime(Sys.time(), t0, units = "secs")
      )
      
      message("❌ Erreur année ", year)
      message(conditionMessage(e))
      
      data.frame(
        year = year,
        status = paste0("error: ", conditionMessage(e)),
        n_existing = NA_integer_,
        n_saved_this_run = NA_integer_,
        n_total = NA_integer_,
        n_obs = NA_integer_,
        time_sec = elapsed,
        file = NA_character_,
        stringsAsFactors = FALSE
      )
    })
    
    log_list[[as.character(year)]] <- res
    
    gc()
    Sys.sleep(1)
  }
  
  dplyr::bind_rows(log_list)
}

# ==============================
# 5) OUTIL DE CONTRÔLE D’UNE ANNÉE
# ==============================

check_year_progress <- function(year, out_dir = "data_nsfc") {
  
  year_dir <- file.path(out_dir, paste0("year_", year))
  
  if (!dir.exists(year_dir)) {
    stop("Dossier introuvable : ", year_dir)
  }
  
  existing_batches <- list.files(
    year_dir,
    pattern = "^batch_\\d{5}\\.rds$",
    full.names = TRUE
  )
  existing_batches <- sort(existing_batches)
  
  if (length(existing_batches) == 0) {
    cat("Aucun batch trouvé pour", year, "\n")
    return(invisible(NULL))
  }
  
  sizes <- sapply(existing_batches, function(f) {
    x <- readRDS(f)
    n <- nrow(x)
    rm(x)
    gc()
    n
  })
  
  out <- data.frame(
    batch = basename(existing_batches),
    n_rows = sizes,
    stringsAsFactors = FALSE
  )
  
  print(out)
  cat("\nTotal déjà sauvegardé :", sum(out$n_rows), "\n")
  invisible(out)
}

# ==============================
# 6) EXEMPLES D’UTILISATION
# ==============================

# ------------------------------
# A) Vérifier la progression de 2025
# ------------------------------
check_year_progress(2021, out_dir = "data_nsfc")

# ------------------------------
# B) Relancer uniquement 2021
# ------------------------------
log_2021 <- run_nsfc_pipeline(
  years = 2021,
  out_dir = "data_nsfc",
  batch_size = 1000,
  assemble = TRUE
)
print(log_2021)

final_2021 <- assemble_nsfc_year(
  year = 2021,
  out_dir = "data_nsfc"
)

# ------------------------------
# B) Relancer uniquement 2025
# ------------------------------
log_2025 <- run_nsfc_pipeline(
  years = 2025,
  out_dir = "data_nsfc",
  batch_size = 1000,
  assemble = TRUE
)
print(log_2025)

# ------------------------------
# C) Relancer plusieurs années
# ------------------------------
# years_to_fetch <- 1997:2025
# log <- run_nsfc_pipeline(
#   years = years_to_fetch,
#   out_dir = "data_nsfc",
#   batch_size = 1000,
#   assemble = TRUE
# )
# print(log)