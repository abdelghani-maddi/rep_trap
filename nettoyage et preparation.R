# =========================================================
# NSFC — VERSION SLIM À PARTIR DES FICHIERS ANNUELS
# Objectif :
# - lire chaque fichier annuel un par un
# - extraire seulement les infos utiles depuis les colonnes imbriquées
# - sauvegarder une version légère par année
# - assembler ensuite un fichier final léger
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

slim_dir <- file.path(data_dir, "slim_years")
dir.create(slim_dir, showWarnings = FALSE, recursive = TRUE)

slim_chunk_dir <- file.path(data_dir, "slim_chunks")
dir.create(slim_chunk_dir, showWarnings = FALSE, recursive = TRUE)

final_slim_file <- file.path(data_dir, "works_nsfc_1997_2025_slim_final.rds")
log_file <- file.path(data_dir, "nsfc_slim_log.csv")

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

slim_year_file <- function(year, slim_dir) {
  file.path(slim_dir, paste0("works_nsfc_", year, "_slim.rds"))
}

# --------------------------------
# open_access
# --------------------------------
normalize_open_access <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  
  if (is.list(x) && length(x) == 1 && is.list(x[[1]]) && is.null(names(x))) {
    x <- x[[1]]
  }
  
  if (is.list(x) && length(x) == 1 && is.list(x[[1]]) && !is.null(names(x[[1]]))) {
    x <- x[[1]]
  }
  
  x
}

extract_oa_status <- function(x) {
  x <- normalize_open_access(x)
  if (is.null(x)) return(NA_character_)
  
  if (!is.null(x$oa_status)) return(as.character(x$oa_status))
  if (!is.null(names(x)) && "oa_status" %in% names(x)) return(as.character(x[["oa_status"]]))
  
  NA_character_
}

extract_is_oa <- function(x) {
  x <- normalize_open_access(x)
  if (is.null(x)) return(NA)
  
  if (!is.null(x$is_oa)) return(as.logical(x$is_oa))
  if (!is.null(names(x)) && "is_oa" %in% names(x)) return(as.logical(x[["is_oa"]]))
  
  NA
}

# --------------------------------
# authorships
# --------------------------------
normalize_authorships <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  
  if (is.list(x) && length(x) == 1 && is.list(x[[1]]) && is.null(names(x))) {
    if (length(x[[1]]) > 0 && is.list(x[[1]][[1]])) {
      x <- x[[1]]
    }
  }
  
  x
}

extract_n_authors <- function(x) {
  x <- normalize_authorships(x)
  if (is.null(x) || length(x) == 0) return(0L)
  as.integer(length(x))
}

extract_country_vector <- function(x) {
  x <- normalize_authorships(x)
  
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  out <- character(0)
  
  for (au in x) {
    if (is.null(au)) next
    
    if (!is.null(au$countries) && length(au$countries) > 0) {
      tmp <- unlist(au$countries, use.names = FALSE)
      tmp <- tmp[!is.na(tmp) & nzchar(tmp)]
      out <- c(out, tmp)
    }
    
    if (!is.null(au$institutions) && length(au$institutions) > 0) {
      inst_cc <- vapply(
        au$institutions,
        function(inst) {
          cc <- inst$country_code %||% NA_character_
          if (length(cc) == 0) NA_character_ else as.character(cc)
        },
        FUN.VALUE = character(1)
      )
      inst_cc <- inst_cc[!is.na(inst_cc) & nzchar(inst_cc)]
      out <- c(out, inst_cc)
    }
  }
  
  out <- unique(out)
  out <- out[!is.na(out) & nzchar(out)]
  out <- sort(out)
  out
}

vec_to_string <- function(x, sep = ", ") {
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = sep)
}

# --------------------------------
# topics -> domain
# --------------------------------
normalize_topics <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  
  # Cas enveloppé dans une couche supplémentaire
  if (is.list(x) && length(x) == 1 && is.list(x[[1]]) && is.null(names(x))) {
    if (length(x[[1]]) > 0 && is.list(x[[1]][[1]])) {
      x <- x[[1]]
    }
  }
  
  x
}

extract_topic_scores <- function(x) {
  x <- normalize_topics(x)
  if (is.null(x) || length(x) == 0) return(numeric(0))
  
  vapply(
    x,
    function(tp) {
      sc <- tp$score %||% NA_real_
      as.numeric(sc)
    },
    FUN.VALUE = numeric(1)
  )
}

extract_domain_vector <- function(x) {
  x <- normalize_topics(x)
  
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  out <- character(0)
  
  for (tp in x) {
    if (is.null(tp)) next
    
    dn <- tp$domain$display_name %||% NA_character_
    dn <- as.character(dn)
    
    if (length(dn) > 0 && !is.na(dn) && nzchar(dn)) {
      out <- c(out, dn)
    }
  }
  
  out <- unique(out)
  out <- out[!is.na(out) & nzchar(out)]
  out
}

extract_main_domain <- function(x) {
  x <- normalize_topics(x)
  
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  scores <- extract_topic_scores(x)
  
  # si scores exploitables, prendre le domaine du topic au score max
  if (length(scores) > 0 && any(!is.na(scores))) {
    idx <- which.max(scores)
    dn <- x[[idx]]$domain$display_name %||% NA_character_
    dn <- as.character(dn)
    if (length(dn) > 0 && !is.na(dn) && nzchar(dn)) {
      return(dn)
    }
  }
  
  # fallback : premier domaine non vide
  doms <- extract_domain_vector(x)
  if (length(doms) == 0) return(NA_character_)
  doms[1]
}

extract_n_domains <- function(x) {
  length(extract_domain_vector(x))
}

# ==============================
# 3) FONCTION DE TRAITEMENT D'UNE ANNÉE
# ==============================

process_one_year_slim <- function(year, data_dir, slim_dir) {
  
  in_file <- year_file(year, data_dir)
  out_file <- slim_year_file(year, slim_dir)
  
  if (!file.exists(in_file)) {
    stop("Fichier introuvable : ", in_file)
  }
  
  if (file.exists(out_file)) {
    message("✔ Fichier slim déjà présent pour ", year, " : ", out_file)
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
  
  message("  - n lignes : ", nrow(dt))
  message("  - n colonnes : ", ncol(dt))
  
  scalar_cols <- c(
    "id",
    "title",
    "display_name",
    "doi",
    "publication_year",
    "fwci",
    "cited_by_count",
    "type",
    "source_display_name",
    "issn_l",
    "is_retracted",
    "language"
  )
  
  keep_scalar <- intersect(scalar_cols, names(dt))
  slim_dt <- copy(dt[, ..keep_scalar])
  
  if (!"year" %in% names(slim_dt)) {
    slim_dt[, year := year]
  }
  
  # -----------------------------
  # OA
  # -----------------------------
  if ("open_access" %in% names(dt)) {
    slim_dt[, oa_status := vapply(dt$open_access, extract_oa_status, FUN.VALUE = character(1))]
    slim_dt[, is_oa := vapply(dt$open_access, extract_is_oa, FUN.VALUE = logical(1))]
  } else {
    slim_dt[, oa_status := NA_character_]
    slim_dt[, is_oa := NA]
  }
  
  # -----------------------------
  # Authorships
  # -----------------------------
  if ("authorships" %in% names(dt)) {
    
    message("  - Extraction n_authors...")
    slim_dt[, n_authors := vapply(dt$authorships, extract_n_authors, FUN.VALUE = integer(1))]
    
    message("  - Extraction country_list...")
    country_list <- lapply(dt$authorships, extract_country_vector)
    
    slim_dt[, country_list := country_list]
    slim_dt[, n_countries := lengths(country_list)]
    slim_dt[, country_string := vapply(country_list, vec_to_string, FUN.VALUE = character(1))]
    
  } else {
    slim_dt[, n_authors := NA_integer_]
    slim_dt[, country_list := vector("list", .N)]
    slim_dt[, n_countries := NA_integer_]
    slim_dt[, country_string := NA_character_]
  }
  
  # -----------------------------
  # Topics -> domains
  # -----------------------------
  if ("topics" %in% names(dt)) {
    
    message("  - Extraction main_domain / domain_list...")
    domain_list <- lapply(dt$topics, extract_domain_vector)
    
    slim_dt[, main_domain := vapply(dt$topics, extract_main_domain, FUN.VALUE = character(1))]
    slim_dt[, domain_list := domain_list]
    slim_dt[, n_domains := lengths(domain_list)]
    slim_dt[, domain_string := vapply(domain_list, vec_to_string, FUN.VALUE = character(1))]
    
  } else {
    slim_dt[, main_domain := NA_character_]
    slim_dt[, domain_list := vector("list", .N)]
    slim_dt[, n_domains := NA_integer_]
    slim_dt[, domain_string := NA_character_]
  }
  
  saveRDS(slim_dt, out_file, compress = "xz")
  
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  
  message("  - OK")
  message("  - Fichier slim : ", out_file)
  message("  - Temps : ", elapsed, " sec")
  
  n_obs <- nrow(slim_dt)
  
  rm(dt, slim_dt, country_list, domain_list)
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
# 4) PIPELINE DE CRÉATION DES FICHIERS SLIM
# ==============================

run_slim_pipeline <- function(years, data_dir, slim_dir, log_file = NULL) {
  
  log_list <- vector("list", length(years))
  
  for (i in seq_along(years)) {
    yy <- years[i]
    
    res <- tryCatch(
      {
        process_one_year_slim(
          year = yy,
          data_dir = data_dir,
          slim_dir = slim_dir
        )
      },
      error = function(e) {
        message("❌ Erreur année ", yy, " : ", conditionMessage(e))
        data.frame(
          year = yy,
          status = paste0("error: ", conditionMessage(e)),
          n_obs = NA_integer_,
          file_in = year_file(yy, data_dir),
          file_out = slim_year_file(yy, slim_dir),
          stringsAsFactors = FALSE
        )
      }
    )
    
    log_list[[i]] <- res
    gc()
  }
  
  log_dt <- rbindlist(log_list, fill = TRUE)
  
  if (!is.null(log_file)) {
    fwrite(log_dt, log_file)
  }
  
  log_dt
}

# ==============================
# 5) ASSEMBLAGE FINAL DES FICHIERS SLIM
# ==============================

make_slim_chunks <- function(years, slim_dir, slim_chunk_dir, years_per_chunk = 3) {
  
  dir.create(slim_chunk_dir, showWarnings = FALSE, recursive = TRUE)
  
  split_years <- split(years, ceiling(seq_along(years) / years_per_chunk))
  log_list <- vector("list", length(split_years))
  
  for (i in seq_along(split_years)) {
    
    ys <- split_years[[i]]
    
    chunk_file <- file.path(
      slim_chunk_dir,
      sprintf("slim_chunk_%03d_%s_%s.rds", i, min(ys), max(ys))
    )
    
    if (file.exists(chunk_file)) {
      message("✔ Chunk déjà présent : ", chunk_file)
      tmp <- readRDS(chunk_file)
      n0 <- nrow(tmp)
      rm(tmp); gc()
      
      log_list[[i]] <- data.frame(
        chunk = i,
        years = paste(ys, collapse = "-"),
        status = "skipped_exists",
        n_obs = n0,
        file = chunk_file,
        stringsAsFactors = FALSE
      )
      next
    }
    
    message("\n------------------------------------")
    message("Chunk ", i, " | années : ", paste(ys, collapse = ", "))
    
    dt_list <- vector("list", length(ys))
    
    for (j in seq_along(ys)) {
      yy <- ys[j]
      f <- slim_year_file(yy, slim_dir)
      
      if (!file.exists(f)) {
        stop("Fichier slim introuvable : ", f)
      }
      
      message("  Lecture slim : ", basename(f))
      dt_list[[j]] <- as.data.table(readRDS(f))
    }
    
    chunk_dt <- rbindlist(dt_list, fill = TRUE, use.names = TRUE)
    saveRDS(chunk_dt, chunk_file, compress = "xz")
    
    n_obs <- nrow(chunk_dt)
    
    rm(dt_list, chunk_dt)
    gc()
    
    log_list[[i]] <- data.frame(
      chunk = i,
      years = paste(ys, collapse = "-"),
      status = "success",
      n_obs = n_obs,
      file = chunk_file,
      stringsAsFactors = FALSE
    )
  }
  
  rbindlist(log_list, fill = TRUE)
}

assemble_slim_final <- function(slim_chunk_dir, final_slim_file) {
  
  chunk_files <- list.files(
    slim_chunk_dir,
    pattern = "^slim_chunk_.*\\.rds$",
    full.names = TRUE
  )
  
  chunk_files <- sort(chunk_files)
  
  if (length(chunk_files) == 0) {
    stop("Aucun chunk slim trouvé dans : ", slim_chunk_dir)
  }
  
  message("\n====================================")
  message("Assemblage final slim")
  message("Nombre de chunks : ", length(chunk_files))
  
  dt_list <- lapply(chunk_files, readRDS)
  final_dt <- rbindlist(dt_list, fill = TRUE, use.names = TRUE)
  
  saveRDS(final_dt, final_slim_file, compress = "xz")
  
  message("✔ Fichier final slim : ", final_slim_file)
  message("✔ n lignes : ", nrow(final_dt))
  message("✔ n colonnes : ", ncol(final_dt))
  
  invisible(final_dt)
}

# ==============================
# 6) CONTRÔLES RAPIDES
# ==============================

check_slim_year <- function(year, slim_dir) {
  f <- slim_year_file(year, slim_dir)
  if (!file.exists(f)) stop("Fichier introuvable : ", f)
  
  dt <- readRDS(f)
  print(glimpse(dt))
  print(head(dt, 3))
  invisible(dt)
}


# ==============================
# 7) LANCEMENT
# ==============================

log_slim <- run_slim_pipeline(
  years = years,
  data_dir = data_dir,
  slim_dir = slim_dir,
  log_file = log_file
)

log_slim <- run_slim_pipeline(
  years = 2021,
  data_dir = "data_nsfc",
  slim_dir = slim_dir,
  log_file = log_file
)

print(log_slim)


log_chunks <- make_slim_chunks(
  years = 1997:2025,
  slim_dir = slim_dir,
  slim_chunk_dir = slim_chunk_dir,
  years_per_chunk = years_per_chunk
)

print(log_chunks)


log_chunks <- make_slim_chunks(
  years = years,
  slim_dir = slim_dir,
  slim_chunk_dir = slim_chunk_dir,
  years_per_chunk = years_per_chunk
)

### TEMP ###
log_chunks <- make_slim_chunks(
  years = 1997:2025,
  slim_dir = slim_dir,
  slim_chunk_dir = slim_chunk_dir,
  years_per_chunk = years_per_chunk
)

print(log_chunks)

# réassembler le final slim
final_slim <- assemble_slim_final(
  slim_chunk_dir = slim_chunk_dir,
  final_slim_file = final_slim_file
)

print(dim(final_slim))
print(names(final_slim))
print(table(final_slim$year, useNA = "ifany"))

# ==============================
# 8) EXEMPLES D’USAGE
# ==============================

# Vérifier une année
# check_slim_year(1997, slim_dir)

# OA
# table(final_slim$oa_status, useNA = "ifany")

# Domaines principaux
# sort(table(final_slim$main_domain), decreasing = TRUE)[1:20]

# Nombre moyen d'auteurs par année
# final_slim[, .(
#   n = .N,
#   mean_authors = mean(n_authors, na.rm = TRUE),
#   median_authors = median(n_authors, na.rm = TRUE)
# ), by = year][order(year)]

# Top pays
# all_countries <- unlist(final_slim$country_list, use.names = FALSE)
# sort(table(all_countries), decreasing = TRUE)[1:20]

# Top domaines
# all_domains <- unlist(final_slim$domain_list, use.names = FALSE)
# sort(table(all_domains), decreasing = TRUE)[1:20]