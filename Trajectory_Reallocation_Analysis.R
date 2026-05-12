# ============================================================
# NSFC / CAS Early Warning Lists
# Trajectory interruption and market reallocation analysis
# ============================================================
# Purpose
# -------
# This script is designed as a cleaner alternative / complement to a strict
# Difference-in-Differences framing.
#
# The empirical question is no longer phrased as a clean causal ATT:
#   "Did the CAS list cause a publication decline?"
#
# Instead, the script examines whether CAS-listed journals and related
# publishing ecosystems display:
#   1) pre-warning hypergrowth,
#   2) post-warning trajectory interruption / deceleration,
#   3) market reallocation across publisher groups,
#   4) publisher-level heterogeneity and portfolio spillovers.
#
# Why this design?
# ----------------
# Reviewers correctly noted that the CAS 2020 list was released in December
# 2020 and that many 2021 publications were likely submitted or accepted before
# the warning could affect authors' choices. This script therefore treats 2021
# as a transition/reference year and uses 2022 as the main effective post period
# for publication-year outcomes.
#
# The main models are NOT presented as clean causal DiD estimates. They are
# trajectory and allocation models designed to document dynamic breaks and
# market reallocation patterns in a transparent and reproducible way.
#
# Expected input files
# --------------------
# 1) data_nsfc/nsfc_early_warning_full_analysis.rds
# 2) data_nsfc/nsfc_final_clean_with_authors.rds
#
# Optional input files
# --------------------
# 3) D:/wos_issn.rds
# 4) D:/scopus_issn.rds
#
# Outputs
# -------
# outputs_trajectory_reallocation/
#   tables/
#   figures/
#   data/
#
# ============================================================

# ============================================================
# 0. Packages
# ============================================================

pkgs <- c(
  "data.table", "tidyverse", "fixest", "broom", "modelsummary",
  "scales", "stringr", "forcats", "patchwork", "janitor"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Packages manquants : ", paste(missing_pkgs, collapse = ", "))
}

library(data.table)
library(tidyverse)
library(fixest)
library(broom)
library(modelsummary)
library(scales)
library(stringr)
library(forcats)
library(patchwork)
library(janitor)

setFixest_nthreads(max(1, parallel::detectCores() - 1))
options(scipen = 999)

# ============================================================
# 1. Parameters
# ============================================================

YEAR_MIN <- 2016
YEAR_MAX <- 2025

# CAS first list was released in December 2020.
# Publication-year response is expected mainly from 2022.
MAIN_COHORT_YEAR <- 2020
TRANSITION_YEAR <- 2021
POST_START_YEAR <- 2022

YEAR_VAR_MAIN <- "publication_year"
YEAR_VAR_AUTH <- "publication_year.x"  # change to "publication_year" if needed

out_dir <- "outputs_trajectory_reallocation"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "data"), recursive = TRUE, showWarnings = FALSE)

file_nsfc_main <- "data_nsfc/nsfc_early_warning_full_analysis.rds"
file_nsfc_auth <- "data_nsfc/nsfc_final_clean_with_authors.rds"
file_wos <- "D:/wos_issn.rds"
file_scopus <- "D:/scopus_issn.rds"

# Set to FALSE if you only want journal-year and publisher-market analyses.
# Author-journal-year panels are heavier but useful for allocation models.
BUILD_AUTHOR_PANEL <- TRUE

# ============================================================
# 2. Helper functions
# ============================================================

first_non_na_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

first_non_na_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

first_non_na_lgl <- function(x) {
  x <- as.logical(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_min_year <- function(x) {
  x <- suppressWarnings(as.integer(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  min(x)
}

normalize_issn <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^A-Za-z0-9]", "")
  toupper(x)
}

std_publisher_group <- function(publisher_group) {
  fcase(
    publisher_group %in% c("Grey publishers", "Grey"), "Commercial OA challengers",
    publisher_group %in% c("Big 5 publishers", "Big 5", "Top 5 publishers"), "Big 5",
    publisher_group %in% c("Major society / university / non-profit publishers", "Societies / Univ."), "Societies / Univ.",
    publisher_group %in% c("Chinese publishers", "Chinese", "Domestic/regional publishers", "Other (Domestic/Regional)"), "Chinese / domestic",
    publisher_group %in% c("Other international commercial publishers", "Other intl.", "Other (International)"), "Other international",
    default = "Unknown"
  )
}

classify_publisher_top <- function(publisher_family, host_organization_name = NA_character_, publisher_name = NA_character_) {
  x <- coalesce(as.character(publisher_family), as.character(host_organization_name), as.character(publisher_name), "")
  fcase(
    str_detect(x, regex("^MDPI$|Multidisciplinary Digital Publishing", ignore_case = TRUE)), "MDPI",
    str_detect(x, regex("Frontiers", ignore_case = TRUE)), "Frontiers",
    str_detect(x, regex("Hindawi", ignore_case = TRUE)), "Hindawi",
    str_detect(x, regex("IEEE", ignore_case = TRUE)), "IEEE",
    str_detect(x, regex("Elsevier", ignore_case = TRUE)), "Elsevier",
    str_detect(x, regex("Springer|Nature Portfolio|BioMed Central|BMC", ignore_case = TRUE)), "Springer Nature",
    str_detect(x, regex("Wiley", ignore_case = TRUE)), "Wiley",
    str_detect(x, regex("Taylor|Francis|Cogent", ignore_case = TRUE)), "Taylor & Francis",
    str_detect(x, regex("SAGE", ignore_case = TRUE)), "SAGE",
    str_detect(x, regex("Spandidos", ignore_case = TRUE)), "Spandidos",
    str_detect(x, regex("Bentham", ignore_case = TRUE)), "Bentham",
    default = "Other"
  )
}

save_model_html <- function(models, filename, ...) {
  tryCatch(
    modelsummary(
      models,
      output = filename,
      stars = TRUE,
      statistic = "({std.error})",
      ...
    ),
    error = function(e) {
      message("modelsummary failed for ", filename, ": ", conditionMessage(e))
    }
  )
}

extract_i_terms <- function(model, prefix) {
  broom::tidy(model, conf.int = TRUE) |>
    filter(str_detect(term, paste0("^", prefix, "::")))
}

# ============================================================
# 3. Read data
# ============================================================

if (!file.exists(file_nsfc_main)) stop("Missing file: ", file_nsfc_main)
if (!file.exists(file_nsfc_auth)) stop("Missing file: ", file_nsfc_auth)

nsfc_main <- as.data.table(readRDS(file_nsfc_main))
nsfc_auth <- as.data.table(readRDS(file_nsfc_auth))

# Optional indexing metadata
wos_dt <- NULL
scopus_dt <- NULL
if (file.exists(file_wos)) wos_dt <- as.data.table(readRDS(file_wos))
if (file.exists(file_scopus)) scopus_dt <- as.data.table(readRDS(file_scopus))

# Basic checks
required_main <- c("id", "source_id", YEAR_VAR_MAIN, "source_display_name", "issn_l", "in_early_warning", "ewl_year")
missing_main <- setdiff(required_main, names(nsfc_main))
if (length(missing_main) > 0) stop("Missing columns in nsfc_main: ", paste(missing_main, collapse = ", "))

if (!(YEAR_VAR_AUTH %in% names(nsfc_auth))) {
  if ("publication_year" %in% names(nsfc_auth)) {
    YEAR_VAR_AUTH <- "publication_year"
  } else {
    stop("Cannot find publication year in nsfc_auth. Checked: ", YEAR_VAR_AUTH, " and publication_year")
  }
}
required_auth <- c("id", YEAR_VAR_AUTH, "author_ids")
missing_auth <- setdiff(required_auth, names(nsfc_auth))
if (length(missing_auth) > 0) stop("Missing columns in nsfc_auth: ", paste(missing_auth, collapse = ", "))

# Restrict main publication table
nsfc_main <- nsfc_main[
  get(YEAR_VAR_MAIN) >= YEAR_MIN & get(YEAR_VAR_MAIN) <= YEAR_MAX
]

nsfc_main[, year := as.integer(get(YEAR_VAR_MAIN))]
nsfc_main[, ewl_year := as.integer(ewl_year)]
nsfc_main[, ewl_cohort := ewl_year]
nsfc_main[, in_early_warning := as.integer(!is.na(ewl_year) | in_early_warning %in% TRUE)]

# ============================================================
# 4. Journal metadata
# ============================================================

journal_meta <- nsfc_main[
  , .(
    source_display_name = first_non_na_chr(source_display_name),
    issn_l = first_non_na_chr(issn_l),
    issn_l_norm = normalize_issn(first_non_na_chr(issn_l)),
    host_organization = if ("host_organization" %in% names(.SD)) first_non_na_chr(host_organization) else NA_character_,
    host_organization_name = if ("host_organization_name" %in% names(.SD)) first_non_na_chr(host_organization_name) else NA_character_,
    source_country_code = if ("source_country_code" %in% names(.SD)) first_non_na_chr(source_country_code) else NA_character_,
    publisher_name = if ("publisher_name" %in% names(.SD)) first_non_na_chr(publisher_name) else NA_character_,
    publisher_group = if ("publisher_group" %in% names(.SD)) first_non_na_chr(publisher_group) else NA_character_,
    publisher_family = if ("publisher_family" %in% names(.SD)) first_non_na_chr(publisher_family) else NA_character_,
    source_is_oa = if ("source_is_oa" %in% names(.SD)) first_non_na_lgl(source_is_oa) else NA,
    impact_2yr = if ("impact_2yr" %in% names(.SD)) safe_mean(impact_2yr) else NA_real_,
    main_domain = if ("main_domain" %in% names(.SD)) first_non_na_chr(main_domain) else NA_character_,
    ewl_year = safe_min_year(ewl_year[!is.na(ewl_year)]),
    in_early_warning = as.integer(any(!is.na(ewl_year), na.rm = TRUE))
  ),
  by = source_id
]

journal_meta[, publisher_group_simple := std_publisher_group(publisher_group)]
journal_meta[, publisher_top := classify_publisher_top(publisher_family, host_organization_name, publisher_name)]
journal_meta[, log_impact_2yr := log1p(impact_2yr)]
journal_meta[, treated_2020 := as.integer(!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR)]
journal_meta[, never_listed := as.integer(is.na(ewl_year))]
journal_meta[, ewl_effective_year := fifelse(!is.na(ewl_year), ewl_year + 2L, NA_integer_)]

# Optional WOS / Scopus flags if ISSNs are available
journal_meta[, indexed_wos := NA_integer_]
journal_meta[, indexed_scopus := NA_integer_]

if (!is.null(wos_dt)) {
  wos_cols <- names(wos_dt)
  issn_col <- intersect(c("issn", "issn_l", "ISSN", "issn_norm"), wos_cols)[1]
  if (!is.na(issn_col)) {
    wos_dt[, issn_norm_tmp := normalize_issn(get(issn_col))]
    wos_issn <- unique(wos_dt[!is.na(issn_norm_tmp), issn_norm_tmp])
    journal_meta[, indexed_wos := as.integer(issn_l_norm %in% wos_issn)]
  }
}

if (!is.null(scopus_dt)) {
  scopus_cols <- names(scopus_dt)
  issn_col <- intersect(c("issn", "issn_l", "ISSN", "issn_norm"), scopus_cols)[1]
  if (!is.na(issn_col)) {
    scopus_dt[, issn_norm_tmp := normalize_issn(get(issn_col))]
    scopus_issn <- unique(scopus_dt[!is.na(issn_norm_tmp), issn_norm_tmp])
    journal_meta[, indexed_scopus := as.integer(issn_l_norm %in% scopus_issn)]
  }
}

journal_meta[, indexed_wos_scopus := fifelse(indexed_wos == 1 | indexed_scopus == 1, 1L, 0L, na = NA_integer_)]

# Save metadata transparency table
fwrite(journal_meta, file.path(out_dir, "tables", "journal_metadata_analysis.csv"))

# ============================================================
# 5. Publication-level table
# ============================================================

# Keep one row per publication id. If duplicated, keep first row after sorting.
publication_cols <- intersect(
  c(
    "id", "year", "source_id", "source_display_name", "issn_l",
    "source_is_oa", "oa_status", "n_authors", "country_string",
    "main_domain", "impact_2yr", "ewl_year", "in_early_warning",
    "publisher_group", "publisher_family", "publisher_name",
    "host_organization_name", "source_country_code"
  ),
  names(nsfc_main)
)

pub_dt <- unique(nsfc_main[, ..publication_cols], by = "id")

# Add metadata columns not present in pub_dt, using journal_meta
meta_join_cols <- setdiff(
  names(journal_meta),
  intersect(names(journal_meta), names(pub_dt))[intersect(names(journal_meta), names(pub_dt)) != "source_id"]
)
pub_dt <- merge(pub_dt, journal_meta[, c("source_id", meta_join_cols), with = FALSE], by = "source_id", all.x = TRUE)

# Re-harmonize after merge
pub_dt[, publisher_group_simple := std_publisher_group(publisher_group)]
pub_dt[, publisher_top := classify_publisher_top(publisher_family, host_organization_name, publisher_name)]
pub_dt[, ewl_year := as.integer(ewl_year)]
pub_dt[, treated_2020 := as.integer(!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR)]
pub_dt[, never_listed := as.integer(is.na(ewl_year))]
pub_dt[, post_2022 := as.integer(year >= POST_START_YEAR)]
pub_dt[, transition_2021 := as.integer(year == TRANSITION_YEAR)]

# ============================================================
# 6. Journal-year panel: core trajectory interruption analysis
# ============================================================

# Counts observed per journal-year
journal_year_counts <- pub_dt[
  , .(
    n_pub = .N,
    n_oa_pub = sum(source_is_oa %in% TRUE, na.rm = TRUE),
    mean_n_authors = if ("n_authors" %in% names(pub_dt)) mean(n_authors, na.rm = TRUE) else NA_real_
  ),
  by = .(source_id, year)
]

# Balanced journal-year panel for all journals observed in the analysis window
journal_year_panel <- CJ(
  source_id = unique(pub_dt$source_id),
  year = YEAR_MIN:YEAR_MAX,
  unique = TRUE
)

journal_year_panel <- merge(journal_year_panel, journal_year_counts, by = c("source_id", "year"), all.x = TRUE)
journal_year_panel[is.na(n_pub), n_pub := 0L]
journal_year_panel[is.na(n_oa_pub), n_oa_pub := 0L]

journal_year_panel <- merge(journal_year_panel, journal_meta, by = "source_id", all.x = TRUE)

journal_year_panel[, treated_2020 := as.integer(!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR)]
journal_year_panel[, never_listed := as.integer(is.na(ewl_year))]
journal_year_panel[, post_2022 := as.integer(year >= POST_START_YEAR)]
journal_year_panel[, transition_2021 := as.integer(year == TRANSITION_YEAR)]
journal_year_panel[, post_time_2022 := pmax(0L, year - POST_START_YEAR + 1L)]
journal_year_panel[, calendar_time := year - YEAR_MIN]
journal_year_panel[, event_time := fifelse(!is.na(ewl_year), year - ewl_year, NA_integer_)]
journal_year_panel[, effective_event_time := fifelse(!is.na(ewl_effective_year), year - ewl_effective_year, NA_integer_)]
journal_year_panel[, log1p_n_pub := log1p(n_pub)]
journal_year_panel[, asinh_n_pub := asinh(n_pub)]

# Main comparison universe: first cohort 2020 vs never listed.
jy_2020 <- journal_year_panel[treated_2020 == 1 | never_listed == 1]

# Diagnostics: treated journal composition
jy_diag <- jy_2020[
  year == MAIN_COHORT_YEAR,
  .(
    n_journals = uniqueN(source_id),
    n_treated_journals = uniqueN(source_id[treated_2020 == 1]),
    n_control_journals = uniqueN(source_id[never_listed == 1]),
    n_pub_total = sum(n_pub, na.rm = TRUE),
    n_pub_treated = sum(n_pub[treated_2020 == 1], na.rm = TRUE),
    n_pub_control = sum(n_pub[never_listed == 1], na.rm = TRUE)
  )
]
fwrite(jy_diag, file.path(out_dir, "tables", "journal_year_sample_diagnostics.csv"))
print(jy_diag)

# Treated composition by publisher group
jy_treated_comp <- jy_2020[
  treated_2020 == 1,
  .(
    n_journals = uniqueN(source_id),
    n_pub_pre = sum(n_pub[year < POST_START_YEAR], na.rm = TRUE),
    n_pub_post = sum(n_pub[year >= POST_START_YEAR], na.rm = TRUE)
  ),
  by = publisher_group_simple
][order(-n_journals)]
fwrite(jy_treated_comp, file.path(out_dir, "tables", "treated_2020_composition_by_publisher_group.csv"))

# ============================================================
# 7. Descriptive figures: levels, shares, and indexed trajectories
# ============================================================
# 7.1 Raw publication volumes: treated 2020 vs never listed
jy_desc <- jy_2020[
  ,
  .(
    total_pub = as.numeric(sum(n_pub, na.rm = TRUE)),
    mean_pub_per_journal = as.numeric(mean(n_pub, na.rm = TRUE)),
    median_pub_per_journal = as.numeric(median(as.numeric(n_pub), na.rm = TRUE)),
    n_journals = as.integer(uniqueN(source_id))
  ),
  by = .(
    year,
    group = fifelse(
      treated_2020 == 1,
      "EWL 2020 cohort",
      "Never listed"
    )
  )
]

p_jy_levels <- ggplot(jy_desc, aes(x = year, y = total_pub, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = POST_START_YEAR, linetype = "dashed") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Publication volumes: EWL 2020 cohort vs never-listed journals",
    subtitle = "Dotted line: December 2020 release year; dashed line: effective post period from 2022",
    x = NULL,
    y = "Total NSFC-funded publications",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "jy_levels_ewl2020_vs_never.png"), p_jy_levels, width = 9, height = 6, dpi = 300)

# 7.2 Indexed trajectories: base 2019 = 100
jy_index <- jy_desc[
  , base_2019 := total_pub[year == 2019][1],
  by = group
][
  , index_2019 := 100 * total_pub / base_2019
]

p_jy_index <- ggplot(jy_index, aes(x = year, y = index_2019, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = POST_START_YEAR, linetype = "dashed") +
  labs(
    title = "Indexed publication trajectories",
    subtitle = "Index: 2019 = 100",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "jy_index_2019_ewl2020_vs_never.png"), p_jy_index, width = 9, height = 6, dpi = 300)

# 7.3 Publisher group market shares among all NSFC publications
market_group_year <- pub_dt[
  ,
  .(n_pub = .N),
  by = .(year, publisher_group_simple)
]

market_group_year[
  ,
  share := n_pub / sum(n_pub),
  by = year
]

p_market_share <- ggplot(
  market_group_year[publisher_group_simple != "Unknown"],
  aes(x = year, y = share, color = publisher_group_simple)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = POST_START_YEAR, linetype = "dashed") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Market reallocation across publisher groups",
    subtitle = "Share of NSFC-funded publications by publisher group",
    x = NULL,
    y = "Share of publications",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "market_share_by_publisher_group.png"), p_market_share, width = 10, height = 6, dpi = 300)

fwrite(jy_desc, file.path(out_dir, "tables", "journal_year_descriptive_ewl2020_vs_never.csv"))
fwrite(market_group_year, file.path(out_dir, "tables", "market_share_by_publisher_group.csv"))

# ============================================================
# 8. Main trajectory interruption models at journal-year level
# ============================================================
# These models estimate differential trajectory changes for EWL 2020 journals
# relative to never-listed journals. They should be interpreted as evidence of
# trajectory interruption / deceleration, NOT as a clean causal ATT.
#
# Model logic:
#   source_id FE absorb permanent journal differences.
#   year FE absorb global NSFC-year shocks.
#   treated_2020:calendar_time captures differential pre-existing trend.
#   treated_2020:post_2022 captures level shift after effective post period.
#   treated_2020:post_time_2022 captures post-2022 slope change.

m_jy_interrupted_log <- feols(
  log1p_n_pub ~ treated_2020:calendar_time + treated_2020:post_2022 + treated_2020:post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = jy_2020
)

m_jy_interrupted_asinh <- feols(
  asinh_n_pub ~ treated_2020:calendar_time + treated_2020:post_2022 + treated_2020:post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = jy_2020
)

m_jy_interrupted_count <- fepois(
  n_pub ~ treated_2020:calendar_time + treated_2020:post_2022 + treated_2020:post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = jy_2020
)

save_model_html(
  list(
    "log(1+count)" = m_jy_interrupted_log,
    "asinh(count)" = m_jy_interrupted_asinh,
    "Poisson" = m_jy_interrupted_count
  ),
  file.path(out_dir, "tables", "trajectory_interruption_journal_year_2020.html")
)

# Extract and save readable estimates
trajectory_terms <- bind_rows(
  broom::tidy(m_jy_interrupted_log, conf.int = TRUE) |> mutate(model = "log1p"),
  broom::tidy(m_jy_interrupted_asinh, conf.int = TRUE) |> mutate(model = "asinh"),
  broom::tidy(m_jy_interrupted_count, conf.int = TRUE) |> mutate(model = "poisson")
) |>
  filter(str_detect(term, "treated_2020"))

fwrite(trajectory_terms, file.path(out_dir, "tables", "trajectory_interruption_terms.csv"))

# ============================================================
# 9. Event-time trajectory models by listing cohort
# ============================================================
# This section uses all EWL cohorts and asks whether within-listed-journal
# publication intensity changes around listing. It is descriptive/dynamic.

jy_listed <- journal_year_panel[!is.na(ewl_year) & ewl_year <= 2024]
jy_listed <- jy_listed[event_time >= -6 & event_time <= 5]

cohort_event_summary <- jy_listed[
  ,
  .(
    mean_pub = as.numeric(mean(n_pub, na.rm = TRUE)),
    median_pub = as.numeric(median(as.numeric(n_pub), na.rm = TRUE)),
    total_pub = as.numeric(sum(n_pub, na.rm = TRUE)),
    n_journals = as.integer(uniqueN(source_id))
  ),
  by = .(ewl_year, event_time)
]

p_cohort_event <- ggplot(
  cohort_event_summary,
  aes(x = event_time, y = mean_pub, group = factor(ewl_year), color = factor(ewl_year))
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2, linetype = "dashed") +
  labs(
    title = "Publication intensity around CAS listing cohorts",
    subtitle = "Event time 0 = release/listing year; dashed line = expected publication response around +2",
    x = "Years relative to listing",
    y = "Mean publications per listed journal",
    color = "EWL year"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "cohort_event_publication_intensity.png"), p_cohort_event, width = 10, height = 6, dpi = 300)

fwrite(cohort_event_summary, file.path(out_dir, "tables", "cohort_event_summary_listed_journals.csv"))

# Listed-only interrupted trajectory model:
# This does not use never-listed controls. It asks whether listed journals show
# a post-effective-year slope change relative to their own pre-listing trend.
jy_listed[, post_effective := as.integer(!is.na(ewl_effective_year) & year >= ewl_effective_year)]
jy_listed[, post_effective_time := fifelse(post_effective == 1, year - ewl_effective_year + 1L, 0L)]
jy_listed[, event_time_effective := year - ewl_effective_year]

m_listed_interrupted <- feols(
  log1p_n_pub ~ event_time + post_effective + post_effective_time |
    source_id + ewl_year,
  cluster = ~ source_id,
  data = jy_listed
)

save_model_html(
  list("Listed journals only" = m_listed_interrupted),
  file.path(out_dir, "tables", "listed_only_interrupted_trajectory.html")
)

# ============================================================
# 10. Publisher-level heterogeneity in trajectory interruption
# ============================================================
# Focus on groups with enough treated journals; categories with very few treated
# journals should be interpreted as case studies, not group effects.

jy_2020[, publisher_group_simple := factor(
  publisher_group_simple,
  levels = c("Other international", "Commercial OA challengers", "Big 5", "Societies / Univ.", "Chinese / domestic", "Unknown")
)]

# Group-specific post-slope changes. Reference = Other international.
m_jy_hetero_group <- feols(
  log1p_n_pub ~
    i(publisher_group_simple, treated_2020 * post_2022, ref = "Other international") +
    i(publisher_group_simple, treated_2020 * post_time_2022, ref = "Other international") |
    source_id + year,
  cluster = ~ source_id,
  data = jy_2020[publisher_group_simple != "Unknown"]
)

save_model_html(
  list("Publisher-group trajectory heterogeneity" = m_jy_hetero_group),
  file.path(out_dir, "tables", "trajectory_heterogeneity_publisher_group.html")
)

hetero_terms <- broom::tidy(m_jy_hetero_group, conf.int = TRUE)
fwrite(hetero_terms, file.path(out_dir, "tables", "trajectory_heterogeneity_publisher_group_terms.csv"))

# Diagnostic table: number of treated journals per group
hetero_group_diag <- jy_2020[
  treated_2020 == 1,
  .(
    n_journals = uniqueN(source_id),
    n_pub_pre = sum(n_pub[year < POST_START_YEAR], na.rm = TRUE),
    n_pub_post = sum(n_pub[year >= POST_START_YEAR], na.rm = TRUE)
  ),
  by = publisher_group_simple
][order(-n_journals)]
fwrite(hetero_group_diag, file.path(out_dir, "tables", "trajectory_heterogeneity_group_diagnostics.csv"))

# ============================================================
# 11. Publisher-specific trajectories for key publishers
# ============================================================

key_publishers <- c(
  "MDPI", "Frontiers", "Elsevier", "Springer Nature", "Wiley", "Taylor & Francis"
)

publisher_year <- pub_dt[
  publisher_top %in% key_publishers,
  .(n_pub = .N),
  by = .(year, publisher_top)
]

publisher_year[, base_2019 := n_pub[year == 2019][1], by = publisher_top]
publisher_year[, index_2019 := 100 * n_pub / base_2019]

p_top_publishers_levels <- ggplot(publisher_year, aes(x = year, y = n_pub, color = publisher_top)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = POST_START_YEAR, linetype = "dashed") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "NSFC publication volumes by major publisher",
    subtitle = "Levels, not shares",
    x = NULL,
    y = "Publications",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "top_publishers_publication_levels.png"), p_top_publishers_levels, width = 10, height = 6, dpi = 300)

p_top_publishers_index <- ggplot(publisher_year[!is.na(index_2019)], aes(x = year, y = index_2019, color = publisher_top)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = POST_START_YEAR, linetype = "dashed") +
  labs(
    title = "Indexed trajectories by major publisher",
    subtitle = "Index: 2019 = 100",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "top_publishers_index_2019.png"), p_top_publishers_index, width = 10, height = 6, dpi = 300)

fwrite(publisher_year, file.path(out_dir, "tables", "top_publishers_yearly_trajectories.csv"))

# ============================================================
# 12. Portfolio spillover at publisher level
# ============================================================
# Question: do never-listed journals belonging to publishers with at least one
# EWL 2020 journal show post-2022 decline or gain?
# This is a portfolio-level association, not a clean causal estimate.

publisher_exposure <- journal_meta[
  , .(
    publisher_has_ewl2020 = as.integer(any(!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR, na.rm = TRUE)),
    n_ewl2020_journals = uniqueN(source_id[!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR])
  ),
  by = .(publisher_top, publisher_family, host_organization_name)
]

jy_spill <- merge(
  journal_year_panel,
  publisher_exposure,
  by = c("publisher_top", "publisher_family", "host_organization_name"),
  all.x = TRUE
)
jy_spill[is.na(publisher_has_ewl2020), publisher_has_ewl2020 := 0L]
jy_spill[, spillover_control := as.integer(never_listed == 1 & publisher_has_ewl2020 == 1)]
jy_spill[, spill_post_2022 := spillover_control * post_2022]
jy_spill[, spill_post_time_2022 := spillover_control * post_time_2022]

jy_spill_controls <- jy_spill[never_listed == 1]

m_spill_jy <- feols(
  log1p_n_pub ~ spillover_control:calendar_time + spill_post_2022 + spill_post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = jy_spill_controls
)

save_model_html(
  list("Never-listed journals in exposed portfolios" = m_spill_jy),
  file.path(out_dir, "tables", "portfolio_spillover_journal_year.html")
)

spill_terms <- broom::tidy(m_spill_jy, conf.int = TRUE)
fwrite(spill_terms, file.path(out_dir, "tables", "portfolio_spillover_terms.csv"))

spill_diag <- jy_spill_controls[
  year == POST_START_YEAR,
  .(
    n_journals = uniqueN(source_id),
    n_exposed_journals = uniqueN(source_id[spillover_control == 1]),
    n_unexposed_journals = uniqueN(source_id[spillover_control == 0])
  )
]
fwrite(spill_diag, file.path(out_dir, "tables", "portfolio_spillover_diagnostics.csv"))

# ============================================================
# 13. Optional author-journal-year allocation panel
# ============================================================
# This section is closer to the earlier DiD logic but reframed as allocation
# within author-year rather than as the main causal design.

if (BUILD_AUTHOR_PANEL) {
  message("Building author-journal-year allocation panel. This may take time.")
  
  auth_cols_optional <- c(
    "source_display_name", "issn_l", "fwci", "oa_status", "is_oa", "n_authors",
    "country_string", "main_domain", "first_author_id", "last_author_id"
  )
  auth_cols_keep <- intersect(auth_cols_optional, names(nsfc_auth))
  cols_auth_small <- unique(c("id", YEAR_VAR_AUTH, "author_ids", auth_cols_keep))
  
  nsfc_auth_small <- nsfc_auth[
    get(YEAR_VAR_AUTH) >= YEAR_MIN & get(YEAR_VAR_AUTH) <= YEAR_MAX,
    cols_auth_small,
    with = FALSE
  ]
  setnames(nsfc_auth_small, YEAR_VAR_AUTH, "year")
  nsfc_auth_small[, year := as.integer(year)]
  
  pub_join <- unique(pub_dt[, .(
    id,
    source_id,
    source_display_name,
    publisher_group_simple,
    publisher_top,
    publisher_family,
    host_organization_name,
    source_is_oa,
    impact_2yr,
    log_impact_2yr,
    main_domain,
    ewl_year,
    treated_2020,
    never_listed
  )], by = "id")
  
  nsfc_auth_small <- merge(nsfc_auth_small, pub_join, by = "id", all.x = TRUE)
  nsfc_auth_small <- nsfc_auth_small[!is.na(source_id)]
  
  if (!is.list(nsfc_auth_small$author_ids)) {
    stop("author_ids must be a list-column.")
  }
  
  pub_author <- nsfc_auth_small[
    lengths(author_ids) > 0,
    .(author_id = unlist(author_ids, use.names = FALSE)),
    by = .(
      id, year, source_id, source_display_name, publisher_group_simple, publisher_top,
      publisher_family, host_organization_name, source_is_oa, impact_2yr, log_impact_2yr,
      main_domain, ewl_year, treated_2020, never_listed
    )
  ]
  pub_author <- unique(pub_author[!is.na(author_id) & nzchar(author_id)], by = c("id", "author_id"))
  
  # Observed support: author-journal combinations ever observed.
  support <- unique(pub_author[, .(
    author_id, source_id, source_display_name, publisher_group_simple, publisher_top,
    publisher_family, host_organization_name, source_is_oa, impact_2yr, log_impact_2yr,
    main_domain, ewl_year, treated_2020, never_listed
  )])
  
  # Keep first cohort 2020 vs never-listed for allocation panel.
  support_2020 <- support[treated_2020 == 1 | never_listed == 1]
  
  panel_alloc <- support_2020[
    , .(year = YEAR_MIN:YEAR_MAX),
    by = .(
      author_id, source_id, source_display_name, publisher_group_simple, publisher_top,
      publisher_family, host_organization_name, source_is_oa, impact_2yr, log_impact_2yr,
      main_domain, ewl_year, treated_2020, never_listed
    )
  ]
  
  counts_author_journal_year <- pub_author[
    , .(n_pub = .N),
    by = .(author_id, source_id, year)
  ]
  
  panel_alloc <- merge(panel_alloc, counts_author_journal_year, by = c("author_id", "source_id", "year"), all.x = TRUE)
  panel_alloc[is.na(n_pub), n_pub := 0L]
  
  panel_alloc[, post_2022 := as.integer(year >= POST_START_YEAR)]
  panel_alloc[, post_time_2022 := pmax(0L, year - POST_START_YEAR + 1L)]
  panel_alloc[, calendar_time := year - YEAR_MIN]
  panel_alloc[, did_2020 := treated_2020 * post_2022]
  panel_alloc[, author_year_fe := interaction(author_id, year, drop = TRUE)]
  panel_alloc[, total_pub_author_year := sum(n_pub, na.rm = TRUE), by = .(author_id, year)]
  panel_alloc[, share_pub := fifelse(total_pub_author_year > 0, n_pub / total_pub_author_year, NA_real_)]
  
  # Restrict to authors with treated and control exposure pre/post.
  author_support <- panel_alloc[
    , .(
      has_treated = as.integer(any(treated_2020 == 1, na.rm = TRUE)),
      has_control = as.integer(any(never_listed == 1, na.rm = TRUE)),
      has_pre = as.integer(any(year < POST_START_YEAR, na.rm = TRUE)),
      has_post = as.integer(any(year >= POST_START_YEAR, na.rm = TRUE))
    ),
    by = author_id
  ]
  eligible_authors <- author_support[has_treated == 1 & has_control == 1 & has_pre == 1 & has_post == 1, author_id]
  panel_alloc <- panel_alloc[author_id %in% eligible_authors]
  
  saveRDS(panel_alloc, file.path(out_dir, "data", "author_journal_year_allocation_panel_2020_vs_never.rds"), compress = "xz")
  
  alloc_diag <- data.table(
    item = c("observations", "authors", "journals", "treated journals", "control journals"),
    value = c(
      nrow(panel_alloc),
      uniqueN(panel_alloc$author_id),
      uniqueN(panel_alloc$source_id),
      uniqueN(panel_alloc[treated_2020 == 1, source_id]),
      uniqueN(panel_alloc[never_listed == 1, source_id])
    )
  )
  fwrite(alloc_diag, file.path(out_dir, "tables", "allocation_panel_diagnostics.csv"))
  print(alloc_diag)
  
  # Allocation models: interpreted as within-author-year reallocation evidence.
  m_alloc_count <- feols(
    n_pub ~ did_2020 |
      author_year_fe + source_id,
    cluster = ~ author_id,
    data = panel_alloc
  )
  
  m_alloc_share <- feols(
    share_pub ~ did_2020 |
      author_year_fe + source_id,
    cluster = ~ author_id,
    data = panel_alloc[!is.na(share_pub)]
  )
  
  # Trajectory-interruption allocation model: allows treated journals to have
  # different pretrend and post-2022 slope.
  m_alloc_interrupted <- feols(
    n_pub ~ treated_2020:calendar_time + did_2020 + treated_2020:post_time_2022 |
      author_year_fe + source_id,
    cluster = ~ author_id,
    data = panel_alloc
  )
  
  save_model_html(
    list(
      "Allocation count" = m_alloc_count,
      "Allocation share" = m_alloc_share,
      "Allocation interrupted trajectory" = m_alloc_interrupted
    ),
    file.path(out_dir, "tables", "author_year_allocation_models.html")
  )
  
  allocation_terms <- bind_rows(
    broom::tidy(m_alloc_count, conf.int = TRUE) |> mutate(model = "count"),
    broom::tidy(m_alloc_share, conf.int = TRUE) |> mutate(model = "share"),
    broom::tidy(m_alloc_interrupted, conf.int = TRUE) |> mutate(model = "interrupted")
  )
  fwrite(allocation_terms, file.path(out_dir, "tables", "author_year_allocation_terms.csv"))
}

# ============================================================
# 14. Output summary file
# ============================================================

interpret_coef <- function(model, term_candidates) {
  ct <- broom::tidy(model, conf.int = TRUE)
  for (tm in term_candidates) {
    row <- ct[ct$term == tm, ]
    if (nrow(row) > 0) {
      return(paste0(
        tm, " = ", round(row$estimate, 5),
        " [", round(row$conf.low, 5), "; ", round(row$conf.high, 5), "]",
        ", p = ", signif(row$p.value, 3)
      ))
    }
  }
  NA_character_
}

summary_lines <- c(
  "NSFC / CAS EWL trajectory-reallocation analysis",
  "",
  "Interpretive stance:",
  "This script does not treat the CAS list as a clean exogenous shock producing a simple causal ATT.",
  "It documents trajectory interruption, post-warning deceleration, and market reallocation patterns.",
  "",
  "Main journal-year trajectory model:",
  paste("log1p model, differential pretrend:", interpret_coef(m_jy_interrupted_log, c("treated_2020:calendar_time"))),
  paste("log1p model, post-2022 level shift:", interpret_coef(m_jy_interrupted_log, c("treated_2020:post_2022"))),
  paste("log1p model, post-2022 slope change:", interpret_coef(m_jy_interrupted_log, c("treated_2020:post_time_2022"))),
  "",
  "Key outputs:",
  paste0("- ", file.path(out_dir, "tables", "trajectory_interruption_journal_year_2020.html")),
  paste0("- ", file.path(out_dir, "figures", "jy_levels_ewl2020_vs_never.png")),
  paste0("- ", file.path(out_dir, "figures", "jy_index_2019_ewl2020_vs_never.png")),
  paste0("- ", file.path(out_dir, "figures", "market_share_by_publisher_group.png")),
  paste0("- ", file.path(out_dir, "figures", "cohort_event_publication_intensity.png")),
  paste0("- ", file.path(out_dir, "figures", "top_publishers_publication_levels.png")),
  paste0("- ", file.path(out_dir, "figures", "top_publishers_index_2019.png"))
)

writeLines(summary_lines, file.path(out_dir, "tables", "trajectory_reallocation_interpretation.txt"))

message("\n============================================================")
message("Trajectory-reallocation analysis completed.")
message("Outputs saved in: ", out_dir)
message("Main model table: ", file.path(out_dir, "tables", "trajectory_interruption_journal_year_2020.html"))
message("Summary file: ", file.path(out_dir, "tables", "trajectory_reallocation_interpretation.txt"))
message("============================================================")
