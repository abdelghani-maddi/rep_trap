# ============================================================
# NSFC — DiD reputation trap around CAS Early Warning Lists
# Version v2: cohort-specific design
# ============================================================
# Main design:
#   - main causal analysis restricted to the first EWL cohort: journals first listed in 2020
#   - because the list was released in December 2020 and outcomes are publication years,
#     the effective post period starts in 2022
#   - 2021 is treated as a transition / reference year
# Robustness:
#   - alternative post starts in 2021 and 2023
#   - exclusion of 2021 as transition year
#   - staggered DiD using fixest::sunab(ewl_year, year), excluding very late cohorts
#   - heterogeneity by publisher group and top publishers
#   - spillover tests for non-listed journals within exposed publisher portfolios
# ============================================================

# ============================================================
# 0. Packages
# ============================================================

pkgs <- c(
  "tidyverse", "data.table", "fixest", "lubridate", "stringr", "janitor",
  "broom", "modelsummary", "ggplot2", "patchwork", "scales", "forcats"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Packages manquants : ", paste(missing_pkgs, collapse = ", "))
}

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
library(scales)
library(forcats)

setFixest_nthreads(max(1, parallel::detectCores() - 1))
options(scipen = 999)

# ============================================================
# 1. Parameters
# ============================================================

YEAR_MIN <- 2016
YEAR_MAX <- 2025

# The first EWL was released in December 2020.
# Because we observe publication years, not submission dates, the main post period starts in 2022.
MAIN_COHORT_YEAR <- 2020
POST_START_YEAR <- 2022
REFERENCE_YEAR <- 2021

YEAR_VAR_MAIN <- "publication_year"
# YEAR_VAR_AUTH <- "publication_year"
YEAR_VAR_AUTH <- "publication_year.x"

out_dir <- "outputs_did_reputation_trap_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "data"), showWarnings = FALSE, recursive = TRUE)

# Input files
file_nsfc_main <- "data_nsfc/nsfc_early_warning_full_analysis.rds"
file_nsfc_auth <- "data_nsfc/nsfc_final_clean_with_authors.rds"
file_wos <- "D:/wos_issn.rds"
file_scopus <- "D:/scopus_issn.rds"

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
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9]", "")
  toupper(x)
}

save_model_html <- function(models, filename, ...) {
  tryCatch(
    modelsummary(models, output = filename, stars = TRUE, statistic = "({std.error})", ...),
    error = function(e) message("modelsummary failed for ", filename, ": ", conditionMessage(e))
  )
}

extract_event_2020 <- function(model){
  
  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl("^event_time_2020::", term)) %>%
    mutate(
      event_time = stringr::str_extract(
        term,
        "(?<=event_time_2020::)-?[0-9]+"
      ) %>%
        as.integer(),
      
      period = dplyr::case_when(
        event_time < 0 ~ "Pre",
        event_time == 0 ~ "Release year",
        event_time == 1 ~ "Reference year",
        event_time >= 2 ~ "Post"
      )
    ) %>%
    arrange(event_time)
  
}

plot_event_study <- function(es_df, title, subtitle, ylab, filename, vline_x = 0) {
  p <- ggplot(es_df, aes(x = event_time, y = estimate, ymin = conf.low, ymax = conf.high, color = period)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_vline(xintercept = vline_x, linetype = 2) +
    geom_pointrange() +
    scale_x_continuous(breaks = sort(unique(es_df$event_time))) +
    labs(title = title, subtitle = subtitle, x = "Years relative to first listing", y = ylab, color = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  ggsave(filename = file.path(out_dir, "figures", filename), plot = p, width = 9, height = 6, dpi = 300)
  p
}

# ============================================================
# 3. Read data
# ============================================================

needed_files <- c(file_nsfc_main, file_nsfc_auth, file_wos, file_scopus)
missing_files <- needed_files[!file.exists(needed_files)]
if (length(missing_files) > 0) {
  stop("Fichiers introuvables :\n", paste(missing_files, collapse = "\n"))
}

nsfc_early_warning_full_analysis <- readRDS(file_nsfc_main)
nsfc_with_authors <- readRDS(file_nsfc_auth)
wos <- readRDS(file_wos)
scopus <- readRDS(file_scopus)

nsfc_main <- as.data.table(nsfc_early_warning_full_analysis)
nsfc_auth <- as.data.table(nsfc_with_authors)
wos_dt <- as.data.table(wos)
scopus_dt <- as.data.table(scopus)

# ============================================================
# 4. Basic checks
# ============================================================

required_main <- c("id", "source_id", YEAR_VAR_MAIN, "source_display_name", "issn_l", "in_early_warning", "ewl_year")
missing_main <- setdiff(required_main, names(nsfc_main))
if (length(missing_main) > 0) stop("Colonnes manquantes dans nsfc_main : ", paste(missing_main, collapse = ", "))

required_auth <- c("id", YEAR_VAR_AUTH, "author_ids")
missing_auth <- setdiff(required_auth, names(nsfc_auth))
if (length(missing_auth) > 0) stop("Colonnes manquantes dans nsfc_auth : ", paste(missing_auth, collapse = ", "))

# ============================================================
# 5. Journal metadata
# ============================================================

journal_meta <- nsfc_main[
  get(YEAR_VAR_MAIN) >= YEAR_MIN & get(YEAR_VAR_MAIN) <= YEAR_MAX,
  .(
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
    
    in_early_warning = as.integer(any(in_early_warning %in% TRUE, na.rm = TRUE)),
    
    ewl_year = safe_min_year(
      ewl_year[in_early_warning %in% TRUE]
    ),
    
    warning_reason = if ("warning_reason" %in% names(.SD)) first_non_na_chr(warning_reason) else NA_character_,
    
    is_big5 = if ("is_big5" %in% names(.SD)) as.integer(any(is_big5 %in% TRUE, na.rm = TRUE)) else NA_integer_,
    is_grey = if ("is_grey" %in% names(.SD)) as.integer(any(is_grey %in% TRUE, na.rm = TRUE)) else NA_integer_,
    is_chinese_publisher = if ("is_chinese_publisher" %in% names(.SD)) as.integer(any(is_chinese_publisher %in% TRUE, na.rm = TRUE)) else NA_integer_
  ),
  by = .(source_id)
]

journal_meta[is.infinite(ewl_year), ewl_year := NA_real_]
journal_meta[, ewl_year := as.integer(ewl_year)]
journal_meta[, listed := as.integer(!is.na(ewl_year))]

journal_meta[, publisher_group_simple := case_when(
  publisher_group == "Grey publishers" ~ "Grey",
  publisher_group == "Big 5 publishers" ~ "Big 5",
  publisher_group == "Major society / university / non-profit publishers" ~ "Societies / Univ.",
  publisher_group == "Chinese publishers" ~ "Chinese",
  publisher_group == "Other international commercial publishers" ~ "Other intl.",
  TRUE ~ NA_character_
)]

journal_meta[, publisher_group_simple := factor(
  publisher_group_simple,
  levels = c("Other intl.", "Grey", "Big 5", "Societies / Univ.", "Chinese")
)]

# ============================================================
# 6. WoS / Scopus indexation
# ============================================================

wos_dt <- clean_names(wos_dt) |> as.data.table()
scopus_dt <- clean_names(scopus_dt) |> as.data.table()

if (!"wos_issn" %in% names(wos_dt)) stop("La base wos doit contenir la colonne wos_issn")
if (!"scopus_issn" %in% names(scopus_dt)) stop("La base scopus doit contenir la colonne scopus_issn")

wos_dt[, issn_l_norm := normalize_issn(wos_issn)]
scopus_dt[, issn_l_norm := normalize_issn(scopus_issn)]

journal_meta[, in_wos := as.integer(issn_l_norm %in% unique(wos_dt$issn_l_norm))]
journal_meta[, in_scopus := as.integer(issn_l_norm %in% unique(scopus_dt$issn_l_norm))]
journal_meta[, indexed_wos_scopus := as.integer(in_wos == 1 | in_scopus == 1)]


# ============================================================
# 7. Publication-author table
# ============================================================

auth_cols_optional <- c(
  "source_display_name", "issn_l", "fwci", "oa_status", "is_oa", "n_authors",
  "country_string", "main_domain", "first_author_id", "last_author_id"
)

auth_cols_keep <- intersect(auth_cols_optional, names(nsfc_auth))

required_auth <- c("id", YEAR_VAR_AUTH, "author_ids")
missing_auth <- setdiff(required_auth, names(nsfc_auth))

if (length(missing_auth) > 0) {
  stop("Colonnes manquantes dans nsfc_auth : ", paste(missing_auth, collapse = ", "))
}

required_auth <- c("id", YEAR_VAR_AUTH, "author_ids")
missing_auth <- setdiff(required_auth, names(nsfc_auth))

if (length(missing_auth) > 0) {
  stop("Colonnes manquantes dans nsfc_auth : ", paste(missing_auth, collapse = ", "))
}

# Colonnes à garder, sans créer de doublons
cols_auth_small <- unique(c(
  "id",
  YEAR_VAR_AUTH,
  "author_ids",
  auth_cols_keep
))

nsfc_auth_small <- nsfc_auth[
  get(YEAR_VAR_AUTH) >= YEAR_MIN & get(YEAR_VAR_AUTH) <= YEAR_MAX,
  cols_auth_small,
  with = FALSE
]

# Renommer l'année proprement
setnames(nsfc_auth_small, YEAR_VAR_AUTH, "year")


pub_join_cols_optional <- c(
  "source_display_name", "host_organization_name", "publisher_name", "publisher_family",
  "source_country_code", "source_is_oa", "impact_2yr", "main_domain", "in_early_warning",
  "ewl_year", "warning_reason", "before_after", "event_time", "is_big5", "is_grey",
  "is_chinese_publisher", "issn_l"
)
pub_join_cols <- intersect(pub_join_cols_optional, names(nsfc_main))

pub_join <- unique(
  nsfc_main[
    get(YEAR_VAR_MAIN) >= YEAR_MIN & get(YEAR_VAR_MAIN) <= YEAR_MAX,
    c("id", "source_id", pub_join_cols),
    with = FALSE
  ],
  by = "id"
)

nsfc_auth_small <- merge(nsfc_auth_small, pub_join, by = "id", all.x = TRUE)
nsfc_auth_small <- nsfc_auth_small[!is.na(source_id)]

if (!is.list(nsfc_auth_small$author_ids)) {
  stop("La colonne author_ids doit être une list-column contenant les identifiants auteurs.")
}

pub_author <- nsfc_auth_small[
  lengths(author_ids) > 0,
  .(author_id = unlist(author_ids, use.names = FALSE)),
  by = .(id, year, source_id)
]

pub_author <- unique(pub_author[!is.na(author_id) & nzchar(author_id)], by = c("id", "author_id"))

# ============================================================
# 8. Author × journal × year observed panel
# ============================================================

author_journal_year <- pub_author[
  , .(n_pub = .N),
  by = .(author_id, source_id, year)
]

panel <- merge(author_journal_year, journal_meta, by = "source_id", all.x = TRUE)
panel <- panel[year >= YEAR_MIN & year <= YEAR_MAX]

# Keep observations with a usable publisher group for heterogeneity.
# For the baseline, keep all journals; unknown publisher group is allowed.
panel[, publisher_group_simple := fct_explicit_na(publisher_group_simple, na_level = "Unknown")]
panel[, source_is_oa := as.integer(source_is_oa %in% TRUE)]
panel[, log_impact_2yr := log1p(impact_2yr)]
panel[, main_domain := fct_explicit_na(as.factor(main_domain), na_level = "Unknown")]
panel[, author_year_fe := interaction(author_id, year, drop = TRUE)]
panel[, author_fe := factor(author_id)]
panel[, year_fe := factor(year)]
panel[, any_pub := as.integer(n_pub > 0)]

# Publisher exposure for spillovers
publisher_exposure <- journal_meta[
  , .(publisher_has_listed = max(listed, na.rm = TRUE)),
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

# Top publisher coding
panel[, publisher_top := fcase(
  str_detect(publisher_family, regex("^MDPI$", ignore_case = TRUE)) | str_detect(publisher_name, regex("^MDPI$", ignore_case = TRUE)), "MDPI",
  str_detect(publisher_family, regex("Frontiers", ignore_case = TRUE)) | str_detect(publisher_name, regex("Frontiers", ignore_case = TRUE)), "Frontiers",
  str_detect(publisher_family, regex("Hindawi", ignore_case = TRUE)) | str_detect(publisher_name, regex("Hindawi", ignore_case = TRUE)), "Hindawi",
  str_detect(publisher_family, regex("Elsevier", ignore_case = TRUE)) | str_detect(publisher_name, regex("Elsevier", ignore_case = TRUE)), "Elsevier",
  str_detect(publisher_family, regex("Springer", ignore_case = TRUE)) | str_detect(publisher_name, regex("Springer", ignore_case = TRUE)), "Springer Nature",
  str_detect(publisher_family, regex("Wiley", ignore_case = TRUE)) | str_detect(publisher_name, regex("Wiley", ignore_case = TRUE)), "Wiley",
  str_detect(publisher_family, regex("Taylor", ignore_case = TRUE)) | str_detect(publisher_name, regex("Taylor", ignore_case = TRUE)), "Taylor & Francis",
  str_detect(publisher_family, regex("SAGE", ignore_case = TRUE)) | str_detect(publisher_name, regex("SAGE", ignore_case = TRUE)), "SAGE",
  default = "Other"
)]

panel[, publisher_top := factor(
  publisher_top,
  levels = c("Other", "MDPI", "Frontiers", "Hindawi", "Elsevier", "Springer Nature", "Wiley", "Taylor & Francis", "SAGE")
)]

saveRDS(panel, file.path(out_dir, "data", "panel_author_journal_year_observed.rds"), compress = "xz")

# ============================================================
# 9. Main DiD sample: first cohort 2020 vs never listed
# ============================================================

panel[, ewl_year := as.integer(ewl_year)]

panel[, treated_2020 := as.integer(!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR)]
panel[, never_listed := as.integer(is.na(ewl_year))]

panel[, post_2022 := as.integer(year >= POST_START_YEAR)]
panel[, post_2021 := as.integer(year >= 2021)]
panel[, post_2023 := as.integer(year >= 2023)]
panel[, event_time_2020 := year - MAIN_COHORT_YEAR]

panel[, group_2020 := fifelse(
  treated_2020 == 1,
  "EWL 2020 cohort",
  "Never listed"
)]

panel[, group_2020 := factor(
  group_2020,
  levels = c("Never listed", "EWL 2020 cohort")
)]

panel_2020 <- panel[
  treated_2020 == 1 | never_listed == 1
]

author_support_2020 <- panel_2020[
  ,
  .(
    has_treated_2020 = as.integer(any(treated_2020 == 1, na.rm = TRUE)),
    has_control      = as.integer(any(never_listed == 1, na.rm = TRUE)),
    has_pre          = as.integer(any(year < POST_START_YEAR, na.rm = TRUE)),
    has_post         = as.integer(any(year >= POST_START_YEAR, na.rm = TRUE))
  ),
  by = author_id
]

eligible_authors_2020 <- author_support_2020[
  has_treated_2020 == 1 &
    has_control == 1 &
    has_pre == 1 &
    has_post == 1,
  author_id
]

panel_did_2020 <- panel_2020[
  author_id %in% eligible_authors_2020
]

panel_did_2020[, did_2020 := treated_2020 * post_2022]

panel_did_2020[
  ,
  total_pub_author_year := sum(n_pub, na.rm = TRUE),
  by = .(author_id, year)
]

panel_did_2020[
  ,
  share_pub := fifelse(
    total_pub_author_year > 0,
    n_pub / total_pub_author_year,
    NA_real_
  )
]

panel_did_2020[
  ,
  log_total_pub_author_year := log1p(total_pub_author_year)
]

panel_did_2020[, author_year_fe := interaction(author_id, year, drop = TRUE)]
panel_did_2020[, author_fe := factor(author_id)]
panel_did_2020[, year_fe := factor(year)]

saveRDS(
  panel_did_2020,
  file.path(out_dir, "data", "panel_did_cohort_2020_vs_never.rds"),
  compress = "xz"
)

sample_diag <- data.table(
  item = c(
    "Observations",
    "Authors",
    "Journals",
    "Treated 2020 journals",
    "Never-listed control journals",
    "Min year",
    "Max year"
  ),
  value = c(
    nrow(panel_did_2020),
    uniqueN(panel_did_2020$author_id),
    uniqueN(panel_did_2020$source_id),
    uniqueN(panel_did_2020[treated_2020 == 1, source_id]),
    uniqueN(panel_did_2020[never_listed == 1, source_id]),
    min(panel_did_2020$year, na.rm = TRUE),
    max(panel_did_2020$year, na.rm = TRUE)
  )
)

fwrite(
  sample_diag,
  file.path(out_dir, "tables", "sample_diagnostics_cohort_2020.csv")
)

print(sample_diag)

freq(panel_did_2020$treated_2020)
freq(panel_did_2020$never_listed)
freq(panel_did_2020$group_2020)


# ============================================================
# 10. Descriptive plots: 2020 cohort vs never listed
# ============================================================

desc_2020 <- panel_did_2020[
  ,
  .(
    avg_pub = mean(n_pub, na.rm = TRUE),
    med_pub = median(n_pub, na.rm = TRUE),
    n_obs = .N
  ),
  by = .(year, group = group_2020)
]

plot_desc_2020 <- ggplot(
  desc_2020,
  aes(x = year, y = avg_pub, color = group)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = POST_START_YEAR, linetype = "dashed") +
  labs(
    title = "Average trajectories: EWL 2020 cohort vs never-listed journals",
    subtitle = "Dotted line: December 2020 release year; dashed line: effective post period from 2022",
    x = "Publication year",
    y = "Average number of publications per author × journal",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "figures", "desc_cohort_2020_vs_never.png"),
  plot_desc_2020,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 11. Main DiD: effective post starts in 2022
# ============================================================

m_did_2020_count <- feols(
  n_pub ~ did_2020 +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020
)

m_did_2020_share <- feols(
  share_pub ~ did_2020 +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020[!is.na(share_pub)]
)

m_did_2020_scale <- feols(
  n_pub ~ did_2020 + log_total_pub_author_year |
    author_fe + year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020
)

m_did_2020_pois <- fepois(
  n_pub ~ did_2020 +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020
)

save_model_html(
  list(
    "Count FE" = m_did_2020_count,
    "Share FE" = m_did_2020_share,
    "Scale-controlled" = m_did_2020_scale,
    "Poisson FE" = m_did_2020_pois
  ),
  file.path(out_dir, "tables", "did_main_cohort_2020_post2022.html")
)
# ============================================================
# 12. Event-study: centered on release year, reference = 2021
# ============================================================
# Interpretation:
# - event_time = 0 is publication year 2020, the release year of the list.
# - event_time = 1 is 2021, treated as transition/reference.
# - expected publication response should appear mainly from event_time >= 2.

m_event_2020_count <- feols(
  n_pub ~ i(event_time_2020, treated_2020, ref = 1) + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020
)

m_event_2020_share <- feols(
  share_pub ~ i(event_time_2020, treated_2020, ref = 1) + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020[!is.na(share_pub)]
)

es_2020_count <- extract_event_2020(m_event_2020_count)
es_2020_share <- extract_event_2020(m_event_2020_share)

print(es_2020_count)
print(es_2020_share)

fwrite(es_2020_count, file.path(out_dir, "tables", "event_study_cohort_2020_count.csv"))
fwrite(es_2020_share, file.path(out_dir, "tables", "event_study_cohort_2020_share.csv"))

plot_event_2020_count <- plot_event_study(
  es_2020_count,
  title = "Event-study around the first CAS Early Warning List",
  subtitle = "Main analysis: EWL 2020 cohort vs never-listed journals; reference year = 2021",
  ylab = "Estimated effect on publication counts",
  filename = "event_study_cohort_2020_count.png",
  vline_x = 0
)

plot_event_2020_share <- plot_event_study(
  es_2020_share,
  title = "Event-study using publication shares",
  subtitle = "Main analysis: EWL 2020 cohort vs never-listed journals; reference year = 2021",
  ylab = "Estimated effect on author-year publication share",
  filename = "event_study_cohort_2020_share.png",
  vline_x = 0
)

# Pre-trend tests: all event-time coefficients before release year, excluding reference year (+1).
pretrend_terms_count <- grep("event_time_2020::-[0-9]+:treated_2020", names(coef(m_event_2020_count)), value = TRUE)
pretrend_terms_share <- grep("event_time_2020::-[0-9]+:treated_2020", names(coef(m_event_2020_share)), value = TRUE)

pretrend_test_count <- if (length(pretrend_terms_count) > 0) wald(m_event_2020_count, pretrend_terms_count) else NULL
pretrend_test_share <- if (length(pretrend_terms_share) > 0) wald(m_event_2020_share, pretrend_terms_share) else NULL

capture.output(pretrend_test_count, file = file.path(out_dir, "tables", "pretrend_test_cohort_2020_count.txt"))
capture.output(pretrend_test_share, file = file.path(out_dir, "tables", "pretrend_test_cohort_2020_share.txt"))

# ============================================================
# 13. Timing robustness: post starts in 2021 / 2023, and excluding 2021
# ============================================================

m_did_2020_post2021 <- feols(
  n_pub ~ treated_2020:post_2021 + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020
)

m_did_2020_post2023 <- feols(
  n_pub ~ treated_2020:post_2023 + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020
)

panel_did_2020_no2021 <- panel_did_2020[year != 2021]

m_did_2020_exclude2021 <- feols(
  n_pub ~ treated_2020:post_2022 + source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020_no2021
)

save_model_html(
  list(
    "Main: post>=2022" = m_did_2020_count,
    "Post>=2021" = m_did_2020_post2021,
    "Post>=2023" = m_did_2020_post2023,
    "Exclude 2021" = m_did_2020_exclude2021
  ),
  file.path(out_dir, "tables", "did_timing_robustness_cohort_2020.html")
)

# ============================================================
# 14. Heterogeneity by publisher group: main cohort 2020
# ============================================================

# Recode publisher groups cleanly
panel_did_2020[, publisher_group_simple := fcase(
  publisher_group %in% c("Other international commercial publishers", "Other intl."), "Other intl.",
  publisher_group %in% c("Grey publishers", "Grey"), "Grey",
  publisher_group %in% c("Big 5 publishers", "Big 5"), "Big 5",
  publisher_group %in% c("Major society / university / non-profit publishers", "Societies / Univ."), "Societies / Univ.",
  publisher_group %in% c("Chinese publishers", "Chinese"), "Chinese",
  default = "Unknown"
)]

panel_did_2020[, publisher_group_simple := factor(
  publisher_group_simple,
  levels = c("Other intl.", "Grey", "Big 5", "Societies / Univ.", "Chinese", "Unknown")
)]

# Recode top publishers
panel_did_2020[, publisher_top := fcase(
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

panel_did_2020[, publisher_top := factor(
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

# Ensure DiD variable exists
panel_did_2020[, did_2020 := treated_2020 * post_2022]

# Publisher-group heterogeneity
m_did_2020_hetero_group <- feols(
  n_pub ~ i(publisher_group_simple, did_2020, ref = "Other intl.") +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020[
    !is.na(publisher_group_simple) &
      publisher_group_simple != "Unknown"
  ]
)

# Top-publisher heterogeneity
m_did_2020_hetero_top <- feols(
  n_pub ~ i(publisher_top, did_2020, ref = "Other") +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_did_2020[
    !is.na(publisher_top)
  ]
)

save_model_html(
  list(
    "Publisher groups" = m_did_2020_hetero_group,
    "Top publishers" = m_did_2020_hetero_top
  ),
  file.path(out_dir, "tables", "did_heterogeneity_cohort_2020.html")
)

# Extract publisher-group coefficients
hetero_group_df <- broom::tidy(
  m_did_2020_hetero_group,
  conf.int = TRUE
) |>
  filter(str_detect(term, ":did_2020")) |>
  mutate(
    group = str_extract(
      term,
      "(?<=publisher_group_simple::).*?(?=:did_2020)"
    )
  ) |>
  filter(!is.na(group))

fwrite(
  hetero_group_df,
  file.path(out_dir, "tables", "heterogeneity_publisher_group_cohort_2020.csv")
)

if (nrow(hetero_group_df) > 0) {
  p_hetero_group <- ggplot(
    hetero_group_df,
    aes(
      x = reorder(group, estimate),
      y = estimate,
      ymin = conf.low,
      ymax = conf.high
    )
  ) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange() +
    coord_flip() +
    labs(
      title = "Heterogeneous DiD effects by publisher group",
      subtitle = "Main cohort 2020; post period starts in 2022",
      x = NULL,
      y = "Estimated effect on publication counts"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(
    file.path(out_dir, "figures", "heterogeneity_publisher_group_cohort_2020.png"),
    p_hetero_group,
    width = 9,
    height = 6,
    dpi = 300
  )
}

# Extract top-publisher coefficients
hetero_top_df <- broom::tidy(
  m_did_2020_hetero_top,
  conf.int = TRUE
) |>
  filter(str_detect(term, ":did_2020")) |>
  mutate(
    publisher = str_extract(
      term,
      "(?<=publisher_top::).*?(?=:did_2020)"
    )
  ) |>
  filter(!is.na(publisher))

fwrite(
  hetero_top_df,
  file.path(out_dir, "tables", "heterogeneity_top_publishers_cohort_2020.csv")
)

if (nrow(hetero_top_df) > 0) {
  p_hetero_top <- ggplot(
    hetero_top_df,
    aes(
      x = reorder(publisher, estimate),
      y = estimate,
      ymin = conf.low,
      ymax = conf.high
    )
  ) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange() +
    coord_flip() +
    labs(
      title = "Heterogeneous DiD effects by publisher",
      subtitle = "Main cohort 2020; post period starts in 2022",
      x = NULL,
      y = "Estimated effect on publication counts"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(
    file.path(out_dir, "figures", "heterogeneity_top_publishers_cohort_2020.png"),
    p_hetero_top,
    width = 9,
    height = 6,
    dpi = 300
  )
}

# ============================================================
# 15. Spillover test: non-listed journals in exposed publisher portfolios
# ============================================================
# This test asks whether never-listed journals belonging to publishers that had
# at least one EWL 2020 journal gain or lose publications after the reputation shock.
#
# Interpretation:
# - positive coefficient: possible within-publisher reallocation toward non-listed journals
# - negative coefficient: possible publisher-wide reputational contamination
# - null coefficient: no detectable spillover among never-listed journals

# Make sure publisher_group_simple also exists in journal_meta
journal_meta[, publisher_group_simple := fcase(
  publisher_group %in% c("Other international commercial publishers", "Other intl."), "Other intl.",
  publisher_group %in% c("Grey publishers", "Grey"), "Grey",
  publisher_group %in% c("Big 5 publishers", "Big 5"), "Big 5",
  publisher_group %in% c("Major society / university / non-profit publishers", "Societies / Univ."), "Societies / Univ.",
  publisher_group %in% c("Chinese publishers", "Chinese"), "Chinese",
  default = "Unknown"
)]

# Define publisher portfolio exposure
publisher_exposure_2020 <- journal_meta[
  ,
  .(
    publisher_has_2020 = as.integer(any(
      !is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR,
      na.rm = TRUE
    ))
  ),
  by = .(
    host_organization_name,
    publisher_family,
    publisher_group_simple
  )
]

# Use the same 2020 sample universe
panel_spill_2020 <- copy(panel_2020)

# Ensure publisher_group_simple exists in spillover panel
panel_spill_2020[, publisher_group_simple := fcase(
  publisher_group %in% c("Other international commercial publishers", "Other intl."), "Other intl.",
  publisher_group %in% c("Grey publishers", "Grey"), "Grey",
  publisher_group %in% c("Big 5 publishers", "Big 5"), "Big 5",
  publisher_group %in% c("Major society / university / non-profit publishers", "Societies / Univ."), "Societies / Univ.",
  publisher_group %in% c("Chinese publishers", "Chinese"), "Chinese",
  default = "Unknown"
)]

panel_spill_2020 <- merge(
  panel_spill_2020,
  publisher_exposure_2020,
  by = c("host_organization_name", "publisher_family", "publisher_group_simple"),
  all.x = TRUE
)

panel_spill_2020[
  is.na(publisher_has_2020),
  publisher_has_2020 := 0L
]

panel_spill_2020[
  ,
  spillover_2020 := as.integer(
    never_listed == 1 &
      publisher_has_2020 == 1
  )
]

# Restrict to never-listed journals only for the spillover estimate
panel_spill_controls <- panel_spill_2020[
  never_listed == 1
]

panel_spill_controls[
  ,
  spill_did_2020 := spillover_2020 * post_2022
]

panel_spill_controls[
  ,
  author_year_fe := interaction(author_id, year, drop = TRUE)
]

panel_spill_controls[
  ,
  total_pub_author_year := sum(n_pub, na.rm = TRUE),
  by = .(author_id, year)
]

panel_spill_controls[
  ,
  share_pub := fifelse(
    total_pub_author_year > 0,
    n_pub / total_pub_author_year,
    NA_real_
  )
]

# Diagnostics
spill_diag <- panel_spill_controls[
  ,
  .(
    n_obs = .N,
    n_authors = uniqueN(author_id),
    n_journals = uniqueN(source_id),
    n_exposed_journals = uniqueN(source_id[spillover_2020 == 1]),
    n_unexposed_journals = uniqueN(source_id[spillover_2020 == 0])
  )
]

fwrite(
  spill_diag,
  file.path(out_dir, "tables", "spillover_sample_diagnostics_2020.csv")
)

print(spill_diag)

# Spillover models
m_spill_2020_count <- feols(
  n_pub ~ spill_did_2020 +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_spill_controls
)

m_spill_2020_share <- feols(
  share_pub ~ spill_did_2020 +
    source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_spill_controls[!is.na(share_pub)]
)

save_model_html(
  list(
    "Spillover count" = m_spill_2020_count,
    "Spillover share" = m_spill_2020_share
  ),
  file.path(out_dir, "tables", "spillover_nonlisted_exposed_publishers_2020.html")
)

spill_results <- broom::tidy(
  m_spill_2020_count,
  conf.int = TRUE
) |>
  filter(term == "spill_did_2020")

fwrite(
  spill_results,
  file.path(out_dir, "tables", "spillover_count_result_2020.csv")
)


# ============================================================
# 15b. tests car résultats surprenants
# ============================================================

journal_group_diag <- panel_did_2020[
  treated_2020 == 1,
  .(
    n_obs = .N,
    n_authors = uniqueN(author_id)
  ),
  by = .(
    publisher_group_simple,
    publisher_family,
    source_display_name,
    source_id
  )
][order(publisher_group_simple, -n_obs)]

View(journal_group_diag)

fwrite(
  journal_group_diag,
  file.path(out_dir, "tables", "diag_journals_by_group.csv")
)


# leave one out test

treated_journals <- unique(
  panel_did_2020[
    treated_2020 == 1,
    source_id
  ]
)

loo_results <- rbindlist(
  lapply(treated_journals, function(jid){
    
    tmp <- panel_did_2020[source_id != jid]
    
    mod <- feols(
      n_pub ~ did_2020 +
        source_is_oa + log_impact_2yr + indexed_wos_scopus + i(main_domain) |
        author_year_fe + source_id,
      cluster = ~ author_id,
      data = tmp
    )
    
    est <- broom::tidy(mod)
    
    est <- est[est$term == "did_2020", ]
    
    data.table(
      removed_source = jid,
      estimate = est$estimate,
      conf_low = est$conf.low,
      conf_high = est$conf.high,
      p_value = est$p.value
    )
    
  }),
  fill = TRUE
)

fwrite(
  loo_results,
  file.path(out_dir, "tables", "leave_one_journal_out.csv")
)

loo_summary <- loo_results[
  ,
  .(
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE),
    min_estimate = min(estimate, na.rm = TRUE),
    max_estimate = max(estimate, na.rm = TRUE),
    pct_positive = mean(estimate > 0, na.rm = TRUE)
  )
]

print(loo_summary)


panel_did_2020[
  treated_2020 == 1,
  .(
    n_obs = .N,
    n_journals = uniqueN(source_id),
    n_authors = uniqueN(author_id)
  ),
  by = publisher_group_simple
][order(-n_journals)]


panel_did_2020[
  treated_2020 == 1 &
    publisher_group_simple == "Societies / Univ.",
  .(
    n_obs = .N,
    n_authors = uniqueN(author_id)
  ),
  by = .(
    publisher_family,
    source_display_name
  )
][order(-n_obs)]


###
top_publishers_to_test <- c(
  "MDPI",
  "Hindawi",
  "IEEE",
  "Springer Nature",
  "Elsevier",
  "Frontiers"
)

loo_pub_results <- rbindlist(
  lapply(top_publishers_to_test, function(pub){
    
    tmp <- panel_did_2020[
      !str_detect(publisher_family, fixed(pub, ignore_case = TRUE))
    ]
    
    mod <- feols(
      n_pub ~ did_2020 |
        author_year_fe + source_id,
      cluster = ~ author_id,
      data = tmp
    )
    
    est <- broom::tidy(mod)
    est <- est[est$term == "did_2020", ]
    
    data.table(
      removed_publisher = pub,
      estimate = est$estimate,
      conf_low = est$conf.low,
      conf_high = est$conf.high,
      p_value = est$p.value
    )
    
  }),
  fill = TRUE
)

print(loo_pub_results)

loo_summary <- loo_pub_results[
  ,
  .(
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE),
    min_estimate = min(estimate, na.rm = TRUE),
    max_estimate = max(estimate, na.rm = TRUE),
    pct_positive = mean(estimate > 0, na.rm = TRUE)
  )
]

print(loo_summary)

print(loo_pub_results[order(estimate)])

print(loo_pub_results[order(-estimate)])


# ============================================================
# 16. Staggered DiD robustness with Sun & Abraham
# ============================================================
# Caution: robustness check only.
# Later cohorts have limited post-treatment years, so 2024/2025 are excluded.

panel_staggered <- copy(panel)

# Keep never-listed journals and cohorts with enough post-treatment time
panel_staggered <- panel_staggered[
  is.na(ewl_year) | ewl_year <= 2023
]

# Effective treatment year for publication outcomes:
# list released in year G -> expected publication response from G + 2
panel_staggered[
  ,
  ewl_effective_year := fifelse(
    !is.na(ewl_year),
    as.integer(ewl_year) + 2L,
    NA_integer_
  )
]

panel_staggered <- panel_staggered[
  is.na(ewl_effective_year) | ewl_effective_year <= YEAR_MAX
]

panel_staggered[, author_year_fe := interaction(author_id, year, drop = TRUE)]

panel_staggered[
  ,
  total_pub_author_year := sum(n_pub, na.rm = TRUE),
  by = .(author_id, year)
]

panel_staggered[
  ,
  share_pub := fifelse(
    total_pub_author_year > 0,
    n_pub / total_pub_author_year,
    NA_real_
  )
]

m_sunab_count <- feols(
  n_pub ~ sunab(ewl_effective_year, year, ref.p = -1) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_staggered
)

m_sunab_share <- feols(
  share_pub ~ sunab(ewl_effective_year, year, ref.p = -1) |
    author_year_fe + source_id,
  cluster = ~ author_id,
  data = panel_staggered[!is.na(share_pub)]
)

save_model_html(
  list(
    "Staggered count" = m_sunab_count,
    "Staggered share" = m_sunab_share
  ),
  file.path(out_dir, "tables", "did_staggered_sunab_robustness.html")
)

png(
  file.path(out_dir, "figures", "sunab_staggered_count.png"),
  width = 1000,
  height = 700,
  res = 120
)
iplot(
  m_sunab_count,
  ref.line = 0,
  main = "Staggered DiD robustness: effective treatment year = EWL year + 2"
)
dev.off()

png(
  file.path(out_dir, "figures", "sunab_staggered_share.png"),
  width = 1000,
  height = 700,
  res = 120
)
iplot(
  m_sunab_share,
  ref.line = 0,
  main = "Staggered DiD robustness: share outcome"
)
dev.off()


# ============================================================
# 17. Main coefficient interpretation file
# ============================================================

interpret_coef <- function(model, term_candidates) {
  ct <- broom::tidy(model, conf.int = TRUE)
  
  for (tm in term_candidates) {
    row <- ct[ct$term == tm, ]
    
    if (nrow(row) > 0) {
      return(
        paste0(
          tm, " = ", round(row$estimate, 5),
          " [", round(row$conf.low, 5), "; ", round(row$conf.high, 5), "]",
          ", p = ", signif(row$p.value, 3)
        )
      )
    }
  }
  
  NA_character_
}

interpretation_lines <- c(
  "NSFC reputation trap DiD — v2 cohort-specific design",
  "",
  "Main design: journals first listed in EWL 2020 vs never-listed journals.",
  "Reason for delayed post: the first list was released in December 2020; because the outcome is publication year, 2021 is treated as transition/reference and post starts in 2022.",
  "",
  paste(
    "Main count FE:",
    interpret_coef(m_did_2020_count, c("did_2020"))
  ),
  paste(
    "Main share FE:",
    interpret_coef(m_did_2020_share, c("did_2020"))
  ),
  paste(
    "Scale-controlled:",
    interpret_coef(m_did_2020_scale, c("did_2020"))
  ),
  paste(
    "Poisson FE:",
    interpret_coef(m_did_2020_pois, c("did_2020"))
  ),
  "",
  "Timing robustness:",
  paste(
    "Post>=2021:",
    interpret_coef(m_did_2020_post2021, c("did_2021", "treated_2020:post_2021", "post_2021:treated_2020"))
  ),
  paste(
    "Post>=2023:",
    interpret_coef(m_did_2020_post2023, c("did_2023", "treated_2020:post_2023", "post_2023:treated_2020"))
  ),
  paste(
    "Exclude 2021:",
    interpret_coef(m_did_2020_exclude2021, c("did_2020", "treated_2020:post_2022", "post_2022:treated_2020"))
  ),
  "",
  "Generated outputs:",
  paste0("- ", file.path(out_dir, "tables", "did_main_cohort_2020_post2022.html")),
  paste0("- ", file.path(out_dir, "tables", "did_timing_robustness_cohort_2020.html")),
  paste0("- ", file.path(out_dir, "tables", "did_staggered_sunab_robustness.html")),
  paste0("- ", file.path(out_dir, "figures", "event_study_cohort_2020_count.png")),
  paste0("- ", file.path(out_dir, "figures", "event_study_cohort_2020_share.png")),
  paste0("- ", file.path(out_dir, "figures", "sunab_staggered_count.png")),
  paste0("- ", file.path(out_dir, "figures", "sunab_staggered_share.png"))
)

writeLines(
  interpretation_lines,
  file.path(out_dir, "tables", "main_interpretation_v2.txt")
)

message("\n============================================================")
message("DiD v2 completed.")
message("Outputs saved in: ", out_dir)
message("Main table: ", file.path(out_dir, "tables", "did_main_cohort_2020_post2022.html"))
message("Main event-study figure: ", file.path(out_dir, "figures", "event_study_cohort_2020_count.png"))
message("Staggered robustness figure: ", file.path(out_dir, "figures", "sunab_staggered_count.png"))
message("============================================================")
