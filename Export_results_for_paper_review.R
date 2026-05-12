# ============================================================
# Export_results_for_paper_review.R
# ============================================================
# Purpose
# -------
# This script gathers the core empirical outputs needed to rewrite the paper:
# descriptive tables, trajectory figures, regression tables, model terms, and
# a compact data bundle that can be shared/re-uploaded for interpretation.
#
# It is designed to be run AFTER your cleaned publication-level dataset and/or
# panel objects have been created. It is intentionally defensive: it checks
# columns, avoids integer/double data.table issues, and writes a manifest.
#
# Expected objects OR input files
# ------------------------------
# Preferred objects in memory:
#   - pub_dt: publication-level NSFC dataset, one row per publication/work
#   - journal_year_panel: journal-year panel with n_pub by source_id/year
#
# Optional object:
#   - panel_did_2020 or author_journal_year panel, if already available
#
# If objects are not in memory, set paths below and the script will read RDS/CSV.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(fixest)
  library(broom)
  library(modelsummary)
  library(readr)
})

# ============================================================
# 0. Parameters
# ============================================================

YEAR_MIN <- 2016L
YEAR_MAX <- 2025L
MAIN_COHORT_YEAR <- 2020L
EFFECTIVE_POST_YEAR <- 2022L
BASELINE_INDEX_YEAR <- 2019L

out_dir <- "outputs_paper_rewrite_bundle"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "models"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "data"), showWarnings = FALSE, recursive = TRUE)

# Optional input paths if objects are not already in memory
pub_dt_path <- NA_character_              # e.g. "data/nsfc_publications_clean.rds"
journal_year_panel_path <- NA_character_  # e.g. "data/journal_year_panel.rds"

# ============================================================
# 1. Helper functions
# ============================================================

need_cols <- function(dt, cols, object_name = deparse(substitute(dt))) {
  miss <- setdiff(cols, names(dt))
  if (length(miss) > 0) {
    stop(object_name, " is missing required columns: ", paste(miss, collapse = ", "))
  }
  invisible(TRUE)
}

first_non_na <- function(x) {
  y <- x[!is.na(x) & x != ""]
  if (length(y) == 0) return(NA_character_)
  as.character(y[1])
}

safe_num_mean <- function(x) as.numeric(mean(as.numeric(x), na.rm = TRUE))
safe_num_median <- function(x) as.numeric(median(as.numeric(x), na.rm = TRUE))
safe_num_sum <- function(x) as.numeric(sum(as.numeric(x), na.rm = TRUE))

save_model_html <- function(models, file) {
  modelsummary(
    models,
    output = file,
    stars = TRUE,
    statistic = "std.error",
    gof_omit = "IC|Log|F|RMSE"
  )
}

clean_publisher_group <- function(x) {
  fcase(
    x %in% c("Commercial OA challengers", "Grey", "Grey publishers"), "Commercial OA challengers",
    x %in% c("Big 5", "Big 5 publishers", "Top 5 publishers"), "Big 5",
    x %in% c("Societies / Univ.", "Major society / university / non-profit publishers"), "Societies / Univ.",
    x %in% c("Chinese", "Chinese publishers", "Chinese / domestic", "Domestic/regional", "Other (Domestic/Regional)"), "Chinese / domestic",
    x %in% c("Other international", "Other intl.", "Other (International)", "Other international commercial publishers"), "Other international",
    default = "Unknown"
  )
}

clean_publisher_top <- function(publisher_family, publisher_name = NA_character_) {
  fam <- paste(publisher_family, publisher_name)
  fcase(
    str_detect(fam, regex("MDPI|Multidisciplinary Digital Publishing", ignore_case = TRUE)), "MDPI",
    str_detect(fam, regex("Frontiers", ignore_case = TRUE)), "Frontiers",
    str_detect(fam, regex("Hindawi", ignore_case = TRUE)), "Hindawi",
    str_detect(fam, regex("IEEE", ignore_case = TRUE)), "IEEE",
    str_detect(fam, regex("Elsevier", ignore_case = TRUE)), "Elsevier",
    str_detect(fam, regex("Springer|Nature", ignore_case = TRUE)), "Springer Nature",
    str_detect(fam, regex("Wiley", ignore_case = TRUE)), "Wiley",
    str_detect(fam, regex("Taylor|Francis|T&F", ignore_case = TRUE)), "Taylor & Francis",
    str_detect(fam, regex("SAGE", ignore_case = TRUE)), "SAGE",
    default = "Other"
  )
}

# ============================================================
# 2. Load or verify data
# ============================================================

if (!exists("pub_dt")) {
  if (is.na(pub_dt_path)) {
    stop("Object pub_dt not found. Either create pub_dt before running this script or set pub_dt_path.")
  }
  pub_dt <- as.data.table(readRDS(pub_dt_path))
}

setDT(pub_dt)

# Required minimal publication-level columns
need_cols(pub_dt, c("year", "source_id"), "pub_dt")

# Harmonise year
pub_dt[, year := as.integer(year)]
pub_dt <- pub_dt[year >= YEAR_MIN & year <= YEAR_MAX]

# Build publisher_group_simple if missing
if (!"publisher_group_simple" %in% names(pub_dt)) {
  if ("publisher_group" %in% names(pub_dt)) {
    pub_dt[, publisher_group_simple := clean_publisher_group(publisher_group)]
  } else {
    warning("publisher_group_simple/publisher_group missing. Setting to Unknown.")
    pub_dt[, publisher_group_simple := "Unknown"]
  }
} else {
  pub_dt[, publisher_group_simple := clean_publisher_group(as.character(publisher_group_simple))]
}

# Build publisher_top if missing
if (!"publisher_top" %in% names(pub_dt)) {
  if ("publisher_family" %in% names(pub_dt)) {
    pname <- if ("publisher_name" %in% names(pub_dt)) pub_dt$publisher_name else NA_character_
    pub_dt[, publisher_top := clean_publisher_top(publisher_family, pname)]
  } else {
    pub_dt[, publisher_top := "Other"]
  }
}

# Harmonise EWL variables if available
if (!"ewl_year" %in% names(pub_dt)) pub_dt[, ewl_year := NA_integer_]
pub_dt[, ewl_year := suppressWarnings(as.integer(ewl_year))]

if (!"in_early_warning" %in% names(pub_dt)) {
  pub_dt[, in_early_warning := !is.na(ewl_year)]
}

# ============================================================
# 3. Build journal-year panel if needed
# ============================================================

if (!exists("journal_year_panel")) {
  if (!is.na(journal_year_panel_path)) {
    journal_year_panel <- as.data.table(readRDS(journal_year_panel_path))
  } else {
    message("Building journal_year_panel from pub_dt...")

    meta_cols <- intersect(
      c(
        "source_display_name", "publisher_group_simple", "publisher_top",
        "publisher_family", "publisher_name", "source_is_oa", "impact_2yr",
        "indexed_wos_scopus", "main_domain", "ewl_year", "in_early_warning"
      ),
      names(pub_dt)
    )

    journal_year_panel <- pub_dt[
      , c(list(n_pub = .N), lapply(.SD, first_non_na)),
      by = .(source_id, year),
      .SDcols = setdiff(meta_cols, c("ewl_year", "in_early_warning"))
    ]

    # Add EWL metadata separately to avoid type issues
    ewl_meta <- pub_dt[
      , .(
        ewl_year = suppressWarnings(min(ewl_year[!is.na(ewl_year)], na.rm = TRUE)),
        in_early_warning = as.integer(any(in_early_warning %in% TRUE | !is.na(ewl_year), na.rm = TRUE))
      ),
      by = source_id
    ]
    ewl_meta[is.infinite(ewl_year), ewl_year := NA_integer_]

    journal_year_panel <- merge(journal_year_panel, ewl_meta, by = "source_id", all.x = TRUE)
  }
}

setDT(journal_year_panel)
need_cols(journal_year_panel, c("source_id", "year", "n_pub"), "journal_year_panel")

journal_year_panel[, year := as.integer(year)]
journal_year_panel[, n_pub := as.numeric(n_pub)]

if (!"publisher_group_simple" %in% names(journal_year_panel)) {
  journal_year_panel[, publisher_group_simple := "Unknown"]
} else {
  journal_year_panel[, publisher_group_simple := clean_publisher_group(as.character(publisher_group_simple))]
}

if (!"publisher_top" %in% names(journal_year_panel)) {
  if ("publisher_family" %in% names(journal_year_panel)) {
    pname <- if ("publisher_name" %in% names(journal_year_panel)) journal_year_panel$publisher_name else NA_character_
    journal_year_panel[, publisher_top := clean_publisher_top(publisher_family, pname)]
  } else {
    journal_year_panel[, publisher_top := "Other"]
  }
}

if (!"ewl_year" %in% names(journal_year_panel)) journal_year_panel[, ewl_year := NA_integer_]
journal_year_panel[, ewl_year := suppressWarnings(as.integer(ewl_year))]

# Ensure balanced journal-year panel over YEAR_MIN:YEAR_MAX
all_sources <- unique(journal_year_panel$source_id)
bal_grid <- CJ(source_id = all_sources, year = YEAR_MIN:YEAR_MAX)
journal_year_panel <- merge(bal_grid, journal_year_panel, by = c("source_id", "year"), all.x = TRUE)
journal_year_panel[is.na(n_pub), n_pub := 0]

# Fill journal-level metadata within source_id
fill_cols <- setdiff(names(journal_year_panel), c("source_id", "year", "n_pub"))
for (cc in fill_cols) {
  journal_year_panel[, (cc) := {
    vals <- get(cc)
    non_na <- vals[!is.na(vals) & vals != ""]
    if (length(non_na) == 0) vals else fifelse(is.na(vals) | vals == "", non_na[1], vals)
  }, by = source_id]
}

journal_year_panel[, treated_2020 := as.integer(!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR)]
journal_year_panel[, never_listed := as.integer(is.na(ewl_year))]
journal_year_panel[, post_2022 := as.integer(year >= EFFECTIVE_POST_YEAR)]
journal_year_panel[, calendar_time := year - BASELINE_INDEX_YEAR]
journal_year_panel[, post_time_2022 := fifelse(year >= EFFECTIVE_POST_YEAR, year - EFFECTIVE_POST_YEAR + 1L, 0L)]
journal_year_panel[, event_time := year - ewl_year]
journal_year_panel[, post_effective := as.integer(!is.na(ewl_year) & year >= ewl_year + 2L)]
journal_year_panel[, post_effective_time := fifelse(post_effective == 1, year - (ewl_year + 2L) + 1L, 0L)]

# ============================================================
# 4. Descriptive tables and figures
# ============================================================

# 4.1 Sample diagnostics
sample_diag <- data.table(
  item = c(
    "Publication-level observations", "Journal-year observations", "Journals",
    "EWL journals", "EWL 2020 journals", "Never-listed journals",
    "Min year", "Max year"
  ),
  value = c(
    nrow(pub_dt),
    nrow(journal_year_panel),
    uniqueN(journal_year_panel$source_id),
    uniqueN(journal_year_panel[!is.na(ewl_year), source_id]),
    uniqueN(journal_year_panel[treated_2020 == 1, source_id]),
    uniqueN(journal_year_panel[never_listed == 1, source_id]),
    min(journal_year_panel$year, na.rm = TRUE),
    max(journal_year_panel$year, na.rm = TRUE)
  )
)
fwrite(sample_diag, file.path(out_dir, "tables", "sample_diagnostics.csv"))

# 4.2 NSFC total volume over time
volume_year <- pub_dt[, .(n_pub = .N), by = year][order(year)]
fwrite(volume_year, file.path(out_dir, "tables", "nsfc_volume_by_year.csv"))

p_volume <- ggplot(volume_year, aes(x = year, y = n_pub)) +
  geom_col(alpha = 0.55) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "The NSFC corpus expands sharply over time",
    subtitle = "Annual number of NSFC-funded publications",
    x = NULL,
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(out_dir, "figures", "figure_nsfc_volume_over_time.png"), p_volume, width = 10, height = 6, dpi = 300)

# 4.3 Market share by publisher group
market_group_year <- pub_dt[, .(n_pub = .N), by = .(year, publisher_group_simple)]
market_group_year[, share := n_pub / sum(n_pub), by = year]
fwrite(market_group_year, file.path(out_dir, "tables", "market_share_by_publisher_group.csv"))

p_market <- ggplot(
  market_group_year[publisher_group_simple != "Unknown"],
  aes(x = year, y = share, color = publisher_group_simple)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = EFFECTIVE_POST_YEAR, linetype = "dashed") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Market reallocation across publisher groups",
    subtitle = "Share of NSFC-funded publications by publisher group",
    x = NULL,
    y = "Share of publications",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "figure_market_reallocation_by_group.png"), p_market, width = 11, height = 7, dpi = 300)

# 4.4 EWL 2020 vs never listed: levels and index
jy_2020 <- journal_year_panel[treated_2020 == 1 | never_listed == 1]

jy_desc <- jy_2020[
  , .(
    total_pub = safe_num_sum(n_pub),
    mean_pub_per_journal = safe_num_mean(n_pub),
    median_pub_per_journal = safe_num_median(n_pub),
    n_journals = as.integer(uniqueN(source_id))
  ),
  by = .(year, group = fifelse(treated_2020 == 1, "EWL 2020 cohort", "Never listed"))
]

base_index <- jy_desc[year == BASELINE_INDEX_YEAR, .(group, base_total_pub = total_pub)]
jy_desc <- merge(jy_desc, base_index, by = "group", all.x = TRUE)
jy_desc[, index_2019 := fifelse(base_total_pub > 0, 100 * total_pub / base_total_pub, NA_real_)]
fwrite(jy_desc, file.path(out_dir, "tables", "journal_year_descriptive_ewl2020_vs_never.csv"))

p_jy_levels <- ggplot(jy_desc, aes(x = year, y = total_pub, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = EFFECTIVE_POST_YEAR, linetype = "dashed") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Publication volumes: EWL 2020 cohort vs never-listed journals",
    subtitle = "Dotted line: release year; dashed line: effective post period from 2022",
    x = NULL,
    y = "Total NSFC-funded publications",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "figure_jy_levels_ewl2020_vs_never.png"), p_jy_levels, width = 10, height = 6.5, dpi = 300)

p_jy_index <- ggplot(jy_desc, aes(x = year, y = index_2019, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = EFFECTIVE_POST_YEAR, linetype = "dashed") +
  labs(
    title = "Indexed publication trajectories",
    subtitle = "Index: 2019 = 100",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "figure_jy_index_2019_ewl2020_vs_never.png"), p_jy_index, width = 10, height = 6.5, dpi = 300)

# 4.5 Cohort event summary
jy_listed <- journal_year_panel[!is.na(ewl_year) & ewl_year <= 2024]
jy_listed <- jy_listed[event_time >= -6 & event_time <= 5]

cohort_event_summary <- jy_listed[
  , .(
    mean_pub = safe_num_mean(n_pub),
    median_pub = safe_num_median(n_pub),
    total_pub = safe_num_sum(n_pub),
    n_journals = as.integer(uniqueN(source_id))
  ),
  by = .(ewl_year, event_time)
]
fwrite(cohort_event_summary, file.path(out_dir, "tables", "cohort_event_summary_listed_journals.csv"))

p_cohort <- ggplot(
  cohort_event_summary[ewl_year %in% c(2020, 2021, 2023, 2024)],
  aes(x = event_time, y = mean_pub, color = factor(ewl_year))
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2, linetype = "dashed") +
  labs(
    title = "Publication intensity around CAS listing cohorts",
    subtitle = "Event time 0 = listing year; dashed line = expected publication response around +2",
    x = "Years relative to listing",
    y = "Mean publications per listed journal",
    color = "EWL year"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "figure_cohort_event_publication_intensity.png"), p_cohort, width = 10, height = 6.5, dpi = 300)

# 4.6 Top publishers trajectories
keep_publishers <- c("MDPI", "Frontiers", "Hindawi", "IEEE", "Elsevier", "Springer Nature", "Wiley", "Taylor & Francis", "SAGE")
top_pub_year <- pub_dt[publisher_top %in% keep_publishers, .(n_pub = .N), by = .(year, publisher_top)]
base_top <- top_pub_year[year == BASELINE_INDEX_YEAR, .(publisher_top, base_n = n_pub)]
top_pub_year <- merge(top_pub_year, base_top, by = "publisher_top", all.x = TRUE)
top_pub_year[, index_2019 := fifelse(base_n > 0, 100 * n_pub / base_n, NA_real_)]
fwrite(top_pub_year, file.path(out_dir, "tables", "top_publishers_yearly_trajectories.csv"))

p_top_levels <- ggplot(top_pub_year, aes(x = year, y = n_pub, color = publisher_top)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = EFFECTIVE_POST_YEAR, linetype = "dashed") +
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

ggsave(file.path(out_dir, "figures", "figure_top_publishers_publication_levels.png"), p_top_levels, width = 11, height = 7, dpi = 300)

p_top_index <- ggplot(top_pub_year, aes(x = year, y = index_2019, color = publisher_top)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_vline(xintercept = MAIN_COHORT_YEAR, linetype = "dotted") +
  geom_vline(xintercept = EFFECTIVE_POST_YEAR, linetype = "dashed") +
  labs(
    title = "Indexed trajectories by major publisher",
    subtitle = "Index: 2019 = 100",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "figure_top_publishers_index_2019.png"), p_top_index, width = 11, height = 7, dpi = 300)

# ============================================================
# 5. Regression models: trajectory interruption
# ============================================================

reg_dt <- journal_year_panel[treated_2020 == 1 | never_listed == 1]
reg_dt[, log_count := log1p(n_pub)]
reg_dt[, asinh_count := asinh(n_pub)]

# Table 1: 2020 cohort vs never-listed, interrupted trajectory
m_traj_log <- feols(
  log_count ~ treated_2020:calendar_time + treated_2020:post_2022 + treated_2020:post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = reg_dt
)

m_traj_asinh <- feols(
  asinh_count ~ treated_2020:calendar_time + treated_2020:post_2022 + treated_2020:post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = reg_dt
)

m_traj_pois <- fepois(
  n_pub ~ treated_2020:calendar_time + treated_2020:post_2022 + treated_2020:post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = reg_dt
)

save_model_html(
  list("log(1+count)" = m_traj_log, "asinh(count)" = m_traj_asinh, "Poisson" = m_traj_pois),
  file.path(out_dir, "tables", "table1_trajectory_interruption_models.html")
)

traj_terms <- rbindlist(list(
  broom::tidy(m_traj_log, conf.int = TRUE) |> mutate(model = "log_count"),
  broom::tidy(m_traj_asinh, conf.int = TRUE) |> mutate(model = "asinh_count"),
  broom::tidy(m_traj_pois, conf.int = TRUE) |> mutate(model = "poisson")
), fill = TRUE)
fwrite(traj_terms, file.path(out_dir, "tables", "table1_trajectory_interruption_terms.csv"))

# Table 2: Listed journals only, cohort-relative interruption
listed_only <- journal_year_panel[!is.na(ewl_year) & ewl_year <= 2024]
listed_only <- listed_only[event_time >= -6 & event_time <= 5]
listed_only[, log_count := log1p(n_pub)]

m_listed_only <- feols(
  log_count ~ event_time + post_effective + post_effective_time |
    source_id + ewl_year,
  cluster = ~ source_id,
  data = listed_only
)

save_model_html(
  list("Listed journals only" = m_listed_only),
  file.path(out_dir, "tables", "table2_listed_journals_only.html")
)
fwrite(broom::tidy(m_listed_only, conf.int = TRUE), file.path(out_dir, "tables", "table2_listed_journals_only_terms.csv"))

# Table 3: Spillover among never-listed journals in exposed portfolios
# Define exposed publisher portfolios: publishers with at least one EWL 2020 journal.
if (!"publisher_family" %in% names(journal_year_panel)) {
  warning("publisher_family missing; spillover model will use publisher_top only, which is less precise.")
  journal_year_panel[, publisher_family := publisher_top]
}

portfolio_exposure <- unique(journal_year_panel[, .(source_id, publisher_family, publisher_top, ewl_year)])[
  , .(publisher_has_2020 = as.integer(any(!is.na(ewl_year) & ewl_year == MAIN_COHORT_YEAR, na.rm = TRUE))),
  by = .(publisher_family, publisher_top)
]

spill_dt <- merge(journal_year_panel, portfolio_exposure, by = c("publisher_family", "publisher_top"), all.x = TRUE)
spill_dt[is.na(publisher_has_2020), publisher_has_2020 := 0L]
spill_dt <- spill_dt[never_listed == 1]
spill_dt[, spillover_control := as.integer(publisher_has_2020 == 1)]
spill_dt[, spill_post_2022 := spillover_control * post_2022]
spill_dt[, spill_post_time_2022 := spillover_control * post_time_2022]
spill_dt[, log_count := log1p(n_pub)]

spill_diag <- spill_dt[
  , .(
    n_obs = .N,
    n_journals = uniqueN(source_id),
    total_pub = safe_num_sum(n_pub),
    mean_pub = safe_num_mean(n_pub)
  ),
  by = spillover_control
]
fwrite(spill_diag, file.path(out_dir, "tables", "table3_spillover_sample_diagnostics.csv"))

m_spill <- feols(
  log_count ~ spillover_control:calendar_time + spill_post_2022 + spill_post_time_2022 |
    source_id + year,
  cluster = ~ source_id,
  data = spill_dt
)

save_model_html(
  list("Never-listed journals in exposed portfolios" = m_spill),
  file.path(out_dir, "tables", "table3_portfolio_spillover.html")
)
fwrite(broom::tidy(m_spill, conf.int = TRUE), file.path(out_dir, "tables", "table3_portfolio_spillover_terms.csv"))

# Table 4 optional: heterogeneity by publisher group
het_dt <- reg_dt[publisher_group_simple != "Unknown"]
het_dt[, publisher_group_simple := factor(
  publisher_group_simple,
  levels = c("Other international", "Commercial OA challengers", "Big 5", "Societies / Univ.", "Chinese / domestic")
)]

m_het_group <- feols(
  log_count ~ i(publisher_group_simple, treated_2020 * post_2022, ref = "Other international") +
    i(publisher_group_simple, treated_2020 * post_time_2022, ref = "Other international") |
    source_id + year,
  cluster = ~ source_id,
  data = het_dt
)

save_model_html(
  list("Publisher-group trajectory heterogeneity" = m_het_group),
  file.path(out_dir, "tables", "table4_optional_publisher_group_heterogeneity.html")
)
fwrite(broom::tidy(m_het_group, conf.int = TRUE), file.path(out_dir, "tables", "table4_optional_publisher_group_heterogeneity_terms.csv"))

het_diag <- het_dt[
  treated_2020 == 1,
  .(
    n_journals = uniqueN(source_id),
    total_pub = safe_num_sum(n_pub),
    mean_pub = safe_num_mean(n_pub)
  ),
  by = publisher_group_simple
]
fwrite(het_diag, file.path(out_dir, "tables", "table4_optional_heterogeneity_group_diagnostics.csv"))

# ============================================================
# 6. Compact bundle for later interpretation/re-upload
# ============================================================

bundle_files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
manifest <- data.table(
  file = bundle_files,
  size_bytes = file.info(bundle_files)$size,
  modified = as.character(file.info(bundle_files)$mtime)
)
fwrite(manifest, file.path(out_dir, "manifest.csv"))

saveRDS(
  list(
    sample_diag = sample_diag,
    volume_year = volume_year,
    market_group_year = market_group_year,
    jy_desc = jy_desc,
    cohort_event_summary = cohort_event_summary,
    top_pub_year = top_pub_year,
    trajectory_terms = traj_terms,
    spill_diag = spill_diag,
    heterogeneity_diag = het_diag
  ),
  file.path(out_dir, "data", "paper_rewrite_core_outputs.rds"),
  compress = "xz"
)


bundle <- readRDS("outputs_paper_rewrite_bundle/data/paper_rewrite_core_outputs.rds")

out_dir <- "paper_rewrite_export"
dir.create(out_dir, showWarnings = FALSE)

for (nm in names(bundle)) {
  obj <- bundle[[nm]]
  
  if (is.data.frame(obj) || data.table::is.data.table(obj)) {
    readr::write_csv(
      as.data.frame(obj),
      file.path(out_dir, paste0(nm, ".csv"))
    )
  }
}

saveRDS(names(bundle), file.path(out_dir, "bundle_names.rds"))

zip(
  zipfile = "paper_rewrite_export.zip",
  files = list.files(out_dir, full.names = TRUE)
)

message("\n============================================================")
message("Paper rewrite bundle completed.")
message("Outputs saved in: ", out_dir)
message("Main figures: ", file.path(out_dir, "figures"))
message("Main tables: ", file.path(out_dir, "tables"))
message("Manifest: ", file.path(out_dir, "manifest.csv"))
message("============================================================\n")
