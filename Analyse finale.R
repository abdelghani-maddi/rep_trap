# =========================================================
# NSFC — CONSTRUCTION DU DATASET ANALYTIQUE FINAL
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(data.table)
library(dplyr)
library(httr)
library(jsonlite)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(patchwork)
library(data.table)
library(tidyverse)
library(questionr)

# ==============================
# 1) CHARGEMENT
# ==============================

file_in <- "data_nsfc/works_nsfc_1997_2025_slim_final.rds"

nsfc <- readRDS(file_in)
setDT(nsfc)

cat("Dimensions initiales :", dim(nsfc), "\n")

# ==============================
# 2) DIAGNOSTICS AVANT FILTRAGE
# ==============================

# ---- Types de documents ----
tab_type_before <- nsfc %>%
  count(type, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    pct = scales::percent(pct, accuracy = 0.1)
  )

print(tab_type_before)

# ---- ISSN ----
tab_issn <- nsfc %>%
  summarise(
    n_total = n(),
    n_with_issn = sum(!is.na(issn_l)),
    n_without_issn = sum(is.na(issn_l)),
    pct_with_issn = n_with_issn / n_total
  )

print(tab_issn)

# ---- Rétractations ----
tab_retraction <- nsfc %>%
  summarise(
    n_total = n(),
    n_retracted = sum(is_retracted == TRUE, na.rm = TRUE),
    pct_retracted = n_retracted / n_total
  )

print(tab_retraction)

# ==============================
# 3) FILTRAGE PRINCIPAL
# ==============================

nsfc_clean <- nsfc %>%
  filter(
    !is.na(issn_l),        # proxy articles
    is_retracted == FALSE  # exclure rétractations
  )

cat("Dimensions après filtrage :", dim(nsfc_clean), "\n")

# ==============================
# 4) DIAGNOSTICS APRÈS FILTRAGE
# ==============================

tab_type_after <- nsfc_clean %>%
  count(type, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    pct = scales::percent(pct, accuracy = 0.1)
  )

print(tab_type_after)

# ==============================
# 5) TABLEAU COMPARATIF AVANT / APRÈS
# ==============================

tab_compare <- full_join(
  tab_type_before %>% rename(n_before = n),
  tab_type_after %>% rename(n_after = n),
  by = "type"
) %>%
  mutate(
    n_before = coalesce(n_before, 0),
    n_after = coalesce(n_after, 0),
    pct_before = n_before / sum(n_before),
    pct_after = n_after / sum(n_after)
  ) %>%
  arrange(desc(n_before))

print(tab_compare)

# ==============================
# 6) STAT RÉSUMÉES POUR ANNEXE
# ==============================

summary_stats <- data.frame(
  step = c("Initial", "After ISSN + no retraction"),
  n_obs = c(nrow(nsfc), nrow(nsfc_clean))
)

print(summary_stats)

# ==============================
# 7) SAUVEGARDE DATASET FINAL
# ==============================

file_out <- "data_nsfc/nsfc_final_clean.rds"

saveRDS(nsfc_clean, file_out, compress = "xz")

cat("✔ Dataset final sauvegardé :", file_out, "\n")


# =========================================================
# NSFC — DATA DESCRIPTION SECTION
# =========================================================
# Objectif :
# 1. Situer NSFC dans l'ensemble mondial / Chine
# 2. Décrire le corpus NSFC propre
# 3. Produire des figures publication-ready
# =========================================================


# ==============================
# 1) CHARGEMENT DU DATASET FINAL
# ==============================

nsfc <- nsfc_final_clean
rm(nsfc_final_clean)
cat("Dimensions NSFC clean :", dim(nsfc), "\n")

# ==============================
# 2) PARAMÈTRES
# ==============================

year_min <- 2000
year_max <- 2025

# On restreint aux années utiles pour les figures
nsfc_desc <- nsfc[
  publication_year >= year_min & publication_year <= year_max
]

# ==============================
# 3) HELPERS OPENALEX API
# ==============================

base_url <- "https://api.openalex.org/works"
group_by_field <- "publication_year"
per_page <- 200

get_counts <- function(country_code = NULL, is_oa = NULL) {
  
  filters <- c()
  
  if (!is.null(country_code)) {
    filters <- c(filters, paste0("institutions.country_code:", country_code))
  }
  
  if (!is.null(is_oa)) {
    filters <- c(filters, paste0("is_oa:", tolower(as.character(is_oa))))
  }
  
  filter_query <- paste(filters, collapse = ",")
  
  url <- paste0(
    base_url,
    "?group_by=", group_by_field,
    "&per_page=", per_page
  )
  
  if (nzchar(filter_query)) {
    url <- paste0(url, "&filter=", URLencode(filter_query, reserved = TRUE))
  }
  
  cat("Requesting:", url, "\n")
  
  r <- httr::GET(url)
  httr::stop_for_status(r)
  
  txt <- httr::content(r, as = "text", encoding = "UTF-8")
  js <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
  
  if (is.null(js$group_by) || nrow(js$group_by) == 0) {
    return(tibble(
      year = integer(),
      count = integer()
    ))
  }
  
  tibble(
    year = as.integer(js$group_by$key),
    count = as.integer(js$group_by$count)
  )
}

# ==============================
# 4) MONDE / CHINE — OA PAR ANNÉE
# ==============================

# ----- World
world_total <- get_counts()
world_oa <- get_counts(is_oa = TRUE)

results_world <- world_total %>%
  left_join(world_oa, by = "year", suffix = c("_total", "_oa")) %>%
  mutate(
    count_oa = coalesce(count_oa, 0L),
    oa_rate = count_oa / count_total,
    group = "World"
  )

# ----- China
china_total <- get_counts(country_code = "CN")
china_oa <- get_counts(country_code = "CN", is_oa = TRUE)

results_china <- china_total %>%
  left_join(china_oa, by = "year", suffix = c("_total", "_oa")) %>%
  mutate(
    count_oa = coalesce(count_oa, 0L),
    oa_rate = count_oa / count_total,
    group = "China"
  )

# ==============================
# 5) NSFC — OA PAR ANNÉE
# ==============================

results_nsfc <- nsfc_desc %>%
  as_tibble() %>%
  group_by(publication_year) %>%
  summarise(
    count_total = n(),
    count_oa = sum(is_oa, na.rm = TRUE),
    oa_rate = count_oa / count_total,
    .groups = "drop"
  ) %>%
  rename(year = publication_year) %>%
  mutate(group = "NSFC")

# ==============================
# 6) FUSION DES 3 SÉRIES
# ==============================

results_oa_all <- bind_rows(
  results_world,
  results_china,
  results_nsfc
) %>%
  filter(year >= year_min, year <= year_max)

# ==============================
# 7) THÈME ET PALETTES
# ==============================

theme_pub <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0, colour = "#1A1A1A"),
      plot.subtitle = element_text(size = base_size, hjust = 0, colour = "#4D4D4D"),
      plot.caption = element_text(size = base_size - 2, colour = "#666666"),
      
      axis.title = element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = element_text(size = base_size - 1, colour = "#262626"),
      
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFEFEF", linewidth = 0.30),
      
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      
      strip.text = element_text(face = "bold", colour = "#1A1A1A"),
      plot.margin = margin(12, 14, 12, 12)
    )
}

pal_groups <- c(
  "World" = "#7A7A7A",
  "China" = "#B04A5A",
  "NSFC"  = "#1F4E79"
)

pal_oa_status <- c(
  "gold"   = "#D4A017",
  "green"  = "#4C9F70",
  "hybrid" = "#2C7FB8",
  "bronze" = "#B87333",
  "closed" = "#6E6E6E",
  "diamond" = "#2C7F25"
)

# ==============================
# 8) FIGURE 1
# OA SHARE — WORLD / CHINA / NSFC
# ==============================

g1 <- ggplot(results_oa_all, aes(x = year, y = oa_rate, colour = group)) +
  geom_line(linewidth = 1.35, lineend = "round") +
  geom_point(size = 2.4) +
  scale_colour_manual(values = pal_groups) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = seq(year_min, year_max, by = 2),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Open Access uptake in NSFC-funded publications",
    subtitle = "Annual share of Open Access publications in the World, China, and the NSFC corpus",
    x = NULL,
    y = "Open Access share",
    caption = "World and China aggregates are retrieved from OpenAlex grouped counts; NSFC is computed from the cleaned analytical corpus."
  ) +
  theme_pub()

# ==============================
# 9) FIGURE 2
# ANNUAL VOLUME OF NSFC PUBLICATIONS
# ==============================

fig2_data <- nsfc_desc %>%
  as_tibble() %>%
  count(publication_year, name = "n")

g2 <- ggplot(fig2_data, aes(x = publication_year, y = n)) +
  geom_col(width = 0.82, fill = "#A9B8C8") +
  geom_line(linewidth = 0.9, colour = "#1F4E79") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  scale_x_continuous(
    breaks = seq(year_min, year_max, by = 2),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "The NSFC corpus expands sharply over time",
    subtitle = "Annual number of NSFC-funded journal-indexed, non-retracted publications",
    x = NULL,
    y = "Number of publications"
  ) +
  theme_pub()

# ==============================
# 10) FIGURE 3
# OA STATUS COMPOSITION OVER TIME (NSFC ONLY)
# ==============================

fig3_data <- nsfc_desc %>%
  as_tibble() %>%
  mutate(
    oa_status2 = case_when(
      is.na(oa_status) ~ "unknown",
      TRUE ~ as.character(oa_status)
    )
  ) %>%
  count(publication_year, oa_status2) %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

g3 <- ggplot(fig3_data, aes(x = publication_year, y = share, fill = oa_status2)) +
  geom_area(position = "fill", alpha = 0.98, colour = "white", linewidth = 0.25) +
  scale_fill_manual(values = pal_oa_status) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = seq(year_min, year_max, by = 2),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "The Open Access composition of NSFC publications shifts over time",
    subtitle = "Annual composition of NSFC publications by Open Access status",
    x = NULL,
    y = "Share of publications"
  ) +
  theme_pub()

# ==============================
# 11) FIGURE 4
# DISCIPLINARY PROFILE OF NSFC
# ==============================

fig4_data <- nsfc_desc %>%
  as_tibble() %>%
  mutate(
    main_domain = if_else(is.na(main_domain), "Unknown", main_domain)
  ) %>%
  count(main_domain, sort = TRUE) %>%
  slice_head(n = 8) %>%
  mutate(
    share = n / sum(n),
    label = percent(share, accuracy = 0.1),
    main_domain = fct_reorder(main_domain, n)
  ) %>%
  filter(main_domain != "Unknown")

g4 <- ggplot(fig4_data, aes(x = main_domain, y = n)) +
  geom_col(width = 0.72, fill = "#1F4E79") +
  geom_text(
    aes(label = label),
    hjust = -0.05,
    size = 4.1,
    colour = "#222222"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "The NSFC publication portfolio, by domains",
    subtitle = "Top domains in the cleaned NSFC corpus",
    x = NULL,
    y = "Number of publications"
  ) +
  theme_pub()

# ==============================
# 12) FIGURE 5
# TEAM SIZE DISTRIBUTION
# ==============================

fig5_data <- nsfc_desc %>%
  as_tibble() %>%
  mutate(
    team_size_group = case_when(
      is.na(n_authors) ~ "Unknown",
      n_authors == 1 ~ "1",
      n_authors == 2 ~ "2",
      n_authors == 3 ~ "3",
      n_authors == 4 ~ "4",
      n_authors == 5 ~ "5",
      n_authors >= 6 & n_authors <= 10 ~ "6–10",
      n_authors > 10 ~ "11+",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(team_size_group) %>%
  mutate(
    team_size_group = factor(
      team_size_group,
      levels = c("1", "2", "3", "4", "5", "6–10", "11+", "Unknown")
    ),
    share = n / sum(n),
    label = percent(share, accuracy = 0.1)
  ) %>%
  filter(team_size_group != "Unknown")

g5 <- ggplot(fig5_data, aes(x = team_size_group, y = n)) +
  geom_col(width = 0.72, fill = "#6E6E6E") +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 4.0,
    colour = "#222222"
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "The NSFC corpus is overwhelmingly composed of collaborative publications",
    subtitle = "Distribution of publications by number of authors",
    x = "Number of authors",
    y = "Number of publications"
  ) +
  theme_pub()

# ==============================
# 13) FIGURE 6
# INTERNATIONALISATION — NUMBER OF COUNTRIES
# ==============================

fig6_data <- nsfc_desc %>%
  as_tibble() %>%
  mutate(
    country_group = case_when(
      is.na(n_countries) ~ "Unknown",
      n_countries == 1 ~ "1 country",
      n_countries == 2 ~ "2 countries",
      n_countries >= 3 ~ "3+ countries",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(country_group) %>%
  mutate(
    country_group = factor(
      country_group,
      levels = c("1 country", "2 countries", "3+ countries", "Unknown")
    ),
    share = n / sum(n),
    label = percent(share, accuracy = 0.1)
  ) %>%
  filter(country_group != "Unknown")

g6 <- ggplot(fig6_data, aes(x = country_group, y = n)) +
  geom_col(width = 0.66, fill = "#B04A5A") +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 4.0,
    colour = "#222222"
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Most NSFC-funded publications remain nationally anchored",
    subtitle = "Distribution by number of countries represented in author affiliations",
    x = NULL,
    y = "Number of publications"
  ) +
  theme_pub()

# ==============================
# 14) AFFICHAGE
# ==============================

print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
print(g6)

# ==============================
# 15) PANNEAUX COMBINÉS
# ==============================

panel_positioning <- g1 / g2
panel_characteristics <- (g3 | g4) / (g5 | g6)

print(panel_positioning)
print(panel_characteristics)

# ==============================
# 16) EXPORT
# ==============================

dir.create("figures_nsfc_description", showWarnings = FALSE)

ggsave("figures_nsfc_description/panel_positioning.png", panel_positioning,
       width = 11, height = 6.5, dpi = 350, bg = "white")
ggsave("figures_nsfc_description/panel_characteristics.png", panel_characteristics,
       width = 19, height = 6.5, dpi = 350, bg = "white")

ggsave("figures_nsfc_description/Figure1_oa_world_china_nsfc.png", g1,
       width = 11, height = 6.5, dpi = 350, bg = "white")
ggsave("figures_nsfc_description/Figure2_nsfc_volume_over_time.png", g2,
       width = 11, height = 6.3, dpi = 350, bg = "white")
ggsave("figures_nsfc_description/Figure3_nsfc_oa_status_composition.png", g3,
       width = 11, height = 6.5, dpi = 350, bg = "white")
ggsave("figures_nsfc_description/Figure4_nsfc_domains.png", g4,
       width = 10.5, height = 6.5, dpi = 350, bg = "white")
ggsave("figures_nsfc_description/Figure5_nsfc_team_size.png", g5,
       width = 10.5, height = 6.2, dpi = 350, bg = "white")
ggsave("figures_nsfc_description/Figure6_nsfc_internationalisation.png", g6,
       width = 10.5, height = 6.2, dpi = 350, bg = "white")

# ==============================
# 17) TABLEAUX DE CONTRÔLE
# ==============================

tab_oa <- nsfc_desc %>%
  as_tibble() %>%
  count(oa_status, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1))

tab_domains <- nsfc_desc %>%
  as_tibble() %>%
  count(main_domain, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1))

tab_team <- nsfc_desc %>%
  as_tibble() %>%
  summarise(
    n = n(),
    mean_authors = mean(n_authors, na.rm = TRUE),
    median_authors = median(n_authors, na.rm = TRUE),
    p90_authors = quantile(n_authors, 0.90, na.rm = TRUE)
  )

tab_countries <- nsfc_desc %>%
  as_tibble() %>%
  summarise(
    mean_countries = mean(n_countries, na.rm = TRUE),
    pct_single_country = mean(n_countries == 1, na.rm = TRUE),
    pct_multi_country = mean(n_countries >= 2, na.rm = TRUE)
  )

print(tab_oa)
print(tab_domains)
print(tab_team)
print(tab_countries)


#############################
#############################
# =========================================================
# NSFC × SOURCES OPENALEX
# Publisher segmentation and publishing practices
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(purrr)
library(stringr)
library(patchwork)

# ==============================
# 1) CHARGEMENT
# ==============================

nsfc <- readRDS("data_nsfc/nsfc_final_clean.rds")
jours_all <- readRDS("data_nsfc/sources_openalex.rds")

setDT(nsfc)
setDT(jours_all)

cat("NSFC:", dim(nsfc), "\n")
cat("Sources:", dim(jours_all), "\n")

# ==============================
# 2) HELPERS
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

extract_stat <- function(x, stat_name) {
  if (is.null(x)) return(NA_real_)
  
  # cas list named
  if (!is.null(names(x)) && stat_name %in% names(x)) {
    return(as.numeric(x[[stat_name]]))
  }
  
  # cas vecteur nommé
  if (!is.null(names(x)) && stat_name %in% names(x)) {
    return(as.numeric(x[stat_name]))
  }
  
  NA_real_
}

theme_pub <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0, colour = "#1A1A1A"),
      plot.subtitle = element_text(size = base_size, hjust = 0, colour = "#4D4D4D"),
      plot.caption = element_text(size = base_size - 2, colour = "#666666"),
      axis.title = element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = element_text(size = base_size - 1, colour = "#262626"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFEFEF", linewidth = 0.30),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(12, 14, 12, 12)
    )
}

pal_publishers <- c(
  "Grey publishers" = "#C44E52",
  "Big 5 publishers" = "#1F4E79",
  "Chinese publishers" = "#4C9F70",
  "Other international publishers" = "#7A7A7A",
  "Unknown / unmatched" = "#C9C9C9"
)

# ==============================
# 3) APLATIR LA TABLE SOURCES
# ==============================

sources_flat <- jours_all %>%
  as_tibble() %>%
  transmute(
    source_id = id,
    issn_l = issn_l,
    source_name_oa = display_name,
    host_organization = host_organization,
    host_organization_name = host_organization_name,
    source_country_code = country_code,
    source_type = type,
    source_is_oa = is_oa,
    source_is_in_doaj = is_in_doaj,
    apc_usd = apc_usd,
    source_works_count = works_count,
    source_cited_by_count = cited_by_count,
    impact_2yr = map_dbl(summary_stats, ~ extract_stat(.x, "2yr_mean_citedness")),
    h_index = map_dbl(summary_stats, ~ extract_stat(.x, "h_index"))
  ) %>%
  distinct(issn_l, .keep_all = TRUE)

# contrôle
glimpse(sources_flat)

# ==============================
# 4) JOINTURE PROPRE SUR ISSN_L
# ==============================

all_nsfc_augmented <- nsfc %>%
  as_tibble() %>%
  left_join(sources_flat, by = "issn_l")

# Taux de matching
match_stats <- all_nsfc_augmented %>%
  summarise(
    n_total = n(),
    n_matched = sum(!is.na(source_name_oa)),
    pct_matched = n_matched / n_total
  )

print(match_stats)

# ==============================
# 5) CLASSIFICATION DES ÉDITEURS
# ==============================

library(dplyr)
library(stringr)

# ==============================
# 1) NORMALISATION DU NOM D'ÉDITEUR
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_publisher_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\bthe\\b", " ") %>%
    str_replace_all("\\bltd\\b|\\blimited\\b|\\bllc\\b|\\binc\\b|\\bco\\b|\\bcompany\\b", " ") %>%
    str_replace_all("\\bbv\\b|\\bb v\\b|\\bplc\\b|\\buk\\b|\\bag\\b|\\bkg\\b|\\bgmbh\\b|\\bsrl\\b|\\bsro\\b|\\bpte\\b|\\bsdn\\b|\\bbhd\\b", " ") %>%
    str_replace_all("\\bpublishing\\b|\\bpublisher\\b|\\bpublications\\b|\\bpress\\b", " ") %>%
    str_replace_all("\\bservices\\b", " ") %>%
    str_replace_all("\\bon behalf of\\b", " ") %>%
    str_squish()
}

has_chinese_script <- function(x) {
  str_detect(x %||% "", "[\\p{Han}]")
}

has_china_keyword <- function(x) {
  x2 <- str_to_lower(x %||% "")
  str_detect(
    x2,
    "\\bchina\\b|\\bchinese\\b|\\bbeijing\\b|\\bshanghai\\b|\\bshaghai\\b|\\bwuhan\\b|\\bnanjing\\b|\\bpeking\\b|\\btsinghua\\b|\\bcas\\b|academy of sciences"
  )
}

is_numeric_like <- function(x) {
  str_detect(x %||% "", "^\\d+$")
}

# ==============================
# 2) RECODAGE VERS FAMILLE D'ÉDITEUR
# ==============================

classify_publisher_family <- function(x_raw) {
  
  x <- normalize_publisher_name(x_raw)
  
  case_when(
    is.na(x) | x == "" ~ "Unknown",
    str_detect(x, "^\\d+$") ~ "Unknown",
    str_detect(x, "^aaas\\d+$") ~ "AAAS",
    
    # ===== BIG 5 =====
    str_detect(x, "\\belsevier\\b|\\bkeai\\b") ~ "Elsevier",
    str_detect(x, "\\bspringer\\b|\\bnature\\b|\\bbiomed central\\b|\\bbmc\\b|\\bpalgrave\\b") ~ "Springer Nature",
    str_detect(x, "\\bwiley\\b|\\bblackwell\\b") ~ "Wiley",
    str_detect(x, "\\btaylor\\b|\\bfrancis\\b|\\binforma\\b|\\broutledge\\b") ~ "Taylor & Francis",
    str_detect(x, "\\bsage\\b") ~ "SAGE Publishing",
    
    # ===== GREY / BORDERLINE =====
    str_detect(x, "\\bmdpi\\b|multidisciplinary digital") ~ "MDPI",
    str_detect(x, "\\bhindawi\\b") ~ "Hindawi",
    str_detect(x, "\\bfrontiers\\b") ~ "Frontiers",
    str_detect(x, "\\bbentham\\b") ~ "Bentham",
    str_detect(x, "\\bcogent\\b") ~ "Cogent",
    str_detect(x, "\\bomics\\b") ~ "OMICS",
    str_detect(x, "\\bspandidos\\b") ~ "Spandidos",
    str_detect(x, "\\boae\\b") ~ "OAE Publishing",
    str_detect(x, "\\bimr\\b") ~ "IMR Press",
    str_detect(x, "scientific research publishing|\\bscirp\\b") ~ "SCIRP",
    str_detect(x, "science publishing group") ~ "Science Publishing Group",
    str_detect(x, "academic journals") ~ "Academic Journals",
    str_detect(x, "maxwell scientific") ~ "Maxwell Scientific",
    str_detect(x, "hard publishing") ~ "HARD Publishing",
    str_detect(x, "friends science") ~ "Friends Science Publishers",
    str_detect(x, "international scientific research publications") ~ "International Scientific Research Publications",
    str_detect(x, "bioscience research institute") ~ "Bioscience Research Institute",
    
    # ===== SOCIETY / UNIVERSITY / NON-PROFIT =====
    str_detect(x, "institute of electrical and electronics engineers|\\bieee\\b") ~ "IEEE",
    str_detect(x, "institution of electrical engineers") ~ "Institution of Electrical Engineers",
    str_detect(x, "american chemical society|\\bacs\\b") ~ "American Chemical Society",
    str_detect(x, "royal society of chemistry") ~ "Royal Society of Chemistry",
    str_detect(x, "\\biop\\b") ~ "IOP Publishing",
    str_detect(x, "american institute of physics") ~ "American Institute of Physics",
    str_detect(x, "optical society") ~ "Optical Society",
    str_detect(x, "oxford university") ~ "Oxford University Press",
    str_detect(x, "cambridge university") ~ "Cambridge University Press",
    str_detect(x, "public library of science|\\bplos\\b") ~ "PLOS",
    str_detect(x, "american physical society") ~ "American Physical Society",
    str_detect(x, "american society for microbiology") ~ "American Society for Microbiology",
    str_detect(x, "association for computing machinery") ~ "Association for Computing Machinery",
    str_detect(x, "association for the advancement of artificial intelligence|\\baaa?i\\b") ~ "AAAI",
    str_detect(x, "american astronomical society") ~ "American Astronomical Society",
    str_detect(x, "proceedings of the national academy of sciences|\\bpnas\\b") ~ "PNAS",
    str_detect(x, "\\bbmj\\b") ~ "BMJ",
    str_detect(x, "american meteorological society") ~ "American Meteorological Society",
    str_detect(x, "american association for the advancement of science|\\baaas\\b") ~ "AAAS",
    str_detect(x, "society for industrial and applied mathematics|\\bsiam\\b") ~ "SIAM",
    str_detect(x, "microbiology society") ~ "Microbiology Society",
    str_detect(x, "royal society\\b") ~ "Royal Society",
    str_detect(x, "\\belife\\b") ~ "eLife",
    str_detect(x, "international union of crystallography") ~ "IUCr",
    str_detect(x, "american mathematical society") ~ "American Mathematical Society",
    str_detect(x, "american institute of mathematical sciences") ~ "AIMS",
    str_detect(x, "electrochemical society") ~ "Electrochemical Society",
    str_detect(x, "american institute of aeronautics and astronautics") ~ "AIAA",
    str_detect(x, "acoustical society of america") ~ "Acoustical Society of America",
    str_detect(x, "american physiological society") ~ "American Physiological Society",
    str_detect(x, "endocrine society") ~ "Endocrine Society",
    str_detect(x, "american association for cancer research") ~ "AACR",
    str_detect(x, "american society of civil engineers|\\basce\\b") ~ "ASCE",
    str_detect(x, "\\basme\\b|american society of mechanical engineers") ~ "ASME",
    str_detect(x, "american society for clinical investigation") ~ "ASCI",
    str_detect(x, "american society of clinical oncology") ~ "ASCO",
    str_detect(x, "american diabetes association") ~ "American Diabetes Association",
    str_detect(x, "american association of immunologists") ~ "American Association of Immunologists",
    str_detect(x, "society of exploration geophysicists") ~ "SEG",
    str_detect(x, "society for neuroscience") ~ "Society for Neuroscience",
    str_detect(x, "pharmaceutical society of japan") ~ "Pharmaceutical Society of Japan",
    str_detect(x, "association for research in vision and ophthalmology|\\barvo\\b") ~ "ARVO",
    str_detect(x, "american vacuum society") ~ "American Vacuum Society",
    str_detect(x, "american psychological association") ~ "American Psychological Association",
    str_detect(x, "company of biologists") ~ "Company of Biologists",
    str_detect(x, "beilstein") ~ "Beilstein-Institut",
    str_detect(x, "geological society of london") ~ "Geological Society of London",
    str_detect(x, "geological society of america") ~ "Geological Society of America",
    str_detect(x, "geoscienceworld") ~ "GeoScienceWorld",
    str_detect(x, "rockefeller university") ~ "Rockefeller University Press",
    str_detect(x, "cold spring harbor") ~ "Cold Spring Harbor Laboratory Press",
    str_detect(x, "institute of mathematics polish academy of sciences") ~ "Polish Academy of Sciences",
    str_detect(x, "japan society for analytical chemistry") ~ "Japan Society for Analytical Chemistry",
    str_detect(x, "japan society of mechanical engineers") ~ "Japan Society of Mechanical Engineers",
    str_detect(x, "iron and steel institute of japan") ~ "Iron and Steel Institute of Japan",
    str_detect(x, "japan institute of metals") ~ "Japan Institute of Metals",
    str_detect(x, "korean society for internet information") ~ "Korean Society for Internet Information",
    str_detect(x, "mathematical society of the republic of china") ~ "Mathematical Society of the Republic of China",
    str_detect(x, "taiwan association for aerosol research") ~ "Taiwan Association for Aerosol Research",
    str_detect(x, "electromagnetics academy") ~ "Electromagnetics Academy",
    str_detect(x, "national library of serbia") ~ "National Library of Serbia",
    str_detect(x, "institute of experimental botany") ~ "Institute of Experimental Botany",
    str_detect(x, "scientific societies") ~ "Scientific Societies",
    str_detect(x, "\\bembo\\b") ~ "EMBO",
    str_detect(x, "electronic journal of combinatorics") ~ "Electronic Journal of Combinatorics",
    str_detect(x, "czech academy of agricultural sciences") ~ "Czech Academy of Agricultural Sciences",
    
    # ===== OTHER INTERNATIONAL COMMERCIAL =====
    str_detect(x, "de gruyter") ~ "De Gruyter",
    str_detect(x, "wolters kluwer|ovid") ~ "Wolters Kluwer",
    str_detect(x, "world scientific") ~ "World Scientific",
    str_detect(x, "dove medical") ~ "Dove Medical Press",
    str_detect(x, "impact journals") ~ "Impact Journals",
    str_detect(x, "ivyspring") ~ "Ivyspring",
    str_detect(x, "edp sciences") ~ "EDP Sciences",
    str_detect(x, "georg thieme|\\bthieme\\b") ~ "Thieme",
    str_detect(x, "mary ann liebert") ~ "Mary Ann Liebert",
    str_detect(x, "\\bios\\b|\\bios press\\b") ~ "IOS Press",
    str_detect(x, "inderscience") ~ "Inderscience",
    str_detect(x, "canadian science") ~ "Canadian Science Publishing",
    str_detect(x, "canadian center of science and education") ~ "Canadian Center of Science and Education",
    str_detect(x, "\\bkarger\\b") ~ "Karger",
    str_detect(x, "future medicine") ~ "Future Medicine",
    str_detect(x, "emerald") ~ "Emerald",
    str_detect(x, "ame publishing") ~ "AME Publishing",
    str_detect(x, "pensoft") ~ "Pensoft",
    str_detect(x, "medknow") ~ "Medknow",
    str_detect(x, "scielo") ~ "SciELO",
    str_detect(x, "magnolia") ~ "Magnolia Press",
    str_detect(x, "global science") ~ "Global Science Press",
    str_detect(x, "tech science") ~ "Tech Science Press",
    str_detect(x, "\\bpeerj\\b") ~ "PeerJ",
    str_detect(x, "\\bspie\\b") ~ "SPIE",
    str_detect(x, "trans tech") ~ "Trans Tech",
    str_detect(x, "bioscientifica") ~ "Bioscientifica",
    str_detect(x, "portland") ~ "Portland Press",
    str_detect(x, "jve international") ~ "JVE International",
    str_detect(x, "engineering sciences") ~ "Engineering Sciences Press",
    str_detect(x, "international scientific information") ~ "International Scientific Information",
    str_detect(x, "international press of boston") ~ "International Press of Boston",
    str_detect(x, "oldenbourg") ~ "Oldenbourg",
    str_detect(x, "thomas telford") ~ "Thomas Telford",
    str_detect(x, "\\bigi global\\b") ~ "IGI Global",
    str_detect(x, "copernicus") ~ "Copernicus",
    str_detect(x, "termedia") ~ "Termedia",
    str_detect(x, "mycotaxon") ~ "Mycotaxon",
    str_detect(x, "aepress") ~ "AEPress",
    str_detect(x, "vilnius gediminas technical university") ~ "Vilnius Gediminas Technical University",
    str_detect(x, "kaunas university of technology") ~ "Kaunas University of Technology",
    str_detect(x, "politechnika wroclawska") ~ "Wroclaw University of Science and Technology",
    str_detect(x, "mechanical engineering faculty in slavonski brod") ~ "Mechanical Engineering Faculty in Slavonski Brod",
    str_detect(x, "bioresources") ~ "BioResources",
    str_detect(x, "genetics and molecular research") ~ "Genetics and Molecular Research",
    str_detect(x, "aging and disease") ~ "Aging and Disease",
    str_detect(x, "asian journal of chemistry") ~ "Asian Journal of Chemistry",
    str_detect(x, "progress in geography") ~ "Progress in Geography",
    str_detect(x, "nadia\\b") ~ "NADIA",
    
    # ===== CHINESE IDENTIFIABLE FAMILIES =====
    str_detect(x, "china science publishing|science press") ~ "China Science Publishing",
    str_detect(x, "tsinghua university") ~ "Tsinghua University Press",
    str_detect(x, "acta physica sinica") ~ "Chinese Physical Society",
    str_detect(x, "acta physico chimica sinica") ~ "Chinese Chemical Society",
    str_detect(x, "chinese journal of plant ecology") ~ "Chinese Journal of Plant Ecology",
    str_detect(x, "journal of modern power systems and clean energy") ~ "Modern Power Systems",
    str_detect(x, "shanghai institute|shaghai institute") ~ "Shanghai Institute",
    str_detect(x, "changchun institute") ~ "Changchun Institute",
    str_detect(x, "baishideng") ~ "Baishideng",
    str_detect(x, "xia and he") ~ "Xia & He Publishing",
    str_detect(x, "press of international journal of ophthalmology|ijo ") ~ "International Journal of Ophthalmology Press",
    
    # ===== AUTRES CAS TROUVES =========
    str_detect(x, "proceedings of the national academy of sciences|\\bpnas\\b") ~ "PNAS",
    str_detect(x, "china science publishing|science publishing and media|science press") ~ "China Science Publishing",
    str_detect(x, "institute of electronics information and communications engineers|\\bieice\\b") ~ "IEICE",
    str_detect(x, "\\biwa\\b") ~ "IWA Publishing",
    str_detect(x, "company of biologists") ~ "Company of Biologists",
    str_detect(x, "csiro") ~ "CSIRO Publishing",
    str_detect(x, "international scientific research publications") ~ "International Scientific Research Publications",
    str_detect(x, "mathematical society of the republic of china") ~ "Mathematical Society of the Republic of China",
    str_detect(x, "press of international journal of ophthalmology|ijo ") ~ "International Journal of Ophthalmology Press",
    str_detect(x, "\\bjmir\\b") ~ "JMIR Publications",
    str_detect(x, "american medical association|\\bama\\b") ~ "American Medical Association",
    str_detect(x, "radiological society of north america|\\brsna\\b") ~ "RSNA",
    str_detect(x, "social science electronic publishing|\\bssrn\\b") ~ "SSRN",
    str_detect(x, "american society for pharmacology") ~ "ASPET",
    str_detect(x, "american society of hematology") ~ "ASH",
    str_detect(x, "chinese academy of sciences") ~ "Chinese Academy of Sciences",
    str_detect(x, "international society for horticultural science|\\bishs\\b") ~ "ISHS",
    str_detect(x, "mathematical research") ~ "Mathematical Research Press",
    str_detect(x, "rocky mountain mathematics consortium") ~ "Rocky Mountain Mathematics Consortium",
    str_detect(x, "institute of mathematical statistics") ~ "Institute of Mathematical Statistics",
    str_detect(x, "association for information systems") ~ "Association for Information Systems",
    str_detect(x, "astm international") ~ "ASTM International",
    str_detect(x, "brill") ~ "Brill",
    str_detect(x, "atlantis") ~ "Atlantis Press",
    str_detect(x, "pagepress") ~ "PAGEPress",
    str_detect(x, "african journals online") ~ "African Journals Online",
    
    TRUE ~ "Other"
  )
}

# ==============================
# 3) FAMILIES -> GROUPS
# ==============================

big5_families <- c(
  "Elsevier", "Springer Nature", "Wiley", "Taylor & Francis", "SAGE Publishing"
)

grey_families <- c(
  "MDPI", "Hindawi", "Frontiers", "Bentham", "Cogent", "OMICS",
  "Spandidos", "OAE Publishing", "IMR Press", "SCIRP",
  "Science Publishing Group", "Academic Journals", "Maxwell Scientific",
  "HARD Publishing", "International Scientific Research Publications",
  "Bioscience Research Institute"
)

society_families <- c(
  "IEEE", "Institution of Electrical Engineers", "American Chemical Society",
  "Royal Society of Chemistry", "IOP Publishing", "American Institute of Physics",
  "Optical Society", "Oxford University Press", "Cambridge University Press",
  "PLOS", "American Physical Society", "American Society for Microbiology",
  "Association for Computing Machinery", "AAAI", "American Astronomical Society",
  "PNAS", "BMJ", "American Meteorological Society", "AAAS", "SIAM", "AIMS",
  "Microbiology Society", "Royal Society", "eLife", "IUCr",
  "American Mathematical Society", "Electrochemical Society", "AIAA",
  "Acoustical Society of America", "American Physiological Society",
  "Endocrine Society", "AACR", "ASCE", "ASME", "ASCI", "ASCO",
  "American Diabetes Association", "American Association of Immunologists",
  "SEG", "Society for Neuroscience", "Pharmaceutical Society of Japan",
  "ARVO", "American Vacuum Society", "American Psychological Association",
  "Company of Biologists", "Beilstein-Institut", "Geological Society of London",
  "Geological Society of America", "GeoScienceWorld",
  "Rockefeller University Press", "Cold Spring Harbor Laboratory Press",
  "Polish Academy of Sciences", "Japan Society for Analytical Chemistry",
  "Japan Society of Mechanical Engineers", "Iron and Steel Institute of Japan",
  "Japan Institute of Metals", "Korean Society for Internet Information",
  "Mathematical Society of the Republic of China",
  "Taiwan Association for Aerosol Research", "Electromagnetics Academy",
  "National Library of Serbia", "Institute of Experimental Botany",
  "Scientific Societies", "EMBO", "Electronic Journal of Combinatorics",
  "Czech Academy of Agricultural Sciences",   "PNAS", "IEICE", "Company of Biologists", "Mathematical Society of the Republic of China",
  "American Medical Association", "RSNA", "ASPET", "ASH",
  "Institute of Mathematical Statistics", "Association for Information Systems",
  "ASTM International", "ISHS", "Rocky Mountain Mathematics Consortium"
)

commercial_other_families <- c(
  "De Gruyter", "Wolters Kluwer", "World Scientific", "Dove Medical Press",
  "Impact Journals", "Ivyspring", "EDP Sciences", "Thieme",
  "Mary Ann Liebert", "IOS Press", "Inderscience",
  "Canadian Science Publishing", "Canadian Center of Science and Education",
  "Karger", "Future Medicine", "Emerald", "AME Publishing", "Pensoft",
  "Medknow", "SciELO", "Magnolia Press", "Global Science Press",
  "Tech Science Press", "PeerJ", "SPIE", "Trans Tech", "Bioscientifica",
  "Portland Press", "JVE International", "Engineering Sciences Press",
  "International Scientific Information", "International Press of Boston",
  "Oldenbourg", "Thomas Telford", "IGI Global", "Copernicus", "Termedia",
  "Mycotaxon", "AEPress", "Vilnius Gediminas Technical University",
  "Kaunas University of Technology", "Wroclaw University of Science and Technology",
  "Mechanical Engineering Faculty in Slavonski Brod", "BioResources",
  "Genetics and Molecular Research", "Aging and Disease",
  "Asian Journal of Chemistry", "Progress in Geography", "NADIA",   "IWA Publishing", "CSIRO Publishing", "International Scientific Research Publications",
  "International Journal of Ophthalmology Press", "JMIR Publications", "SSRN",
  "Brill", "Atlantis Press", "PAGEPress", "African Journals Online",
  "Mathematical Research Press"
)

chinese_families <- c(
  "China Science Publishing", "Tsinghua University Press",
  "Chinese Physical Society", "Chinese Chemical Society",
  "Chinese Journal of Plant Ecology", "Modern Power Systems",
  "Shanghai Institute", "Changchun Institute", "Baishideng",
  "Xia & He Publishing", "International Journal of Ophthalmology Press",
  "China Science Publishing", "Chinese Academy of Sciences"
)

# ==============================
# 4) APPLICATION
# ==============================

all_nsfc_augmented <- all_nsfc_augmented %>%
  mutate(
    publisher_name_raw = coalesce(host_organization_name, host_organization),
    publisher_name_clean = normalize_publisher_name(publisher_name_raw),
    publisher_name_has_han = has_chinese_script(publisher_name_raw),
    publisher_name_has_china_keyword = has_china_keyword(publisher_name_raw),
    publisher_name_is_numeric = is_numeric_like(publisher_name_raw),
    
    publisher_family = classify_publisher_family(publisher_name_raw),
    
    publisher_group = case_when(
      publisher_name_is_numeric ~ "Unknown / unmatched",
      publisher_family %in% grey_families ~ "Grey publishers",
      publisher_family %in% big5_families ~ "Big 5 publishers",
      publisher_family %in% society_families ~ "Major society / university / non-profit publishers",
      publisher_family %in% chinese_families ~ "Chinese publishers",
      source_country_code == "CN" ~ "Chinese publishers",
      is.na(source_country_code) & publisher_name_has_han ~ "Chinese publishers",
      is.na(source_country_code) & publisher_name_has_china_keyword ~ "Chinese publishers",
      publisher_family %in% commercial_other_families ~ "Other international commercial publishers",
      publisher_family == "Other" & !is.na(source_country_code) & source_country_code != "CN" ~ "Other international commercial publishers",
      publisher_family == "Other" & source_country_code == "CN" ~ "Chinese publishers",
      publisher_family == "Unknown" & source_country_code == "CN" ~ "Chinese publishers",
      publisher_family == "Unknown" & is.na(source_country_code) & publisher_name_has_han ~ "Chinese publishers",
      publisher_family == "Unknown" & is.na(source_country_code) & publisher_name_has_china_keyword ~ "Chinese publishers",
      TRUE ~ "Unknown / unmatched"
    ),
    
    is_grey = publisher_group == "Grey publishers",
    is_big5 = publisher_group == "Big 5 publishers"
  )

all_nsfc_augmented$publisher_group <- factor(
  all_nsfc_augmented$publisher_group,
  levels = c(
    "Grey publishers",
    "Big 5 publishers",
    "Major society / university / non-profit publishers",
    "Chinese publishers",
    "Other international commercial publishers",
    "Unknown / unmatched"
  )
)

saveRDS(all_nsfc_augmented, "data_nsfc/all_nsfc_augmented.rds")
all_nsfc_augmented <- readRDS("data_nsfc/all_nsfc_augmented.rds")
# ==============================
# 6) TABLES DE CONTRÔLE
# ==============================
unknown_publishers_true <- all_nsfc_augmented %>%
  filter(publisher_group == "Unknown / unmatched") %>%
  count(publisher_name_raw, sort = TRUE)

print(unknown_publishers_true, n = 100)

tab_publishers <- all_nsfc_augmented %>%
  count(publisher_group, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1))

tab_top_publishers <- all_nsfc_augmented %>%
  count(publisher_name, sort = TRUE) %>%
  slice_head(n = 20)

tab_oa_by_group <- all_nsfc_augmented %>%
  group_by(publisher_group) %>%
  summarise(
    n = n(),
    oa_rate = mean(is_oa, na.rm = TRUE),
    mean_authors = mean(n_authors, na.rm = TRUE),
    mean_countries = mean(n_countries, na.rm = TRUE),
    mean_impact_2yr = mean(impact_2yr, na.rm = TRUE),
    mean_h_index = mean(h_index, na.rm = TRUE),
    mean_apc_usd = mean(apc_usd, na.rm = TRUE),
    .groups = "drop"
  )

print(tab_publishers)
print(tab_top_publishers)
print(tab_oa_by_group)

# ==============================
# 7) FIGURE 1
# ÉVOLUTION DES VOLUMES PAR GROUPE ÉDITEUR
# ==============================


# =========================================================
# NSFC × PUBLISHER GROUPS
# FIGURES AMÉLIORÉES — VERSION PUBLICATION-READY
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(patchwork)
library(data.table)

# ==============================
# 1) DONNÉES
# ==============================

# suppose que all_nsfc_augmented existe déjà
# avec :
# - publication_year
# - publisher_group
# - is_grey
# - is_oa
# - main_domain
# - n_countries
# - impact_2yr
# - h_index
# - apc_usd

# ==============================
# 2) PRÉPARATION
# ==============================

publisher_levels_keep <- c(
  "Grey publishers",
  "Big 5 publishers",
  "Major society / university / non-profit publishers",
  "Chinese publishers",
  "Other international commercial publishers"
)

publisher_labels_short <- c(
  "Grey publishers" = "Grey",
  "Big 5 publishers" = "Big 5",
  "Major society / university / non-profit publishers" = "Societies / Univ.",
  "Chinese publishers" = "Chinese",
  "Other international commercial publishers" = "Other intl."
)

pal_publishers <- c(
  "Grey publishers" = "#C44E52",
  "Big 5 publishers" = "#1F4E79",
  "Major society / university / non-profit publishers" = "#4C9F70",
  "Chinese publishers" = "#B07AA1",
  "Other international commercial publishers" = "#7A7A7A"
)

df_pub <- all_nsfc_augmented %>%
  filter(publisher_group %in% publisher_levels_keep) %>%
  mutate(
    publisher_group = factor(publisher_group, levels = publisher_levels_keep)
  )

# ==============================
# 3) THÈME
# ==============================

theme_pub <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0, colour = "#1A1A1A"),
      plot.subtitle = element_text(size = base_size, hjust = 0, colour = "#4D4D4D"),
      plot.caption = element_text(size = base_size - 2, colour = "#666666"),
      
      axis.title = element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = element_text(size = base_size - 1, colour = "#262626"),
      
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFEFEF", linewidth = 0.30),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.width = unit(1.2, "cm"),
      
      strip.text = element_text(face = "bold", colour = "#1A1A1A"),
      plot.margin = margin(12, 14, 12, 12)
    )
}

common_x_scale <- scale_x_continuous(
  breaks = pretty(df_pub$publication_year, n = 10),
  expand = expansion(mult = c(0.01, 0.01))
)

legend_guides_colour <- guides(
  colour = guide_legend(nrow = 2, byrow = TRUE)
)

legend_guides_fill <- guides(
  fill = guide_legend(nrow = 2, byrow = TRUE)
)

# ==============================
# 4) FIGURE 1
# ÉVOLUTION DES VOLUMES PAR GROUPE ÉDITEUR
# ==============================

fig_pub_vol <- df_pub %>%
  group_by(publication_year, publisher_group) %>%
  summarise(n = n(), .groups = "drop")

g_pub_vol <- ggplot(
  fig_pub_vol,
  aes(x = publication_year, y = n, colour = publisher_group)
) +
  geom_line(linewidth = 1.3, lineend = "round") +
  geom_point(size = 2.1) +
  scale_color_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  common_x_scale +
  labs(
    title = "Grey publishers gain ground within NSFC-funded output",
    subtitle = "Annual number of publications by publisher group",
    x = NULL,
    y = "Number of publications"
  ) +
  legend_guides_colour +
  theme_pub()

# ==============================
# 5) FIGURE 2
# PART DES GREY PUBLISHERS DANS L’OA
# ==============================

fig_grey_oa <- df_pub %>%
  filter(is_oa == TRUE) %>%
  group_by(publication_year) %>%
  summarise(
    total_oa = n(),
    grey_oa = sum(is_grey, na.rm = TRUE),
    share_grey = grey_oa / total_oa,
    .groups = "drop"
  )

g_grey_oa <- ggplot(
  fig_grey_oa,
  aes(x = publication_year, y = share_grey)
) +
  geom_line(linewidth = 1.7, colour = "#C44E52", lineend = "round") +
  geom_point(size = 2.8, colour = "#C44E52") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  common_x_scale +
  labs(
    title = "Grey publishers account for a growing share of NSFC Open Access output",
    subtitle = "Annual share of Open Access publications published by grey publishers",
    x = NULL,
    y = "Share of OA publications"
  ) +
  theme_pub()


# ==============================
# GRAPHE COMPLÉMENTAIRE
# Part de chaque groupe dans l'OA NSFC
# ==============================

library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)

# ------------------------------
# 1) Préparation
# ------------------------------

publisher_levels_keep <- c(
  "Grey publishers",
  "Big 5 publishers",
  "Major society / university / non-profit publishers",
  "Chinese publishers",
  "Other international commercial publishers"
)

publisher_labels_short <- c(
  "Grey publishers" = "Grey",
  "Big 5 publishers" = "Big 5",
  "Major society / university / non-profit publishers" = "Societies / Univ.",
  "Chinese publishers" = "Chinese",
  "Other international commercial publishers" = "Other intl."
)

pal_publishers <- c(
  "Grey publishers" = "#C44E52",
  "Big 5 publishers" = "#1F4E79",
  "Major society / university / non-profit publishers" = "#4C9F70",
  "Chinese publishers" = "#B07AA1",
  "Other international commercial publishers" = "#7A7A7A"
)

theme_pub <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0, colour = "#1A1A1A"),
      plot.subtitle = element_text(size = base_size, hjust = 0, colour = "#4D4D4D"),
      plot.caption = element_text(size = base_size - 2, colour = "#666666"),
      axis.title = element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = element_text(size = base_size - 1, colour = "#262626"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFEFEF", linewidth = 0.30),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.width = unit(1.15, "cm"),
      plot.margin = margin(12, 14, 12, 12)
    )
}

df_pub <- all_nsfc_augmented %>%
  filter(
    publisher_group %in% publisher_levels_keep,
    is_oa == TRUE
  ) %>%
  mutate(
    publisher_group = factor(publisher_group, levels = publisher_levels_keep)
  )

# ------------------------------
# 2) Part de chaque groupe dans l'OA
# ------------------------------

fig_oa_group_share <- df_pub %>%
  group_by(publication_year, publisher_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(
    total_oa = sum(n),
    share = n / total_oa
  ) %>%
  ungroup()

# ------------------------------
# 3) Graphe principal
# ------------------------------

g_oa_group_share <- ggplot(
  fig_oa_group_share,
  aes(x = publication_year, y = share, colour = publisher_group)
) +
  geom_line(linewidth = 1.35, lineend = "round") +
  geom_point(size = 2.2) +
  scale_color_manual(
    values = pal_publishers,
    labels = publisher_labels_short,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = pretty(fig_oa_group_share$publication_year, n = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(
    title = "The composition of NSFC Open Access output changes markedly over time",
    subtitle = "Annual share of Open Access publications by publisher group",
    x = NULL,
    y = "Share of OA publications"
  ) +
  theme_pub()

print(g_oa_group_share)

# ------------------------------
# 4) Version focalisée sur Grey vs autres
# ------------------------------

fig_grey_vs_rest <- df_pub %>%
  mutate(
    group2 = if_else(
      publisher_group == "Grey publishers",
      "Grey publishers",
      "All other groups"
    )
  ) %>%
  group_by(publication_year, group2) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

g_grey_vs_rest <- ggplot(
  fig_grey_vs_rest,
  aes(x = publication_year, y = share, colour = group2)
) +
  geom_line(linewidth = 1.5, lineend = "round") +
  geom_point(size = 2.4) +
  scale_color_manual(
    values = c(
      "Grey publishers" = "#C44E52",
      "All other groups" = "#7A7A7A"
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = pretty(fig_grey_vs_rest$publication_year, n = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Grey publishers rise strongly, then recede after 2022",
    subtitle = "Annual share of grey publishers within NSFC Open Access publications",
    x = NULL,
    y = "Share of OA publications"
  ) +
  theme_pub()

print(g_grey_vs_rest)

# ------------------------------
# 5) Option : petit panneau
# ------------------------------

panel_oa_publishers <- g_grey_oa / g_oa_group_share

print(panel_oa_publishers)

# ------------------------------
# 6) Contrôle rapide
# ------------------------------

tab_oa_group_share <- fig_oa_group_share %>%
  mutate(
    share_pct = percent(share, accuracy = 0.1),
    publisher_group = recode(as.character(publisher_group), !!!publisher_labels_short)
  )

print(tab_oa_group_share)

# ------------------------------
# 7) Export
# ------------------------------

dir.create("figures_nsfc_publishers", showWarnings = FALSE)

ggsave(
  "figures_nsfc_publishers/FigureP_share_oa_by_publisher_group_lines.png",
  g_oa_group_share,
  width = 11, height = 6.5, dpi = 350, bg = "white"
)

ggsave(
  "figures_nsfc_publishers/FigureP_grey_vs_rest_oa_share.png",
  g_grey_vs_rest,
  width = 11, height = 6.3, dpi = 350, bg = "white"
)

ggsave(
  "figures_nsfc_publishers/FigureP_panel_grey_and_groups.png",
  panel_oa_publishers,
  width = 11, height = 11, dpi = 350, bg = "white"
)

# ==============================
# 6) FIGURE 3
# COMPOSITION DES PUBLICATIONS OA PAR GROUPE ÉDITEUR
# ==============================

fig_pub_oa_comp <- df_pub %>%
  filter(is_oa == TRUE) %>%
  group_by(publication_year, publisher_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

g_pub_oa_comp <- ggplot(
  fig_pub_oa_comp,
  aes(x = publication_year, y = share, fill = publisher_group)
) +
  geom_area(alpha = 0.98, colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  common_x_scale +
  labs(
    title = "The OA publishing space of NSFC-funded research is being restructured",
    subtitle = "Annual composition of Open Access publications by publisher group",
    x = NULL,
    y = "Share within OA publications"
  ) +
  legend_guides_fill +
  theme_pub()

# ==============================
# 7) FIGURE 4
# OA RATE PAR GROUPE ÉDITEUR DANS LE TEMPS
# ==============================

fig_oa_rate_group <- df_pub %>%
  group_by(publication_year, publisher_group) %>%
  summarise(
    n = n(),
    oa_rate = mean(is_oa, na.rm = TRUE),
    .groups = "drop"
  )

g_oa_rate_group <- ggplot(
  fig_oa_rate_group,
  aes(x = publication_year, y = oa_rate, colour = publisher_group)
) +
  geom_line(linewidth = 1.3, lineend = "round") +
  geom_point(size = 2.0) +
  scale_color_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  common_x_scale +
  labs(
    title = "Publisher groups differ strongly in their Open Access orientation",
    subtitle = "Annual Open Access rate within each publisher group",
    x = NULL,
    y = "Open Access share"
  ) +
  legend_guides_colour +
  theme_pub()

# ==============================
# 8) FIGURE 5
# DOMAINES × GROUPE ÉDITEUR
# ==============================

fig_domain_pub <- df_pub %>%
  mutate(main_domain = if_else(is.na(main_domain), "Unknown", main_domain)) %>%
  group_by(main_domain, publisher_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(main_domain) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

domain_order <- fig_domain_pub %>%
  group_by(main_domain) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  arrange(total_n) %>%
  pull(main_domain)

fig_domain_pub <- fig_domain_pub %>%
  mutate(main_domain = factor(main_domain, levels = domain_order))

g_domain_pub <- ggplot(
  fig_domain_pub,
  aes(x = main_domain, y = share, fill = publisher_group)
) +
  geom_col(position = "fill", width = 0.72) +
  coord_flip() +
  scale_fill_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Publisher segmentation varies sharply across scientific domains",
    subtitle = "Distribution of publisher groups within each broad domain",
    x = NULL,
    y = "Share within domain"
  ) +
  legend_guides_fill +
  theme_pub()

# ==============================
# 9) FIGURE 6
# COLLABORATION INTERNATIONALE × GROUPE ÉDITEUR
# ==============================

fig_collab_pub <- df_pub %>%
  mutate(
    collab_type = case_when(
      n_countries == 1 ~ "Domestic",
      n_countries == 2 ~ "Bilateral",
      n_countries >= 3 ~ "Multinational",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(collab_type)) %>%
  group_by(collab_type, publisher_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(collab_type) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    collab_type = factor(collab_type, levels = c("Domestic", "Bilateral", "Multinational"))
  )

g_collab_pub <- ggplot(
  fig_collab_pub,
  aes(x = collab_type, y = share, fill = publisher_group)
) +
  geom_col(position = "fill", width = 0.68) +
  scale_fill_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Publishing channels differ by collaboration profile",
    subtitle = "Distribution of publisher groups by international collaboration structure",
    x = NULL,
    y = "Share within collaboration type"
  ) +
  legend_guides_fill +
  theme_pub()

# ==============================
# 10) TABLEAU MÉTRIQUES
# ==============================

fig_metrics <- df_pub %>%
  group_by(publisher_group) %>%
  summarise(
    n = n(),
    mean_impact_2yr = mean(impact_2yr, na.rm = TRUE),
    mean_h_index = mean(h_index, na.rm = TRUE),
    mean_apc_usd = mean(apc_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    publisher_group = recode(as.character(publisher_group), !!!publisher_labels_short)
  )

print(fig_metrics)

# ==============================
# 11) AFFICHAGE
# ==============================

print(g_pub_vol)
print(g_grey_oa)
print(g_pub_oa_comp)
print(g_oa_rate_group)
print(g_domain_pub)
print(g_collab_pub)

# ==============================
# 12) PANNEAUX
# ==============================

panel_publishers_1 <- g_pub_vol / g_grey_oa
panel_publishers_2 <- g_pub_oa_comp / g_oa_rate_group
panel_publishers_3 <- g_domain_pub / g_collab_pub

print(panel_publishers_1)
print(panel_publishers_2)
print(panel_publishers_3)

# ==============================
# 13) EXPORT
# ==============================

dir.create("figures_nsfc_publishers", showWarnings = FALSE)

ggsave("figures_nsfc_publishers/FigureP1_volumes_by_publisher_group.png",
       g_pub_vol, width = 11, height = 6.5, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP2_grey_share_in_oa.png",
       g_grey_oa, width = 11, height = 6.2, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP3_oa_composition_by_publisher_group.png",
       g_pub_oa_comp, width = 11, height = 6.5, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP4_oa_rate_by_publisher_group.png",
       g_oa_rate_group, width = 11, height = 6.5, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP5_domains_by_publisher_group.png",
       g_domain_pub, width = 11, height = 7.2, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP6_collaboration_by_publisher_group.png",
       g_collab_pub, width = 11, height = 6.5, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP_panel_1.png",
       panel_publishers_1, width = 12, height = 10, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP_panel_2.png",
       panel_publishers_2, width = 12, height = 10, dpi = 350, bg = "white")

ggsave("figures_nsfc_publishers/FigureP_panel_3.png",
       panel_publishers_3, width = 12, height = 10, dpi = 350, bg = "white")

# =========================================================
# EARLY WARNING LISTS — MATCHING TO OPENALEX SOURCES
# =========================================================
# Objectif :
# - construire une table unique des Early Warning Lists
# - matcher aux sources OpenAlex (jours_all)
# - 2024/2025 : ISSN prioritaire
# - 2020/2021/2023 : titre exact + fuzzy
# - produire une table finale documentée avec confiance de match
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(fuzzyjoin)
library(stringdist)

# ==============================
# 1) CHARGEMENT
# ==============================

# jours_all <- readRDS("data_nsfc/sources_openalex.rds")
setDT(jours_all)

# ==============================
# 2) HELPERS
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_title <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("[:punct:]", " ") %>%
    str_replace_all("\\bJOURNAL OF\\b", "JOURNAL OF ") %>%
    str_replace_all("\\bINTL\\b", "INTERNATIONAL") %>%
    str_replace_all("\\bBASel\\b", "BASEL") %>%
    str_squish()
}

normalize_issn <- function(x) {
  x <- as.character(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^0-9X]", "")
  ifelse(nchar(x) == 8, x, NA_character_)
}

extract_issn_vector <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  out <- unlist(x, use.names = FALSE)
  out <- normalize_issn(out)
  out <- out[!is.na(out)]
  unique(out)
}

vec_to_string <- function(x, sep = "; ") {
  if (length(x) == 0) return(NA_character_)
  paste(unique(x), collapse = sep)
}

# ==============================
# 3) APLATIR JOURS_ALL
# ==============================

sources_flat <- jours_all %>%
  as_tibble() %>%
  mutate(
    issn_l_norm = normalize_issn(issn_l),
    source_title_norm = normalize_title(display_name),
    alt_titles_chr = map_chr(
      alternate_titles,
      ~ {
        if (is.null(.x) || all(is.na(.x))) return(NA_character_)
        paste(as.character(unlist(.x, use.names = FALSE)), collapse = " || ")
      }
    ),
    alt_title_list = map(
      alternate_titles,
      ~ {
        if (is.null(.x) || all(is.na(.x))) return(character(0))
        vals <- as.character(unlist(.x, use.names = FALSE))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        normalize_title(vals)
      }
    ),
    issn_vec = map(issn, extract_issn_vector)
  ) %>%
  transmute(
    source_id = id,
    display_name,
    source_title_norm,
    alt_title_list,
    issn_l,
    issn_l_norm,
    issn_vec,
    host_organization,
    host_organization_name,
    country_code,
    type,
    is_oa,
    is_in_doaj,
    apc_usd,
    works_count,
    cited_by_count,
    summary_stats
  )

# table longue des ISSN pour matching 2024/2025
source_issn_long <- bind_rows(
  sources_flat %>%
    filter(!is.na(issn_l_norm)) %>%
    transmute(
      source_id,
      display_name,
      host_organization_name,
      country_code,
      source_type = type,
      issn_norm = as.character(issn_l_norm),
      match_issn_source = "issn_l"
    ),
  
  sources_flat %>%
    transmute(
      source_id,
      display_name,
      host_organization_name,
      country_code,
      source_type = type,
      issn_vec = map(issn_vec, ~ as.character(.x))
    ) %>%
    tidyr::unnest_longer(issn_vec, values_to = "issn_norm") %>%
    mutate(issn_norm = as.character(issn_norm)) %>%
    filter(!is.na(issn_norm), nzchar(issn_norm)) %>%
    transmute(
      source_id,
      display_name,
      host_organization_name,
      country_code,
      source_type,
      issn_norm,
      match_issn_source = "issn"
    )
) %>%
  distinct(source_id, issn_norm, .keep_all = TRUE)

# ==============================
# 4) CONSTRUIRE LES EARLY WARNING LISTS
# ==============================

ewl_2020 <- tribble(
  ~ewl_year, ~field, ~journal_title, ~warning_level, ~warning_reason,
  2020, "Materials Science", "Metals", "Low", NA_character_,
  2020, "Materials Science", "Coatings", "Low", NA_character_,
  2020, "Materials Science", "Materials", "Low", NA_character_,
  2020, "Materials Science", "JOURNAL OF NANOSCIENCE AND NANOTECHNOLOGY", "Low", NA_character_,
  2020, "GeoSciences", "Minerals", "Low", NA_character_,
  2020, "GeoSciences", "Atmosphere", "Low", NA_character_,
  2020, "Engineering", "Artificial Cells Nanomedicine and Biotechnology", "High", NA_character_,
  2020, "Engineering", "Advances in Civil Engineering", "Medium", NA_character_,
  2020, "Engineering", "INTERNATIONAL JOURNAL OF ENERGY RESEARCH", "Medium", NA_character_,
  2020, "Engineering", "MATHEMATICAL PROBLEMS IN ENGINEERING", "Medium", NA_character_,
  2020, "Engineering", "SENSORS", "Low", NA_character_,
  2020, "Engineering", "Energies", "Low", NA_character_,
  2020, "Engineering", "Applied Sciences-Basel", "Low", NA_character_,
  2020, "Engineering", "Polymers", "Low", NA_character_,
  2020, "Engineering", "Electronics", "Low", NA_character_,
  2020, "Engineering", "Processes", "Low", NA_character_,
  2020, "Engineering", "COMPLEXITY", "Low", NA_character_,
  2020, "Engineering", "Desalination and Water Treatment", "Low", NA_character_,
  2020, "Chemistry", "International Journal of Electrochemical Science", "Medium", NA_character_,
  2020, "Chemistry", "Catalysts", "Low", NA_character_,
  2020, "Chemistry", "MOLECULES", "Low", NA_character_,
  2020, "Chemistry", "NATURAL PRODUCT RESEARCH", "Low", NA_character_,
  2020, "Chemistry", "ZEITSCHRIFT FUR KRISTALLOGRAPHIE-NEW CRYSTAL STRUCTURES", "Low", NA_character_,
  2020, "Environment Science and Ecology", "Sustainability", "Medium", NA_character_,
  2020, "Environment Science and Ecology", "Water", "Low", NA_character_,
  2020, "Computer Science", "IEEE Access", "Medium", NA_character_,
  2020, "Agricultural and Forestry Science", "Agronomy-Basel", "Low", NA_character_,
  2020, "Biology", "JOURNAL OF CELLULAR BIOCHEMISTRY", "High", NA_character_,
  2020, "Biology", "JOURNAL OF CELLULAR PHYSIOLOGY", "Medium", NA_character_,
  2020, "Biology", "BIOSCIENCE REPORTS", "Medium", NA_character_,
  2020, "Biology", "Biomed Research International", "Medium", NA_character_,
  2020, "Biology", "BIOFACTORS", "Low", NA_character_,
  2020, "Biology", "Plants-Basel", "Low", NA_character_,
  2020, "Biology", "Cells", "Low", NA_character_,
  2020, "Mathematics", "Boundary Value Problems", "High", NA_character_,
  2020, "Mathematics", "Advances in Difference Equations", "High", NA_character_,
  2020, "Mathematics", "JOURNAL OF INEQUALITIES AND APPLICATIONS", "Medium", NA_character_,
  2020, "Mathematics", "Mathematics", "Low", NA_character_,
  2020, "Medicine", "European Review for Medical and Pharmacological Sciences", "High", NA_character_,
  2020, "Medicine", "International Journal of Clinical and Experimental Pathology", "High", NA_character_,
  2020, "Medicine", "MEDICINE", "High", NA_character_,
  2020, "Medicine", "International Journal of Clinical and Experimental Medicine", "High", NA_character_,
  2020, "Medicine", "BIOMEDICINE & PHARMACOTHERAPY", "Medium", NA_character_,
  2020, "Medicine", "EXPERIMENTAL AND MOLECULAR PATHOLOGY", "Medium", NA_character_,
  2020, "Medicine", "BRAZILIAN JOURNAL OF MEDICAL AND BIOLOGICAL RESEARCH", "Medium", NA_character_,
  2020, "Medicine", "Cancer Biomarkers", "Medium", NA_character_,
  2020, "Medicine", "INTERNATIONAL JOURNAL OF IMMUNOPATHOLOGY AND PHARMACOLOGY", "Medium", NA_character_,
  2020, "Medicine", "ONCOLOGY RESEARCH", "Medium", NA_character_,
  2020, "Medicine", "American Journal of Cancer Research", "Medium", NA_character_,
  2020, "Medicine", "MEDICAL SCIENCE MONITOR", "Medium", NA_character_,
  2020, "Medicine", "Oncology Letters", "Medium", NA_character_,
  2020, "Medicine", "Experimental and Therapeutic Medicine", "Medium", NA_character_,
  2020, "Medicine", "OncoTargets and Therapy", "Medium", NA_character_,
  2020, "Medicine", "ONCOLOGY REPORTS", "Medium", NA_character_,
  2020, "Medicine", "Molecular Medicine Reports", "Medium", NA_character_,
  2020, "Medicine", "INTERNATIONAL JOURNAL OF MOLECULAR MEDICINE", "Medium", NA_character_,
  2020, "Medicine", "JOURNAL OF INTERNATIONAL MEDICAL RESEARCH", "Medium", NA_character_,
  2020, "Medicine", "American Journal of Translational Research", "Medium", NA_character_,
  2020, "Medicine", "Journal of Biomaterials and Tissue Engineering", "Medium", NA_character_,
  2020, "Medicine", "Aging-US", "Medium", NA_character_,
  2020, "Medicine", "LIFE SCIENCES", "Low", NA_character_,
  2020, "Medicine", "Journal of Clinical Medicine", "Low", NA_character_,
  2020, "Medicine", "International Journal of Environmental Research and Public Health", "Low", NA_character_,
  2020, "Medicine", "Acta Medica Mediterranea", "Low", NA_character_,
  2020, "Multidisciplinary Science", "Symmetry-Basel", "Low", NA_character_
)

ewl_2021 <- tribble(
  ~ewl_year, ~field, ~journal_title, ~warning_level, ~warning_reason,
  2021, "Engineering", "Complexity", "Medium", NA_character_,
  2021, "Engineering", "Shock and Vibration", "Medium", NA_character_,
  2021, "Engineering", "Advances in Civil Engineering", "Medium", NA_character_,
  2021, "Engineering", "Biomedicines", "Low", NA_character_,
  2021, "Computer Science", "Microprocessors and Microsystems", "High", NA_character_,
  2021, "Computer Science", "Scientific Programming", "Low", NA_character_,
  2021, "Biology", "BioFactors", "High", NA_character_,
  2021, "Biology", "Mitochondrial DNA Part B-Resources", "Medium", NA_character_,
  2021, "Biology", "Frontiers in Molecular Biosciences", "Low", NA_character_,
  2021, "Biology", "Frontiers in Cell and Developmental Biology", "Low", NA_character_,
  2021, "Biology", "Life-Basel", "Low", NA_character_,
  2021, "Biology", "Biology-Basel", "Low", NA_character_,
  2021, "Mathematics", "Discrete Dynamics in Nature and Society", "Medium", NA_character_,
  2021, "Mathematics", "AIMS Mathematics", "Low", NA_character_,
  2021, "Mathematics", "Journal of Mathematics", "Low", NA_character_,
  2021, "Medicine", "Pharmazie", "High", NA_character_,
  2021, "Medicine", "Molecular Therapy-Nucleic Acids", "High", NA_character_,
  2021, "Medicine", "Experimental and Molecular Pathology", "High", NA_character_,
  2021, "Medicine", "Journal of Cellular and Molecular Medicine", "Medium", NA_character_,
  2021, "Medicine", "Molecular Medicine Reports", "Medium", NA_character_,
  2021, "Medicine", "Journal of International Medical Research", "Medium", NA_character_,
  2021, "Medicine", "Journal of Cancer", "Medium", NA_character_,
  2021, "Medicine", "Medical Science Monitor", "Medium", NA_character_,
  2021, "Medicine", "Aging-US", "Medium", NA_character_,
  2021, "Medicine", "OncoTargets and Therapy", "Medium", NA_character_,
  2021, "Medicine", "Cancer Management and Research", "Medium", NA_character_,
  2021, "Medicine", "Cancer Cell International", "Medium", NA_character_,
  2021, "Medicine", "World Journal of Clinical Cases", "Medium", NA_character_,
  2021, "Medicine", "Annals of Palliative Medicine", "Low", NA_character_,
  2021, "Medicine", "International Journal of General Medicine", "Low", NA_character_,
  2021, "Medicine", "Frontiers in Medicine", "Low", NA_character_,
  2021, "Medicine", "Journal of Personalized Medicine", "Low", NA_character_,
  2021, "Medicine", "Healthcare", "Low", NA_character_,
  2021, "Medicine", "Diagnostics", "Low", NA_character_,
  2021, "Medicine", "Vaccines", "Low", NA_character_
)

ewl_2023 <- tribble(
  ~ewl_year, ~field, ~journal_title, ~warning_level, ~warning_reason,
  2023, "Materials Science", "TEXTILE RESEARCH JOURNAL", "Medium", NA_character_,
  2023, "GeoSciences", "GEOFLUIDS", "Medium", NA_character_,
  2023, "GeoSciences", "FRONTIERS IN EARTH SCIENCE", "Low", NA_character_,
  2023, "Engineering", "JOURNAL OF INDUSTRIAL AND MANAGEMENT OPTIMIZATION", "Medium", NA_character_,
  2023, "Engineering", "MATHEMATICAL PROBLEMS IN ENGINEERING", "Medium", NA_character_,
  2023, "Engineering", "AEROSPACE", "Low", NA_character_,
  2023, "Engineering", "BUILDINGS", "Low", NA_character_,
  2023, "Engineering", "COMPUTATIONAL AND MATHEMATICAL METHODS IN MEDICINE", "Low", NA_character_,
  2023, "Engineering", "ENERGY REPORTS", "Low", NA_character_,
  2023, "Engineering", "MACHINES", "Low", NA_character_,
  2023, "Chemistry", "INORGANIC AND NANO-METAL CHEMISTRY", "High", NA_character_,
  2023, "Chemistry", "JOURNAL OF STRUCTURAL CHEMISTRY", "High", NA_character_,
  2023, "Chemistry", "INTERNATIONAL JOURNAL OF ELECTROCHEMICAL SCIENCE", "Medium", NA_character_,
  2023, "Environment Science and Ecology", "FRONTIERS IN ENVIRONMENTAL SCIENCE", "Low", NA_character_,
  2023, "Computer Science", "MICROPROCESSORS AND MICROSYSTEMS", "High", NA_character_,
  2023, "Computer Science", "INTERNATIONAL JOURNAL OF CONTROL AUTOMATION AND SYSTEMS", "Medium", NA_character_,
  2023, "Computer Science", "MOBILE INFORMATION SYSTEMS", "Low", NA_character_,
  2023, "Economics", "ECONOMIC RESEARCH-EKONOMSKA ISTRAZIVANJA", "Low", NA_character_,
  2023, "Agricultural and Forestry Science", "FOOD SCIENCE AND TECHNOLOGY", "Low", NA_character_,
  2023, "Medicine", "JOURNAL OF ENVIRONMENTAL AND PUBLIC HEALTH", "High", NA_character_,
  2023, "Medicine", "PHARMAZIE", "High", NA_character_,
  2023, "Medicine", "PSYCHIATRIA DANUBINA", "High", NA_character_,
  2023, "Medicine", "ACTA MEDICA MEDITERRANEA", "Medium", NA_character_,
  2023, "Medicine", "AMERICAN JOURNAL OF TRANSLATIONAL RESEARCH", "Medium", NA_character_,
  2023, "Medicine", "JOURNAL OF BIOMATERIALS AND TISSUE ENGINEERING", "Medium", NA_character_,
  2023, "Medicine", "JOURNAL OF CLINICAL LABORATORY ANALYSIS", "Medium", NA_character_,
  2023, "Medicine", "WORLD JOURNAL OF CLINICAL CASES", "Medium", NA_character_,
  2023, "Medicine", "FRONTIERS IN SURGERY", "Low", NA_character_
)

ewl_2024 <- tribble(
  ~ewl_year, ~journal_title, ~issn_raw, ~warning_reason,
  2024, "CANCERS", "2072-6694", "Citation Manipulation",
  2024, "DIAGNOSTICS", "2075-4418", "Citation Manipulation",
  2024, "ENVIRONMENTAL SCIENCE AND POLLUTION RESEARCH", "0944-1344", "Citation Manipulation; Paper Mill",
  2024, "FUEL", "0016-2361", "Citation Manipulation",
  2024, "JOURNAL OF CLINICAL MEDICINE", "2077-0383", "Citation Manipulation",
  2024, "JOURNAL OF PERSONALIZED MEDICINE", "2075-4426", "Citation Manipulation",
  2024, "RADIOLOGIA MEDICA", "0033-8362", "Citation Manipulation",
  2024, "BIOENGINEERED", "2165-5979", "Paper Mill",
  2024, "CONNECTION SCIENCE", "0954-0091", "Paper Mill",
  2024, "MULTIMEDIA TOOLS AND APPLICATIONS", "1380-7501", "Paper Mill",
  2024, "PSYCHIATRIA DANUBINA", "0353-5053", "Paper Mill",
  2024, "JOURNAL OF BIOBASED MATERIALS AND BIOENERGY", "1556-6560", "Paper Mill; Over-presentation Authors of Specific Country",
  2024, "JOURNAL OF BIOMATERIALS AND TISSUE ENGINEERING", "2157-9083", "Paper Mill; Over-presentation Authors of Specific Country",
  2024, "JOURNAL OF BIOMEDICAL NANOTECHNOLOGY", "1550-7033", "Paper Mill; Over-presentation Authors of Specific Country",
  2024, "JOURNAL OF NANOELECTRONICS AND OPTOELECTRONICS", "1555-130X", "Paper Mill; Over-presentation Authors of Specific Country",
  2024, "JOURNAL OF SENSORS", "1687-725X", "Paper Mill; Over-presentation Authors of Specific Country",
  2024, "MATERIALS EXPRESS", "2158-5849", "Paper Mill; Over-presentation Authors of Specific Country",
  2024, "SCIENCE OF ADVANCED MATERIALS", "1947-2935", "Paper Mill; Over-presentation Authors of Specific Country",
  2024, "ALTERNATIVE THERAPIES IN HEALTH AND MEDICINE", "1078-6791", "Over-presentation Authors of Specific Country",
  2024, "CMES-COMPUTER MODELING IN ENGINEERING & SCIENCES", "1526-1492", "Over-presentation Authors of Specific Country",
  2024, "EXPERIMENTAL AND THERAPEUTIC MEDICINE", "1792-0981", "Over-presentation Authors of Specific Country",
  2024, "FRONTIERS IN ENERGY RESEARCH", "2296-598X", "Over-presentation Authors of Specific Country",
  2024, "MATHEMATICAL BIOSCIENCES AND ENGINEERING", "1547-1063", "Over-presentation Authors of Specific Country",
  2024, "TROPICAL JOURNAL OF PHARMACEUTICAL RESEARCH", "1596-5996", "Over-presentation Authors of Specific Country"
)

ewl_2025 <- tribble(
  ~ewl_year, ~journal_title, ~issn_raw, ~warning_reason,
  2025, "Wireless Personal Communications", "0929-6212", "Paper Mill",
  2025, "Natural Resources Forum", "0165-0203", "Paper Mill",
  2025, "Computers & Electrical Engineering", "0045-7906", "Paper Mill",
  2025, "NUMERICAL HEAT TRANSFER PART A-APPLICATIONS", "1040-7782", "Paper Mill",
  2025, "SCALABLE COMPUTING-PRACTICE AND EXPERIENCE", "1895-1767", "Paper Mill"
)

ewl_all <- bind_rows(
  ewl_2020,
  ewl_2021,
  ewl_2023,
  ewl_2024,
  ewl_2025
) %>%
  mutate(
    journal_title_norm = normalize_title(journal_title),
    issn_norm = normalize_issn(issn_raw)
  )

saveRDS(ewl_all, "data_nsfc/ewl_all.rds")

# fix for years without issn column
if (!"issn_raw" %in% names(ewl_all)) ewl_all$issn_raw <- NA_character_
if (!"warning_reason" %in% names(ewl_all)) ewl_all$warning_reason <- NA_character_
if (!"field" %in% names(ewl_all)) ewl_all$field <- NA_character_
if (!"warning_level" %in% names(ewl_all)) ewl_all$warning_level <- NA_character_

# ==============================
# 5) MATCH 2024–2025 BY ISSN
# ==============================
# 
# # =========================================================
# # EARLY WARNING LISTS — MATCHING ROBUSTE ET DOCUMENTÉ
# # =========================================================
# 
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(purrr)
# library(tibble)
# library(fuzzyjoin)
# library(data.table)
# 
# # ==============================
# # 0) HELPERS
# # ==============================
# 
# `%||%` <- function(x, y) {
#   if (is.null(x)) y else x
# }
# 
# normalize_title <- function(x) {
#   x %>%
#     str_to_upper() %>%
#     str_replace_all("&", " AND ") %>%
#     str_replace_all("-", " ") %>%
#     str_replace_all("[:punct:]", " ") %>%
#     str_replace_all("\\bJOURNAL OF\\b", "JOURNAL OF ") %>%
#     str_replace_all("\\bINTL\\b", "INTERNATIONAL") %>%
#     str_squish()
# }
# 
# normalize_issn <- function(x) {
#   x <- as.character(x)
#   x <- str_to_upper(x)
#   x <- str_replace_all(x, "[^0-9X]", "")
#   ifelse(nchar(x) == 8, x, NA_character_)
# }
# 
# extract_issn_vector <- function(x) {
#   if (is.null(x) || length(x) == 0) return(character(0))
#   out <- unlist(x, use.names = FALSE)
#   out <- normalize_issn(out)
#   out <- out[!is.na(out)]
#   unique(out)
# }
# 
# # ==============================
# # 1) APLATIR SOURCES
# # ==============================
# 
# sources_flat <- jours_all %>%
#   as_tibble() %>%
#   mutate(
#     issn_l_norm = as.character(normalize_issn(issn_l)),
#     source_title_norm = normalize_title(display_name),
#     alt_title_list = map(
#       alternate_titles,
#       ~ {
#         if (is.null(.x) || all(is.na(.x))) return(character(0))
#         vals <- as.character(unlist(.x, use.names = FALSE))
#         vals <- vals[!is.na(vals) & nzchar(vals)]
#         normalize_title(vals)
#       }
#     ),
#     issn_vec = map(issn, ~ as.character(extract_issn_vector(.x)))
#   ) %>%
#   transmute(
#     source_id = id,
#     display_name,
#     source_title_norm,
#     alt_title_list,
#     issn_l,
#     issn_l_norm,
#     issn_vec,
#     host_organization_name,
#     country_code,
#     type,
#     works_count
#   )
# 
# # ==============================
# # 2) TABLE LONGUE DES ISSN
# # ==============================
# 
# source_issn_long <- bind_rows(
#   sources_flat %>%
#     filter(!is.na(issn_l_norm), nzchar(issn_l_norm)) %>%
#     transmute(
#       source_id,
#       display_name,
#       host_organization_name,
#       country_code,
#       source_type = type,
#       works_count,
#       issn_norm = as.character(issn_l_norm),
#       match_issn_source = "issn_l"
#     ),
#   sources_flat %>%
#     transmute(
#       source_id,
#       display_name,
#       host_organization_name,
#       country_code,
#       source_type = type,
#       works_count,
#       issn_vec = map(issn_vec, as.character)
#     ) %>%
#     tidyr::unnest_longer(issn_vec, values_to = "issn_norm") %>%
#     mutate(issn_norm = as.character(issn_norm)) %>%
#     filter(!is.na(issn_norm), nzchar(issn_norm)) %>%
#     transmute(
#       source_id,
#       display_name,
#       host_organization_name,
#       country_code,
#       source_type,
#       works_count,
#       issn_norm,
#       match_issn_source = "issn"
#     )
# ) %>%
#   distinct(source_id, issn_norm, .keep_all = TRUE)
# 
# # ==============================
# # 3) TABLE EWL
# # ==============================
# # suppose ewl_all déjà créé :)
# 
# ewl_all <- ewl_all %>%
#   mutate(
#     journal_title_norm = normalize_title(journal_title),
#     issn_norm = normalize_issn(issn_raw)
#   )
# 
# # ==============================
# # 4) MATCH 2024–2025 BY ISSN
# # ==============================
# 
# ewl_issn <- ewl_all %>%
#   filter(ewl_year >= 2024, !is.na(issn_norm))
# 
# match_issn_raw <- ewl_issn %>%
#   left_join(source_issn_long, by = "issn_norm")
# 
# match_issn_resolved <- match_issn_raw %>%
#   group_by(ewl_year, journal_title, issn_norm) %>%
#   mutate(
#     n_matches_total = sum(!is.na(source_id)),
#     n_matches_with_works = sum(!is.na(source_id) & !is.na(works_count) & works_count > 0)
#   ) %>%
#   ungroup()
# 
# # cas propre : un seul candidat avec works_count > 0
# matched_issn_unique_with_works <- match_issn_resolved %>%
#   filter(!is.na(source_id), works_count > 0) %>%
#   group_by(ewl_year, journal_title, issn_norm) %>%
#   filter(n() == 1) %>%
#   ungroup() %>%
#   mutate(
#     match_method = "issn",
#     match_confidence = "high",
#     match_resolution = "unique_with_works"
#   )
# 
# # cas acceptable : un seul match total, mais works_count non positif/manquant
# matched_issn_unique_no_works <- match_issn_resolved %>%
#   group_by(ewl_year, journal_title, issn_norm) %>%
#   filter(
#     n_matches_total == 1,
#     n_matches_with_works == 0,
#     !is.na(source_id)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     match_method = "issn",
#     match_confidence = "medium",
#     match_resolution = "unique_no_works"
#   )
# 
# # cas ambigus ISSN
# ambiguous_issn_multi_with_works <- match_issn_resolved %>%
#   filter(!is.na(source_id), works_count > 0) %>%
#   group_by(ewl_year, journal_title, issn_norm) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   mutate(
#     ambiguity_reason = "issn_multiple_matches_with_works"
#   )
# 
# ambiguous_issn_multi_no_works <- match_issn_resolved %>%
#   group_by(ewl_year, journal_title, issn_norm) %>%
#   filter(
#     n_matches_total > 1,
#     n_matches_with_works == 0,
#     !is.na(source_id)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     ambiguity_reason = "issn_multiple_matches_no_works"
#   )
# 
# ambiguous_issn <- bind_rows(
#   ambiguous_issn_multi_with_works,
#   ambiguous_issn_multi_no_works
# ) %>%
#   distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)
# 
# matched_by_issn <- bind_rows(
#   matched_issn_unique_with_works,
#   matched_issn_unique_no_works
# ) %>%
#   distinct(ewl_year, journal_title, .keep_all = TRUE)
# 
# unmatched_issn <- ewl_issn %>%
#   anti_join(
#     matched_by_issn %>% select(ewl_year, journal_title),
#     by = c("ewl_year", "journal_title")
#   )
# 
# # ==============================
# # 5) MATCH 2020–2023 BY TITLE EXACT
# # ==============================
# 
# ewl_title_old <- ewl_all %>%
#   filter(ewl_year <= 2023)
# 
# source_exact_display <- sources_flat %>%
#   select(
#     source_id,
#     display_name,
#     source_title_norm,
#     alt_title_list,
#     host_organization_name,
#     country_code,
#     type,
#     works_count
#   )
# 
# match_exact_display_raw <- ewl_title_old %>%
#   left_join(
#     source_exact_display,
#     by = c("journal_title_norm" = "source_title_norm")
#   )
# 
# match_exact_display_resolved <- match_exact_display_raw %>%
#   group_by(ewl_year, journal_title) %>%
#   mutate(
#     n_matches_total = sum(!is.na(source_id)),
#     n_matches_with_works = sum(!is.na(source_id) & !is.na(works_count) & works_count > 0)
#   ) %>%
#   ungroup()
# 
# matched_exact_display_unique_with_works <- match_exact_display_resolved %>%
#   filter(!is.na(source_id), works_count > 0) %>%
#   group_by(ewl_year, journal_title) %>%
#   filter(n() == 1) %>%
#   ungroup() %>%
#   mutate(
#     match_method = "title_exact_display",
#     match_confidence = "high",
#     match_resolution = "unique_with_works"
#   )
# 
# matched_exact_display_unique_no_works <- match_exact_display_resolved %>%
#   group_by(ewl_year, journal_title) %>%
#   filter(
#     n_matches_total == 1,
#     n_matches_with_works == 0,
#     !is.na(source_id)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     match_method = "title_exact_display",
#     match_confidence = "medium",
#     match_resolution = "unique_no_works"
#   )
# 
# ambiguous_exact_display <- bind_rows(
#   match_exact_display_resolved %>%
#     filter(!is.na(source_id), works_count > 0) %>%
#     group_by(ewl_year, journal_title) %>%
#     filter(n() > 1) %>%
#     ungroup() %>%
#     mutate(ambiguity_reason = "title_exact_multiple_matches_with_works"),
#   
#   match_exact_display_resolved %>%
#     group_by(ewl_year, journal_title) %>%
#     filter(
#       n_matches_total > 1,
#       n_matches_with_works == 0,
#       !is.na(source_id)
#     ) %>%
#     ungroup() %>%
#     mutate(ambiguity_reason = "title_exact_multiple_matches_no_works")
# ) %>%
#   distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)
# 
# matched_exact <- bind_rows(
#   matched_exact_display_unique_with_works,
#   matched_exact_display_unique_no_works
# ) %>%
#   distinct(ewl_year, journal_title, .keep_all = TRUE)
# 
# unmatched_old <- ewl_title_old %>%
#   anti_join(
#     matched_exact %>% select(ewl_year, journal_title),
#     by = c("ewl_year", "journal_title")
#   )
# 
# # ==============================
# # 6) MATCH 2020–2023 BY ALTERNATE TITLES
# # ==============================
# 
# source_alt_long <- sources_flat %>%
#   select(
#     source_id,
#     display_name,
#     alt_title_list,
#     host_organization_name,
#     country_code,
#     type,
#     works_count
#   ) %>%
#   tidyr::unnest_longer(alt_title_list, values_to = "alt_title_norm") %>%
#   filter(!is.na(alt_title_norm), nzchar(alt_title_norm)) %>%
#   distinct(source_id, alt_title_norm, .keep_all = TRUE)
# 
# match_alt_raw <- unmatched_old %>%
#   left_join(
#     source_alt_long,
#     by = c("journal_title_norm" = "alt_title_norm")
#   )
# 
# match_alt_resolved <- match_alt_raw %>%
#   group_by(ewl_year, journal_title) %>%
#   mutate(
#     n_matches_total = sum(!is.na(source_id)),
#     n_matches_with_works = sum(!is.na(source_id) & !is.na(works_count) & works_count > 0)
#   ) %>%
#   ungroup()
# 
# matched_alt_unique_with_works <- match_alt_resolved %>%
#   filter(!is.na(source_id), works_count > 0) %>%
#   group_by(ewl_year, journal_title) %>%
#   filter(n() == 1) %>%
#   ungroup() %>%
#   mutate(
#     match_method = "title_exact_alternate",
#     match_confidence = "high",
#     match_resolution = "unique_with_works"
#   )
# 
# matched_alt_unique_no_works <- match_alt_resolved %>%
#   group_by(ewl_year, journal_title) %>%
#   filter(
#     n_matches_total == 1,
#     n_matches_with_works == 0,
#     !is.na(source_id)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     match_method = "title_exact_alternate",
#     match_confidence = "medium",
#     match_resolution = "unique_no_works"
#   )
# 
# ambiguous_alt <- bind_rows(
#   match_alt_resolved %>%
#     filter(!is.na(source_id), works_count > 0) %>%
#     group_by(ewl_year, journal_title) %>%
#     filter(n() > 1) %>%
#     ungroup() %>%
#     mutate(ambiguity_reason = "title_alt_multiple_matches_with_works"),
#   
#   match_alt_resolved %>%
#     group_by(ewl_year, journal_title) %>%
#     filter(
#       n_matches_total > 1,
#       n_matches_with_works == 0,
#       !is.na(source_id)
#     ) %>%
#     ungroup() %>%
#     mutate(ambiguity_reason = "title_alt_multiple_matches_no_works")
# ) %>%
#   distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)
# 
# matched_alt <- bind_rows(
#   matched_alt_unique_with_works,
#   matched_alt_unique_no_works
# ) %>%
#   distinct(ewl_year, journal_title, .keep_all = TRUE)
# 
# unmatched_old2 <- unmatched_old %>%
#   anti_join(
#     matched_alt %>% select(ewl_year, journal_title),
#     by = c("ewl_year", "journal_title")
#   )
# 
# # ==============================
# # 7) FUZZY MATCH FOR REMAINING 2020–2023
# # ==============================
# 
# source_titles_for_fuzzy <- sources_flat %>%
#   select(
#     source_id,
#     display_name,
#     source_title_norm,
#     host_organization_name,
#     country_code,
#     type,
#     works_count
#   ) %>%
#   filter(!is.na(source_title_norm), nzchar(source_title_norm))
# 
# matched_fuzzy_raw <- stringdist_left_join(
#   unmatched_old2,
#   source_titles_for_fuzzy,
#   by = c("journal_title_norm" = "source_title_norm"),
#   method = "jw",
#   max_dist = 0.08,
#   distance_col = "dist_jw"
# )
# 
# matched_fuzzy_resolved <- matched_fuzzy_raw %>%
#   group_by(ewl_year, journal_title) %>%
#   arrange(dist_jw, desc(works_count), .by_group = TRUE) %>%
#   mutate(
#     rank_candidate = row_number(),
#     best_dist = first(dist_jw),
#     n_best = sum(dist_jw == best_dist, na.rm = TRUE),
#     n_best_with_works = sum(dist_jw == best_dist & !is.na(works_count) & works_count > 0, na.rm = TRUE)
#   ) %>%
#   ungroup()
# 
# # garder seulement si meilleur candidat unique
# matched_fuzzy_unique <- matched_fuzzy_resolved %>%
#   group_by(ewl_year, journal_title) %>%
#   filter(rank_candidate == 1) %>%
#   ungroup() %>%
#   filter(!is.na(source_id)) %>%
#   filter(
#     (n_best == 1 & n_best_with_works <= 1) |
#       (n_best_with_works == 1)
#   ) %>%
#   mutate(
#     match_method = "title_fuzzy_jw",
#     match_confidence = case_when(
#       !is.na(dist_jw) & dist_jw <= 0.02 ~ "high",
#       !is.na(dist_jw) & dist_jw <= 0.05 ~ "medium",
#       !is.na(dist_jw) & dist_jw <= 0.08 ~ "low",
#       TRUE ~ NA_character_
#     ),
#     match_resolution = "best_unique_fuzzy"
#   )
# 
# # cas ambigus fuzzy
# ambiguous_fuzzy <- matched_fuzzy_resolved %>%
#   filter(!is.na(source_id)) %>%
#   filter(
#     (n_best > 1) |
#       (n_best_with_works > 1)
#   ) %>%
#   mutate(
#     ambiguity_reason = "fuzzy_tied_best_match"
#   ) %>%
#   distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)
# 
# matched_fuzzy2 <- matched_fuzzy_unique %>%
#   distinct(ewl_year, journal_title, .keep_all = TRUE)
# 
# unmatched_fuzzy <- unmatched_old2 %>%
#   anti_join(
#     matched_fuzzy2 %>% select(ewl_year, journal_title),
#     by = c("ewl_year", "journal_title")
#   )
# 
# # ==============================
# # 8) ASSEMBLER TOUS LES MATCHES
# # ==============================
# 
# matched_all <- bind_rows(
#   matched_by_issn %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       matched_display_name = display_name,
#       matched_host_organization = host_organization_name,
#       matched_country_code = country_code,
#       matched_source_type = source_type,
#       works_count,
#       match_method, match_confidence,
#       match_distance = NA_real_,
#       match_resolution
#     ),
#   
#   matched_exact %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       matched_display_name = display_name,
#       matched_host_organization = host_organization_name,
#       matched_country_code = country_code,
#       matched_source_type = type,
#       works_count,
#       match_method, match_confidence,
#       match_distance = NA_real_,
#       match_resolution
#     ),
#   
#   matched_alt %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       matched_display_name = display_name,
#       matched_host_organization = host_organization_name,
#       matched_country_code = country_code,
#       matched_source_type = type,
#       works_count,
#       match_method, match_confidence,
#       match_distance = NA_real_,
#       match_resolution
#     ),
#   
#   matched_fuzzy2 %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       matched_display_name = display_name,
#       matched_host_organization = host_organization_name,
#       matched_country_code = country_code,
#       matched_source_type = type,
#       works_count,
#       match_method, match_confidence,
#       match_distance = dist_jw,
#       match_resolution
#     )
# ) %>%
#   distinct(ewl_year, journal_title, .keep_all = TRUE) %>%
#   mutate(
#     needs_manual_check = case_when(
#       match_method == "title_fuzzy_jw" & match_confidence != "high" ~ TRUE,
#       TRUE ~ FALSE
#     )
#   )
# 
# # ==============================
# # 9) DOCUMENTER LES CAS AMBIGUS
# # ==============================
# 
# ambiguous_all <- bind_rows(
#   ambiguous_issn %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       candidate_display_name = display_name,
#       candidate_host_organization = host_organization_name,
#       candidate_country_code = country_code,
#       candidate_source_type = source_type,
#       works_count,
#       ambiguity_reason
#     ),
#   ambiguous_exact_display %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       candidate_display_name = display_name,
#       candidate_host_organization = host_organization_name,
#       candidate_country_code = country_code,
#       candidate_source_type = type,
#       works_count,
#       ambiguity_reason
#     ),
#   ambiguous_alt %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       candidate_display_name = display_name,
#       candidate_host_organization = host_organization_name,
#       candidate_country_code = country_code,
#       candidate_source_type = type,
#       works_count,
#       ambiguity_reason
#     ),
#   ambiguous_fuzzy %>%
#     transmute(
#       ewl_year, field, journal_title, warning_level, warning_reason,
#       issn_raw, issn_norm,
#       source_id,
#       candidate_display_name = display_name,
#       candidate_host_organization = host_organization_name,
#       candidate_country_code = country_code,
#       candidate_source_type = type,
#       works_count,
#       ambiguity_reason
#     )
# ) %>%
#   distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)
# 
# # ==============================
# # 10) NON MATCHÉS
# # ==============================
# 
# unmatched_all <- bind_rows(
#   unmatched_issn,
#   unmatched_fuzzy
# ) %>%
#   distinct(ewl_year, journal_title, .keep_all = TRUE) %>%
#   mutate(
#     source_id = NA_character_,
#     matched_display_name = NA_character_,
#     matched_host_organization = NA_character_,
#     matched_country_code = NA_character_,
#     matched_source_type = NA_character_,
#     works_count = NA_real_,
#     match_method = NA_character_,
#     match_confidence = NA_character_,
#     match_distance = NA_real_,
#     match_resolution = NA_character_,
#     needs_manual_check = TRUE
#   )
# 
# # ==============================
# # 11) TABLE FINALE
# # ==============================
# 
# early_warning_master <- bind_rows(
#   matched_all,
#   unmatched_all
# ) %>%
#   distinct(ewl_year, journal_title, .keep_all = TRUE) %>%
#   arrange(ewl_year, journal_title)
# 
# # ==============================
# # 12) CONTRÔLES
# # ==============================
# 
# match_summary <- early_warning_master %>%
#   count(ewl_year, match_method, match_confidence, match_resolution, needs_manual_check, sort = TRUE)
# 
# year_summary <- early_warning_master %>%
#   group_by(ewl_year) %>%
#   summarise(
#     n_titles = n(),
#     n_matched = sum(!is.na(source_id)),
#     match_rate = n_matched / n_titles,
#     n_manual_check = sum(needs_manual_check),
#     .groups = "drop"
#   )
# 
# ambiguity_summary <- ambiguous_all %>%
#   count(ewl_year, ambiguity_reason, sort = TRUE)
# 
# print(match_summary)
# print(year_summary)
# print(ambiguity_summary)
# 
# # ==============================
# # 13) SAUVEGARDE
# # ==============================
# 
# saveRDS(early_warning_master, "data_nsfc/early_warning_master_matched.rds", compress = "xz")
# fwrite(early_warning_master, "data_nsfc/early_warning_master_matched.csv")
# early_warning_master <- readRDS("data_nsfc/early_warning_master_matched.rds")
# 
# saveRDS(ambiguous_all, "data_nsfc/early_warning_ambiguous_cases.rds", compress = "xz")
# fwrite(ambiguous_all, "data_nsfc/early_warning_ambiguous_cases.csv")
# ambiguous_all <- readRDS("data_nsfc/early_warning_ambiguous_cases.rds")
# 
# fwrite(unmatched_all, "data_nsfc/early_warning_unmatched_cases.csv")


# =========================================================
# EARLY WARNING LISTS — MATCHING ROBUSTE ET DOCUMENTÉ
# =========================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(fuzzyjoin)
library(data.table)

# ==============================
# 0) HELPERS
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_title <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("[:punct:]", " ") %>%
    str_replace_all("\\bJOURNAL OF\\b", "JOURNAL OF ") %>%
    str_replace_all("\\bINTL\\b", "INTERNATIONAL") %>%
    str_squish()
}

normalize_issn <- function(x) {
  x <- as.character(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^0-9X]", "")
  ifelse(nchar(x) == 8, x, NA_character_)
}

extract_issn_vector <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  out <- unlist(x, use.names = FALSE)
  out <- normalize_issn(out)
  out <- out[!is.na(out)]
  unique(out)
}

# ==============================
# 1) APLATIR SOURCES
# ==============================

sources_flat <- jours_all %>%
  as_tibble() %>%
  mutate(
    issn_l_norm = as.character(normalize_issn(issn_l)),
    source_title_norm = normalize_title(display_name),
    alt_title_list = map(
      alternate_titles,
      ~ {
        if (is.null(.x) || all(is.na(.x))) return(character(0))
        vals <- as.character(unlist(.x, use.names = FALSE))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        normalize_title(vals)
      }
    ),
    issn_vec = map(issn, ~ as.character(extract_issn_vector(.x)))
  ) %>%
  transmute(
    source_id = id,
    display_name,
    source_title_norm,
    alt_title_list,
    issn_l,
    issn_l_norm,
    issn_vec,
    host_organization_name,
    country_code,
    type,
    works_count
  )

# ==============================
# 2) TABLE LONGUE DES ISSN
# ==============================

source_issn_long <- bind_rows(
  sources_flat %>%
    filter(!is.na(issn_l_norm), nzchar(issn_l_norm)) %>%
    transmute(
      source_id,
      display_name,
      host_organization_name,
      country_code,
      source_type = type,
      works_count,
      issn_norm = as.character(issn_l_norm),
      match_issn_source = "issn_l"
    ),
  sources_flat %>%
    transmute(
      source_id,
      display_name,
      host_organization_name,
      country_code,
      source_type = type,
      works_count,
      issn_vec = map(issn_vec, as.character)
    ) %>%
    tidyr::unnest_longer(issn_vec, values_to = "issn_norm") %>%
    mutate(issn_norm = as.character(issn_norm)) %>%
    filter(!is.na(issn_norm), nzchar(issn_norm)) %>%
    transmute(
      source_id,
      display_name,
      host_organization_name,
      country_code,
      source_type,
      works_count,
      issn_norm,
      match_issn_source = "issn"
    )
) %>%
  distinct(source_id, issn_norm, .keep_all = TRUE)

# ==============================
# 3) TABLE EWL
# ==============================
# suppose ewl_all déjà créé

ewl_all <- ewl_all %>%
  mutate(
    journal_title_norm = normalize_title(journal_title),
    issn_norm = normalize_issn(issn_raw)
  )

# ==============================
# 4) MATCH 2024–2025 BY ISSN
# ==============================

ewl_issn <- ewl_all %>%
  filter(ewl_year >= 2024, !is.na(issn_norm))

match_issn_raw <- ewl_issn %>%
  left_join(source_issn_long, by = "issn_norm")

match_issn_resolved <- match_issn_raw %>%
  group_by(ewl_year, journal_title, issn_norm) %>%
  mutate(
    n_matches_total = sum(!is.na(source_id)),
    n_matches_with_works = sum(!is.na(source_id) & !is.na(works_count) & works_count > 0)
  ) %>%
  ungroup()

# cas propre : un seul candidat avec works_count > 0
matched_issn_unique_with_works <- match_issn_resolved %>%
  filter(!is.na(source_id), works_count > 0) %>%
  group_by(ewl_year, journal_title, issn_norm) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  mutate(
    match_method = "issn",
    match_confidence = "high",
    match_resolution = "unique_with_works"
  )

# cas acceptable : un seul match total, mais works_count non positif/manquant
matched_issn_unique_no_works <- match_issn_resolved %>%
  group_by(ewl_year, journal_title, issn_norm) %>%
  filter(
    n_matches_total == 1,
    n_matches_with_works == 0,
    !is.na(source_id)
  ) %>%
  ungroup() %>%
  mutate(
    match_method = "issn",
    match_confidence = "medium",
    match_resolution = "unique_no_works"
  )

# cas ambigus ISSN
ambiguous_issn_multi_with_works <- match_issn_resolved %>%
  filter(!is.na(source_id), works_count > 0) %>%
  group_by(ewl_year, journal_title, issn_norm) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(
    ambiguity_reason = "issn_multiple_matches_with_works"
  )

ambiguous_issn_multi_no_works <- match_issn_resolved %>%
  group_by(ewl_year, journal_title, issn_norm) %>%
  filter(
    n_matches_total > 1,
    n_matches_with_works == 0,
    !is.na(source_id)
  ) %>%
  ungroup() %>%
  mutate(
    ambiguity_reason = "issn_multiple_matches_no_works"
  )

ambiguous_issn <- bind_rows(
  ambiguous_issn_multi_with_works,
  ambiguous_issn_multi_no_works
) %>%
  distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)

matched_by_issn <- bind_rows(
  matched_issn_unique_with_works,
  matched_issn_unique_no_works
) %>%
  distinct(ewl_year, journal_title, .keep_all = TRUE)

unmatched_issn <- ewl_issn %>%
  anti_join(
    matched_by_issn %>% select(ewl_year, journal_title),
    by = c("ewl_year", "journal_title")
  )

# ==============================
# 5) MATCH 2020–2023 BY TITLE EXACT
# ==============================

ewl_title_old <- ewl_all %>%
  filter(ewl_year <= 2023)

source_exact_display <- sources_flat %>%
  select(
    source_id,
    display_name,
    source_title_norm,
    alt_title_list,
    host_organization_name,
    country_code,
    type,
    works_count
  )

match_exact_display_raw <- ewl_title_old %>%
  left_join(
    source_exact_display,
    by = c("journal_title_norm" = "source_title_norm")
  )

match_exact_display_resolved <- match_exact_display_raw %>%
  group_by(ewl_year, journal_title) %>%
  mutate(
    n_matches_total = sum(!is.na(source_id)),
    n_matches_with_works = sum(!is.na(source_id) & !is.na(works_count) & works_count > 0)
  ) %>%
  ungroup()

matched_exact_display_unique_with_works <- match_exact_display_resolved %>%
  filter(!is.na(source_id), works_count > 0) %>%
  group_by(ewl_year, journal_title) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  mutate(
    match_method = "title_exact_display",
    match_confidence = "high",
    match_resolution = "unique_with_works"
  )

matched_exact_display_unique_no_works <- match_exact_display_resolved %>%
  group_by(ewl_year, journal_title) %>%
  filter(
    n_matches_total == 1,
    n_matches_with_works == 0,
    !is.na(source_id)
  ) %>%
  ungroup() %>%
  mutate(
    match_method = "title_exact_display",
    match_confidence = "medium",
    match_resolution = "unique_no_works"
  )

ambiguous_exact_display <- bind_rows(
  match_exact_display_resolved %>%
    filter(!is.na(source_id), works_count > 0) %>%
    group_by(ewl_year, journal_title) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    mutate(ambiguity_reason = "title_exact_multiple_matches_with_works"),
  
  match_exact_display_resolved %>%
    group_by(ewl_year, journal_title) %>%
    filter(
      n_matches_total > 1,
      n_matches_with_works == 0,
      !is.na(source_id)
    ) %>%
    ungroup() %>%
    mutate(ambiguity_reason = "title_exact_multiple_matches_no_works")
) %>%
  distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)

matched_exact <- bind_rows(
  matched_exact_display_unique_with_works,
  matched_exact_display_unique_no_works
) %>%
  distinct(ewl_year, journal_title, .keep_all = TRUE)

unmatched_old <- ewl_title_old %>%
  anti_join(
    matched_exact %>% select(ewl_year, journal_title),
    by = c("ewl_year", "journal_title")
  )

# ==============================
# 6) MATCH 2020–2023 BY ALTERNATE TITLES
# ==============================

source_alt_long <- sources_flat %>%
  select(
    source_id,
    display_name,
    alt_title_list,
    host_organization_name,
    country_code,
    type,
    works_count
  ) %>%
  tidyr::unnest_longer(alt_title_list, values_to = "alt_title_norm") %>%
  filter(!is.na(alt_title_norm), nzchar(alt_title_norm)) %>%
  distinct(source_id, alt_title_norm, .keep_all = TRUE)

match_alt_raw <- unmatched_old %>%
  left_join(
    source_alt_long,
    by = c("journal_title_norm" = "alt_title_norm")
  )

match_alt_resolved <- match_alt_raw %>%
  group_by(ewl_year, journal_title) %>%
  mutate(
    n_matches_total = sum(!is.na(source_id)),
    n_matches_with_works = sum(!is.na(source_id) & !is.na(works_count) & works_count > 0)
  ) %>%
  ungroup()

matched_alt_unique_with_works <- match_alt_resolved %>%
  filter(!is.na(source_id), works_count > 0) %>%
  group_by(ewl_year, journal_title) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  mutate(
    match_method = "title_exact_alternate",
    match_confidence = "high",
    match_resolution = "unique_with_works"
  )

matched_alt_unique_no_works <- match_alt_resolved %>%
  group_by(ewl_year, journal_title) %>%
  filter(
    n_matches_total == 1,
    n_matches_with_works == 0,
    !is.na(source_id)
  ) %>%
  ungroup() %>%
  mutate(
    match_method = "title_exact_alternate",
    match_confidence = "medium",
    match_resolution = "unique_no_works"
  )

ambiguous_alt <- bind_rows(
  match_alt_resolved %>%
    filter(!is.na(source_id), works_count > 0) %>%
    group_by(ewl_year, journal_title) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    mutate(ambiguity_reason = "title_alt_multiple_matches_with_works"),
  
  match_alt_resolved %>%
    group_by(ewl_year, journal_title) %>%
    filter(
      n_matches_total > 1,
      n_matches_with_works == 0,
      !is.na(source_id)
    ) %>%
    ungroup() %>%
    mutate(ambiguity_reason = "title_alt_multiple_matches_no_works")
) %>%
  distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)

matched_alt <- bind_rows(
  matched_alt_unique_with_works,
  matched_alt_unique_no_works
) %>%
  distinct(ewl_year, journal_title, .keep_all = TRUE)

unmatched_old2 <- unmatched_old %>%
  anti_join(
    matched_alt %>% select(ewl_year, journal_title),
    by = c("ewl_year", "journal_title")
  )

# ==============================
# 7) FUZZY MATCH FOR REMAINING 2020–2023
# ==============================

source_titles_for_fuzzy <- sources_flat %>%
  select(
    source_id,
    display_name,
    source_title_norm,
    host_organization_name,
    country_code,
    type,
    works_count
  ) %>%
  filter(!is.na(source_title_norm), nzchar(source_title_norm))

matched_fuzzy_raw <- stringdist_left_join(
  unmatched_old2,
  source_titles_for_fuzzy,
  by = c("journal_title_norm" = "source_title_norm"),
  method = "jw",
  max_dist = 0.08,
  distance_col = "dist_jw"
)

matched_fuzzy_resolved <- matched_fuzzy_raw %>%
  group_by(ewl_year, journal_title) %>%
  arrange(dist_jw, desc(works_count), .by_group = TRUE) %>%
  mutate(
    rank_candidate = row_number(),
    best_dist = first(dist_jw),
    n_best = sum(dist_jw == best_dist, na.rm = TRUE),
    n_best_with_works = sum(dist_jw == best_dist & !is.na(works_count) & works_count > 0, na.rm = TRUE)
  ) %>%
  ungroup()

# garder seulement si meilleur candidat unique
matched_fuzzy_unique <- matched_fuzzy_resolved %>%
  group_by(ewl_year, journal_title) %>%
  filter(rank_candidate == 1) %>%
  ungroup() %>%
  filter(!is.na(source_id)) %>%
  filter(
    (n_best == 1 & n_best_with_works <= 1) |
      (n_best_with_works == 1)
  ) %>%
  mutate(
    match_method = "title_fuzzy_jw",
    match_confidence = case_when(
      !is.na(dist_jw) & dist_jw <= 0.02 ~ "high",
      !is.na(dist_jw) & dist_jw <= 0.05 ~ "medium",
      !is.na(dist_jw) & dist_jw <= 0.08 ~ "low",
      TRUE ~ NA_character_
    ),
    match_resolution = "best_unique_fuzzy"
  )

# cas ambigus fuzzy
ambiguous_fuzzy <- matched_fuzzy_resolved %>%
  filter(!is.na(source_id)) %>%
  filter(
    (n_best > 1) |
      (n_best_with_works > 1)
  ) %>%
  mutate(
    ambiguity_reason = "fuzzy_tied_best_match"
  ) %>%
  distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)

matched_fuzzy2 <- matched_fuzzy_unique %>%
  distinct(ewl_year, journal_title, .keep_all = TRUE)

unmatched_fuzzy <- unmatched_old2 %>%
  anti_join(
    matched_fuzzy2 %>% select(ewl_year, journal_title),
    by = c("ewl_year", "journal_title")
  )

# ==============================
# 8) ASSEMBLER TOUS LES MATCHES
# ==============================

matched_all <- bind_rows(
  matched_by_issn %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      matched_display_name = display_name,
      matched_host_organization = host_organization_name,
      matched_country_code = country_code,
      matched_source_type = source_type,
      works_count,
      match_method, match_confidence,
      match_distance = NA_real_,
      match_resolution
    ),
  
  matched_exact %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      matched_display_name = display_name,
      matched_host_organization = host_organization_name,
      matched_country_code = country_code,
      matched_source_type = type,
      works_count,
      match_method, match_confidence,
      match_distance = NA_real_,
      match_resolution
    ),
  
  matched_alt %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      matched_display_name = display_name,
      matched_host_organization = host_organization_name,
      matched_country_code = country_code,
      matched_source_type = type,
      works_count,
      match_method, match_confidence,
      match_distance = NA_real_,
      match_resolution
    ),
  
  matched_fuzzy2 %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      matched_display_name = display_name,
      matched_host_organization = host_organization_name,
      matched_country_code = country_code,
      matched_source_type = type,
      works_count,
      match_method, match_confidence,
      match_distance = dist_jw,
      match_resolution
    )
) %>%
  distinct(ewl_year, journal_title, .keep_all = TRUE) %>%
  mutate(
    needs_manual_check = case_when(
      match_method == "title_fuzzy_jw" & match_confidence != "high" ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ==============================
# 9) DOCUMENTER LES CAS AMBIGUS
# ==============================

ambiguous_all <- bind_rows(
  ambiguous_issn %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      candidate_display_name = display_name,
      candidate_host_organization = host_organization_name,
      candidate_country_code = country_code,
      candidate_source_type = source_type,
      works_count,
      ambiguity_reason
    ),
  ambiguous_exact_display %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      candidate_display_name = display_name,
      candidate_host_organization = host_organization_name,
      candidate_country_code = country_code,
      candidate_source_type = type,
      works_count,
      ambiguity_reason
    ),
  ambiguous_alt %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      candidate_display_name = display_name,
      candidate_host_organization = host_organization_name,
      candidate_country_code = country_code,
      candidate_source_type = type,
      works_count,
      ambiguity_reason
    ),
  ambiguous_fuzzy %>%
    transmute(
      ewl_year, field, journal_title, warning_level, warning_reason,
      issn_raw, issn_norm,
      source_id,
      candidate_display_name = display_name,
      candidate_host_organization = host_organization_name,
      candidate_country_code = country_code,
      candidate_source_type = type,
      works_count,
      ambiguity_reason
    )
) %>%
  distinct(ewl_year, journal_title, source_id, .keep_all = TRUE)

# ==============================
# 10) NON MATCHÉS
# ==============================

unmatched_all <- bind_rows(
  unmatched_issn,
  unmatched_fuzzy
) %>%
  distinct(ewl_year, journal_title, .keep_all = TRUE) %>%
  mutate(
    source_id = NA_character_,
    matched_display_name = NA_character_,
    matched_host_organization = NA_character_,
    matched_country_code = NA_character_,
    matched_source_type = NA_character_,
    works_count = NA_real_,
    match_method = NA_character_,
    match_confidence = NA_character_,
    match_distance = NA_real_,
    match_resolution = NA_character_,
    needs_manual_check = TRUE
  )

# ==============================
# 11) TABLE FINALE
# ==============================

early_warning_master <- bind_rows(
  matched_all,
  unmatched_all
) %>%
  distinct(ewl_year, journal_title, .keep_all = TRUE) %>%
  arrange(ewl_year, journal_title)

# ==============================
# 12) CONTRÔLES
# ==============================

match_summary <- early_warning_master %>%
  count(ewl_year, match_method, match_confidence, match_resolution, needs_manual_check, sort = TRUE)

year_summary <- early_warning_master %>%
  group_by(ewl_year) %>%
  summarise(
    n_titles = n(),
    n_matched = sum(!is.na(source_id)),
    match_rate = n_matched / n_titles,
    n_manual_check = sum(needs_manual_check),
    .groups = "drop"
  )

ambiguity_summary <- ambiguous_all %>%
  count(ewl_year, ambiguity_reason, sort = TRUE)

print(match_summary)
print(year_summary)
print(ambiguity_summary)

# ==============================
# 13) SAUVEGARDE
# ==============================

saveRDS(early_warning_master, "data_nsfc/early_warning_master_matched.rds", compress = "xz")
fwrite(early_warning_master, "data_nsfc/early_warning_master_matched.csv")

saveRDS(ambiguous_all, "data_nsfc/early_warning_ambiguous_cases.rds", compress = "xz")
fwrite(ambiguous_all, "data_nsfc/early_warning_ambiguous_cases.csv")

fwrite(unmatched_all, "data_nsfc/early_warning_unmatched_cases.csv")


# =========================================================
# NSFC × EARLY WARNING JOURNALS
# SECTION RÉSULTATS — DESCRIPTION APPROFONDIE
# =========================================================
# Prérequis :
# - data_nsfc/nsfc_augmented_publishers.rds
# - data_nsfc/early_warning_master_matched.rds
#
# Sorties :
# - dataset analytique nsfc_ewl_analysis
# - figures publication-ready
# - tableaux de contrôle
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(patchwork)
library(tidyr)

# ==============================
# 1) CHARGEMENT
# ==============================

nsfc <- readRDS("data_nsfc/all_nsfc_augmented.rds")
ewl_master <- readRDS("data_nsfc/early_warning_master_matched.rds")

nsfc <- as_tibble(nsfc)
ewl_master <- as_tibble(ewl_master)

# ==============================
# 2) HELPERS
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_issn <- function(x) {
  x <- as.character(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^0-9X]", "")
  ifelse(nchar(x) == 8, x, NA_character_)
}

theme_pub <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0, colour = "#1A1A1A"),
      plot.subtitle = element_text(size = base_size, hjust = 0, colour = "#4D4D4D"),
      plot.caption = element_text(size = base_size - 2, colour = "#666666"),
      axis.title = element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = element_text(size = base_size - 1, colour = "#262626"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFEFEF", linewidth = 0.30),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.width = unit(1.2, "cm"),
      strip.text = element_text(face = "bold", colour = "#1A1A1A"),
      plot.margin = margin(12, 14, 12, 12)
    )
}

# ==============================
# 3) PARAMÈTRES ANALYTIQUES
# ==============================

publisher_levels_keep <- c(
  "Grey publishers",
  "Big 5 publishers",
  "Major society / university / non-profit publishers",
  "Chinese publishers",
  "Other international commercial publishers"
)

publisher_labels_short <- c(
  "Grey publishers" = "Grey",
  "Big 5 publishers" = "Big 5",
  "Major society / university / non-profit publishers" = "Societies / Univ.",
  "Chinese publishers" = "Chinese",
  "Other international commercial publishers" = "Other intl."
)

pal_publishers <- c(
  "Grey publishers" = "#C44E52",
  "Big 5 publishers" = "#1F4E79",
  "Major society / university / non-profit publishers" = "#4C9F70",
  "Chinese publishers" = "#B07AA1",
  "Other international commercial publishers" = "#7A7A7A"
)

pal_before_after <- c(
  "Before listing" = "#7A7A7A",
  "After listing" = "#C44E52"
)

pal_warning <- c(
  "High" = "#9C1C26",
  "Medium" = "#E08E45",
  "Low" = "#4C78A8",
  "Other / reason-based" = "#7A7A7A"
)

event_window_min <- -5
event_window_max <- 5

# ==============================
# 4) PRÉPARER LE DATASET D’ANALYSE
# ==============================

nsfc <- nsfc %>%
  mutate(
    issn_l_norm = normalize_issn(issn_l)
  )

ewl_match <- ewl_master %>%
  filter(!is.na(source_id)) %>%
  mutate(
    issn_norm = normalize_issn(issn_norm),
    warning_level2 = case_when(
      !is.na(warning_level) ~ warning_level,
      TRUE ~ "Other / reason-based"
    )
  ) %>%
  group_by(source_id) %>%
  arrange(ewl_year, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

nsfc_ewl_analysis <- nsfc %>%
  left_join(
    ewl_match %>%
      select(
        source_id,
        ewl_year,
        field,
        journal_title,
        warning_level,
        warning_level2,
        warning_reason,
        matched_display_name,
        matched_host_organization,
        matched_country_code,
        match_method,
        match_confidence
      ),
    by = c("source_id")
  ) %>%
  mutate(
    in_early_warning = !is.na(ewl_year),
    event_time = publication_year - ewl_year,
    before_after = case_when(
      in_early_warning & publication_year < ewl_year ~ "Before listing",
      in_early_warning & publication_year >= ewl_year ~ "After listing",
      TRUE ~ NA_character_
    ),
    before_after = factor(before_after, levels = c("Before listing", "After listing")),
    publisher_group = factor(publisher_group, levels = publisher_levels_keep)
  )

# Restreindre à la typologie d'éditeurs utile
nsfc_ewl_analysis2 <- nsfc_ewl_analysis %>%
  filter(
    publisher_group %in% publisher_levels_keep | is.na(publisher_group)
  )

# Sous-ensemble des pubs dans revues EWL
ewl_pubs <- nsfc_ewl_analysis2 %>%
  filter(in_early_warning, !is.na(publisher_group))

# Sous-ensemble avec fenêtre événementielle
ewl_event <- ewl_pubs %>%
  filter(event_time >= event_window_min, 
         event_time <= event_window_max)

# ==============================
# 5) CONTRÔLES RAPIDES
# ==============================

tab_main <- nsfc_ewl_analysis2 %>%
  summarise(
    n_total = n(),
    n_ewl = sum(in_early_warning, na.rm = TRUE),
    share_ewl = n_ewl / n_total
  )

tab_warning <- ewl_pubs %>%
  count(warning_level2, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1))

tab_publishers_ewl <- ewl_pubs %>%
  count(publisher_group, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1))

print(tab_main)
print(tab_warning)
print(tab_publishers_ewl)

# ==============================
# 6) FIGURE 1
# PART DES REVUES EWL DANS LE CORPUS NSFC
# ==============================

fig1_data <- nsfc_ewl_analysis2 %>%
  group_by(publication_year) %>%
  summarise(
    total = n(),
    ewl_n = sum(in_early_warning, na.rm = TRUE),
    ewl_share = ewl_n / total,
    .groups = "drop"
  )

g1 <- ggplot(fig1_data, aes(x = publication_year, y = ewl_share)) +
  geom_line(linewidth = 1.6, colour = "#C44E52", lineend = "round") +
  geom_point(size = 2.7, colour = "#C44E52") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(
    breaks = pretty(fig1_data$publication_year, n = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "The exposure of NSFC-funded research to listed journals changes over time",
    subtitle = "Annual share of NSFC publications appearing in journals that entered an Early Warning List",
    x = NULL,
    y = "Share of publications"
  ) +
  theme_pub()

# ==============================
# 7) FIGURE 2
# EVENT STUDY — VOLUMES AUTOUR DE L’INSCRIPTION
# ==============================

fig2_data <- ewl_event %>%
  count(event_time, name = "n")

g2 <- ggplot(fig2_data, aes(x = event_time, y = n)) +
  geom_line(linewidth = 1.6, colour = "#C44E52", lineend = "round") +
  geom_point(size = 2.7, colour = "#C44E52") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "black") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  scale_x_continuous(
    breaks = seq(event_window_min, event_window_max, by = 1)
  ) +
  labs(
    title = "Publication activity shifts around the year journals are listed",
    subtitle = "Event-time profile of NSFC-funded publications in journals that entered an Early Warning List",
    x = "Years relative to listing",
    y = "Number of publications",
    caption = "Event time 0 denotes the year of first appearance in an Early Warning List."
  ) +
  theme_pub()

# ==============================
# 8) FIGURE 3
# AVANT / APRÈS — PART PAR GROUPE ÉDITEUR
# ==============================

fig3_data <- ewl_pubs %>%
  filter(!is.na(before_after), publisher_group %in% publisher_levels_keep) %>%
  count(before_after, publisher_group) %>%
  group_by(before_after) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

g3 <- ggplot(
  fig3_data,
  aes(x = before_after, y = share, fill = publisher_group)
) +
  geom_col(position = "fill", width = 0.68) +
  scale_fill_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "The publisher profile of listed journals changes before and after listing",
    subtitle = "Distribution of publisher groups among NSFC publications in listed journals",
    x = NULL,
    y = "Share within period"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_pub()

# ==============================
# 9) FIGURE 4
# EVENT STUDY PAR GROUPE ÉDITEUR
# ==============================

fig4_data <- ewl_event %>%
  filter(publisher_group %in% publisher_levels_keep) %>%
  count(event_time, publisher_group, name = "n")

g4 <- ggplot(
  fig4_data,
  aes(x = event_time, y = n, colour = publisher_group)
) +
  geom_line(linewidth = 1.25, lineend = "round") +
  geom_point(size = 2.0) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "black") +
  scale_color_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  scale_x_continuous(breaks = seq(event_window_min, event_window_max, by = 1)) +
  labs(
    title = "The before/after pattern differs across publisher groups",
    subtitle = "Event-time publication dynamics by publisher group",
    x = "Years relative to listing",
    y = "Number of publications"
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_pub()

# ==============================
# 10) FIGURE 5
# OA AUTOUR DE L’ÉVÉNEMENT PAR GROUPE ÉDITEUR
# ==============================

fig5_data <- ewl_event %>%
  filter(publisher_group %in% publisher_levels_keep) %>%
  group_by(event_time, publisher_group) %>%
  summarise(
    oa_rate = mean(is_oa, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

g5 <- ggplot(
  fig5_data,
  aes(x = event_time, y = oa_rate, colour = publisher_group)
) +
  geom_line(linewidth = 1.25, lineend = "round") +
  geom_point(size = 2.0) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "black") +
  scale_color_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(event_window_min, event_window_max, by = 1)) +
  labs(
    title = "The Open Access profile of listed journals also changes around listing events",
    subtitle = "Open Access share by publisher group in journals that entered an Early Warning List",
    x = "Years relative to listing",
    y = "Open Access share"
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_pub()

# ==============================
# 11) FIGURE 6
# COMPOSITION TEMPORELLE DES REVUES EWL PAR GROUPE ÉDITEUR
# ==============================

fig6_data <- ewl_pubs %>%
  filter(publisher_group %in% publisher_levels_keep) %>%
  count(publication_year, publisher_group) %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

g6 <- ggplot(
  fig6_data,
  aes(x = publication_year, y = share, fill = publisher_group)
) +
  geom_area(alpha = 0.98, colour = "white", linewidth = 0.28) +
  scale_fill_manual(values = pal_publishers, labels = publisher_labels_short, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = pretty(fig6_data$publication_year, n = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "The composition of listed-journal publications is restructured over time",
    subtitle = "Annual publisher-group composition of NSFC publications in journals that entered an Early Warning List",
    x = NULL,
    y = "Share within listed-journal publications"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_pub()

# ==============================
# 12) FIGURE 7
# DOMAINES × AVANT / APRÈS
# ==============================

fig7_data <- ewl_pubs %>%
  filter(!is.na(before_after)) %>%
  mutate(
    main_domain = if_else(is.na(main_domain), "Unknown", main_domain)
  ) %>%
  count(main_domain, before_after) %>%
  group_by(main_domain) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

domain_order <- fig7_data %>%
  group_by(main_domain) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  arrange(total_n) %>%
  pull(main_domain)

fig7_data <- fig7_data %>%
  mutate(main_domain = factor(main_domain, levels = domain_order))

g7 <- ggplot(
  fig7_data,
  aes(x = main_domain, y = share, fill = before_after)
) +
  geom_col(position = "fill", width = 0.72) +
  coord_flip() +
  scale_fill_manual(values = pal_before_after, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "The before/after balance also differs across scientific domains",
    subtitle = "Distribution of NSFC publications in listed journals before and after listing, by broad domain",
    x = NULL,
    y = "Share within domain"
  ) +
  theme_pub()

# ==============================
# 13) FIGURE 8
# COLLABORATION INTERNATIONALE AVANT / APRÈS
# ==============================

fig8_data <- ewl_pubs %>%
  filter(!is.na(before_after)) %>%
  mutate(
    collab_type = case_when(
      n_countries == 1 ~ "Domestic",
      n_countries == 2 ~ "Bilateral",
      n_countries >= 3 ~ "Multinational",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(collab_type)) %>%
  count(before_after, collab_type) %>%
  group_by(before_after) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    collab_type = factor(collab_type, levels = c("Domestic", "Bilateral", "Multinational"))
  )

g8 <- ggplot(
  fig8_data,
  aes(x = before_after, y = share, fill = collab_type)
) +
  geom_col(position = "fill", width = 0.68) +
  scale_fill_manual(
    values = c(
      "Domestic" = "#7A7A7A",
      "Bilateral" = "#4C78A8",
      "Multinational" = "#4C9F70"
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "The collaboration profile of publications in listed journals shifts after listing",
    subtitle = "Distribution of collaboration profiles before and after listing",
    x = NULL,
    y = "Share within period"
  ) +
  theme_pub()

# ==============================
# 14) TABLEAUX DE CONTRÔLE
# ==============================

tab_before_after_publishers <- ewl_pubs %>%
  filter(!is.na(before_after), publisher_group %in% publisher_levels_keep) %>%
  count(before_after, publisher_group) %>%
  group_by(before_after) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1)) %>%
  ungroup()

tab_event <- ewl_event %>%
  count(event_time)

tab_domain_before_after <- ewl_pubs %>%
  filter(!is.na(before_after)) %>%
  count(main_domain, before_after) %>%
  group_by(main_domain) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1)) %>%
  ungroup()

tab_collab_before_after <- ewl_pubs %>%
  filter(!is.na(before_after)) %>%
  mutate(
    collab_type = case_when(
      n_countries == 1 ~ "Domestic",
      n_countries == 2 ~ "Bilateral",
      n_countries >= 3 ~ "Multinational",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(collab_type)) %>%
  count(before_after, collab_type) %>%
  group_by(before_after) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1)) %>%
  ungroup()

print(tab_before_after_publishers)
print(tab_event)
print(tab_domain_before_after)
print(tab_collab_before_after)

# ==============================
# 15) AFFICHAGE
# ==============================

print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
print(g6)
print(g7)
print(g8)

# ==============================
# 16) PANNEAUX
# ==============================

panel_main_1 <- (g1 | g2) / (g3 | g4)
panel_main_2 <- (g5 | g6) / (g7 | g8)

print(panel_main_1)
print(panel_main_2)

# ==============================
# 17) SAUVEGARDE
# ==============================

saveRDS(nsfc_ewl_analysis2, "data_nsfc/nsfc_early_warning_full_analysis.rds", compress = "xz")

# ==============================
# 18) EXPORT FIGURES
# ==============================

dir.create("figures_nsfc_early_warning", showWarnings = FALSE)

ggsave("figures_nsfc_early_warning/FigureEW1_share_ewl_in_nsfc.png",
       g1, width = 11, height = 6.4, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW2_event_study_volume.png",
       g2, width = 11, height = 6.4, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW3_before_after_publishers.png",
       g3, width = 10.5, height = 6.4, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW4_event_publishers.png",
       g4, width = 11, height = 6.6, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW5_oa_event_publishers.png",
       g5, width = 11, height = 6.6, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW6_time_composition_publishers.png",
       g6, width = 11, height = 6.6, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW7_domain_before_after.png",
       g7, width = 11, height = 7.0, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW8_collab_before_after.png",
       g8, width = 10.5, height = 6.2, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW_panel_1.png",
       panel_main_1, width = 14, height = 10.5, dpi = 350, bg = "white")

ggsave("figures_nsfc_early_warning/FigureEW_panel_2.png",
       panel_main_2, width = 14, height = 10.5, dpi = 350, bg = "white")



## =======================

## Analyse du pool des revues flaguées

## =======================

# =========================================================
# NSFC × EARLY WARNING JOURNALS
# ANALYSE PAR COHORTE DE LISTE
# =========================================================
# Objectif :
# 1) pour chaque cohorte de liste (2020, 2021, 2023, 2024, 2025),
#    tracer l'évolution annuelle du nombre moyen de publications NSFC
#    par revue listée ;
# 2) distinguer grey publishers vs autres ;
# 3) proposer un ridge plot propre comme complément visuel.
# =========================================================

# ==============================
# 0) PACKAGES
# ==============================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(data.table)
library(patchwork)
library(ggridges)
library(viridis)

# ==============================
# 1) CHARGEMENT
# ==============================

# nsfc <- all_nsfc_augmented
# nsfc <- readRDS("data_nsfc/all_nsfc_augmented.rds")
# early_warning_master <- readRDS("data_nsfc/early_warning_master_matched.rds")


ewl_master <- early_warning_master %>%
  filter(needs_manual_check == FALSE)
# 
nsfc <- as_tibble(nsfc)
ewl_master <- as_tibble(ewl_master)

# ==============================
# 2) HELPERS
# ==============================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_issn <- function(x) {
  x <- as.character(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^0-9X]", "")
  ifelse(nchar(x) == 8, x, NA_character_)
}

mode_character <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

theme_pub <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0, colour = "#1A1A1A"),
      plot.subtitle = element_text(size = base_size, hjust = 0, colour = "#4D4D4D"),
      plot.caption = element_text(size = base_size - 2, colour = "#666666"),
      axis.title = element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = element_text(size = base_size - 1, colour = "#262626"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFEFEF", linewidth = 0.30),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.width = unit(1.2, "cm"),
      strip.text = element_text(face = "bold", colour = "#1A1A1A"),
      plot.margin = margin(12, 14, 12, 12)
    )
}

# ==============================
# 3) PRÉPARATION DES DONNÉES
# ==============================

# sécuriser ISSN
nsfc <- nsfc %>%
  mutate(
    issn_l_norm = normalize_issn(issn_l)
  )


# On associe chaque publication NSFC à une éventuelle cohorte EWL
nsfc_ewl <- nsfc %>%
  left_join(
    ewl_match %>%
      select(
        issn_norm,
        source_id,
        ewl_year,
        field,
        journal_title,
        matched_display_name,
        matched_host_organization,
        matched_country_code
      ),
    by = c("source_id")
  ) %>%
  mutate(
    in_ewl = !is.na(ewl_year)
  )

# ==============================
# 4) TABLE DES REVUES EWL
# ==============================

# Pour chaque revue listée, on récupère :
# - sa cohorte (ewl_year)
# - son groupe éditeur principal observé dans NSFC
# - un flag grey vs other

ewl_journals <- nsfc_ewl %>% # 11 revues sans publication (bien matchées avec la table des revues OpenAlex, mais aucun record nsfc donc on a 112 au lieu des 123)
  filter(in_ewl) %>%
  group_by(source_id, ewl_year) %>%
  summarise(
    journal_name = mode_character(coalesce(matched_display_name, source_display_name, journal_title)),
    publisher_group_main = mode_character(as.character(publisher_group)),
    .groups = "drop"
  ) %>%
  mutate(
    grey_flag = case_when(
      publisher_group_main == "Grey publishers" ~ "Grey publishers",
      TRUE ~ "Other publishers"
    ),
    grey_flag = factor(grey_flag, levels = c("Grey publishers", "Other publishers"))
  )

# ==============================
# 5) PANEL REVUE-ANNÉE
# ==============================

start_year <- 2010
end_year <- min(max(nsfc$publication_year, na.rm = TRUE), 2025)

# Comptes observés par revue-année
journal_year_counts <- nsfc_ewl %>%
  filter(in_ewl, !is.na(source_id)) %>%
  count(source_id, ewl_year, publication_year, name = "n_pub")

# Compléter les années manquantes avec 0 pour chaque revue listée
journal_year_panel <- ewl_journals %>%
  select(source_id, ewl_year, journal_name, publisher_group_main, grey_flag) %>%
  tidyr::crossing(publication_year = start_year:end_year) %>%
  left_join(
    journal_year_counts,
    by = c("source_id", "ewl_year", "publication_year")
  ) %>%
  mutate(
    n_pub = coalesce(n_pub, 0L),
    release_year = ewl_year,
    listed_yet = publication_year >= release_year
  )

# ==============================
# 6) AGRÉGATS POUR FIGURE 1
# Nombre moyen de publications par revue, par cohorte et par année
# ==============================
# 
# fig1_data <- journal_year_panel %>%
#   group_by(ewl_year, publication_year) %>%
#   summarise(
#     n_journals = n_distinct(issn_l_norm),
#     mean_pub = mean(n_pub, na.rm = TRUE),
#     median_pub = median(n_pub, na.rm = TRUE),
#     q25 = quantile(n_pub, 0.25, na.rm = TRUE),
#     q75 = quantile(n_pub, 0.75, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     cohort = factor(ewl_year, levels = sort(unique(ewl_year)))
#   )
# 
# fig1_data <- fig1_data %>%
#   filter(ewl_year<2025)
# # ==============================
# # 7) FIGURE 1
# # FACETS PAR COHORTE : moyenne par revue dans le temps
# # ==============================
# 
# g1 <- ggplot(fig1_data, aes(x = publication_year, y = mean_pub)) +
#   geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#D9D9D9", alpha = 0.55) +
#   geom_line(linewidth = 1.35, colour = "#1F4E79", lineend = "round") +
#   geom_point(size = 2.2, colour = "#1F4E79") +
#   geom_vline(aes(xintercept = ewl_year), linetype = "dashed", linewidth = 0.7, colour = "#C44E52") +
#   facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
#   scale_x_continuous(
#     breaks = seq(start_year, end_year, by = 2),
#     expand = expansion(mult = c(0.01, 0.01))
#   ) +
#   scale_y_continuous(labels = label_number(accuracy = 0.1)) +
#   labs(
#     title = "NSFC publication intensity in listed journals can be tracked cohort by cohort",
#     subtitle = "Average number of NSFC-funded publications per listed journal by calendar year; shaded area = interquartile range",
#     x = NULL,
#     y = "Mean publications per journal",
#     caption = "The dashed vertical line marks the release year of each Early Warning List cohort."
#   ) +
#   theme_pub()
# 
# # ==============================
# # 8) AGRÉGATS POUR FIGURE 2
# # Même logique, mais grey vs other
# # ==============================
# 
# fig2_data <- journal_year_panel %>%
#   mutate(
#     grey_flag = factor(grey_flag, levels = c("Grey publishers", "Other publishers"))
#   ) %>%
#   group_by(ewl_year, publication_year, grey_flag) %>%
#   summarise(
#     n_journals = n_distinct(issn_l_norm),
#     mean_pub = mean(n_pub, na.rm = TRUE),
#     median_pub = median(n_pub, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     cohort = factor(ewl_year, levels = sort(unique(ewl_year)))
#   )
# 
# fig2_data <- fig2_data %>%
#   filter(ewl_year < 2025)
# 
# pal_grey <- c(
#   "Grey publishers" = "#C44E52",
#   "Other publishers" = "#7A7A7A"
# )
# 
# g2 <- ggplot(fig2_data, aes(x = publication_year, y = mean_pub, colour = grey_flag)) +
#   geom_line(linewidth = 1.3, lineend = "round") +
#   geom_point(size = 2.0) +
#   geom_vline(aes(xintercept = ewl_year), linetype = "dashed", linewidth = 0.7, colour = "black") +
#   facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
#   scale_colour_manual(values = pal_grey, drop = FALSE) +
#   scale_x_continuous(
#     breaks = seq(start_year, end_year, by = 2),
#     expand = expansion(mult = c(0.01, 0.01))
#   ) +
#   scale_y_continuous(labels = label_number(accuracy = 0.1)) +
#   labs(
#     title = "The trajectory differs between grey-publisher journals and other listed journals",
#     subtitle = "Average number of NSFC-funded publications per listed journal, by calendar year and publisher type",
#     x = NULL,
#     y = "Mean publications per journal"
#   ) +
#   theme_pub()
# 
# # ==============================
# # 9) FIGURE 3
# # RIDGE PLOT : distribution des comptes par revue selon les années
# # facetté par cohorte
# # ==============================
# 
# # Pour éviter des facets trop chargés, on garde la fenêtre 2010-2025 déjà définie
# fig3_data <- journal_year_panel %>%
#   mutate(
#     publication_year_f = factor(publication_year, levels = rev(start_year:end_year)),
#     cohort = factor(ewl_year, levels = sort(unique(ewl_year)))
#   )
# 
# fig3_data <- fig3_data %>%
#   filter(ewl_year < 2025)
# 
# g3 <- ggplot(
#   fig3_data,
#   aes(x = n_pub, y = publication_year_f, fill = after_stat(x))
# ) +
#   geom_density_ridges_gradient(
#     scale = 2.3,
#     rel_min_height = 0.01,
#     size = 0.35,
#     colour = "black"
#   ) +
#   facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
#   scale_fill_viridis(option = "C", direction = 1) +
#   labs(
#     title = "The distribution of publication counts per listed journal changes over calendar time",
#     subtitle = "Ridge plot of journal-level NSFC publication counts, faceted by Early Warning List cohort",
#     x = "Publications per journal",
#     y = "Calendar year"
#   ) +
#   theme_pub() +
#   theme(
#     legend.position = "none"
#   )
# 
# # ==============================
# # 10) FIGURE 4
# # VERSION PLUS SOBRE : médiane par revue
# # utile si la moyenne est trop sensible aux extrêmes
# # ==============================
# 
# fig4_data <- journal_year_panel %>%
#   group_by(ewl_year, publication_year) %>%
#   summarise(
#     median_pub = median(n_pub, na.rm = TRUE),
#     mean_pub = mean(n_pub, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     cohort = factor(ewl_year, levels = sort(unique(ewl_year)))
#   )
# 
# fig4_data <- fig4_data %>%
#   filter(ewl_year < 2025)
# 
# g4 <- ggplot(fig4_data, aes(x = publication_year, y = median_pub)) +
#   geom_line(linewidth = 1.35, colour = "#4C9F70", lineend = "round") +
#   geom_point(size = 2.1, colour = "#4C9F70") +
#   geom_vline(aes(xintercept = ewl_year), linetype = "dashed", linewidth = 0.7, colour = "#C44E52") +
#   facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
#   scale_x_continuous(
#     breaks = seq(start_year, end_year, by = 2),
#     expand = expansion(mult = c(0.01, 0.01))
#   ) +
#   scale_y_continuous(labels = label_number(accuracy = 0.1)) +
#   labs(
#     title = "Median publication intensity also provides a robust view of the before/after profile",
#     subtitle = "Median number of NSFC-funded publications per listed journal by calendar year",
#     x = NULL,
#     y = "Median publications per journal"
#   ) +
#   theme_pub()
# 
# # ==============================
# # 11) TABLEAUX DE CONTRÔLE
# # ==============================
# 
# tab_cohort_size <- ewl_journals %>%
#   count(ewl_year, grey_flag, name = "n_journals") %>%
#   arrange(ewl_year, grey_flag)
# 
# tab_yearly_mean <- fig1_data %>%
#   mutate(
#     mean_pub = round(mean_pub, 2),
#     median_pub = round(median_pub, 2),
#     q25 = round(q25, 2),
#     q75 = round(q75, 2)
#   )
# 
# print(tab_cohort_size)
# print(tab_yearly_mean, n = 200)
# 
# # ==============================
# # 12) AFFICHAGE
# # ==============================
# 
# print(g1)
# print(g2)
# print(g3)
# print(g4)
# 
# # ==============================
# # 13) PANNEAUX
# # ==============================
# 
# panel_main <- g1 / g2
# panel_dist <- g3 / g4
# 
# print(panel_main)
# print(panel_dist)
# 
# # ==============================
# # 14) SAUVEGARDE
# # ==============================
# 
# saveRDS(journal_year_panel, "data_nsfc/nsfc_ewl_journal_year_panel.rds", compress = "xz")

# # ==============================
# # 15) EXPORT
# # ==============================
# 
# dir.create("figures_nsfc_ewl_cohorts", showWarnings = FALSE)
# 
# ggsave(
#   "figures_nsfc_ewl_cohorts/FigureEWL_cohort_mean_publications.png",
#   g1, width = 13, height = 9, dpi = 350, bg = "white"
# )
# 
# ggsave(
#   "figures_nsfc_ewl_cohorts/FigureEWL_cohort_grey_vs_other.png",
#   g2, width = 13, height = 9, dpi = 350, bg = "white"
# )
# 
# ggsave(
#   "figures_nsfc_ewl_cohorts/FigureEWL_cohort_ridge_distribution.png",
#   g3, width = 13, height = 10, dpi = 350, bg = "white"
# )
# 
# ggsave(
#   "figures_nsfc_ewl_cohorts/FigureEWL_cohort_median_publications.png",
#   g4, width = 13, height = 9, dpi = 350, bg = "white"
# )
# 
# ggsave(
#   "figures_nsfc_ewl_cohorts/FigureEWL_panel_main.png",
#   panel_main, width = 13, height = 14, dpi = 350, bg = "white"
# )
# 
# ggsave(
#   "figures_nsfc_ewl_cohorts/FigureEWL_panel_distribution.png",
#   panel_dist, width = 13, height = 15, dpi = 350, bg = "white"
# )

# =========================================================
# NSFC × EARLY WARNING JOURNALS
# COHORT ANALYSIS — REFINED VISUAL VERSION
# =========================================================

# =========================================================
# NSFC × EARLY WARNING JOURNALS
# COHORT ANALYSIS — REFINED VERSION
# Adaptée à l'objet journal_year_panel existant
# =========================================================

library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(patchwork)

# ==============================
# 1) DONNÉES
# ==============================

# suppose que journal_year_panel existe déjà
# glimpse(journal_year_panel)
# issn_l_norm, ewl_year, journal_name, publisher_group_main, grey_flag,
# publication_year, n_pub, release_year, listed_yet

# journal_year_panel <- readRDS("data_nsfc/nsfc_ewl_journal_year_panel")

journal_year_panel <- as_tibble(journal_year_panel)

cohorts_keep <- c(2020, 2021, 2023, 2024, 2025)

start_year <- max(2010, min(journal_year_panel$publication_year, na.rm = TRUE))
end_year <- max(journal_year_panel$publication_year, na.rm = TRUE)

# ==============================
# 2) HELPERS
# ==============================

theme_pub_refined <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 4, hjust = 0, colour = "#111111"),
      plot.subtitle = element_text(size = base_size + 0.3, hjust = 0, colour = "#4D4D4D"),
      plot.caption = element_text(size = base_size - 2, colour = "#666666"),
      axis.title = element_text(size = base_size + 0.2, colour = "#1A1A1A"),
      axis.text = element_text(size = base_size - 0.5, colour = "#262626"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E5E5E5", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFEFEF", linewidth = 0.30),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(face = "bold", size = base_size, colour = "#1A1A1A"),
      plot.margin = margin(12, 28, 12, 12)
    )
}

make_endpoint_labels <- function(df, group_var, y_var, x_var = "publication_year") {
  df %>%
    group_by(across(all_of(c("cohort", group_var)))) %>%
    filter(.data[[x_var]] == max(.data[[x_var]], na.rm = TRUE)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      label_x = .data[[x_var]] + 0.35,
      label_y = .data[[y_var]]
    )
}

pal_compare <- c(
  "Before release" = "#7A7A7A",
  "After release" = "#1F4E79",
  "Grey publishers" = "#C44E52",
  "Other publishers" = "#7A7A7A"
)

# ==============================
# 3) PRÉPARATION
# ==============================

jyp <- journal_year_panel %>%
  filter(ewl_year %in% cohorts_keep) %>%
  mutate(
    cohort = factor(ewl_year, levels = cohorts_keep),
    period_rel = case_when(
      publication_year < release_year ~ "Before release",
      publication_year >= release_year ~ "After release",
      TRUE ~ NA_character_
    ),
    period_rel = factor(period_rel, levels = c("Before release", "After release")),
    grey_flag = factor(grey_flag, levels = c("Grey publishers", "Other publishers"))
  )

# ==============================
# 4) FIGURE 1
# COHORTES LISTÉES — moyenne + médiane + IQR
# ==============================

fig1_data <- jyp %>%
  filter(ewl_year < 2025) %>%
  group_by(ewl_year, cohort, publication_year, release_year) %>%
  summarise(
    mean_pub = mean(n_pub, na.rm = TRUE),
    median_pub = median(n_pub, na.rm = TRUE),
    q25 = quantile(n_pub, 0.25, na.rm = TRUE),
    q75 = quantile(n_pub, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

fig1_labels <- fig1_data %>%
  group_by(cohort) %>%
  filter(publication_year == max(publication_year, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    cohort,
    label_x = publication_year + 0.35,
    label_y_mean = mean_pub,
    label_y_median = median_pub
  )

g1_refined <- ggplot(fig1_data, aes(x = publication_year)) +
  geom_ribbon(
    aes(ymin = q25, ymax = q75),
    fill = "#D9D9D9", alpha = 0.50
  ) +
  geom_line(
    aes(y = mean_pub),
    linewidth = 1.35, colour = "#1F4E79", lineend = "round"
  ) +
  geom_point(
    aes(y = mean_pub),
    size = 2.0, colour = "#1F4E79"
  ) +
  geom_line(
    aes(y = median_pub),
    linewidth = 1.0, colour = "#4C9F70", linetype = "22"
  ) +
  geom_vline(
    aes(xintercept = release_year),
    linetype = "dashed", linewidth = 0.7, colour = "#C44E52"
  ) +
  geom_text(
    data = fig1_labels,
    aes(x = label_x, y = label_y_mean, label = "Mean"),
    inherit.aes = FALSE,
    hjust = 0, size = 3.4, colour = "#1F4E79", fontface = "bold"
  ) +
  geom_text(
    data = fig1_labels,
    aes(x = label_x, y = label_y_median, label = "Median"),
    inherit.aes = FALSE,
    hjust = 0, size = 3.2, colour = "#4C9F70"
  ) +
  facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
  scale_x_continuous(
    breaks = seq(start_year, end_year, by = 2),
    expand = expansion(mult = c(0.01, 0.10))
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Listed-journal cohorts rise strongly before release and often soften afterwards",
    subtitle = "NSFC-funded publications per journal by calendar year. Solid line = mean; dashed green line = median; shaded area = interquartile range.",
    x = NULL,
    y = "Publications per journal",
    caption = "The dashed vertical line marks the release year of each Early Warning List cohort."
  ) +
  theme_pub_refined()

# ==============================
# 5) FIGURE 2
# AU SEIN DE CHAQUE COHORTE :
# avant release vs après release
# ==============================

fig2_data <- jyp %>%
  filter(ewl_year < 2025) %>%
  group_by(cohort, publication_year, release_year, period_rel) %>%
  summarise(
    mean_pub = mean(n_pub, na.rm = TRUE),
    median_pub = median(n_pub, na.rm = TRUE),
    .groups = "drop"
  )

# fig2_data <- fig2_data %>%
#   filter(release_year < 2025)

fig2_labels <- make_endpoint_labels(fig2_data, group_var = "period_rel", y_var = "mean_pub")

# g2_refined <- ggplot(
#   fig2_data,
#   aes(x = publication_year, y = mean_pub, colour = period_rel)
# ) +
#   geom_line(linewidth = 1.3, lineend = "round") +
#   geom_point(size = 1.9) +
#   geom_vline(
#     aes(xintercept = release_year),
#     linetype = "dashed", linewidth = 0.7, colour = "black"
#   ) +
#   geom_text(
#     data = fig2_labels,
#     aes(x = label_x, y = label_y, label = period_rel, colour = period_rel),
#     inherit.aes = FALSE,
#     hjust = 0, size = 3.4, fontface = "bold", show.legend = FALSE
#   ) +
#   facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
#   scale_colour_manual(
#     values = pal_compare[c("Before release", "After release")],
#     drop = FALSE
#   ) +
#   scale_x_continuous(
#     breaks = seq(start_year, end_year, by = 2),
#     expand = expansion(mult = c(0.01, 0.12))
#   ) +
#   scale_y_continuous(labels = label_number(accuracy = 0.1)) +
#   labs(
#     title = "Within each cohort, publication intensity is visibly different before and after release",
#     subtitle = "The same journals are followed over calendar time; direct labels indicate the two periods relative to the release year.",
#     x = NULL,
#     y = "Mean publications per journal"
#   ) +
#   theme_pub_refined() +
#   theme(legend.position = "none")

fig2_labels_refined <- fig2_labels %>%
  mutate(
    x_lab = case_when(
      period_rel == "Before release" ~ label_x - 0.50,
      period_rel == "After release"  ~ label_x - 0.50
    ),
    y_lab = case_when(
      period_rel == "Before release" ~ label_y,
      period_rel == "After release"  ~ label_y - 0.15 * label_y
    ),
    hjust_lab = case_when(
      period_rel == "Before release" ~ 1,
      period_rel == "After release"  ~ 0
    ),
    vjust_lab = case_when(
      period_rel == "Before release" ~ 0.5,
      period_rel == "After release"  ~ 1
    )
  )

g2_refined <- ggplot(
  fig2_data,
  aes(x = publication_year, y = mean_pub, colour = period_rel)
) +
  geom_line(linewidth = 1.3, lineend = "round") +
  geom_point(size = 1.9) +
  geom_vline(
    aes(xintercept = release_year),
    linetype = "dashed",
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_text(
    data = fig2_labels_refined,
    aes(
      x = x_lab,
      y = y_lab,
      label = period_rel,
      colour = period_rel,
      hjust = hjust_lab,
      vjust = vjust_lab
    ),
    inherit.aes = FALSE,
    size = 3.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = pal_compare[c("Before release", "After release")],
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(start_year, end_year, by = 2),
    expand = expansion(mult = c(0.01, 0.12))
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Within each cohort, publication intensity is visibly different before and after release",
    subtitle = "The same journals are followed over calendar time; direct labels indicate the two periods relative to the release year.",
    x = NULL,
    y = "Mean publications per journal"
  ) +
  theme_pub_refined() +
  theme(legend.position = "none")

g2_refined
# ==============================
# 6) FIGURE 3
# GREY VS OTHER AU SEIN DES COHORTES
# moyenne + médiane discrète
# ==============================

fig3_data <- jyp %>%
  filter(ewl_year < 2025) %>%
  group_by(cohort, publication_year, release_year, grey_flag) %>%
  summarise(
    mean_pub = mean(n_pub, na.rm = TRUE),
    median_pub = median(n_pub, na.rm = TRUE),
    .groups = "drop"
  )

fig3_labels <- make_endpoint_labels(fig3_data, group_var = "grey_flag", y_var = "mean_pub")

g3_refined <- ggplot(
  fig3_data,
  aes(x = publication_year, y = mean_pub, colour = grey_flag)
) +
  geom_line(linewidth = 1.3, lineend = "round") +
  geom_point(size = 1.9) +
  geom_line(
    aes(y = median_pub, colour = grey_flag),
    linewidth = 0.9, linetype = "22", alpha = 0.8
  ) +
  geom_vline(
    aes(xintercept = release_year),
    linetype = "dashed", linewidth = 0.7, colour = "black"
  ) +
  geom_text(
    data = fig3_labels,
    aes(x = label_x, y = label_y, label = grey_flag, colour = grey_flag),
    inherit.aes = FALSE,
    hjust = 0, size = 3.4, fontface = "bold", show.legend = FALSE
  ) +
  facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = pal_compare[c("Grey publishers", "Other publishers")],
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(start_year, end_year, by = 2),
    expand = expansion(mult = c(0.01, 0.12))
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Grey-publisher journals and other listed journals do not follow the same temporal path",
    subtitle = "Solid line = mean; dashed line = median. Direct labels replace the legend for faster reading.",
    x = NULL,
    y = "Publications per journal"
  ) +
  theme_pub_refined() +
  theme(legend.position = "none")

# ==============================
# 7) FIGURE 4
# VERSION NORMALISÉE (index = 100 à l’année de release)
# pour comparer les formes malgré des niveaux différents
# ==============================

fig4_data <- jyp %>%
  filter(ewl_year < 2025) %>%
  group_by(cohort, release_year, publication_year) %>%
  summarise(
    mean_pub = mean(n_pub, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cohort) %>%
  mutate(
    ref_value = mean_pub[publication_year == unique(release_year)][1],
    index_release_100 = ifelse(!is.na(ref_value) & ref_value > 0, 100 * mean_pub / ref_value, NA_real_)
  ) %>%
  ungroup()

g4_refined <- ggplot(
  fig4_data,
  aes(x = publication_year, y = index_release_100)
) +
  geom_line(linewidth = 1.35, colour = "#B07AA1", lineend = "round") +
  geom_point(size = 2.0, colour = "#B07AA1") +
  geom_hline(yintercept = 100, linetype = "22", linewidth = 0.6, colour = "#7A7A7A") +
  geom_vline(
    aes(xintercept = release_year),
    linetype = "dashed", linewidth = 0.7, colour = "#C44E52"
  ) +
  facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
  scale_x_continuous(
    breaks = seq(start_year, end_year, by = 2),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "Normalizing at the release year makes the shape of each cohort easier to compare",
    subtitle = "Index of mean publications per journal, with the release year set to 100 in each cohort.",
    x = NULL,
    y = "Index (release year = 100)"
  ) +
  theme_pub_refined()

# ==============================
# 8) AFFICHAGE
# ==============================

print(g1_refined)
print(g2_refined)
print(g3_refined)
print(g4_refined)

# ==============================
# 9) PANNEAUX
# ==============================

panel_refined_main <- g1_refined / g2_refined
panel_refined_secondary <- g3_refined / g4_refined

print(panel_refined_main)
print(panel_refined_secondary)

# ==============================
# 10) EXPORT
# ==============================

dir.create("figures_nsfc_ewl_refined", showWarnings = FALSE)

ggsave(
  "figures_nsfc_ewl_refined/Figure1_refined_cohort_mean_median.png",
  g1_refined, width = 13.2, height = 9.2, dpi = 350, bg = "white"
)

ggsave(
  "figures_nsfc_ewl_refined/Figure2_refined_before_after.png",
  g2_refined, width = 13.2, height = 9.2, dpi = 350, bg = "white"
)

ggsave(
  "figures_nsfc_ewl_refined/Figure3_refined_grey_vs_other.png",
  g3_refined, width = 13.2, height = 9.2, dpi = 350, bg = "white"
)

ggsave(
  "figures_nsfc_ewl_refined/Figure4_refined_index_release100.png",
  g4_refined, width = 13.2, height = 9.2, dpi = 350, bg = "white"
)

ggsave(
  "figures_nsfc_ewl_refined/Panel_refined_main.png",
  panel_refined_main, width = 13.5, height = 14.5, dpi = 350, bg = "white"
)

ggsave(
  "figures_nsfc_ewl_refined/Panel_refined_secondary.png",
  panel_refined_secondary, width = 13.5, height = 14.5, dpi = 350, bg = "white"
)

# ==============================
# FIGURE 5
# COHORTE vs reste du corpus NSFC
# avec moyenne + médiane
# ==============================

# 1) Construire un panel revue-année pour tout le corpus NSFC
# à partir de nsfc (pas seulement des revues listées)

# Si nécessaire, recharge nsfc enrichi
# nsfc <- readRDS("data_nsfc/nsfc_augmented_publishers.rds")

# nsfc_all_journals <- nsfc %>%
#   filter(!is.na(source_id))
# 
# # méta revue
# all_journal_meta <- nsfc_all_journals %>%
#   group_by(source_id) %>%
#   summarise(
#     journal_name = mode_character(coalesce(source_display_name, title)),
#     .groups = "drop"
#   )
# 
# # comptes revue-année sur tout le corpus NSFC
# all_journal_year_counts <- nsfc_all_journals %>%
#   count(source_id, publication_year, name = "n_pub")
# 
# # panel complet
# all_journal_year_panel <- all_journal_meta %>%
#   tidyr::crossing(publication_year = start_year:end_year) %>%
#   left_join(
#     all_journal_year_counts,
#     by = c("source_id")
#   ) %>%
#   mutate(
#     n_pub = coalesce(n_pub, 0L)
#   )
# 
# # 2) Reprendre les revues de cohorte
# cohort_journals <- jyp %>%
#   distinct(issn_l_norm, cohort, release_year)
# 
# # 3) Benchmark = tout le corpus NSFC hors revues de la cohorte
# benchmark_data <- lapply(cohorts_keep, function(cy) {
#   
#   cohort_ids <- cohort_journals %>%
#     filter(as.numeric(as.character(cohort)) == cy) %>%
#     pull(issn_l_norm) %>%
#     unique()
#   
#   all_journal_year_panel %>%
#     filter(!source_id %in% cohort_ids) %>%
#     group_by(publication_year) %>%
#     summarise(
#       benchmark_mean = mean(n_pub, na.rm = TRUE),
#       benchmark_median = median(n_pub, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       cohort = factor(cy, levels = cohorts_keep),
#       release_year = cy
#     )
# }) %>%
#   bind_rows()
# 
# # 4) Série de la cohorte
# cohort_series <- jyp %>%
#   group_by(cohort, publication_year, release_year) %>%
#   summarise(
#     mean_pub = mean(n_pub, na.rm = TRUE),
#     median_pub = median(n_pub, na.rm = TRUE),
#     q25 = quantile(n_pub, 0.25, na.rm = TRUE),
#     q75 = quantile(n_pub, 0.75, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # labels directs pour la cohorte
# cohort_labels <- cohort_series %>%
#   group_by(cohort) %>%
#   filter(publication_year == max(publication_year, na.rm = TRUE)) %>%
#   slice_tail(n = 1) %>%
#   ungroup() %>%
#   transmute(
#     cohort,
#     label_x = publication_year + 0.35,
#     label_y_mean = mean_pub,
#     label_y_median = median_pub
#   )
# 
# # labels directs pour benchmark
# benchmark_labels <- benchmark_data %>%
#   group_by(cohort) %>%
#   filter(publication_year == max(publication_year, na.rm = TRUE)) %>%
#   slice_tail(n = 1) %>%
#   ungroup() %>%
#   transmute(
#     cohort,
#     label_x = publication_year + 0.35,
#     label_y_mean = benchmark_mean,
#     label_y_median = benchmark_median
#   )
# 
# # 5) Graphe
# g5_refined <- ggplot() +
#   # bande de dispersion cohorte
#   geom_ribbon(
#     data = cohort_series,
#     aes(x = publication_year, ymin = q25, ymax = q75),
#     fill = "#D9D9D9", alpha = 0.45
#   ) +
#   
#   # benchmark mean / median
#   geom_line(
#     data = benchmark_data,
#     aes(x = publication_year, y = benchmark_mean),
#     linewidth = 1.15, colour = "#4D4D4D", lineend = "round"
#   ) +
#   geom_line(
#     data = benchmark_data,
#     aes(x = publication_year, y = benchmark_median),
#     linewidth = 0.95, colour = "#4D4D4D", linetype = "22", alpha = 0.95
#   ) +
#   
#   # cohorte mean / median
#   geom_line(
#     data = cohort_series,
#     aes(x = publication_year, y = mean_pub),
#     linewidth = 1.35, colour = "#1F4E79", lineend = "round"
#   ) +
#   geom_point(
#     data = cohort_series,
#     aes(x = publication_year, y = mean_pub),
#     size = 2.0, colour = "#1F4E79"
#   ) +
#   geom_line(
#     data = cohort_series,
#     aes(x = publication_year, y = median_pub),
#     linewidth = 1.0, colour = "#4C9F70", linetype = "22"
#   ) +
#   
#   # release year
#   geom_vline(
#     data = cohort_series %>% distinct(cohort, release_year),
#     aes(xintercept = release_year),
#     linetype = "dashed", linewidth = 0.7, colour = "#C44E52"
#   ) +
#   
#   # labels cohorte
#   geom_text(
#     data = cohort_labels,
#     aes(x = label_x, y = label_y_mean, label = "Cohort mean"),
#     hjust = 0, size = 3.3, colour = "#1F4E79", fontface = "bold"
#   ) +
#   geom_text(
#     data = cohort_labels,
#     aes(x = label_x, y = label_y_median, label = "Cohort median"),
#     hjust = 0, size = 3.1, colour = "#4C9F70"
#   ) +
#   
#   # labels benchmark
#   geom_text(
#     data = benchmark_labels,
#     aes(x = label_x, y = label_y_mean, label = "Rest of NSFC mean"),
#     hjust = 0, size = 3.2, colour = "#4D4D4D", fontface = "bold"
#   ) +
#   geom_text(
#     data = benchmark_labels,
#     aes(x = label_x, y = label_y_median, label = "Rest of NSFC median"),
#     hjust = 0, size = 3.0, colour = "#4D4D4D"
#   ) +
#   
#   facet_wrap(~ cohort, scales = "free_y", ncol = 2) +
#   scale_x_continuous(
#     breaks = seq(start_year, end_year, by = 2),
#     expand = expansion(mult = c(0.01, 0.16))
#   ) +
#   scale_y_continuous(labels = label_number(accuracy = 0.1)) +
#   labs(
#     title = "Listed-journal cohorts can be compared with the broader NSFC publication baseline",
#     subtitle = "Blue/green lines show the listed cohort; dark grey lines show the rest of the NSFC corpus excluding journals from that cohort.",
#     x = NULL,
#     y = "Publications per journal",
#     caption = "Solid lines = mean; dashed lines = median; shaded area = interquartile range for the listed cohort."
#   ) +
#   theme_pub_refined()
# 
# print(g5_refined)
# 
# # ==============================
# # PANNEAU OPTIONNEL
# # ==============================
# 
# panel_refined_extended <- g1_refined / g5_refined
# print(panel_refined_extended)
# 
# # ==============================
# # EXPORT OPTIONNEL
# # ==============================
# 
# # ggsave(
# #   "figures_nsfc_ewl_refined/Figure5_refined_cohort_vs_rest_nsfc.png",
# #   g5_refined, width = 13.5, height = 9.5, dpi = 350, bg = "white"
# # )
# 
# # ggsave(
# #   "figures_nsfc_ewl_refined/Panel_refined_extended.png",
# #   panel_refined_extended, width = 13.5, height = 15.5, dpi = 350, bg = "white"
# # )