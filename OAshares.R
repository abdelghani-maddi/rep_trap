# rm(list = ls())


# Load required packages
library(tidyverse)
library(httr)
library(jsonlite)
library(questionr)
library(openxlsx)
library(fixest)
library(gtsummary)
library(gt)
library(scales)
library(ggthemes)
library(patchwork)
library(ggridges)
library(viridis)
library(hrbrthemes)




# data from worksOA.R
for (year in 1997:2001) {
  cat("Fetching data for year:", year, "\n")
  
  # Tenter de récupérer les données pour l’année en cours
  try({
    all_nfsc <- oa_fetch(
      entity = "works",
      grants.funder = "f4320321001",
      publication_year = year,
      verbose = TRUE
    )
    
    # Enregistrement du fichier RDS
    saveRDS(works_nsfc, file = paste0("works_nsfc_", year, ".rds"))
  }, silent = TRUE)
}
all_nfsc <- readRDS("D:/all_nfsc.rds")
# df_awards <- readRDS("D:/df_awards.rds")
# project_pub <- readRDS("D:/project_pub.rds")


# Set parameters
base_url <- "https://api.openalex.org/works"
per_page <- 200  # maximum allowed per page
group_by_field <- "publication_year"

# Function to fetch grouped publication counts
get_counts <- function(country_code = NULL, funder_id = NULL, is_oa = NULL) {
  # Build filter string
  filters <- c()
  if (!is.null(country_code)) {
    filters <- c(filters, paste0("institutions.country_code:", country_code))
  }
  if (!is.null(funder_id)) {
    filters <- c(filters, paste0("grants.funder:", funder_id))
  }
  if (!is.null(is_oa)) {
    filters <- c(filters, paste0("is_oa:", tolower(as.character(is_oa))))
  }
  
  # Assemble final filter query
  filter_query <- paste(filters, collapse = ",")
  
  # Construct API URL
  url <- paste0(base_url,
                "?filter=", filter_query,
                "&group_by=", group_by_field,
                "&per_page=", per_page)
  
  # API call
  response <- GET(url)
  content <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(content)
  
  # Extract grouped counts
  counts <- data$group_by
  counts_df <- tibble(
    year = as.integer(counts$key),
    count = counts$count
  )
  return(counts_df)
}

# ======= Fetch data for China =======
total_counts_cn <- get_counts(country_code = "CN")
oa_counts_cn <- get_counts(country_code = "CN", is_oa = TRUE)

results_cn <- total_counts_cn %>%
  left_join(oa_counts_cn, by = "year", suffix = c("_total", "_oa")) %>%
  mutate(
    oa_rate = count_oa / count_total,
    group = "China"
  )

# ======= Fetch data for the World =======
total_counts_world <- get_counts()
oa_counts_world <- get_counts(is_oa = TRUE)

results_world <- total_counts_world %>%
  left_join(oa_counts_world, by = "year", suffix = c("_total", "_oa")) %>%
  mutate(
    oa_rate = count_oa / count_total,
    group = "World"
  )

#=====================================
#=====================================

# ======= Merge all datasets =======
results_all <- bind_rows(results_cn, results_world) %>% # , results_nsfc) %>%
  arrange(year, group)

# Filter for reasonable publication years
results_all <- results_all %>%
  filter(year >= 2000 & year <= 2025)

# ======= Plot Open Access rates =======

ggplot(results_all, aes(x = year, y = oa_rate, color = group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Open Access Publication Rate - China, World and NSFC (China)",
       x = "Publication Year",
       y = "Open Access Rate",
       color = "Group") +
  theme_minimal()  +
  scale_x_continuous(
    breaks = 2005:2024,
    expand = expansion(mult = c(0.01, 0.01))  # add space for labels
  )


####################################
# 
oa_by_year <- all_nfsc %>%
  filter(publication_year > 1999 &
           publication_year <= 2025 &
           type == "article" # &
           # id %in% project_pub$id &
           #!(host_organization == "Hindawi Publishing Corporation")
         ) %>%
  group_by(publication_year) %>%
  summarise(
    count_total = n(),
    count_oa = sum(is_oa, na.rm = TRUE),
    oa_rate = count_oa / count_total
  ) %>%
  mutate(group = "NSFC") %>%
  rename(year = publication_year)


results_combined <- bind_rows(results_all, oa_by_year)

results_combined2 <- results_combined %>%
  filter(year < 2025 & !group == "NSFC (China)" & year > 2004)

ggplot(results_combined2, aes(x = year, y = oa_rate, color = group)) +
  geom_line(size = 3) +  # <- ici on augmente l'épaisseur
  geom_point(size = 6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Open Access Publication Share - China, World and NSFC",
    x = "Publication Year",
    y = "Open Access Share",
    color = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  +
  scale_x_continuous(
    breaks = 2005:2024,
    expand = expansion(mult = c(0.01, 0.01))  # add space for labels
  )


######################################
##########
# Revues OpenAlex
library(ggtext)
library(openalexR)

jours_all <- oa_fetch(
  entity = "sources",
  works_count = ">10",
  verbose = TRUE
)
saveRDS(jours_all, "D:/jours_all_oax.rds")
jours_all <- readRDS("D:/jours_all_oax.rds")

# on a extrait donc revues de openalex.

#####################################
#####################################
#####################################
#####################################
# revues où publie NSFC

nsfc_jours <- all_nfsc %>%
  select(id, so_id, so, host_organization, 
         is_oa, oa_status, host_organization,
         publication_year, type, cited_by_count) %>%
  unique()


nsfc_jours <- nsfc_jours %>%
  left_join(jours_all, by = c("so_id" = "id"))


nsfc_jnal_oa_type <- nsfc_jours %>%
  group_by(so_id, display_name, host_organization.x, publication_year, is_oa.y, oa_status) %>%
  summarize(nb = n())

saveRDS(nsfc_jnal_oa_type, "D:/nsfc_jnal_oa_type.rds")

nsfc_jnal_oa_type <- readRDS("D:/nsfc_jnal_oa_type.rds")

#########
nsfc_jnal_oa_type_count <- nsfc_jnal_oa_type %>%
  group_by(publication_year, oa_status) %>%
  summarise(nbr = sum(nb), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(part = nbr / sum(nbr))

#######################
#######################
##################
##################
library(tidyverse)
library(scales)
library(patchwork)

# Étape 1 : fusionner les dataframes
# all_nsfc_augmented <- all_nfsc %>%
#   left_join(jours_all %>% select(id, country_code, summary_stats), by = c("so_id" = "id"))
# Extraire 2yr_mean_citedness
all_nsfc_augmented <- all_nfsc %>%
  left_join(jours_all %>% select(id, country_code, summary_stats), by = c("so_id" = "id")) %>%
  mutate(
    impact_2yr = map_dbl(summary_stats, ~ if("2yr_mean_citedness" %in% names(.x)) .x["2yr_mean_citedness"] else NA_real_),
    h_index = map_dbl(summary_stats, ~ if("h_index" %in% names(.x)) .x["h_index"] else NA_real_)
  )

# Étape 2 : classer les éditeurs
grey_publishers <- c(
  "Multidisciplinary Digital Publishing Institute",  # MDPI
  "Hindawi Publishing Corporation",                  # Hindawi
  "Frontiers Media",
  "Bentham Science Publishers",
  "Cogent OA",
  "OMICS Publishing Group"
)

top5_publishers <- c(
  "Springer Nature", "Elsevier BV", "Wiley",
  "Taylor & Francis", "SAGE Publishing"
)

all_nsfc_augmented <- all_nsfc_augmented %>%
  mutate(group = case_when(
    host_organization %in% grey_publishers ~ "Grey publishers",
    host_organization %in% top5_publishers ~ "Top 5 publishers",
    is.na(country_code) ~ "Other (Domestic/Regional)", # sécurité
    country_code == "CN" ~ "Other (Domestic/Regional)",
    TRUE ~ "Other (International)"
  ))

saveRDS(all_nsfc_augmented, "D:/all_nsfc_augmented.rds")
all_nsfc_augmented <- readRDS("D:/all_nsfc_augmented.rds")


all_nsfc_augmented %>%
  filter(publication_year > 2004) %>%
  select(id) %>%
  unique() %>%
  count()

library(gtsummary)

sampl <- all_nsfc_augmented %>%
  as.data.frame() %>%
  filter(publication_year > 2004) %>%
  select(id, group, is_) %>% 
  unique()


# Création du tableau résumé
table_res <- sampl %>%
  labelled::set_variable_labels(
    group = "Publishers groups") %>%
  tbl_summary(
    include = c(group)
  ) %>%
  modify_header(
    list(label ~ "**Variable**")
  ) %>%
  bold_labels()

# Conversion du tableau en objet flextable
table_flex <- as_flex_table(table_res)

# Exportation en fichier Word dans D:/
flextable::save_as_docx(
  table_flex,
  path = "D:/table_summary.docx"
)

# Message de confirmation
cat("✅ Le fichier Word a été exporté dans D:/table_summary.docx\n")

  
#########################
#########################
#########################

market_share <- all_nsfc_augmented %>%
  select(id, publication_year, group) %>%
  distinct() %>%  # unique() ou distinct(), même effet
  group_by(publication_year, group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()
write.xlsx(market_share, "D:/market_share.xlsx")

#########################
#########################
#########################

# Étape 3 : agrégation
pubs_by_group <- all_nsfc_augmented %>%
  filter(publication_year >= 2005) %>%
  count(publication_year, group) %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n))

# Palette
publisher_colors <- c(
  "Grey publishers" = "#EF476F",
  "Top 5 publishers" = "#118AB2",
  "Other (Domestic/Regional)" = "#06D6A0",
  "Other (International)" = "#FFD166"
)

# Étape 4 : graphiques
plot_counts <- ggplot(pubs_by_group, aes(x = publication_year, y = n, fill = group)) +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = publisher_colors) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "NSFC publications by publisher type (A)",
    x = "Publication Year",
    y = "Number of Publications",
    fill = "Publisher Group"
  ) +
  scale_x_continuous(breaks = 2005:2024) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 0.5)
  )

plot_share <- ggplot(pubs_by_group, aes(x = publication_year, y = share, fill = group)) +
  geom_area(alpha = 0.9) +
  geom_vline(xintercept = c(2013,2019, 2022), linetype = "dashed", color = "gray30", linewidth = 0.8) +
  scale_fill_manual(values = publisher_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Share of NSFC publications by publisher type (B)",
    x = "Publication Year",
    y = "Share of publications (%)",
    fill = "Publisher Group"
  ) +

    # Annotations explicatives avec retour à la ligne
    annotate("text", x = 2012.5, y = 0.93,
             label = "Rise of Hindawi\n(2011–2013)",
             size = 3, fontface = "italic", color = "white") +
    annotate("text", x = 2018, y = 0.97,
             label = "Rise of MDPI and Frontiers",
             size = 3.5, fontface = "italic", color = "white") +
    annotate("text", x = 2020, y = 0.75,
             label = "Research\n assessment\n reform",
             size = 3.5, fontface = "italic", color = "gray31") +
  
      annotate("text", x = 2022, y = 0.91,
             label = "Collapse of MDPI\n and Frontiers\n(after 2022)",
             size = 3, fontface = "italic", color = "white") +

    scale_x_continuous(breaks = 2005:2024) +
  
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 0.5)
  )

# Affichage côte à côte
plot_counts + plot_share + plot_layout(ncol = 1)

##########
##########
##########

library(ggplot2)
library(scales)
library(viridisLite)  # pour palettes inclusives
library(patchwork)

library(ggplot2)
library(scales)
library(viridisLite)
library(patchwork)

# Palette inclusive avec Grey publishers en gris
groups <- unique(pubs_by_group$group)
base_colors <- viridis(length(groups) - 1, option = "D", begin = 0.1, end = 0.9)
publisher_colors <- setNames(c("grey40", base_colors), groups)

# Paramètres de thème (texte plus grand)
big_theme <- theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

# Plot A
plot_counts <- ggplot(pubs_by_group, aes(x = publication_year, y = n, fill = group)) +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = publisher_colors) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "Absolute number of NSFC publications by publisher group (A)",
    x = "Publication Year",
    y = "Number of publications",
    fill = "Publisher group"
  ) +
  scale_x_continuous(breaks = 2005:2024) +
  big_theme

# Plot B
plot_share <- ggplot(pubs_by_group, aes(x = publication_year, y = share, fill = group)) +
  geom_area(alpha = 0.9) +
  geom_vline(xintercept = c(2013, 2019, 2022),
             linetype = "dashed", color = "gray30", linewidth = 0.8) +
  scale_fill_manual(values = publisher_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Relative shares of NSFC publications by publisher group (B)",
    x = "Publication Year",
    y = "Share of publications (%)",
    fill = "Publisher group"
  ) +
  scale_x_continuous(breaks = 2005:2024) +
  
  # Annotations avec fond semi-transparent pour plus de lisibilité
  annotate("label", x = 2012.5, y = 0.3,
           label = "Rise of Hindawi\n(2011–2013)",
           size = 4, fontface = "italic", fill = "grey20", color = "white") +
  annotate("label", x = 2018, y = 0.2,
           label = "Rise of MDPI and Frontiers\n(2015-2021)",
           size = 4, fontface = "italic", fill = "grey20", color = "white") +
  annotate("label", x = 2020, y = 0.75,
           label = "Research\nassessment\nreform\n(2020)",
           size = 4, fontface = "italic", fill = "grey90", color = "black") +
  annotate("label", x = 2022, y = 0.3,
           label = "Collapse of MDPI\nand Frontiers\n(after 2022)",
           size = 4, fontface = "italic", fill = "grey20", color = "white") +
  
  big_theme

# Affichage (plots plus grands et espacés)
(plot_counts / plot_share) +
  plot_layout(heights = c(1, 1.2))  # plus de place au graphique B


##########
##########
##########



###########
# other_publishers_na <- jours_all %>%
#   filter(is.na(country_code) & type == "journal")
# (elles sont très majoritairement des revues chinoises ou régionales)

###########
# distribution impact des revues par type éditeur

impact_jours <- all_nsfc_augmented %>%
  select(so_id, group, impact_2yr, h_index) %>%
  unique()


## Réordonnancement de all_nsfc_augmented$group
all_nsfc_augmented <- all_nsfc_augmented %>%
  mutate(group = fct_relevel(
    group,
    "Other (Domestic/Regional)", "Grey publishers", "Other (International)",
    "Top 5 publishers"
  ))

#### # beaucoup de données manquantes
# impact_jours2 <- impact_jours %>%
#   filter(impact_2yr>0)
# # Plot
# ggplot(impact_jours2, aes(x = log(1+impact_2yr), y = fct_reorder(group, impact_2yr, .fun = median), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, color = "white") +
#   scale_fill_viridis(name = "2-Year Mean Citedness", option = "C") +
#   labs(
#     title = "Distribution of 2-Year Mean Citedness by Publisher Group",
#     x = "2-Year Mean Citedness",
#     y = "Publisher Group"
#   ) +
#   theme_ipsum(base_family = "Lato") +
#   theme(
#     legend.position = "right",
#     panel.spacing = unit(0.1, "lines"),
#     plot.title = element_text(face = "bold", size = 14),
#     axis.title.x = element_text(margin = margin(t = 10)),
#     axis.title.y = element_text(margin = margin(r = 10))
#   ) +
#   scale_fill_viridis(name = "log(1 + Impact 2yr)", option = "C")

## H index car les citations à 2 ans contiennet des données manquantes dans les métadonnées OpenAlex
# Plot
ggplot(impact_jours, aes(x = log(1+h_index), y = fct_reorder(group, h_index, .fun = median), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, color = "white") +
  scale_fill_viridis(name = "h-index (log)", option = "C") +
  labs(
    title = "Distribution of h-index (log) by Publisher Group",
    x = "h-index (log)",
    y = "Publisher Group"
  ) +
  theme_ipsum(base_family = "Lato") +
  theme(
    legend.position = "right",
    panel.spacing = unit(0.1, "lines"),
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_fill_viridis(name = "h-index (log)", option = "C")


##################
# # Étape 1 — Définir la liste des éditeurs gris

## Nombre de publications
oa_status_group <- all_nsfc_augmented %>%
  # filter(!(oa_status == "closed")) %>%
  group_by(group, oa_status, publication_year) %>%
  summarise(nbr = n())

##############
##############
##############
##############
##############
##############
# nombre de publications par éditeur gris
all_nfsc %>%
  filter(publication_year > 2004 & publication_year < 2025) %>%
  select(id) %>%
  distinct() %>%
  count()
  
count_grey <- all_nsfc_augmented %>%
  filter(group == "Grey publishers" & publication_year > 2004 & publication_year < 2025
         & !(id=="https://openalex.org/W2085145734")) %>% # Erreur dans les données OpenAlex
  select(id, host_organization, publication_year) %>%
  distinct() %>%
  group_by(publication_year, host_organization) %>%
  summarise(nbr = n(), .groups = "drop")

##############
# 1. Data: manual label placement (x = year, y = vertical pos)
custom_labels <- tibble(
  host_organization = c(
    "Bentham Science Publishers",
    "Cogent OA",
    "Frontiers Media",
    "Hindawi Publishing Corporation",
    "Multidisciplinary Digital Publishing Institute"
  ),
  x = c(2019.1, 2019.5, 2021.3, 2010, 2015.5),  # horizontal position
  y = c(2000, 3000, 17000, 6000, 25000),             # vertical position
  label = host_organization
)

# 2. Plot
ggplot(count_grey, aes(x = publication_year, y = nbr, color = host_organization)) +
  geom_line(size = 3) +
  geom_point(size = 6) +
  # 3. Custom labels (manual position)
  geom_text(
    data = custom_labels,
    aes(x = x, y = y, label = label, color = host_organization),
    hjust = 0,  # left-aligned
    size = 5,
    show.legend = FALSE
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Trends in Publications by Grey Publishers",
    subtitle = "Based on NSFC-funded publications",
    x = "Publication Year",
    y = "Number of Publications",
    color = "Publisher"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  scale_x_continuous(
    breaks = 2005:2024,
    expand = expansion(mult = c(0.01, 0.01))  # add space for labels
  )


##############
##############

# Total publications par year et group
oa_total <- oa_status_group %>%
  group_by(publication_year, group) %>%
  summarise(total = sum(nbr), .groups = "drop")

# Ajout de la part relative
oa_status_group <- oa_status_group %>%
  left_join(oa_total, by = c("publication_year", "group")) %>%
  mutate(share = nbr / total)

# oa_status_group_check <- oa_status_group %>%
#   group_by(group, publication_year) %>%
#   summarise(test_sum = sum(share), .groups = "drop") %>%
#   arrange(desc(test_sum)) %>%
#   group_by(group, publication_year) %>%
#   summarise(nb = sum(test_sum))# ça doit être au plus ≈1 (100%)

# Total global annuel par statut OA (pour le graph 3)
all_nsfc_augmented2 <- all_nsfc_augmented %>%
  mutate(
    oa_status_recode = case_when(
      oa_status == "gold" & group == "Grey publishers" ~ "gold_grey",
      oa_status == "gold" & group != "Grey publishers" ~ "gold_other",
      TRUE ~ oa_status
    )
  )

oa_status_evol <- all_nsfc_augmented2 %>%
  filter(publication_year > 2004 & oa_status_recode != "closed") %>%
  count(publication_year, oa_status_recode) %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()


oa_status_total_year_vrif <- oa_status_evol %>%
  group_by(publication_year) %>%
  summarise(sum = sum(share))

write.xlsx(oa_status_evol, "D:/oa_status_evol.xlsx")

# Pour graph 4 : part au sein des OA uniquement
oa_only <- all_nsfc_augmented %>%
  filter(oa_status != "closed") %>%
  group_by(publication_year, oa_status) %>%
#  summarise(n = n(), .groups = "drop") %>%
  summarise(n = n()) %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n))

# Pour graph 5 : Grey vs le reste parmi OA
oa_grey_vs_others <- all_nsfc_augmented %>%
  filter(oa_status != "closed") %>%
  # mutate(group = if_else(group == "Grey publishers", "Grey publishers", "Other publishers")) %>%
  group_by(publication_year, group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n))

##############
##############
##############
##############


ggplot(oa_total, aes(x = publication_year, y = total, fill = group)) +
  geom_area(alpha = 0.9) +
  scale_fill_brewer(palette = "Set2") +
  #scale_y_continuous(labels = comma) +
  labs(
    title = "Total NSFC Publications per Year by Publisher Group",
    x = "Publication Year",
    y = "Number of Publications",
    fill = "Publisher Group"
  ) +
  theme_minimal(base_family = "Lato")


##############
##############

ggplot(oa_status_group, aes(x = publication_year, y = share, fill = oa_status)) +
  geom_area(position = "stack") +
  facet_wrap(~ group, ncol = 2) +
  #scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Share of OA Status per Publisher Group",
    x = "Publication Year",
    y = "Share of Publications",
    fill = "OA Status"
  ) +
  theme_minimal(base_family = "Lato")


##############
##############

ggplot(oa_status_total_year, aes(x = publication_year, y = share, fill = oa_status)) +
  geom_area() +
  #scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Overall Share of Publications by OA Status",
    x = "Publication Year",
    y = "Share of Publications",
    fill = "OA Status"
  ) +
  theme_minimal(base_family = "Lato")


##############
##############

ggplot(oa_only, aes(x = publication_year, y = share, fill = oa_status)) +
  geom_area() +
  #scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribution of OA Types",
    x = "Publication Year",
    y = "Share within OA",
    fill = "OA Type"
  ) +
  theme_minimal(base_family = "Lato")


##############
##############
oa_grey_vs_others %>%
  filter(publication_year>2004) %>%
ggplot(aes(x = publication_year, y = share, fill = group)) +
  geom_area() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  #scale_fill_manual(values = c("Grey publishers" = "#d73027", "Other publishers" = "#4575b4")) +
  labs(
    title = "Grey vs Other Publishers (among OA only)",
    x = "Publication Year",
    y = "Share within OA",
    fill = "Publisher Group"
  ) +
  theme_minimal(base_family = "Lato") +
  scale_x_continuous(breaks = 2000:2024) +
  # Annotations explicatives avec retour à la ligne
  annotate("text", x = 2011.5, y = 0.75,
           label = "Rise of Hindawi\n(2011–2013)",
           size = 3.5, fontface = "italic", color = "white") +
  annotate("text", x = 2018, y = 0.93,
           label = "Rise of MDPI and Frontiers\n(2015–2022)",
           
           size = 3.5, fontface = "italic", color = "white") +
  annotate("text", x = 2023, y = 0.91,
           label = "Collapse of MDPI\n and Frontiers\n(after 2022)",
           
           size = 3, fontface = "italic", color = "white") +
  annotate("text", x = 2014, y = 0.8,
           label = "Collapse of Hindawi\n(after 2013)",
           
           size = 3, fontface = "italic", color = "white") +
  geom_vline(xintercept = c(2013, 2015, 2022), linetype = "dashed", color = "gray30", linewidth = 0.8)
  
write.xlsx(oa_grey_vs_others, "D:/oa_grey_vs_others.xlsx")
##############
##############
##############


authors_df <- map_dfr(
  seq_len(nrow(all_nsfc_augmented)),
  ~ {
    df_authors <- all_nsfc_augmented$author[[.x]]
    
    # Vérifie si c'est bien un data.frame non vide
    if (is.data.frame(df_authors) && nrow(df_authors) > 0) {
      pub_id    <- all_nsfc_augmented$id[.x]
      pub_year  <- all_nsfc_augmented$publication_year[.x]
      pub_group <- all_nsfc_augmented$group[.x]
      pub_oa    <- all_nsfc_augmented$oa_status[.x]
      src_id    <- all_nsfc_augmented$so_id[.x]
      src_name  <- all_nsfc_augmented$so[.x]
      hst_org   <- all_nsfc_augmented$host_organization[.x]
      
      df_authors %>%
        mutate(pub_id           = pub_id,
               publication_year = pub_year,
               publisher_group  = pub_group,
               oa_status        = pub_oa,
               source_id        = src_id,
               source_name      = src_name,
               host_org         = hst_org)
    } else {
      NULL
    }
  }
)

saveRDS(authors_df, "D:/authors_df.rds")
authors_df <- readRDS("D:/authors_df.rds")


#####################
#####################

# Revues douteuses https://mp.weixin.qq.com/s?__biz=MzI1MzA2MzM1NA==&mid=2659567089&idx=1&sn=f4cad93ba3a425524eb19d25ffb25960&chksm=f2ab5c24c5dcd53275aa36b9167d9788577a6a919e2bc80b1144088ea78b32bae8350535b344&scene=0&xtrack=1
revues_cas_brut <- c(
  "Metals",
  "Coatings",
  "Materials",
  "JOURNAL OF NANOSCIENCE AND NANOTECHNOLOGY",
  "Minerals",
  "Atmosphere",
  "Artificial Cells Nanomedicine and Biotechnology",
  "Advances in Civil Engineering",
  "INTERNATIONAL JOURNAL OF ENERGY RESEARCH",
  "MATHEMATICAL PROBLEMS IN ENGINEERING",
  "SENSORS",
  "Energies",
  "Applied Sciences",#"Applied Sciences-Basel",
  "Polymers",
  "Electronics",
  "Processes",
  "COMPLEXITY",
  "Desalination and Water Treatment",
  "International Journal of Electrochemical Science",
  "Catalysts",
  "MOLECULES",
  "NATURAL PRODUCT RESEARCH",
  "ZEITSCHRIFT FUR KRISTALLOGRAPHIE-NEW CRYSTAL STRUCTURES",
  "Sustainability",
  "Water",
  "IEEE Access",
  "Agronomy",# "Agronomy-Basel",
  "JOURNAL OF CELLULAR BIOCHEMISTRY",
  "JOURNAL OF CELLULAR PHYSIOLOGY",
  "BIOSCIENCE REPORTS",
  "Biomed Research International",
  "Plants",# "Plants-Basel",
  "Cells",
  "Boundary Value Problems",
  "Advances in Difference Equations",
  "JOURNAL OF INEQUALITIES AND APPLICATIONS",
  "Mathematics",
  "European Review for Medical and Pharmacological Sciences",
  "International Journal of Clinical and Experimental Pathology",
  "MEDICINE",
  "International Journal of Clinical and Experimental Medicine",
  "BIOMEDICINE & PHARMACOTHERAPY",
  "EXPERIMENTAL AND MOLECULAR PATHOLOGY",
  "BRAZILIAN JOURNAL OF MEDICAL AND BIOLOGICAL RESEARCH",
  "Cancer Biomarkers",
  "INTERNATIONAL JOURNAL OF IMMUNOPATHOLOGY AND PHARMACOLOGY",
  "ONCOLOGY RESEARCH",
  "American Journal of Cancer Research",
  "MEDICAL SCIENCE MONITOR",
  "Oncology Letters",
  "Experimental and Therapeutic Medicine",
  "OncoTargets and Therapy",
  "ONCOLOGY REPORTS",
  "Molecular Medicine Reports",
  "INTERNATIONAL JOURNAL OF MOLECULAR MEDICINE",
  "JOURNAL OF INTERNATIONAL MEDICAL RESEARCH",
  "American Journal of Translational Research",
  "Journal of Biomaterials and Tissue Engineering",
  "Aging-US",
  "LIFE SCIENCES",
  "Journal of Clinical Medicine",
  "International Journal of Environmental Research and Public Health",
  "Acta Medica Mediterranea"
)

###########
revues_cas_norm <- tolower(gsub("[[:punct:]]", "", revues_cas_brut))


revues_cas_norm <- revues_cas_brut %>%
  tolower() %>%
  gsub("[[:punct:]]", " ", .) %>%   # ponctuation -> espace
  gsub("\\s+", " ", .) %>%          # compresser espaces multiples
  trimws()


###########

revues_cas_df <- data.frame(
  title = revues_cas_norm,
  risk_level = c(
    "low", "low", "low", "low",
    "low", "low",
    "high", "medium", "medium", "medium", "low",
    "low", "low", "low", "low", "low",
    "high", "low", "medium", "low",
    "low", "low", "medium", "medium", "low",
    "medium", "low",
    "low", "medium", "medium", "medium", "medium", "low",
    "high", "high", "medium", "low", "high", "high", "high",
    "medium", "medium", "medium", "medium", "medium",
    "medium", "medium", "medium", "medium", "medium",
    "medium", "medium", "medium", "medium", "medium", "medium",
    "medium", "medium", "low", "low", "low", "low", "low"
  )
)


library(dplyr)
library(stringr)

# ---- 1. Normaliser les noms de revues dans les deux tables ----

authors_df <- authors_df %>%
  mutate(source_name_norm = str_to_lower(source_name) %>%
           str_replace_all("[[:punct:]]", "") %>%
           str_squish())

# ---- 2. Jointure par nom normalisé ----
authors_cas <- authors_df %>%
  left_join(revues_cas_df %>% select(title, risk_level),
            by = c("source_name_norm" = "title"))

authors_cas <- authors_cas %>%
  filter(!is.na(risk_level))

saveRDS(authors_cas, "D:/authors_cas.rds")
authors_cas <- readRDS("D:/authors_cas.rds")

#######################
#######################
list_rev_cas_trouvee <- authors_cas %>% select(source_name_norm) %>% unique()
list_rev_cas_trouvee$existe <- "Revue trouvée"

rev_non_trouvees <-revues_cas_df   %>%
  left_join(list_rev_cas_trouvee,
            by = c("title" = "source_name_norm")) %>%
  filter(is.na(existe))

# seulement deux revues : 
# Brazilian Journal of Medical and Biological Research : 6 publications
# Cancer Biomarkers : 5 publications

# authors_df_bio <- authors_df %>%
# distinct(source_name, source_name_norm) %>%   # remplace select() + unique()
#   filter(str_detect(tolower(source_name), "biomarkers")) # insensible à la casse

# https://api.openalex.org/works?page=1&filter=primary_location.source.id:s109719883|s25690712|s133947007|s2738977497|s100363452|s4210230202|s168512209|s15111687|s183843087|s61994691|s198195518|s156858864|s58832373|s151424340|s120324992|s124357017|s95395365|s192722082|s25983388|s2484002919&sort=cited_by_count:desc&per_page=10
# https://api.openalex.org/works?page=9&filter=primary_location.source.id:s109719883|s25690712|s133947007|s2738977497|s100363452|s4210230202|s168512209|s15111687|s183843087|s61994691|s198195518|s156858864|s58832373|s151424340|s120324992|s124357017|s95395365|s192722082|s25983388|s2484002919,grants.funder:f4320321001&sort=cited_by_count:desc&per_page=10

################
################################
authors_non_cas <- authors_df %>%
  left_join(revues_cas_df %>% select(title, risk_level),
            by = c("source_name_norm" = "title"))

authors_non_cas <- authors_non_cas %>%
  filter(is.na(risk_level) & au_id %in% unique(authors_cas$au_id))

saveRDS(authors_non_cas, "D:/authors_non_cas.rds")
authors_non_cas <- readRDS("D:/authors_non_cas.rds")

###################################################

library(openalexR)

ids <- gsub("https://openalex.org/", "", authors_df$source_id) %>% unique()

ids <- na.omit(ids)


jours_all <- oa_fetch(
  entity = "sources",
  id = ids,
  verbose = TRUE
)

saveRDS(jours_all, "D:/jours_all_nsfc.rds")
jours_all <- readRDS("D:/jours_all_nsfc.rds")


authors_cas <- authors_cas %>%
  left_join(jours_all, by = c("source_id" = "id"))

authors_non_cas <- authors_non_cas %>%
  left_join(jours_all, by = c("source_id" = "id"))

authors_non_cas <- authors_non_cas %>%
  filter(au_id %in% authors_cas$au_id)

# l'idée est de comparer le comportement des mêmes auteurs 
# qui ont publié dans les revues "douteuses" 
# avec le comportement des mêmes auteurs dans d'autres revues
# l'objectif est de voir l'impact de l'avènement de la liste 
# sur les revues listées, mais de façon globale sur les 
# éditeurs concernés. 
# mon hypothèse est que l'impact est plus marqué pour les éditeurs
# nouveaux OA que pour les éditeurs bien établis, où l'impact 
# serait localisé au niveau des revues listées.

############ description ----
library(tidyverse)
library(fixest)
library(dplyr)
library(purrr)

## is indexed in wos or scopus
jours_all$is_indexed <- ifelse(jours_all$issn_l %in% wos$wos_issn |
                                 jours_all$issn_l %in% scopus$scopus_issn, 1, 0)

##
jnal_concpt_imp <- map_dfr(
  seq_len(nrow(jours_all)),
  ~ {
    concepts_df <- jours_all$x_concepts[[.x]]
    
    if (is.data.frame(concepts_df) && nrow(concepts_df) > 0) {
      src_id <- jours_all$id[.x]
      
      concepts_df %>%
        filter(level == 0) %>%
        arrange(desc(score)) %>%
        slice(1) %>% # garde le concept avec score max
        mutate(source_id = src_id)
    } else {
      NULL
    }
  }
)


#######
# Stats
jnal_summary_stats <- map_dfr(
  seq_len(nrow(jours_all)),
  ~ {
    stats_vec <- jours_all$summary_stats[[.x]]
    
    if (!is.null(stats_vec) && length(stats_vec) > 0) {
      src_id <- jours_all$id[.x]
      is_oa <- jours_all$is_oa[.x]
      is_indexed <- jours_all$is_indexed[.x]
      
      
      tibble(
        source_id          = src_id,
        is_oa              = is_oa,
        mean_citedness_2yr = as.numeric(stats_vec["2yr_mean_citedness"]),
        h_index            = as.numeric(stats_vec["h_index"]),
        i10_index          = as.numeric(stats_vec["i10_index"]),
        is_indexed         = is_indexed
      )
    } else {
      NULL
    }
  }
)

# Vérification rapide
head(jnal_summary_stats)

# Sauvegarde
saveRDS(jnal_summary_stats, "D:/jnal_summary_stats.rds")
jnal_summary_stats <- readRDS("D:/jnal_summary_stats.rds")

####

# On garde uniquement les colonnes utiles
jnal_main_concept <- jnal_concpt_imp %>%
  select(source_id, main_concept = display_name, main_concept_id = id, main_concept_score = score)

# Sauvegarde
saveRDS(jnal_main_concept, "D:/jnal_main_concept.rds")
jnal_main_concept <- readRDS("D:/jnal_main_concept.rds")



#############################
#############################
# Panel data construction corrigée
#############################
authors_non_cas$risk_level <- "Not_listed"

panel_data <- bind_rows(
  authors_cas,
  authors_non_cas
) %>%
  mutate(
    post   = as.integer(publication_year >= 2020),
    listed = ifelse(risk_level == "Not_listed" | is.na(risk_level), 0, 1)
  ) %>%
  
  left_join(jnal_summary_stats, by = "source_id") %>%
  left_join(jnal_main_concept,    by = "source_id") %>%
  
  group_by(
    au_id, source_id, is_oa, publication_year, listed, post, risk_level,
    publisher_group, main_concept, mean_citedness_2yr, is_indexed #, pub_id
  ) %>%
  summarise(n_pubs = n_distinct(pub_id), .groups = "drop")


# panel_data$risk_level <- ifelse(is.na(panel_data$risk_level), "Not_listed", panel_data$risk_level)
panel_data$risk_level <- as.factor(panel_data$risk_level)

# dans les metadonnées openalex certaines revues n'ont pas de concept/citation..
panel_data <- panel_data %>%
  filter(!is.na(main_concept))


# Restreindre aux auteurs présents avant ET après 2020
authors_keep <- panel_data %>%
  group_by(au_id) %>%
  summarise(min_y = min(publication_year), max_y = max(publication_year)) %>%
  filter(min_y < 2020, max_y > 2020) %>% pull(au_id)

panel_data <- panel_data %>% filter(au_id %in% authors_keep)

# panel_data <- panel_data %>%
#   filter(publication_year > 2015 & !is.na(main_concept))

# Filtrage période
panel_data_2016_24 <- panel_data %>%
  filter(publication_year > 2015 & !is.na(main_concept))

# old befor correction 1,941,674 distinctes publications
total_pubs <- bind_rows(authors_cas, authors_non_cas) %>%
  filter(publication_year > 2015) %>% 
  select(pub_id) %>%
  unique() %>%
  count()
# after correction 2,050,088 publications distinctes

# > n_distinct(panel_data_2016_24$au_id)
# [1] 236078 auteurs distincts
# > n_distinct(panel_data_2016_24$source_id)
# [1] 10006 revues distinctes

###

# Réordonnancement des facteurs
panel_data_2016_24$publisher_group <- panel_data_2016_24$publisher_group %>%
  fct_relevel(
    "Other (Domestic/Regional)",
    "Grey publishers",
    "Other (International)",  
    "Top 5 publishers"
  )


# Sauvegarde
saveRDS(panel_data_2016_24, "D:/panel_data_2016_24.rds")
panel_data_2016_24 <- readRDS("D:/panel_data_2016_24.rds")


panel_data_2016_24 %>%
  select(au_id, publication_year, n_pubs) %>%
  group_by(publication_year) %>%
  summarise(moy = mean(n_pubs))

### 
## Réordonnancement de panel_data_2016_24$publisher_group
panel_data_2016_24$publisher_group <- panel_data_2016_24$publisher_group %>%
  fct_relevel(
    "Other (International)", 
    "Top 5 publishers",
    "Other (Domestic/Regional)", 
    "Grey publishers"
  )

# Effet global
mod_core <- feols( n_pubs ~ listed*post + publisher_group + is_oa + 
                     mean_citedness_2yr + main_concept + is_indexed 
                   | au_id^publication_year, 
                   cluster = ~ au_id, 
                   data = panel_data_2016_24 )

summary(mod_core)

# Hétérogénéité par éditeur*
mod_het <- feols(
  n_pubs ~ i(publisher_group, listed*post, ref = "Top 5 publishers") +
    is_oa + mean_citedness_2yr + main_concept + is_indexed
  | au_id^publication_year,
  cluster = ~ au_id,
  data = panel_data_2016_24
)
summary(mod_het)

# Event study
mod_es <- feols(
  n_pubs ~ i(publication_year, listed, ref = 2019) +
    publisher_group + is_oa + mean_citedness_2yr + main_concept + is_indexed
  | au_id^publication_year,
  cluster = ~ au_id,
  data = panel_data_2016_24
)
summary(mod_es)

iplot(mod_es, ref.line = 0, main = "Event-study: CAS listing impact")
###
library(broom)
library(dplyr)

event_df <- broom::tidy(mod_es, conf.int = TRUE)

event_df <- event_df %>%
  filter(grepl("publication_year", term)) %>%
  mutate(
    year = as.numeric(gsub("publication_year::([0-9]{4}):listed", "\\1", term))
  )


library(ggplot2)
library(viridisLite)

ggplot(event_df, aes(x = year, y = estimate, color = estimate > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray30") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "black")) +
  scale_x_continuous(breaks = seq(min(event_df$year), max(event_df$year), 1)) +
  labs(
    title = "Event-study: CAS listing impact",
    subtitle = "OLS estimates with 95% confidence intervals",
    x = "Year",
    y = "Estimated effect on publications (listed vs non-listed)"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


###
# --- 2. Moyenne de publications par groupe (treated / non-treated) ---
moyennes_grp <- panel_data_2016_24 %>%
  group_by(publication_year, listed) %>%
  summarise(moy_pubs = mean(n_pubs), .groups = "drop")

# --- 3. Graphique ---
ggplot(moyennes_grp, aes(x = publication_year, y = moy_pubs,
                         color = factor(listed), group = listed)) +
  geom_line(size = 3) +
  geom_point(size = 5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     labels = c("0" = "Non-listed journals", "1" = "Listed journals (CAS)")) +
  labs(x = "Year",
       y = "Average number of publications per author",
       color = "Group",
       title = "Trajectories of treated vs control groups (2016–2024)",
       subtitle = "Dashed line = CAS list release (2020)") +
  theme_minimal(base_size = 14)


#################################
library(gtsummary)

tbl_core <- tbl_regression(mod_core)
tbl_het  <- tbl_regression(mod_het)
tbl_es   <- tbl_regression(mod_es)


library(modelsummary)


###########
###########
# 1. Renommer les coefficients pour lecture plus claire

coef_labels <- c(
  "listed" = "Listed journal",
  "listed:post" = "Listed × Post",
  "publisher_groupGrey publishers" = "Grey publishers",
  "publisher_groupOther (International)" = "Other (International)",
  "publisher_groupTop 5 publishers" = "Top 5 publishers",
  "is_oaTRUE" = "Open Access journal",
  "mean_citedness_2yr" = "Mean citedness (2yr)",
  "is_indexed" = "Indexed journal",
  "publisher_group::Grey publishers:listed * post" = "Grey × Listed × Post",
  "publisher_group::Other (International):listed * post" = "Other (International) × Listed × Post",
  "publication_year::2016:listed" = "2016 × Listed",
  "publication_year::2017:listed" = "2017 × Listed",
  "publication_year::2018:listed" = "2018 × Listed",
  "publication_year::2020:listed" = "2020 × Listed",
  "publication_year::2021:listed" = "2021 × Listed",
  "publication_year::2022:listed" = "2022 × Listed",
  "publication_year::2023:listed" = "2023 × Listed",
  "publication_year::2024:listed" = "2024 × Listed"
)

# 2. Construire le tableau comparatif
tbl <- modelsummary(
  list(
    "Model 1: Core DiD" = mod_core,
    "Model 2: Heterogeneity" = mod_het,
    "Model 3: Event-study" = mod_es
  ),
  coef_rename = coef_labels,
  coef_omit = "^main_concept",  # regex pour tous les main_concept
  stars = TRUE,
  gof_omit = "IC|Log|Adj|Pseudo|F|Within|Theta",
  statistic = "({std.error})",
  output = "gt",
  notes = "All models control for the main research discipline of the publication."
) |>
  gt::tab_caption("Impact of the CAS warning list on authors’ publishing behavior")


# Export en PNG
gtsave(tbl, "D:/table_models.png")
###########
###########


