# List of required packages
packages <- c(
  "shiny", "bslib", "dplyr", "ggplot2", "ggExtra", "readr",
  "tidyr", "patchwork", "e1071", "forcats", "ggpubr",
  "fmsb", "scales", "shinydashboard", "shinythemes",
  "stringr", "broom", "RColorBrewer", "tibble"
)

missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) install.packages(missing_packages)
lapply(packages, library, character.only = TRUE)



repo <- read_csv("repos_with_scores_filtered.csv", show_col_types = FALSE) %>%
  mutate(
    repo_age_days = as.numeric(repo_age_days),
    commit_count  = as.numeric(commit_count),
    is_active     = as.integer(days_since_last_commit <= 365),
    age_cohort    = factor(case_when(
      repo_age_days <= 730  ~ "0–2 years",
      repo_age_days <= 1825 ~ "2–5 years",
      repo_age_days <= 3650 ~ "5–10 years",
      TRUE                  ~ "10+ years"
    ), levels = c("0–2 years", "2–5 years", "5–10 years", "10+ years"))
  )


# ── Quality groups ────────────────────────────────────────────────────────────
coverage_data <- repo %>%
  mutate(
    quality_group = factor(case_when(
      final_score_1to5 >= 4 ~ "High (>=4*)",
      final_score_1to5 <= 2 ~ "Low (<=2*)",
      TRUE ~ "Mid (2–4*)"
    ), levels = c("Low (<=2*)", "Mid (2–4*)", "High (>=4*)"))
  )


# ── Radar globals (Rmd rescale_metrics logic) ─────────────────────────────────
global_minmax <- repo %>%
  summarise(
    imin = min(backlog_health, na.rm = TRUE),
    imax = max(backlog_health, na.rm = TRUE),
    pmin = min(popularity, na.rm = TRUE),
    pmax = max(popularity, na.rm = TRUE)
  )
global_max <- c(100, 100, 100)
global_min <- c(0,   0,   0)

rescale_metrics <- function(df) {
  df %>% mutate(
    backlog_health = scales::rescale(backlog_health, to = c(0,100),
                                     from = c(global_minmax$imin, global_minmax$imax)),
    popularity     = scales::rescale(popularity,     to = c(0,100),
                                     from = c(global_minmax$pmin, global_minmax$pmax))
  )
}

top100_avg <- repo %>%
  group_by(type) %>%
  arrange(desc(final_score_1to5)) %>%
  slice_head(n = 100) %>%
  summarise(recency = mean(recency_score, na.rm=TRUE),
            backlog_health = mean(backlog_health, na.rm=TRUE),
            popularity = mean(popularity, na.rm=TRUE), .groups="drop") %>%
  rescale_metrics()

bottom100_avg <- repo %>%
  group_by(type) %>%
  arrange(final_score_1to5) %>%
  slice_head(n = 100) %>%
  summarise(recency = mean(recency_score, na.rm=TRUE),
            backlog_health = mean(backlog_health, na.rm=TRUE),
            popularity = mean(popularity, na.rm=TRUE), .groups="drop") %>%
  rescale_metrics()

median100_avg <- repo %>%
  group_by(type) %>%
  arrange(final_score_1to5) %>%
  slice(round(n()/2 - 49) : round(n()/2 + 50)) %>%
  summarise(recency = mean(recency_score, na.rm=TRUE),
            backlog_health = mean(backlog_health, na.rm=TRUE),
            popularity = mean(popularity, na.rm=TRUE), .groups="drop") %>%
  rescale_metrics()

# ── Radar: by-domain & top20/bottom20-by-domain (Rmd Figs S10 / S11) ──────────
all_by_type_radar <- repo %>%
  group_by(type) %>%
  summarise(recency = mean(recency_score, na.rm=TRUE),
            backlog_health = mean(backlog_health, na.rm=TRUE),
            popularity = mean(popularity, na.rm=TRUE), .groups="drop") %>%
  rescale_metrics()

top20_by_type_radar <- repo %>%
  group_by(type) %>%
  arrange(desc(final_score_1to5)) %>%
  slice_head(n = 20) %>%
  summarise(recency = mean(recency_score, na.rm=TRUE),
            backlog_health = mean(backlog_health, na.rm=TRUE),
            popularity = mean(popularity, na.rm=TRUE), .groups="drop") %>%
  rescale_metrics()

bottom20_by_type_radar <- repo %>%
  group_by(type) %>%
  arrange(final_score_1to5) %>%
  slice_head(n = 20) %>%
  summarise(recency = mean(recency_score, na.rm=TRUE),
            backlog_health = mean(backlog_health, na.rm=TRUE),
            popularity = mean(popularity, na.rm=TRUE), .groups="drop") %>%
  rescale_metrics()

# Repo-wide versions (used when Repository Type = "All")
radar_avg <- function(d) {
  d %>% summarise(recency        = mean(recency_score,  na.rm=TRUE),
                  backlog_health = mean(backlog_health, na.rm=TRUE),
                  popularity     = mean(popularity,     na.rm=TRUE)) %>%
    rescale_metrics()
}
all_radar      <- radar_avg(repo)
top20_all      <- repo %>% arrange(desc(final_score_1to5)) %>% slice_head(n = 20) %>% radar_avg()
bottom20_all   <- repo %>% arrange(final_score_1to5)       %>% slice_head(n = 20) %>% radar_avg()

require_two_groups <- function(g, label = "group") {
  validate(need(length(g) > 0,
                paste0("No rows matched for this ", label, " comparison — check that the ",
                       "filter (e.g. domain/type spelling) matches the data exactly.")))
  g <- droplevels(factor(g))
  validate(need(nlevels(g) == 2,
                paste0("Only ", nlevels(g), " ", label, " categor",
                       if (nlevels(g) == 1) "y is" else "ies are",
                       " present in this subset, so no comparison can be made.")))
  g
}


# ── ANOVA / Tukey ─────────────────────────────────────────────────────────────
anova_result <- aov(final_score_1to5 ~ type, data = repo)
Tukey_result <- TukeyHSD(anova_result)

tukey_df <- as.data.frame(Tukey_result$type)
tukey_df$pair <- rownames(tukey_df)

tukey_sig <- tukey_df %>%
  filter(`p adj` < 0.05) %>%
  separate(pair, into = c("group1", "group2"), sep = "-")

repo_summary <- repo %>%
  group_by(type) %>%
  summarise(mean = mean(final_score_1to5, na.rm=TRUE),
            sd   = sd(final_score_1to5, na.rm=TRUE),
            n    = n(),
            se   = sd / sqrt(n),
            ci_lower = mean - qt(0.975, df=n-1)*se,
            ci_upper = mean + qt(0.975, df=n-1)*se)


# ── Within-type CI/test t-tests ───────────────────────────────────────────────
within_type_tests <- repo %>%
  group_by(type) %>%
  summarise(
    ci_p = ifelse(length(unique(ci_present)) > 1 &
                    sum(ci_present==0,na.rm=TRUE)>=2 &
                    sum(ci_present==1,na.rm=TRUE)>=2,
                  t.test(final_score_1to5 ~ ci_present)$p.value, NA),
    tests_p = ifelse(length(unique(tests_present)) > 1 &
                       sum(tests_present==0,na.rm=TRUE)>=2 &
                       sum(tests_present==1,na.rm=TRUE)>=2,
                     t.test(final_score_1to5 ~ tests_present)$p.value, NA),
    mean_ci_yes   = mean(final_score_1to5[ci_present==1],    na.rm=TRUE),
    mean_ci_no    = mean(final_score_1to5[ci_present==0],    na.rm=TRUE),
    mean_tests_yes = mean(final_score_1to5[tests_present==1], na.rm=TRUE),
    mean_tests_no  = mean(final_score_1to5[tests_present==0], na.rm=TRUE),
    .groups = "drop"
  )


# ── Language processing ───────────────────────────────────────────────────────
lang_long <- repo %>%
  select(repo, type, final_score_1to5, tests_present, ci_present, languages) %>%
  filter(!is.na(languages), languages != "") %>%
  mutate(languages = str_replace_all(languages, "\\s+", " ")) %>%
  separate_rows(languages, sep = ",\\s*") %>%
  mutate(
    language = str_trim(str_extract(languages, "^[^\\(]+")),
    bytes    = as.numeric(str_extract(languages, "(?<=\\()[0-9]+"))
  ) %>%
  filter(!is.na(language), language != "", !is.na(bytes))

lang_primary <- lang_long %>%
  group_by(repo) %>%
  slice_max(order_by = bytes, n = 1, with_ties = FALSE) %>%
  ungroup()

min_n <- 20
lang_summary_filt <- lang_primary %>%
  group_by(language) %>%
  summarise(n = n(), mean_rating = mean(final_score_1to5, na.rm=TRUE), .groups="drop") %>%
  filter(n >= min_n)

# Bioinformatics-only language summary (Rmd Figs S15–S17 use a lower n threshold)
min_n_bio <- 10
bio_lang_summary_filt <- lang_primary %>%
  filter(type == "bioinformatics") %>%
  group_by(language) %>%
  summarise(n = n(), mean_rating = mean(final_score_1to5, na.rm=TRUE), .groups="drop") %>%
  filter(n >= min_n_bio)


# ── Cohort summaries ──────────────────────────────────────────────────────────
cohort_ci_summary <- repo %>%
  group_by(age_cohort, ci_present) %>%
  summarise(mean_z = mean(final_score_1to5,na.rm=TRUE),
            se_z   = sd(final_score_1to5,na.rm=TRUE)/sqrt(n()),
            n=n(), .groups="drop") %>%
  mutate(ci_label = ifelse(ci_present==1,"Has CI","No CI"))

cohort_test_summary <- repo %>%
  group_by(age_cohort, tests_present) %>%
  summarise(mean_z = mean(final_score_1to5,na.rm=TRUE),
            se_z   = sd(final_score_1to5,na.rm=TRUE)/sqrt(n()),
            n=n(), .groups="drop") %>%
  mutate(test_label = ifelse(tests_present==1,"Has Tests","No Tests"))


# ── Summary tables (Rmd Table 1 / Table S1) ───────────────────────────────────
summary_tbl_type <- repo %>%
  group_by(type) %>%
  summarise(mean_rqi        = mean(final_score_1to5, na.rm = TRUE),
            mean_recency    = mean(recency_score,     na.rm = TRUE),
            mean_backlog    = mean(backlog_health,    na.rm = TRUE),
            mean_popularity = mean(popularity,        na.rm = TRUE),
            n               = n(),
            .groups = "drop") %>%
  arrange(desc(mean_rqi))

top_languages_tbl <- lang_long %>% count(language, sort = TRUE) %>% head(15)


# ── Shared colour palette ─────────────────────────────────────────────────────
type_colors <- c(
  "bioinformatics"     = "#4C72B0",
  "astrophysics"       = "#DD8452",
  "image_recognition"  = "#937860",
  "open_source"        = "#64B5CD",
  "software_engineering" = "#9467BD"
)

unique_type <- unique(repo$type)


# ── Citation data (loaded if file exists; mirrors Rmd's publication logic) ────
cit_file <- "repos_with_citations_gemma_v2.csv"
has_citations <- file.exists(cit_file)
if (has_citations) {
  citations_raw <- read_csv(cit_file, show_col_types = FALSE) %>%
    distinct(repo, .keep_all = TRUE) %>%
    # Drop any RQI columns already in this file (e.g. stale z-score-based
    # values from before the switch to anchor-point interpolation) and join
    # in the current scores from `repo`, keyed on repo.
    select(-any_of(c("final_score_1to5", "recency_score_1to5",
                     "issue_score_1to5", "pop_score_1to5",
                     "z_normalized_1to5", "recency_z", "issue_z", "pop_z"))) %>%
    left_join(
      repo %>% select(repo, final_score_1to5, recency_score_1to5,
                      issue_score_1to5, pop_score_1to5),
      by = "repo"
    ) %>%
    mutate(
      
      doi_is_paper = !is.na(doi) &
        str_detect(doi, "^10\\.") &
        !str_detect(doi, "^10\\.(5281|6084|17605|32614|5438)/"),
      published = (citation_verified %in% TRUE) | doi_is_paper,
      pub_label = factor(if_else(published, "Published", "Not Published"),
                         levels = c("Not Published", "Published"))
    )
}

# ── Funding data (loaded if file exists; mirrors Rmd's currency conversion) ───
nih_file   <- "oa_publication_summary_gemma.csv"
has_funding <- file.exists(nih_file)
if (has_funding) {
  nih <- read_csv(nih_file, show_col_types = FALSE)
  
  usd_per_unit <- c(
    USD = 1.00, GBP = 1.34, EUR = 1.16, CHF = 1.25, AUD = 0.67,
    CAD = 0.72, CNY = 0.14, HKD = 0.128, SEK = 0.108, JPY = 0.0063, CLP = 0.00105
  )
  
  convert_cost_string <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    total <- 0
    matched_any <- FALSE
    for (part in strsplit(x, ";\\s*")[[1]]) {
      kv <- strsplit(part, ":")[[1]]
      if (length(kv) != 2) next
      cur  <- trimws(kv[1])
      amt  <- suppressWarnings(as.numeric(kv[2]))
      rate <- usd_per_unit[cur]
      if (!is.na(amt) && !is.na(rate)) {
        total <- total + amt * rate
        matched_any <- TRUE
      }
    }
    if (matched_any) total else NA_real_
  }
  
  nih <- nih %>%
    mutate(
      developer_cost_usd = sapply(developer_cost_by_currency, convert_cost_string),
      total_cost_usd     = sapply(total_cost_by_currency,     convert_cost_string)
    )
  
  funding_df <- nih %>%
    left_join(repo %>% select(repo, final_score_1to5, recency_score_1to5,
                              issue_score_1to5, pop_score_1to5, type),
              by = "repo") %>%
    mutate(
      funding_group = factor(
        ifelse(is.na(funding_sources) | funding_sources == "",
               "Not Grant Funded", "Grant Funded"),
        levels = c("Not Grant Funded", "Grant Funded")
      )
    )
  
  funded_sources_list <- funding_df %>%
    filter(!is.na(funding_sources) & funding_sources != "") %>%
    pull(funding_sources) %>%
    strsplit("; ") %>% unlist() %>% trimws() %>% unique() %>% sort()
}



SCREEN_K <- 1.3
SK       <- SCREEN_K
APP_RES  <- 96
h_px     <- function(rmd_height_in) round(rmd_height_in * SCREEN_K * APP_RES)  # Rmd fig.height (in) -> px

if (requireNamespace("ragg", quietly = TRUE)) options(shiny.useragg = TRUE)

FIG_FONT   <- "Helvetica"
PT         <- list(base = 10 * SK, value = 12 * SK)
pt2mm      <- function(pt) pt / ggplot2::.pt
LW_MIN     <- 0.5 * SK
LWD_MIN    <- 1.4 * SK
STROKE_MIN <- 0.8 * SK
TXT_S      <- pt2mm(8 * SK)       # dense labels
TXT_M      <- pt2mm(PT$base)      # labels on main-figure-style plots
deep_blue  <- "#08519C"
comp_colors <- c(Recency = "#E31A1C", Activity = "#33A02C", Popularity = "#FF7F00")
TYPE_COLORS <- type_colors
rqi_pal    <- grDevices::colorRampPalette(c("#D73027", "#FC8D59", "#FEE08B", "#91CF60", "#1A9850"))

try(ggplot2::update_geom_defaults("text", list(family = FIG_FONT)), silent = TRUE)

W          <- function(x, width = 60) stringr::str_wrap(x, width)
type_label <- function(x) stringr::str_to_sentence(gsub("_", " ", x))
p_label_num <- function(p) if (p < 0.001) "p<0.001" else sprintf("p=%.3f", p)

theme_sci <- function() {
  theme_minimal(base_size = PT$base, base_family = FIG_FONT, base_line_size = LW_MIN) +
    theme(
      text             = element_text(size = PT$base, family = FIG_FONT),
      axis.text        = element_text(size = PT$base, colour = "grey15"),
      axis.title       = element_text(size = PT$base),
      legend.text      = element_text(size = PT$base),
      legend.title     = element_text(size = PT$base),
      plot.title       = element_text(size = PT$base, face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(size = PT$base, colour = "grey30", hjust = 0.5),
      plot.tag         = element_text(size = PT$base, face = "bold"),
      strip.text       = element_text(size = PT$base, face = "bold"),
      legend.position  = "bottom"
    )
}

theme_supp <- function() {
  theme_sci() +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(size = PT$base, face = "bold", hjust = 0.5,
                                         margin = margin(b = 8 * SK)),
      panel.spacing       = unit(1.4, "lines"),
      plot.margin         = margin(10 * SK, 10 * SK, 6 * SK, 6 * SK)
    )
}


sig_bracket <- function(x, xend, y, label, dy = 0, tick = NULL, size = TXT_S,
                        lw = LW_MIN, fontface = "plain", vjust = 0.5) {
  d <- data.frame(x = x, xend = xend, y = y, label = label, stringsAsFactors = FALSE)
  out <- list(geom_segment(data = d, aes(x = x, xend = xend, y = y, yend = y),
                           inherit.aes = FALSE, linewidth = lw))
  if (!is.null(tick)) {
    out <- c(out, list(
      geom_segment(data = d, aes(x = x, xend = x, y = y - tick, yend = y),
                   inherit.aes = FALSE, linewidth = lw),
      geom_segment(data = d, aes(x = xend, xend = xend, y = y - tick, yend = y),
                   inherit.aes = FALSE, linewidth = lw)))
  }
  c(out, list(geom_text(data = d, aes(x = (x + xend) / 2, y = y + dy, label = label),
                        inherit.aes = FALSE, size = size, fontface = fontface, vjust = vjust)))
}

# All-pairs Wilcoxon tests between the levels of `group_col`; returns the
# significant pairs (p < alpha) with p-value labels, ready for bracket_stack().
build_sig_brackets <- function(data, group_col, value_col, alpha = 0.05) {
  groups <- levels(droplevels(factor(data[[group_col]])))
  pos    <- setNames(seq_along(groups), groups)
  out    <- list()
  if (length(groups) >= 2) {
    for (i in 1:(length(groups) - 1)) {
      for (j in (i + 1):length(groups)) {
        a <- groups[i]; b <- groups[j]
        sub <- data[data[[group_col]] %in% c(a, b), ]
        sub[[group_col]] <- droplevels(factor(sub[[group_col]]))
        if (nlevels(sub[[group_col]]) < 2) next
        form <- stats::as.formula(paste(value_col, "~", group_col))
        p <- tryCatch(wilcox.test(form, data = sub)$p.value, error = function(e) NA_real_)
        if (is.na(p) || p >= alpha) next
        out[[length(out) + 1]] <- data.frame(g1 = a, g2 = b,
                                             x = unname(pos[a]), xend = unname(pos[b]),
                                             p_value = p, label = p_label_num(p),
                                             stringsAsFactors = FALSE)
      }
    }
  }
  if (length(out) == 0) {
    return(data.frame(g1 = character(), g2 = character(), x = numeric(), xend = numeric(),
                      p_value = numeric(), label = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# Stack brackets one above the other, clear of the tallest bar (and its value
# label), and return the y-limit that leaves room for the top label.
#   step_frac: bracket spacing as a fraction of the tallest bar
#   offset:    1 = leave one extra step above the bars for their value labels
bracket_stack <- function(pairs, max_bar, step_frac = 0.20, offset = 1) {
  if (!is.finite(max_bar) || max_bar <= 0) max_bar <- 1
  step <- max_bar * step_frac
  n    <- nrow(pairs)
  if (n > 0) pairs$y <- max_bar + step * (seq_len(n) + offset)   # Rmd: Fig 5 offset 0, S26/S27 offset 1
  list(pairs = pairs, step = step, ymax = max_bar + step * (n + offset + 2))
}

bracket_layers <- function(pairs, step, size = TXT_M, lw = LW_MIN) {
  if (is.null(pairs) || nrow(pairs) == 0) return(list())
  sig_bracket(pairs$x, pairs$xend, pairs$y, pairs$label,
              dy = step * 0.35, tick = step * 0.15, size = size, lw = lw)
}

# ── Layered RQI + component violins (Fig 4) ───────────────────────────────────
layered_violin <- function(base_data, group, title, subtitle = "Components scaled 0\u20135") {
  dat <- base_data %>% mutate(grp = .data[[group]])
  
  comp_long <- dat %>%
    select(grp, recency_1to5, issue_1to5, pop_1to5) %>%
    pivot_longer(-grp, names_to = "component", values_to = "score") %>%
    mutate(component = recode(component,
                              recency_1to5 = "Recency",
                              issue_1to5   = "Activity",
                              pop_1to5     = "Popularity"))
  
  comp_stats <- comp_long %>%
    group_by(grp, component) %>%
    summarise(med = median(score, na.rm = TRUE), .groups = "drop")
  
  rqi_stats <- dat %>%
    group_by(grp) %>%
    summarise(med = median(final_score_1to5, na.rm = TRUE), n = n(), .groups = "drop")
  
  wilcox_res <- wilcox.test(final_score_1to5 ~ grp, data = dat, exact = FALSE)
  p_label <- ifelse(wilcox_res$p.value < 0.0001, "p < 0.0001",
                    paste0("p = ", round(wilcox_res$p.value, 4)))
  
  fill_pal    <- setNames(rep(deep_blue, nlevels(dat$grp)), levels(dat$grp))
  fill_values <- c(fill_pal, "RQI" = deep_blue, comp_colors)
  txt     <- pt2mm(PT$base)
  txt_val <- pt2mm(PT$value)
  txt_rqi <- pt2mm(PT$value + 2 * SK)
  
  # Stack the component-median labels so they cannot overlap each other
  spread_labels <- function(y, gap = 0.36, ymax = 5.15) {
    o <- order(y); ys <- y[o]
    for (i in seq_along(ys)[-1]) if (ys[i] - ys[i - 1] < gap) ys[i] <- ys[i - 1] + gap
    ys <- ys + (mean(y) - mean(ys))
    if (max(ys) > ymax) ys <- ys - (max(ys) - ymax)
    y[o] <- ys
    y
  }
  comp_stats <- comp_stats %>%
    group_by(grp) %>% mutate(ylab = spread_labels(med)) %>% ungroup()
  
  p <- ggplot() +
    geom_violin(data = dat, aes(x = grp, y = final_score_1to5, fill = grp),
                trim = TRUE, width = 0.75, alpha = 0.20, linewidth = LW_MIN, color = "grey60") +
    geom_violin(data = comp_long %>% filter(component == "Popularity"), aes(x = grp, y = score),
                fill = comp_colors[["Popularity"]], color = NA, trim = TRUE, width = 0.55, alpha = 0.55) +
    geom_violin(data = comp_long %>% filter(component == "Activity"), aes(x = grp, y = score),
                fill = comp_colors[["Activity"]], color = NA, trim = TRUE, width = 0.40, alpha = 0.65) +
    geom_violin(data = comp_long %>% filter(component == "Recency"), aes(x = grp, y = score),
                fill = comp_colors[["Recency"]], color = NA, trim = TRUE, width = 0.25, alpha = 0.75) +
    geom_point(data = comp_stats, aes(x = grp, y = med, fill = component),
               shape = 23, size = 3 * SK, color = "black", stroke = STROKE_MIN) +
    geom_text(data = comp_stats, aes(x = grp, y = ylab, label = sprintf("%.2f", med)),
              color = "black", hjust = -0.55, fontface = "bold", size = txt_val,
              inherit.aes = FALSE, show.legend = FALSE) +
    geom_text(data = rqi_stats, aes(x = grp, y = 5.95, label = paste0("n=", n)),
              vjust = 0.5, fontface = "bold", size = txt, color = "grey30", inherit.aes = FALSE) +
    geom_point(data = rqi_stats, aes(x = grp, y = med, fill = "RQI"),
               shape = 21, size = 4 * SK, color = "black", stroke = STROKE_MIN) +
    geom_text(data = rqi_stats, aes(x = grp, y = med, label = sprintf("%.2f", med)),
              hjust = 1.35, fontface = "bold", size = txt_rqi, color = "black",
              inherit.aes = FALSE, show.legend = FALSE) +
    annotate("text", x = 1.5, y = 5.55, label = paste("Wilcoxon (RQI)", p_label),
             hjust = 0.5, fontface = "italic", size = txt, color = "grey30") +
    scale_fill_manual(values = fill_values,
                      breaks = c("RQI", "Recency", "Activity", "Popularity"), name = NULL) +
    scale_y_continuous(limits = c(0, 6.25), breaks = 0:5, expand = expansion(mult = c(0.02, 0.02))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "RQI Score (0\u20135)") +
    theme_sci() +
    theme(panel.grid.major.x = element_blank(), legend.title = element_blank()) +
    guides(fill = guide_legend(override.aes = list(shape = c(21, 22, 22, 22), size = 3.5 * SK,
                                                   color = "black", stroke = STROKE_MIN)))
  
  list(plot = p, wilcox = wilcox_res)
}

# ── Radar panels (Fig 1 / S9–S11 style; base graphics) ────────────────────────
radar_axis_labels <- c("Recent\nactivity", "Backlog\nhealth", "Popularity")
LINE_IN <- PT$base * 1.2 / 72

make_radar_df <- function(vals) {
  df <- as.data.frame(rbind(global_max, global_min,
                            as.numeric(unlist(vals[1, c("recency", "backlog_health", "popularity")],
                                              use.names = FALSE))))
  colnames(df) <- radar_axis_labels
  df
}

draw_spoke_labels <- function() {
  L  <- lapply(radar_axis_labels, function(x) strsplit(x, "\n")[[1]])
  dy <- yinch(LINE_IN)
  n1 <- length(L[[1]])
  for (k in seq_len(n1))                                   # top vertex: labels stack upward
    text(0, 1 + yinch(0.24 * SK) + (n1 - k) * dy, L[[1]][k], cex = 1, xpd = NA)
  for (k in seq_along(L[[2]]))                             # lower vertices: labels stack downward
    text(-0.866, -0.5 - yinch(0.20 * SK) - (k - 1) * dy, L[[2]][k], cex = 1, xpd = NA)
  for (k in seq_along(L[[3]]))
    text( 0.866, -0.5 - yinch(0.20 * SK) - (k - 1) * dy, L[[3]][k], cex = 1, xpd = NA)
  invisible(list(n_top = n1, n_low = max(length(L[[2]]), length(L[[3]]))))
}

draw_radar_panel <- function(vals, color, main_lab, sub_lab) {
  if (is.null(vals) || nrow(vals) == 0) {
    plot.new()
    text(0.5, 0.5, paste0(main_lab, "\n(insufficient data)"), col = "gray50", cex = 1)
    return(invisible(NULL))
  }
  fmsb::radarchart(make_radar_df(vals),
                   axistype = 0, vlabels = rep("", 3),
                   pcol = color, pfcol = scales::alpha(color, 0.35),
                   plwd = 2 * SK, cglcol = "gray55", cglty = 1, cglwd = LWD_MIN,
                   vlcex = 1, title = "")
  
  # Ring values, with a white halo so they stay readable over the grid lines
  num   <- c("0", "25", "50", "75", "100")
  num_y <- (0:4) / 4
  for (ang in seq(0, 2 * pi, length.out = 9)[-9])
    text(-0.05 + xinch(0.012 * SK) * cos(ang), num_y + yinch(0.012 * SK) * sin(ang), num,
         col = "white", cex = 1, font = 2)
  text(-0.05, num_y, num, col = "black", cex = 1, font = 2)
  
  n  <- draw_spoke_labels()
  dy <- yinch(LINE_IN)
  
  y_low_last <- -0.5 - yinch(0.20 * SK) - (n$n_low - 1) * dy
  y_title    <- y_low_last - yinch(0.23 * SK)
  text(0, y_title, main_lab, cex = 1, font = 2, xpd = NA)
  text(0, y_title - yinch(0.19 * SK), sub_lab, cex = 1, col = color, font = 3, xpd = NA)
}

radar_par <- function(mfrow, mar, oma_top = 1.8) {
  par(mfrow = mfrow, ps = PT$base, cex = 1, family = FIG_FONT,
      mar = mar, oma = c(0, 0, oma_top, 0), bg = "white", xpd = NA)
}

radar_title <- function(txt) {
  mtext(txt, side = 3, outer = TRUE, line = 0.4, cex = 1, font = 2)
}


# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  title = "Continuous Integration and Software Quality in Scientific Software",
  theme = shinytheme("yeti"),
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "preconnect", href = "https://use.typekit.net"),
    tags$link(rel = "preconnect", href = "https://p.typekit.net", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", href = "https://use.typekit.net/mae4nta.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400;0,600;0,700;1,400&family=Montserrat:wght@400;500;600;700&display=swap"
    ),
    tags$style(HTML("
      :root {
        --az-red: #AB0520;
        --az-red-dark: #8B0015;
        --az-blue: #0C234B;
        --az-oasis: #378DBD;
        --az-azurite: #1E5288;
        --az-warmgray: #F4EDE5;
        --az-coolgray: #E2E9EB;
        /* Proxima Nova is a licensed commercial font, loaded here via the
           Adobe Fonts (Typekit) kit linked above (weights 100/400/600).
           Montserrat is the free fallback if that stylesheet fails to load. */
        --font-sans: \"proxima-nova\", 'Montserrat', -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        --font-serif: \"garamond-premier-pro\", 'EB Garamond', 'Times New Roman', serif;
      }

      body {
        margin: 0;
        min-height: 100vh;
        font-family: var(--font-sans);
        font-weight: 400;
        font-style: normal;
        background: var(--az-warmgray);
        color: var(--az-blue);
        /* Reserve space at the bottom so the fixed footer never covers content.
           This only has a visible effect because body no longer has a hard
           height:100% — it uses min-height instead, so real content height
           (plus this padding) can push the page taller than the viewport. */
        padding-bottom: 150px;
      }

      .container-fluid {
        margin-right: auto;
        margin-left: auto;
        padding-left: 15px;
        padding-right: 15px;
        padding-bottom: 40px;
      }

      /* ── Header (bleeds edge-to-edge out of the Bootstrap container) ── */
      .site-header {
        background: #fff;
        border-bottom: 4px solid var(--az-red);
        padding: 18px 40px;
        margin: -20px -15px 24px -15px;
        font-family: var(--font-sans);
        font-weight: 400;
        font-style: normal;
      }
      .site-header h1 {
        font-size: 26px;
        line-height: 1.35;
        margin: 0 0 6px;
        font-weight: 600;
        color: var(--az-blue);
      }
      .site-header .subtitle { font-size: 17px; color: #555; }
      .site-header .subtitle a { color: #555; text-decoration: underline; }
      .site-header .subtitle a:hover { color: var(--az-red); }
      .site-header .subtitle .sep { color: #aaa; }

      /* ── Footer (hovers, fixed to the bottom of the viewport) ── */
      .site-footer {
        position: fixed;
        left: 0;
        right: 0;
        bottom: 0;
        width: 100%;
        box-sizing: border-box;
        background: var(--az-blue);
        color: #dfe6ee;
        padding: 20px 40px;
        margin: 0;
        box-shadow: 0 -2px 10px rgba(12, 35, 75, 0.15);
        display: flex;
        justify-content: space-between;
        align-items: center;
        flex-wrap: wrap;
        gap: 12px;
        font-size: 1.2rem;
        font-family: var(--font-sans);
        font-weight: 400;
        font-style: normal;
        z-index: 1000;
      }
      .site-footer a { color: #fff; text-decoration: none; }
      .site-footer a:hover { text-decoration: underline; }
      .site-footer .footer-left { font-weight: 600; }
      .site-footer .footer-links a { margin-left: 28px; }

      /* ── Sidebar ── */
      .well {
        background: #fff;
        border: 1px solid var(--az-coolgray);
        border-radius: 12px;
        box-shadow: 0 2px 10px rgba(12, 35, 75, 0.08);
      }

      /* ── Plot cards ──
         plotOutput() calls are wrapped in tags$div(class = 'plot-card', ...)
         so this padding lives OUTSIDE the Shiny-managed plot container.
         Padding directly on .shiny-plot-output would shrink the box Shiny
         measures for image sizing without shrinking the rendered plot
         itself, causing the plot to spill past the card edges. */
      .plot-card {
        background: #fff;
        border-radius: 12px;
        box-shadow: 0 2px 10px rgba(12, 35, 75, 0.08);
        padding: 16px;
        margin-bottom: 22px;
        overflow: hidden;
      }

      /* ── Tabs ──
         Every tab keeps the same font-weight and a transparent 3px top
         border at all times, so switching tabs never changes their width
         or the tab bar's height — only color changes on the active tab. */
      .nav-tabs { border-bottom: 2px solid var(--az-coolgray); }
      .nav-tabs > li > a {
        color: var(--az-blue);
        font-weight: 600;
        border-top: 3px solid transparent;
        border-radius: 8px 8px 0 0;
      }
      .nav-tabs > li > a:hover {
        background: var(--az-warmgray);
        border-top-color: transparent;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: var(--az-red);
        background: #fff;
        border-color: var(--az-coolgray) var(--az-coolgray) #fff;
        border-top: 3px solid var(--az-red);
      }

      /* ── Buttons & inputs ── */
      .btn-default, .btn-primary {
        background: var(--az-red);
        border-color: var(--az-red);
        color: #fff;
      }
      .btn-default:hover, .btn-primary:hover,
      .btn-default:focus, .btn-primary:focus {
        background: var(--az-red-dark);
        border-color: var(--az-red-dark);
        color: #fff;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: var(--az-oasis);
        box-shadow: 0 0 0 3px rgba(55, 141, 189, 0.25);
      }

      h4 { color: var(--az-red); font-weight: 600; }
      a { color: var(--az-azurite); }
    "))
  ),
  
  tags$div(class = "site-header",
           tags$h1("Continuous Integration and Software Quality in Scientific Software: A Large-Scale Empirical Analysis of GitHub Repositories"),
           tags$div(class = "subtitle",
                    tags$a(href = "https://doi.org/10.XXXX/XXXXXXX", target = "_blank", "DOI: 10.XXXX/XXXXXXX"),
                    tags$span(" \u2003|\u2003 ", class = "sep"),
                    tags$a(href = "https://paper-link.example.com", target = "_blank",
                           icon("file-text"), "Read the paper"),
                    tags$span(" \u2003|\u2003 ", class = "sep"),
                    tags$a(href = "https://jakobilab.org", target = "_blank",
                           icon("globe"), "jakobilab.org")
           )
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 # Repo-type filter (most tabs)
                 conditionalPanel(
                   condition = paste0("['distribution_tab','coverage_tab','impact_tab',",
                                      "'impact_by_type_tab','radar_tab','cohort_tab',",
                                      "'lang_tab'].indexOf(input.main_tabs) >= 0"),
                   selectInput("repo_type", "Repository Type",
                               choices = c("All", unique_type), selected = "All")
                 ),
                 
                 # Metric filter (coverage tab)
                 conditionalPanel(
                   condition = "input.main_tabs == 'coverage_tab'",
                   selectInput("test_type", "Metric",
                               choices = c("All", "ci_coverage", "test_coverage"), selected = "All")
                 ),
                 
                 # Pair selector (Tukey comparison tab)
                 conditionalPanel(
                   condition = "input.main_tabs == 'comparison_tab'",
                   selectInput("repo_1", "First Repo Type",  choices = unique_type),
                   selectInput("repo_2", "Second Repo Type", choices = unique_type, selected = unique_type[2])
                 ),
                 
                 # Radar view selector
                 conditionalPanel(
                   condition = "input.main_tabs == 'radar_tab'",
                   selectInput("radar_view", "Radar View",
                               choices = c("Top / Mid / Bottom 100 (Fig 1)" = "tmb",
                                           "Average Profile by Domain"      = "by_domain",
                                           "Top 20 vs Bottom 20 by Domain"  = "top_bottom_20"),
                               selected = "tmb"),
                 ),
                 
                 # Language filters
                 conditionalPanel(
                   condition = "input.main_tabs == 'lang_tab'",
                   selectInput("lang_scope", "Domain Scope",
                               choices = c("All Domains" = "all", "Bioinformatics Only" = "bio"),
                               selected = "all"),
                   selectInput("lang_split", "Split by",
                               choices = c("None" = "none", "Tests" = "tests", "CI" = "ci", "QA Bucket" = "qa"),
                               selected = "tests")
                 ),
                 
                 # Citation sub-tab
                 conditionalPanel(
                   condition = "input.main_tabs == 'citation_tab'",
                   selectInput("cit_plot", "Citation Plot",
                               choices = c("Mean vs RQI (loess)"                    = "loess",
                                           "Median vs RQI (loess)"                  = "median_loess",
                                           "Violin by RQI bin"                      = "violin",
                                           "Mean bar by RQI bin"                    = "mean_bar",
                                           "Median bar (Low/High)"                  = "bar_lh",
                                           "Median bar (RQI coarse bins)"           = "bar_bins",
                                           "Median bar (RQI fine bins)"             = "bar_bins_fine",
                                           "Median bar, published only (Fig 5)"     = "fig6",
                                           "Citations vs CI"                        = "bar_ci",
                                           "Median RQI by publication status"       = "pub_bar",
                                           "RQI distribution by publication status" = "pub_violin",
                                           "RQI components by publication (Fig 4a)" = "fig5a",
                                           "Citations vs RQI + Age"                 = "age_scatter",
                                           "Scatter + LM (published)"               = "scatter_lm"),
                               selected = "loess")
                 ),
                 
                 # Funding sub-tab
                 conditionalPanel(
                   condition = "input.main_tabs == 'funding_tab'",
                   selectInput("fund_plot", "Funding Plot",
                               choices = c("Median RQI by Funding (Bioinformatics)"              = "bar_bio",
                                           "RQI Components by Funding, Fig 4b (Bioinformatics)"  = "fig5b_bio",
                                           "RQI vs Developer Award (Bioinformatics)"             = "lm_dev_bio",
                                           "RQI vs Organization Award (Bioinformatics)"          = "lm_org_bio",
                                           "RQI Components by Funding (All Domains)"             = "fig_all",
                                           "RQI vs Developer Award (All Domains)"                = "lm_dev_all",
                                           "RQI vs Organization Award (All Domains)"             = "lm_org_all"),
                               selected = "bar_bio")
                 )
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  
                  # 1 ── Coverage
                  tabPanel("CI & Test Coverage", value = "coverage_tab",
                           h4("CI and Testing Prevalence by Quality Tier"),
                           tags$div(class = "plot-card", plotOutput("test_plot", height = paste0(h_px(4.2), "px"))),
                           hr(),
                           h4("Prevalence Regression Across RQI (Fig 2)"),
                           tags$div(class = "plot-card", plotOutput("prev_reg_plot", height = paste0(h_px(4.8), "px")))
                  ),
                  
                  # 2 ── Distribution
                  tabPanel("Distribution", value = "distribution_tab",
                           h4("Quality Distribution by Type (stacked)"),
                           tags$div(class = "plot-card", plotOutput("dist_plot", height = paste0(h_px(4.8), "px"))),
                           hr(),
                           h4("Overall Distribution + Q-Q"),
                           tags$div(class = "plot-card", plotOutput("dist_qq_plot", height = paste0(h_px(3.8), "px"))),
                           hr(),
                           h4("Density by Type (faceted)"),
                           tags$div(class = "plot-card", plotOutput("dist_facet_plot", height = paste0(h_px(5.5), "px")))
                  ),
                  
                  # 3 ── ANOVA / Tukey overall
                  tabPanel("ANOVA / Tukey", value = "anova_tab",
                           h4("Bioinformatics vs All — Tukey Significant Pairs"),
                           tags$div(class = "plot-card", plotOutput("bio_tukey_plot", height = paste0(h_px(5), "px")))
                  ),
                  
                  # 4 ── Tukey pairwise comparison
                  tabPanel("Pairwise Comparison", value = "comparison_tab",
                           h4("Tukey HSD Pairwise Comparison"),
                           tags$div(class = "plot-card", plotOutput("tukey_plot", height = paste0(h_px(4.2), "px")))
                  ),
                  
                  # 5 ── CI & Test impact (overall)
                  tabPanel("CI & Test Impact", value = "impact_tab",
                           fluidRow(
                             column(6, h4("CI Impact"), tags$div(class = "plot-card", plotOutput("ci_impact_plot", height = paste0(h_px(4), "px")))),
                             column(6, h4("Test Impact"), tags$div(class = "plot-card", plotOutput("test_impact_plot", height = paste0(h_px(4), "px"))))
                           )
                  ),
                  
                  
                  # 7 ── Radar
                  tabPanel("Radar", value = "radar_tab",
                           h4("Repository Quality Radar Profiles"),
                           uiOutput("radar_plot_ui")
                  ),
                  
                  # 8 ── Age cohort
                  tabPanel("Age Cohorts", value = "cohort_tab",
                           h4("Mean RQI by Age Cohort — CI"),
                           tags$div(class = "plot-card", plotOutput("cohort_ci_plot", height = paste0(h_px(4.2), "px"))),
                           hr(),
                           h4("Mean RQI by Age Cohort — Testing"),
                           tags$div(class = "plot-card", plotOutput("cohort_test_plot", height = paste0(h_px(4.2), "px"))),
                           hr(),
                           h4("Bioinformatics CI & Test Coverage by Cohort"),
                           tags$div(class = "plot-card", plotOutput("bio_cohort_plot", height = paste0(h_px(4.2), "px"))),
                           hr(),
                           h4("Combined View (Fig 3)"),
                           tags$div(class = "plot-card", plotOutput("cohort_combined_plot", height = paste0(h_px(4.5), "px")))
                  ),
                  
                  # 9 ── Language
                  tabPanel("Language", value = "lang_tab",
                           h4("Mean RQI by Primary Language"),
                           uiOutput("lang_plot_ui")
                  ),
                  
                  # 10 ── Activity (survival)
                  tabPanel("Activity / Survival", value = "survival_tab",
                           h4("Odds Ratios: Predictors of Repo Activity"),
                           tags$div(class = "plot-card", plotOutput("survival_plot", height = paste0(h_px(4.2), "px"))),
                           hr(),
                           h4("Activity Rate by CI/Test Presence and Domain"),
                           tags$div(class = "plot-card", plotOutput("activity_plot", height = paste0(h_px(5.5), "px")))
                  ),
                  
                  # 11 ── Citations (optional)
                  tabPanel("Citations", value = "citation_tab",
                           uiOutput("citation_ui")
                  ),
                  
                  # 12 ── Funding (optional)
                  tabPanel("Funding", value = "funding_tab",
                           uiOutput("funding_ui")
                  ),
                  
                  # 13 ── Summary tables
                  tabPanel("Summary Tables", value = "summary_tab",
                           h4("Table 1 — Mean Scores by Repository Type"),
                           tableOutput("table1_summary"),
                           hr(),
                           h4("Table S1 — Most Frequent Languages"),
                           tableOutput("tableS1_languages")
                  )
      )
    )
  ),
  
  tags$div(class = "site-footer",
           tags$div(class = "footer-left",
                    tags$a(href = "https://jakobilab.org/", target = "_blank", rel = "noopener", "Jakobi Lab")
           ),
           tags$div(class = "footer-links",
                    tags$a(href = "https://github.com/jakobilab/ci_shiny_reporter", target = "_blank", rel = "noopener", "Source code"),
                    tags$a(href = "https://github.com/jakobilab/integrationStudy", target = "_blank", rel = "noopener", "Continuous Integration Study 2026"),
                    tags$a(href = "https://phoenixmed.arizona.edu/tcrc", target = "_blank", rel = "noopener", "TCRC")
           )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output) {
  
  # ── Reactive filtered repo ─────────────────────────────────────────────────
  filtered_repo <- reactive({
    if (input$repo_type == "All") repo else filter(repo, type == input$repo_type)
  })
  
  # Card with a plot sized to match the Rmd figure's aspect ratio; `w` narrows
  # plots that are half/two-thirds page width in the Rmd.
  plot_card <- function(id, rmd_h, w = 1) {
    tags$div(class = "plot-card",
             tags$div(style = sprintf("width:%d%%;margin:0 auto;", round(w * 100)),
                      plotOutput(id, height = paste0(h_px(rmd_h), "px"))))
  }
  
  # ── 1a. Coverage bar (Fig S1) ───────────────────────────────────────────────
  output$test_plot <- renderPlot(res = APP_RES, {
    src <- if (input$repo_type == "All") coverage_data else filter(coverage_data, type == input$repo_type)
    fs <- src %>%
      group_by(quality_group) %>%
      summarise(ci_coverage   = mean(ci_present==1,    na.rm=TRUE),
                test_coverage = mean(tests_present==1, na.rm=TRUE),
                .groups="drop") %>%
      pivot_longer(c(ci_coverage,test_coverage), names_to="metric", values_to="coverage")
    if (input$test_type != "All") fs <- filter(fs, metric == input$test_type)
    fs <- fs %>%
      mutate(metric_lab = factor(recode(metric, ci_coverage="Continuous Integration",
                                        test_coverage="Testing"),
                                 levels=c("Continuous Integration","Testing")))
    
    ggplot(fs, aes(x=quality_group, y=coverage*100, fill=metric_lab)) +
      geom_col(position=position_dodge(width=0.6), width=0.6, color="white") +
      geom_text(aes(label=paste0(round(coverage*100,1),"%")),
                position=position_dodge(0.6), vjust=-0.5, size=TXT_S, fontface="bold") +
      scale_fill_manual(values=c("Continuous Integration"="#1F78B4","Testing"="#33A02C")) +
      scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
      labs(title=W("CI and Testing Prevalence by Quality Tier",58),
           x="Quality Group by RQI", y="Prevalence (%)", fill="Metric") +
      theme_supp() +
      theme(legend.position="bottom")
  })
  
  # ── 1b. Prevalence regression (Fig 2) ───────────────────────────────────────
  output$prev_reg_plot <- renderPlot(res = APP_RES, {
    df <- filtered_repo()
    prev_reg <- df %>%
      mutate(rqi_bin = round(final_score_1to5/0.15)*0.15) %>%
      group_by(rqi_bin) %>%
      summarise(ci_prev   = mean(ci_present==1,    na.rm=TRUE),
                test_prev = mean(tests_present==1, na.rm=TRUE),
                n=n(), .groups="drop") %>%
      filter(n >= 5)
    validate(need(nrow(prev_reg) >= 3,
                  "Too few RQI bins (n ≥ 5) in this subset to fit a regression."))
    
    lm_ci   <- lm(ci_prev   ~ rqi_bin, data=prev_reg)
    lm_test <- lm(test_prev ~ rqi_bin, data=prev_reg)
    p_txt <- function(m) {
      p <- summary(m)$coefficients[2,4]
      if (is.na(p)) "=NA" else if (p < 0.001) "<0.001" else sprintf("=%.3f", p)
    }
    subtitle_str <- sprintf(
      "CI: slope=%.2f, R²=%.2f, p%s\nTesting: slope=%.2f, R²=%.2f, p%s",
      coef(lm_ci)[2],   summary(lm_ci)$r.squared,   p_txt(lm_ci),
      coef(lm_test)[2], summary(lm_test)$r.squared, p_txt(lm_test)
    )
    
    prev_reg %>%
      pivot_longer(c(ci_prev,test_prev), names_to="metric", values_to="prevalence") %>%
      ggplot(aes(x=rqi_bin, y=prevalence*100, color=metric, fill=metric)) +
      geom_point(aes(size=n), alpha=0.5) +
      geom_smooth(method="lm", se=TRUE, alpha=0.15, linewidth=1.2*SK) +
      scale_color_manual(values=c("ci_prev"="#1F78B4","test_prev"="#33A02C"),
                         labels=c("Continuous integration","Testing")) +
      scale_fill_manual(values=c("ci_prev"="#1F78B4","test_prev"="#33A02C"),
                        labels=c("Continuous integration","Testing")) +
      scale_size_continuous(name="Repositories in bin", range=c(1.5,5)*SK,
                            breaks=scales::breaks_pretty(4)) +
      labs(title=W("Continuous integration (CI) and testing prevalence across repository quality",90),
           subtitle=subtitle_str,
           x="Repository Quality Index (0–5)", y="Prevalence (%)",
           color="Metric", fill="Metric") +
      theme_sci() +
      guides(color=guide_legend(order=1, override.aes=list(size=3*SK)),
             fill =guide_legend(order=1),
             size =guide_legend(order=2, override.aes=list(color="grey40", alpha=0.7)))
  })
  
  # ── 2a. Distribution stacked (Fig S2) ───────────────────────────────────────
  output$dist_plot <- renderPlot(res = APP_RES, {
    df <- filtered_repo()
    mean_df <- df %>% group_by(type) %>%
      summarise(mean_z=mean(final_score_1to5,na.rm=TRUE),.groups="drop")
    
    ggplot(df, aes(x=final_score_1to5, fill=type)) +
      geom_histogram(aes(y=after_stat(count/sum(count))), position="stack",
                     binwidth=0.15, color="white", alpha=0.7) +
      geom_density(aes(y=after_stat(scaled), color=type), linewidth=0.9*SK, alpha=0.9) +
      geom_vline(data=mean_df, aes(xintercept=mean_z, color=type),
                 linetype="dashed", linewidth=0.8*SK) +
      scale_fill_manual(values=TYPE_COLORS) +
      scale_color_manual(values=TYPE_COLORS) +
      labs(title=W("Comparative Quality Distribution Across Repository Types",58),
           x="Normalized (0–5)", y="Density / Proportion",
           fill="Repository Type", color="Repository Type") +
      theme_supp() +
      theme(legend.position="top") +
      guides(fill=guide_legend(nrow=2), color=guide_legend(nrow=2))
  })
  
  # ── 2b. Overall dist + Q-Q (Fig S3) ─────────────────────────────────────────
  output$dist_qq_plot <- renderPlot(res = APP_RES, {
    df     <- filtered_repo()
    z_all  <- df$final_score_1to5[!is.na(df$final_score_1to5)]
    z_samp <- if (length(z_all) > 10000) sample(z_all,10000) else z_all
    
    p_dist <- ggplot(df, aes(x=final_score_1to5)) +
      geom_histogram(aes(y=after_stat(density)), bins=40,
                     fill="#56B4E9", color="white", alpha=0.8) +
      geom_density(color="red", linewidth=0.9*SK) +
      geom_vline(aes(xintercept=mean(final_score_1to5,na.rm=TRUE)),
                 color="darkblue", linetype="dashed", linewidth=0.8*SK) +
      labs(title=W("Overall Distribution of RQI",58), x="RQI (0–5)", y="Density") +
      theme_supp()
    
    p_qq <- ggplot(data.frame(z=z_samp), aes(sample=z)) +
      stat_qq(color="#1F78B4", size=1*SK) +
      stat_qq_line(color="red", linewidth=0.9*SK) +
      labs(title=W("Normal Q–Q Plot",58), x="Theoretical quantiles", y="Sample quantiles") +
      theme_supp()
    
    p_dist + p_qq
  })
  
  # ── 2c. Density faceted (Fig S4) ────────────────────────────────────────────
  output$dist_facet_plot <- renderPlot(res = APP_RES, {
    df <- filtered_repo()
    ggplot(df, aes(x=final_score_1to5, fill=type)) +
      geom_histogram(aes(y=after_stat(density)), bins=30, color="white", alpha=0.7) +
      geom_density(aes(color=type), linewidth=0.9*SK) +
      facet_wrap(~type, ncol=3) +
      scale_fill_manual(values=TYPE_COLORS) +
      scale_color_manual(values=TYPE_COLORS) +
      labs(title=W("Distribution and Density by Repository Type",58),
           x="RQI (0–5)", y="Density") +
      theme_supp() +
      theme(legend.position="none")
  })
  
  # ── 3. Bio Tukey (Fig S5) ───────────────────────────────────────────────────
  output$bio_tukey_plot <- renderPlot(res = APP_RES, {
    bio_comp <- as.data.frame(Tukey_result$type) %>%
      tibble::rownames_to_column("comparison") %>%
      separate(comparison, into=c("group1","group2"), sep="-") %>%
      filter(group1=="bioinformatics" | group2=="bioinformatics") %>%
      mutate(other = ifelse(group1=="bioinformatics",group2,group1),
             sig = case_when(`p adj`<0.001~"***",`p adj`<0.01~"**",
                             `p adj`<0.05~"*",TRUE~"ns"))
    
    all_types   <- c("bioinformatics", bio_comp$other)
    plot_data   <- repo_summary %>% filter(type %in% all_types) %>%
      mutate(type=factor(type, levels=c("bioinformatics",sort(bio_comp$other))))
    type_levels <- levels(plot_data$type)
    bio_x       <- which(type_levels=="bioinformatics")
    col_pal     <- setNames(c("#1F78B4",
                              RColorBrewer::brewer.pal(max(length(type_levels)-1, 3),"Set2")[seq_len(length(type_levels)-1)]),
                            type_levels)
    
    # Brackets stacked shortest-span first, all clear of the tallest error bar
    sig_bars <- bio_comp %>%
      filter(sig!="ns") %>%
      mutate(other_x   = match(other,type_levels),
             bio_x     = bio_x,
             span      = abs(other_x - bio_x),
             sig_label = paste0("p = ",signif(`p adj`,3))) %>%
      arrange(span, other_x) %>%
      mutate(y = max(plot_data$ci_upper) + 0.2 + row_number()*0.5)
    
    ggplot(plot_data, aes(x=type, y=mean, fill=type)) +
      geom_col(alpha=0.85, width=0.5) +
      geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper), width=0.15, linewidth=LW_MIN) +
      geom_segment(data=sig_bars,
                   aes(x=bio_x,xend=other_x,y=y,yend=y),
                   inherit.aes=FALSE, linewidth=0.6*SK) +
      geom_text(data=sig_bars,
                aes(x=(bio_x+other_x)/2,y=y,label=sig_label),
                inherit.aes=FALSE, vjust=-0.5, size=TXT_S, fontface="bold") +
      scale_fill_manual(values=col_pal) +
      scale_x_discrete(expand=expansion(mult=0.18)) +
      scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
      labs(x=NULL, y="Mean RQI", title=W("Bioinformatics vs All Other Categories",58)) +
      theme_supp() +
      theme(legend.position="none",
            panel.grid.major.x=element_blank(),
            axis.text.x=element_text(angle=30,hjust=1),
            plot.title =element_text(margin=margin(b=16*SK)),
            plot.margin=margin(20*SK,20*SK,10*SK,10*SK))
  })
  
  # ── 4. Pairwise Tukey ───────────────────────────────────────────────────────
  output$tukey_plot <- renderPlot(res = APP_RES, {
    req(input$repo_1, input$repo_2)
    validate(need(input$repo_1 != input$repo_2, "Select two different types."))
    
    pair_data <- repo_summary %>% filter(type %in% c(input$repo_1,input$repo_2))
    sig_check <- tukey_sig %>%
      filter((group1==input$repo_1 & group2==input$repo_2) |
               (group1==input$repo_2 & group2==input$repo_1))
    sig_label <- if (nrow(sig_check)>=1) paste0("p = ",signif(as.numeric(sig_check$`p adj`[1]),3)) else "p = ns"
    
    y_bar <- max(pair_data$ci_upper,na.rm=TRUE)+0.2
    
    ggplot(pair_data, aes(x=type, y=mean, fill=type)) +
      geom_col(alpha=0.8, width=0.6) +
      geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper), width=0.15, linewidth=LW_MIN) +
      sig_bracket(1, 2, y_bar, sig_label, dy=0.2, size=TXT_S, fontface="bold") +
      scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
      labs(title=W(paste(input$repo_1,"vs",input$repo_2),58),
           subtitle="Tukey HSD Comparison", x=NULL, y="Mean RQI (±95% CI)") +
      theme_supp() +
      theme(legend.position="none",
            panel.grid.major.x=element_blank(),
            axis.text.x=element_text(angle=30,hjust=1))
  })
  
  # ── 5. CI / test impact overall (Fig S6) ────────────────────────────────────
  make_impact_plot <- function(df, var, colors, xlab, title) {
    lv <- if (var=="ci_present") c("No CI","Has CI") else c("No Tests","Has Tests")
    smry <- df %>%
      mutate(label=factor(ifelse(.data[[var]]==1, lv[2], lv[1]), levels=lv)) %>%
      group_by(label) %>%
      summarise(mean_z=mean(final_score_1to5,na.rm=TRUE),
                sd_z=sd(final_score_1to5,na.rm=TRUE), n=n(),
                se_z=sd_z/sqrt(n), .groups="drop")
    
    p_val <- if (nrow(smry)==2) t.test(df$final_score_1to5 ~ df[[var]])$p.value else NA_real_
    y_top <- max(smry$mean_z + smry$se_z, na.rm=TRUE)
    
    p <- ggplot(smry, aes(x=label,y=mean_z,fill=label)) +
      geom_col(width=0.55,color="white") +
      geom_errorbar(aes(ymin=mean_z-se_z,ymax=mean_z+se_z),width=0.2,linewidth=LW_MIN)
    if (!is.na(p_val)) {
      p <- p + sig_bracket(1, 2, y_top+0.15, paste0("p = ",signif(p_val,3)),
                           dy=0.15, size=TXT_S, fontface="bold")
    }
    p +
      scale_fill_manual(values=colors) +
      scale_y_continuous(limits=c(0,NA), breaks=0:5, expand=expansion(mult=c(0,0.08))) +
      labs(title=W(paste(title,"Impact on Quality"),58), x=xlab, y="Mean RQI (0–5)") +
      theme_supp() +
      theme(legend.position="none")
  }
  
  output$ci_impact_plot <- renderPlot(res = APP_RES, {
    make_impact_plot(filtered_repo(), "ci_present",
                     c("No CI"="#A6CEE3","Has CI"="#1F78B4"), "Continuous Integration", "CI")
  })
  output$test_impact_plot <- renderPlot(res = APP_RES, {
    make_impact_plot(filtered_repo(), "tests_present",
                     c("No Tests"="#B2DF8A","Has Tests"="#33A02C"), "Testing", "Testing")
  })
  
  # ── 6. CI / test impact by type, faceted (Figs S7, S8) ──────────────────────
  # One bracket per panel, placed above THAT panel's tallest error bar (the old
  # code used the tallest bar across all panels, which pushed brackets away from
  # their bars and stretched every free-y panel).
  make_by_type_plot <- function(var, p_col, colors, title, xlab) {
    grp <- repo %>%
      mutate(flag = .data[[var]]) %>%
      group_by(type, flag) %>%
      summarise(mean_z=mean(final_score_1to5,na.rm=TRUE),
                se_z=sd(final_score_1to5,na.rm=TRUE)/sqrt(n()),
                n=n(), .groups="drop") %>%
      left_join(within_type_tests %>% select(type, p = all_of(p_col)), by="type")
    
    bracket_df <- grp %>%
      group_by(type) %>%
      summarise(ymax=max(mean_z+se_z,na.rm=TRUE), p=first(p), .groups="drop") %>%
      filter(!is.na(p)) %>%
      mutate(sig_label=paste0("p = ",signif(p,3)))
    
    ggplot(grp, aes(x=factor(flag),y=mean_z,fill=factor(flag))) +
      geom_col(position="dodge",width=0.6,color="white") +
      geom_errorbar(aes(ymin=mean_z-se_z,ymax=mean_z+se_z),width=0.2,color="black",linewidth=LW_MIN) +
      facet_wrap(~type, scales="free_y") +
      geom_segment(data=bracket_df,
                   aes(x=1,xend=2,y=ymax+0.2,yend=ymax+0.2),
                   inherit.aes=FALSE, color="black", linewidth=LW_MIN) +
      geom_text(data=bracket_df,
                aes(x=1.5,y=ymax+0.4,label=sig_label),
                inherit.aes=FALSE, size=TXT_S, fontface="bold") +
      scale_fill_manual(values=colors) +
      scale_y_continuous(limits=c(0,NA), breaks=seq(0,5,1),
                         expand=expansion(mult=c(0,0.08))) +
      labs(title=W(title,58), x=xlab, y="RQI (0–5)") +
      theme_supp() +
      theme(legend.position="none",
            plot.title =element_text(margin=margin(b=16*SK)),
            plot.margin=margin(20*SK,20*SK,10*SK,10*SK))
  }
  
  output$ci_by_type_plot <- renderPlot(res = APP_RES, {
    make_by_type_plot("ci_present", "ci_p", c("0"="#A6CEE3","1"="#1F78B4"),
                      "CI Presence Effect per Repository Type", "CI (0 = No, 1 = Yes)")
  })
  output$test_by_type_plot <- renderPlot(res = APP_RES, {
    make_by_type_plot("tests_present", "tests_p", c("0"="#B2DF8A","1"="#33A02C"),
                      "Testing Presence Effect per Repository Type", "Tests (0 = No, 1 = Yes)")
  })
  
  # ── 7. Radar (Fig 1, Figs S9–S11 style) ─────────────────────────────────────
  # Every view is drawn for the sidebar's Repository Type, in a single row of
  # panels (one for Average profile, two for Top 20 vs Bottom 20, three for
  # Top / Mid / Bottom 100).
  output$radar_plot_ui <- renderUI({
    tags$div(class = "plot-card", plotOutput("radar_plot", height = paste0(h_px(3.3), "px")))
  })
  
  output$radar_plot <- renderPlot(res = APP_RES, {
    rt   <- input$repo_type
    is_all <- (rt == "All")
    
    if (input$radar_view == "tmb") {
      
      if (is_all) {
        top_df    <- top100_avg    %>% summarise(across(c(recency,backlog_health,popularity),mean))
        mid_df    <- median100_avg %>% summarise(across(c(recency,backlog_health,popularity),mean))
        bottom_df <- bottom100_avg %>% summarise(across(c(recency,backlog_health,popularity),mean))
        main_txt  <- "Repository quality index (RQI) components of top, median, and bottom 100 repositories"
      } else {
        top_df    <- top100_avg    %>% filter(type==rt)
        mid_df    <- median100_avg %>% filter(type==rt)
        bottom_df <- bottom100_avg %>% filter(type==rt)
        main_txt  <- paste0(type_label(rt),
                            ": RQI components of top, median, and bottom 100 repositories")
      }
      req(nrow(top_df)==1, nrow(mid_df)==1, nrow(bottom_df)==1)
      
      radar_par(c(1,3), c(0.3,0.45,2.0,0.45))
      draw_radar_panel(top_df,    "#1F78B4", "Top 100 repositories",    "(highest RQI scores)")
      draw_radar_panel(mid_df,    "#6A3D9A", "Median 100 repositories", "(middle RQI scores)")
      draw_radar_panel(bottom_df, "#E31A1C", "Bottom 100 repositories", "(lowest RQI scores)")
      radar_title(main_txt)
      
    } else if (input$radar_view == "by_domain") {
      
      if (is_all) {
        vals <- all_radar; col <- "#555555"
        lab  <- "All domains"; sub <- "(all repositories)"
        main_txt <- "Average RQI component profile (all domains)"
      } else {
        vals <- all_by_type_radar %>% filter(type==rt); col <- type_colors[[rt]]
        lab  <- type_label(rt); sub <- "(domain average)"
        main_txt <- paste0(type_label(rt), ": average RQI component profile")
      }
      req(nrow(vals)==1)
      
      radar_par(c(1,1), c(0.3,0.45,2.0,0.45))
      draw_radar_panel(vals, col, lab, sub)
      radar_title(main_txt)
      
    } else { # top_bottom_20
      
      if (is_all) {
        top_df <- top20_all;  bot_df <- bottom20_all
        main_txt <- "Top 20 vs bottom 20 repositories (all domains)"
      } else {
        top_df <- top20_by_type_radar    %>% filter(type==rt)
        bot_df <- bottom20_by_type_radar %>% filter(type==rt)
        main_txt <- paste0(type_label(rt), ": top 20 vs bottom 20 repositories")
      }
      req(nrow(top_df)==1, nrow(bot_df)==1)
      
      radar_par(c(1,2), c(0.3,2.9,2.0,2.9))
      draw_radar_panel(top_df, "#1F78B4", "Top 20 repositories",    "(highest RQI)")
      draw_radar_panel(bot_df, "#E31A1C", "Bottom 20 repositories", "(lowest RQI)")
      radar_title(main_txt)
    }
  })
  
  # ── 8. Age cohorts (Fig 3 style) ────────────────────────────────────────────
  cohort_ci_data <- function(df) {
    df %>%
      group_by(age_cohort,ci_present) %>%
      summarise(mean_z=mean(final_score_1to5,na.rm=TRUE),
                se_z=sd(final_score_1to5,na.rm=TRUE)/sqrt(n()),n=n(),.groups="drop") %>%
      mutate(ci_label=ifelse(ci_present==1,"Has CI","No CI"))
  }
  cohort_test_data <- function(df) {
    df %>%
      group_by(age_cohort,tests_present) %>%
      summarise(mean_z=mean(final_score_1to5,na.rm=TRUE),
                se_z=sd(final_score_1to5,na.rm=TRUE)/sqrt(n()),n=n(),.groups="drop") %>%
      mutate(test_label=ifelse(tests_present==1,"Has tests","No tests"))
  }
  cohort_line_layers <- function() {
    list(geom_line(linewidth=0.8*SK), geom_point(size=2.2*SK),
         geom_errorbar(aes(ymin=mean_z-se_z,ymax=mean_z+se_z), width=0.15, linewidth=0.6*SK))
  }
  cohort_theme <- function() {
    theme_sci() +
      theme(axis.text.x=element_text(angle=20,hjust=1), legend.title=element_blank())
  }
  ci_cols   <- c("Has CI"="#1F78B4","No CI"="#A6CEE3")
  test_cols <- c("Has tests"="#33A02C","No tests"="#B2DF8A")
  
  output$cohort_ci_plot <- renderPlot(res = APP_RES, {
    smry <- cohort_ci_data(filtered_repo())
    ggplot(smry,aes(x=age_cohort,y=mean_z,color=ci_label,group=ci_label)) +
      cohort_line_layers() +
      scale_color_manual(values=ci_cols) +
      labs(title=W("Mean RQI by Repo Age Cohort — CI vs No CI",58),
           x="Repository age cohort",y="RQI (0–5)",color=NULL) +
      cohort_theme()
  })
  
  output$cohort_test_plot <- renderPlot(res = APP_RES, {
    smry <- cohort_test_data(filtered_repo())
    ggplot(smry,aes(x=age_cohort,y=mean_z,color=test_label,group=test_label)) +
      cohort_line_layers() +
      scale_color_manual(values=test_cols) +
      labs(title=W("Mean RQI by Repo Age Cohort — Tests vs No Tests",58),
           x="Repository age cohort",y="RQI (0–5)",color=NULL) +
      cohort_theme()
  })
  
  # Fig S18
  output$bio_cohort_plot <- renderPlot(res = APP_RES, {
    bio_cohort <- repo %>%
      filter(type=="bioinformatics") %>%
      group_by(age_cohort) %>%
      summarise(ci_pct=mean(ci_present==1,na.rm=TRUE)*100,
                test_pct=mean(tests_present==1,na.rm=TRUE)*100,
                n=n(),.groups="drop") %>%
      pivot_longer(c(ci_pct,test_pct),names_to="metric",values_to="pct") %>%
      mutate(metric=recode(metric,"ci_pct"="CI Coverage","test_pct"="Test Coverage"))
    
    ggplot(bio_cohort,aes(x=age_cohort,y=pct,fill=metric)) +
      geom_col(position=position_dodge(width=0.6),width=0.55,color="white") +
      geom_text(aes(label=paste0(round(pct,1),"%")),
                position=position_dodge(0.6),vjust=-0.4,size=TXT_S,fontface="bold") +
      scale_fill_manual(values=c("CI Coverage"="#1F78B4","Test Coverage"="#33A02C")) +
      scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
      labs(title=W("Bioinformatics: CI and Test Coverage by Age Cohort",58),
           x="Repository Age Cohort",y="Coverage (%)",fill=NULL) +
      theme_supp() +
      theme(legend.position="bottom")
  })
  
  # Fig 3 (main text): both panels share one y-axis
  output$cohort_combined_plot <- renderPlot(res = APP_RES, {
    df <- filtered_repo()
    cohort_ci_s   <- cohort_ci_data(df)
    cohort_test_s <- cohort_test_data(df)
    
    y_range <- range(c(cohort_ci_s$mean_z   - cohort_ci_s$se_z,
                       cohort_ci_s$mean_z   + cohort_ci_s$se_z,
                       cohort_test_s$mean_z - cohort_test_s$se_z,
                       cohort_test_s$mean_z + cohort_test_s$se_z), na.rm=TRUE)
    y_pad   <- diff(y_range) * 0.08
    y_limits_shared <- c(y_range[1] - y_pad, y_range[2] + y_pad)
    y_breaks_shared <- pretty(y_limits_shared, n = 6)
    
    p_ci <- ggplot(cohort_ci_s, aes(x=age_cohort,y=mean_z,color=ci_label,group=ci_label)) +
      cohort_line_layers() +
      scale_color_manual(values=ci_cols) +
      scale_y_continuous(breaks=y_breaks_shared) +
      coord_cartesian(ylim=y_limits_shared) +
      labs(title="CI vs no CI", x="Repository age cohort", y="RQI (0–5)") +
      cohort_theme()
    
    p_tests <- ggplot(cohort_test_s, aes(x=age_cohort,y=mean_z,color=test_label,group=test_label)) +
      cohort_line_layers() +
      scale_color_manual(values=test_cols) +
      scale_y_continuous(breaks=y_breaks_shared) +
      coord_cartesian(ylim=y_limits_shared) +
      labs(title="Tests vs no tests", x="Repository age cohort", y="RQI (0–5)") +
      cohort_theme()
    
    (p_ci | p_tests) +
      plot_annotation(
        title = stringr::str_wrap(
          "Mean repository quality index (RQI) by repository age cohort for continuous integration (CI) and software testing",
          width = 75),
        theme = theme(plot.title = element_text(face="bold", hjust=0.5, size=PT$base,
                                                family=FIG_FONT, lineheight=1.05))
      ) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  })
  
  # ── 9. Language (Figs S12–S17) ──────────────────────────────────────────────
  lang_filtered <- reactive({
    if (input$lang_scope == "bio") {
      df           <- filter(lang_primary, type == "bioinformatics")
      min_n_use    <- min_n_bio
      valid_langs  <- bio_lang_summary_filt$language
      title_prefix <- "Bioinformatics: "
    } else {
      df           <- if (input$repo_type=="All") lang_primary else filter(lang_primary,type==input$repo_type)
      min_n_use    <- min_n
      valid_langs  <- lang_summary_filt$language
      title_prefix <- ""
    }
    df <- filter(df, language %in% valid_langs)
    list(df=df, min_n_use=min_n_use, title_prefix=title_prefix)
  })
  
  # Plot height follows the Rmd's lang_h(): rows per language x language count.
  output$lang_plot_ui <- renderUI({
    n_langs <- length(unique(lang_filtered()$df$language))
    per     <- switch(input$lang_split, "none"=0.22, "tests"=0.34, "ci"=0.34, "qa"=0.5, 0.34)
    h       <- h_px(max(3.5, n_langs * per + 1.3))
    tags$div(class = "plot-card", plotOutput("lang_plot", height = paste0(h, "px")))
  })
  
  output$lang_plot <- renderPlot(res = APP_RES, {
    fl           <- lang_filtered()
    df           <- fl$df
    min_n_use    <- fl$min_n_use
    title_prefix <- fl$title_prefix
    
    if (input$lang_split == "none") {
      smry <- df %>% group_by(language) %>%
        summarise(n=n(), mean_rating=mean(final_score_1to5,na.rm=TRUE), .groups="drop")
      ggplot(smry,aes(x=fct_reorder(language,mean_rating),y=mean_rating)) +
        geom_col(fill="#4C72B0", width=0.55) +
        coord_flip() +
        scale_x_discrete(expand=expansion(mult=0.08, add=0.6)) +
        labs(title=W(paste0(title_prefix,"Mean RQI by Primary Language (n≥",min_n_use,")")),
             x=NULL,y="Mean RQI (0–5)") +
        theme_supp()
      
    } else if (input$lang_split == "tests") {
      smry <- df %>% group_by(language,tests_present) %>%
        summarise(n=n(),mean_rating=mean(final_score_1to5,na.rm=TRUE),
                  se=sd(final_score_1to5,na.rm=TRUE)/sqrt(n),.groups="drop")
      ggplot(smry,aes(x=fct_reorder(language,mean_rating,.fun=mean),y=mean_rating,
                      fill=factor(tests_present))) +
        geom_col(position=position_dodge(0.6),width=0.5,color="white") +
        geom_errorbar(aes(ymin=mean_rating-se,ymax=mean_rating+se),
                      position=position_dodge(0.6),width=0.2,linewidth=LW_MIN) +
        coord_flip() +
        scale_x_discrete(expand=expansion(mult=0.08, add=0.6)) +
        scale_fill_manual(values=c("0"="#B2DF8A","1"="#33A02C"),
                          labels=c("No Tests","Has Tests")) +
        labs(title=W(paste0(title_prefix,"Mean RQI by Primary Language (split by Testing), n≥", min_n_use)),
             x=NULL,y="Mean RQI (0–5)",fill="Tests Present") +
        theme_supp() +
        theme(legend.position="bottom")
      
    } else if (input$lang_split == "ci") {
      smry <- df %>% group_by(language,ci_present) %>%
        summarise(n=n(),mean_rating=mean(final_score_1to5,na.rm=TRUE),
                  se=sd(final_score_1to5,na.rm=TRUE)/sqrt(n),.groups="drop")
      ggplot(smry,aes(x=fct_reorder(language,mean_rating,.fun=mean),y=mean_rating,
                      fill=factor(ci_present))) +
        geom_col(position=position_dodge(0.6),width=0.5,color="white") +
        geom_errorbar(aes(ymin=mean_rating-se,ymax=mean_rating+se),
                      position=position_dodge(0.6),width=0.2,linewidth=LW_MIN) +
        coord_flip() +
        scale_x_discrete(expand=expansion(mult=0.08, add=0.6)) +
        scale_fill_manual(values=c("0"="#A6CEE3","1"="#1F78B4"),
                          labels=c("No CI","Has CI")) +
        labs(title=W(paste0(title_prefix,"Mean RQI by Primary Language (split by CI), n≥", min_n_use)),
             x=NULL,y="Mean RQI (0–5)",fill="CI Present") +
        theme_supp() +
        theme(legend.position="bottom")
      
    } else {
      smry <- df %>%
        mutate(qa_bucket=factor(case_when(
          ci_present==1 & tests_present==1 ~ "Has CI + Tests",
          ci_present==1 & tests_present==0 ~ "CI only",
          ci_present==0 & tests_present==1 ~ "Tests only",
          TRUE ~ "Neither"
        ), levels=c("Neither","Tests only","CI only","Has CI + Tests"))) %>%
        group_by(language,qa_bucket) %>%
        summarise(n=n(),mean_rating=mean(final_score_1to5,na.rm=TRUE),
                  se=sd(final_score_1to5,na.rm=TRUE)/sqrt(n),.groups="drop")
      ggplot(smry,aes(x=fct_reorder(language,mean_rating,.fun=mean),y=mean_rating,fill=qa_bucket)) +
        geom_col(position=position_dodge(0.7),width=0.6,color="white") +
        geom_errorbar(aes(ymin=mean_rating-se,ymax=mean_rating+se),
                      position=position_dodge(0.7),width=0.2,linewidth=LW_MIN) +
        coord_flip() +
        scale_x_discrete(expand=expansion(mult=0.08, add=0.6)) +
        labs(title=W(paste0(title_prefix,"Mean RQI by Primary Language (CI/Tests buckets), n≥", min_n_use)),
             x=NULL,y="Mean RQI (0–5)",fill="QA Bucket") +
        theme_supp() +
        theme(legend.position="bottom")
    }
  })
  
  # ── 10. Activity / survival (Figs S19, S20) ─────────────────────────────────
  output$survival_plot <- renderPlot(res = APP_RES, {
    surv_model <- glm(
      is_active ~ ci_present + tests_present +
        log1p(repo_age_days) + log1p(commit_count) + type,
      data=repo, family=binomial
    )
    tidy_surv <- broom::tidy(surv_model, conf.int=TRUE, exponentiate=TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(significant = p.value < 0.05,
             term = recode(term,
                           "ci_present"="CI Present","tests_present"="Tests Present",
                           "log1p(repo_age_days)"="log(Repo Age)","log1p(commit_count)"="log(Commit Count)",
                           "typebioinformatics"="Type: Bioinformatics",
                           "typeastrophysics"="Type: Astrophysics",
                           "typeimage_recognition"="Type: Image Recognition",
                           "typeopen_source"="Type: Open Source",
                           "typesoftware_engineering"="Type: Software Engineering"))
    
    ggplot(tidy_surv,aes(x=estimate,y=fct_reorder(term,estimate),color=significant)) +
      geom_point(size=2.2*SK) +
      geom_errorbarh(aes(xmin=conf.low,xmax=conf.high),height=0.2,linewidth=LW_MIN) +
      geom_vline(xintercept=1,linetype="dashed",color="gray40",linewidth=LW_MIN) +
      scale_color_manual(values=c("FALSE"="gray60","TRUE"="#E31A1C"),
                         labels=c("p ≥ 0.05","p < 0.05")) +
      labs(title=W("Odds Ratios: Predictors of Repository Activity",58),
           subtitle=W("Outcome: committed within last 365 days  |  OR > 1 = higher odds of being active",80),
           x="Odds Ratio (±95% CI)",y=NULL,color="Significance") +
      theme_supp() +
      theme(legend.position="bottom")
  })
  
  output$activity_plot <- renderPlot(res = APP_RES, {
    act_smry <- repo %>%
      group_by(type,ci_present,tests_present) %>%
      summarise(activity_rate=mean(is_active,na.rm=TRUE)*100,n=n(),.groups="drop") %>%
      mutate(qa_bucket=factor(case_when(
        ci_present==1&tests_present==1~"CI + Tests",
        ci_present==1&tests_present==0~"CI Only",
        ci_present==0&tests_present==1~"Tests Only",
        TRUE~"Neither"
      ),levels=c("Neither","Tests Only","CI Only","CI + Tests")))
    
    ggplot(act_smry,aes(x=qa_bucket,y=activity_rate,fill=qa_bucket)) +
      geom_col(width=0.65,color="white") +
      geom_text(aes(label=paste0(round(activity_rate,1),"%")),
                vjust=-0.4,size=TXT_S,fontface="bold") +
      facet_wrap(~type) +
      scale_fill_manual(values=c("Neither"="#FDBF6F","Tests Only"="#B2DF8A",
                                 "CI Only"="#A6CEE3","CI + Tests"="#1F78B4")) +
      scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
      labs(title=W("Repository Activity Rate by CI/Test Presence and Domain",60),
           x=NULL,y="Active Repos (%)",fill=NULL) +
      theme_supp() +
      theme(axis.text.x=element_text(angle=30,hjust=1),
            legend.position="none")
  })
  
  # ── 11. Citations (conditional) ─────────────────────────────────────────────
  # Rmd figure height (in) and relative width for each choice
  cit_layout <- list(
    loess        = c(4.2, 1),    median_loess = c(4.2, 1),    violin    = c(4.5, 1),
    mean_bar     = c(4.2, 1),    bar_lh       = c(4.2, 0.7),  bar_bins  = c(5,   1),
    bar_bins_fine= c(5.5, 1),    fig6         = c(5,   1),    bar_ci    = c(4.2, 0.7),
    pub_bar      = c(4.2, 0.7),  pub_violin   = c(5,   0.5),  fig5a     = c(5,   0.5),
    age_scatter  = c(4.6, 1),    scatter_lm   = c(4.2, 1)
  )
  
  output$citation_ui <- renderUI({
    if (!has_citations) {
      p("Citation data not found (repos_with_citations_gemma_v2.csv).", style="color:gray;")
    } else {
      tagList(
        uiOutput("cit_plot_ui"),
        hr(),
        h4("Table S2 — Top 20 Most-Cited Repositories with RQI ≤ 2.5"),
        tableOutput("citation_table_s2")
      )
    }
  })
  
  output$cit_plot_ui <- renderUI({
    req(input$cit_plot)
    lay <- cit_layout[[input$cit_plot]]
    if (is.null(lay)) lay <- c(4.2, 1)
    plot_card("cit_plot_out", lay[1], lay[2])
  })
  
  output$cit_plot_out <- renderPlot(res = APP_RES, {
    req(has_citations)
    craw <- citations_raw %>% filter(!is.na(citation_count), citation_count >= 0)
    
    if (input$cit_plot == "loess") {                                   # Fig S21
      dat <- craw %>% mutate(rqi_bin=round(final_score_1to5*4)/4) %>%
        group_by(rqi_bin) %>% summarise(mean_citations=mean(citation_count),.groups="drop")
      ggplot(dat,aes(x=rqi_bin,y=mean_citations)) +
        geom_smooth(method="loess",span=0.2,color="#1F78B4",fill="#1F78B4",
                    alpha=0.15,linewidth=1.2*SK,se=FALSE) +
        geom_point(color="#1F78B4",size=2*SK) +
        coord_cartesian(ylim=c(0,NA)) +
        labs(title=W("Mean Citation Count vs Repository Quality",58),
             x="RQI (0–5)",y="Mean Citation Count") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "median_loess") {                     # Fig S22
      dat <- craw %>% mutate(rqi_bin=round(final_score_1to5*2)/2) %>%
        group_by(rqi_bin) %>% summarise(median_citations=median(citation_count),.groups="drop")
      ggplot(dat,aes(x=rqi_bin,y=median_citations)) +
        geom_smooth(method="loess",span=0.5,color="#1F78B4",fill="#1F78B4",
                    alpha=0.15,linewidth=1.2*SK,se=FALSE) +
        geom_point(color="#1F78B4",size=2*SK) +
        coord_cartesian(ylim=c(0,NA)) +
        labs(title=W("Median Citation Count vs Repository Quality",58),
             x="RQI (0–5)",y="Median Citation Count") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "violin") {                           # Fig S23
      dat <- craw %>% mutate(rqi_bin=factor(round(final_score_1to5*2)/2))
      med <- dat %>% group_by(rqi_bin) %>%
        summarise(med=median(log1p(citation_count)),.groups="drop")
      ggplot(dat,aes(x=rqi_bin,y=log1p(citation_count),fill=rqi_bin)) +
        geom_violin(trim=TRUE,alpha=0.75,linewidth=LW_MIN,color="grey60") +
        geom_point(data=med,aes(x=rqi_bin,y=med),inherit.aes=FALSE,
                   shape=23,size=3*SK,fill="white",color="black",stroke=STROKE_MIN) +
        scale_fill_manual(values=rqi_pal(nlevels(dat$rqi_bin))) +
        labs(title="Citation count distribution by RQI bin",
             x="RQI (0.5 intervals)",y="log(citation count + 1)") +
        theme_supp() +
        theme(legend.position="none",panel.grid.major.x=element_blank())
      
    } else if (input$cit_plot == "mean_bar") {                         # Fig S24
      dat <- craw %>% mutate(rqi_bin=factor(round(final_score_1to5*2)/2))
      ggplot(dat, aes(x=rqi_bin, y=citation_count, fill=rqi_bin)) +
        stat_summary(fun=mean, geom="col", alpha=0.85, width=0.5) +
        stat_summary(fun=mean, geom="text", aes(label=after_stat(round(y,1))),
                     vjust=-0.5, size=TXT_M) +
        scale_fill_manual(values=rqi_pal(nlevels(dat$rqi_bin))) +
        scale_y_continuous(expand=expansion(mult=c(0,0.08))) +
        labs(title="Mean citation count by RQI bin",
             x="RQI (0.5 intervals)", y="Mean citation count") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "bar_lh") {                           # Fig S25
      grp <- craw %>% filter(!is.na(final_score_1to5)) %>%
        mutate(rqi_group=factor(ifelse(final_score_1to5<=2.5,"Low","High"),levels=c("Low","High")))
      grp$rqi_group <- require_two_groups(grp$rqi_group, "RQI group")
      smry  <- grp %>% group_by(rqi_group) %>% summarise(median_cit=median(citation_count),.groups="drop")
      p_val <- wilcox.test(citation_count~rqi_group,data=grp)$p.value
      bs    <- bracket_stack(data.frame(x=1,xend=2,p_value=p_val,label=p_label_num(p_val)),
                             max(smry$median_cit), step_frac=0.20, offset=0)
      ggplot(smry,aes(x=rqi_group,y=median_cit,fill=rqi_group)) +
        geom_col(alpha=0.85,width=0.5) +
        geom_text(aes(label=round(median_cit,1)),vjust=-0.5,size=TXT_M) +
        bracket_layers(bs$pairs, bs$step) +
        scale_fill_manual(values=c("Low"="#D73027","High"="#1A9850")) +
        scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
        coord_cartesian(ylim=c(0, bs$ymax)) +
        labs(title="Median citation count by quality group",
             x="RQI group",y="Median citation count") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "bar_bins") {                         # Fig S26
      grp <- craw %>% filter(!is.na(final_score_1to5)) %>%
        mutate(rqi_group=factor(case_when(
          final_score_1to5<2~"1-2",final_score_1to5<3~"2-3",
          final_score_1to5<4~"3-4",TRUE~"4-5"),levels=c("1-2","2-3","3-4","4-5")))
      smry <- grp %>% group_by(rqi_group) %>% summarise(median_cit=median(citation_count),.groups="drop")
      bs   <- bracket_stack(build_sig_brackets(grp, "rqi_group", "citation_count"),
                            max(smry$median_cit), step_frac=0.20, offset=1)
      ggplot(smry,aes(x=rqi_group,y=median_cit,fill=rqi_group)) +
        geom_col(alpha=0.85,width=0.6) +
        geom_text(aes(label=round(median_cit,1)),vjust=-0.5,size=TXT_M) +
        bracket_layers(bs$pairs, bs$step) +
        scale_fill_manual(values=c("1-2"="#FC8D59","2-3"="#FEE08B","3-4"="#91CF60","4-5"="#1A9850")) +
        scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
        coord_cartesian(ylim=c(0, bs$ymax)) +
        labs(title="Median citation count by RQI bin",
             x="RQI score bin",y="Median citation count") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "bar_bins_fine") {                    # Fig S27
      grp <- craw %>% filter(!is.na(final_score_1to5)) %>%
        mutate(
          rqi_group = case_when(
            final_score_1to5 < 1.5 ~ "1-1.5", final_score_1to5 < 2.0 ~ "1.5-2",
            final_score_1to5 < 2.5 ~ "2-2.5", final_score_1to5 < 3.0 ~ "2.5-3",
            final_score_1to5 < 3.5 ~ "3-3.5", final_score_1to5 < 4.0 ~ "3.5-4",
            final_score_1to5 < 4.5 ~ "4-4.5", TRUE ~ "4.5-5"
          ),
          rqi_group = factor(rqi_group, levels=c("1-1.5","1.5-2","2-2.5","2.5-3",
                                                 "3-3.5","3.5-4","4-4.5","4.5-5")),
          quality = factor(ifelse(final_score_1to5<2.5,"Low","High"), levels=c("Low","High"))
        )
      smry <- grp %>% group_by(rqi_group,quality) %>% summarise(median_cit=median(citation_count),.groups="drop")
      bs   <- bracket_stack(build_sig_brackets(grp,"rqi_group","citation_count"),
                            max(smry$median_cit), step_frac=0.14, offset=1)
      grp$quality <- require_two_groups(grp$quality, "quality group")
      p_lh     <- wilcox.test(citation_count~quality, data=grp)$p.value
      med_low  <- median(grp$citation_count[grp$quality=="Low"])
      med_high <- median(grp$citation_count[grp$quality=="High"])
      sig_lbl  <- if (p_lh<0.001) "***" else if(p_lh<0.01) "**" else if(p_lh<0.05) "*" else "ns"
      subtitle_str <- sprintf("Low (≤2.5) median = %.1f vs High (>2.5) median = %.1f | Wilcoxon %s (p%s)",
                              med_low, med_high, sig_lbl, ifelse(p_lh<0.001,"<0.001",sprintf("=%.3f",p_lh)))
      ggplot(smry, aes(x=rqi_group,y=median_cit,fill=quality)) +
        geom_col(alpha=0.85,width=0.6) +
        geom_text(aes(label=round(median_cit,1)), vjust=-0.5, size=TXT_M) +
        bracket_layers(bs$pairs, bs$step) +
        scale_fill_manual(values=c("Low"="#D73027","High"="#1A9850")) +
        scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
        coord_cartesian(ylim=c(0, bs$ymax)) +
        labs(title="Median citation count by RQI bin", subtitle=subtitle_str,
             x="RQI score bin", y="Median citation count", fill="Quality group") +
        theme_supp() +
        theme(legend.position="bottom")
      
    } else if (input$cit_plot == "fig6") {                             # Fig 5 (main text)
      grp <- craw %>% filter(citation_count > 0, !is.na(final_score_1to5), published) %>%
        mutate(
          rqi_group = case_when(
            final_score_1to5 < 2   ~ "0-2",
            final_score_1to5 < 2.5 ~ "2-2.5",
            final_score_1to5 < 3.5 ~ "2.5-3.5",
            final_score_1to5 < 4.5 ~ "3.5-4.5",
            TRUE                    ~ "4.5-5"
          ),
          rqi_group = factor(rqi_group, levels=c("0-2","2-2.5","2.5-3.5","3.5-4.5","4.5-5")),
          quality   = factor(ifelse(final_score_1to5 <= 2.5,"Low","High"), levels=c("Low","High"))
        )
      validate(need(nrow(grp) > 0, "No published repositories with at least one citation."))
      
      smry <- grp %>% group_by(rqi_group, quality) %>%
        summarise(median_cit=median(citation_count), .groups="drop")
      bs   <- bracket_stack(build_sig_brackets(grp,"rqi_group","citation_count"),
                            max(smry$median_cit), step_frac=0.20, offset=0)
      
      subtitle_str <- tryCatch({
        p_lh     <- wilcox.test(citation_count ~ quality, data=grp)$p.value
        med_low  <- median(grp$citation_count[grp$quality=="Low"])
        med_high <- median(grp$citation_count[grp$quality=="High"])
        sprintf("Low (≤2.5) median = %.1f vs high (>2.5) median = %.1f | Wilcoxon %s",
                med_low, med_high, p_label_num(p_lh))
      }, error=function(e) NULL)
      
      rqi_gradient <- c("0-2"="#D73027","2-2.5"="#FC8D59","2.5-3.5"="#FEE08B",
                        "3.5-4.5"="#91CF60","4.5-5"="#1A9850")
      
      ggplot(smry, aes(x=rqi_group, y=median_cit, fill=rqi_group)) +
        geom_col(alpha=0.85, width=0.5) +
        geom_text(aes(label=round(median_cit,1)), vjust=-0.5, size=pt2mm(PT$base)) +
        bracket_layers(bs$pairs, bs$step, size=pt2mm(PT$base)) +
        scale_fill_manual(values=rqi_gradient) +
        scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
        coord_cartesian(ylim=c(0, bs$ymax)) +
        labs(title=W("Median citation count by repository quality index (RQI) bin",90),
             subtitle=subtitle_str,
             x="RQI score bin", y="Median citation count") +
        theme_sci() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "bar_ci") {                           # Fig S28
      dat <- craw %>% mutate(ci_label=if_else(has_CI==TRUE,"Has CI","No CI"))
      dat$ci_label <- require_two_groups(dat$ci_label, "CI")
      t_r <- t.test(citation_count~ci_label,data=dat)
      ggplot(dat,aes(x=ci_label,y=citation_count,fill=ci_label)) +
        geom_bar(stat="summary",fun="mean",alpha=0.8,width=0.5) +
        geom_jitter(alpha=0.2,width=0.15,size=0.8*SK) +
        scale_fill_manual(values=c("Has CI"="#1F78B4","No CI"="#A6CEE3")) +
        coord_cartesian(ylim=c(0,quantile(dat$citation_count,0.95))) +
        labs(title=W("Mean Citation Count by CI Presence",58),
             subtitle=paste("t-test p-value:",round(t_r$p.value,4)),
             x=NULL,y="Citation Count") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "pub_bar") {                          # Fig S29
      # Uses citations_raw (not craw) — citation_count is only populated for
      # WoS-matched repos, so filtering on it would collapse pub_label to
      # "Published" only, discarding almost every "Not Published" row.
      bio_pub <- citations_raw %>% filter(tolower(type)=="bioinformatics")
      bio_pub$pub_label <- require_two_groups(bio_pub$pub_label, "publication-status")
      wt <- wilcox.test(final_score_1to5 ~ pub_label, data=bio_pub)
      meds <- bio_pub %>% group_by(pub_label) %>%
        summarise(median_rqi=median(final_score_1to5,na.rm=TRUE),.groups="drop")
      y_max <- max(meds$median_rqi); sig_y <- y_max*1.1
      ggplot(bio_pub, aes(x=pub_label,y=final_score_1to5,fill=pub_label)) +
        geom_bar(stat="summary", fun="median", alpha=0.8, width=0.5) +
        geom_text(data=meds, aes(x=pub_label,y=median_rqi,label=round(median_rqi,2)),
                  vjust=-0.5, size=TXT_M, inherit.aes=FALSE) +
        annotate("segment", x=1, xend=2, y=sig_y, yend=sig_y, linewidth=LW_MIN) +
        annotate("text", x=1.5, y=sig_y*1.05,
                 label=ifelse(wt$p.value<0.0001,"p < 0.0001",paste("p =",round(wt$p.value,4))),
                 hjust=0.5, size=TXT_M) +
        scale_fill_manual(values=c("Published"="#1F78B4","Not Published"="#A6CEE3")) +
        scale_x_discrete(labels=c("Not Published"="Not published","Published"="Published")) +
        coord_cartesian(ylim=c(0,sig_y*1.15)) +
        labs(title="Median RQI by publication status (bioinformatics)",
             x=NULL, y="Median RQI (0–5)") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$cit_plot == "pub_violin") {                       # Fig S30
      bio_pub <- citations_raw %>% filter(tolower(type)=="bioinformatics")
      bio_pub$pub_label <- require_two_groups(bio_pub$pub_label, "publication-status")
      wt <- wilcox.test(final_score_1to5 ~ pub_label, data=bio_pub)
      meds <- bio_pub %>% group_by(pub_label) %>%
        summarise(median_rqi=median(final_score_1to5,na.rm=TRUE), n=n(), .groups="drop")
      p_label <- ifelse(wt$p.value<0.0001,"p < 0.0001", paste("p =",round(wt$p.value,4)))
      pub_lv  <- c("Published","Not published")
      bio_pub <- bio_pub %>%
        mutate(pub_plot=factor(ifelse(pub_label=="Published","Published","Not published"), levels=pub_lv))
      meds    <- meds %>%
        mutate(pub_plot=factor(ifelse(pub_label=="Published","Published","Not published"), levels=pub_lv))
      
      ggplot(bio_pub, aes(x=pub_plot, y=final_score_1to5)) +
        geom_violin(fill=deep_blue, alpha=0.20, trim=TRUE, width=0.75,
                    linewidth=LW_MIN, color="grey60") +
        geom_point(data=meds, aes(x=pub_plot,y=median_rqi), inherit.aes=FALSE,
                   shape=21, size=4*SK, fill=deep_blue, color="black", stroke=STROKE_MIN) +
        geom_text(data=meds, aes(x=pub_plot,y=median_rqi,label=sprintf("%.2f",median_rqi)),
                  hjust=1.35, fontface="bold", size=pt2mm(PT$value+2*SK), color="black",
                  inherit.aes=FALSE) +
        geom_text(data=meds, aes(x=pub_plot,y=5.95,label=paste0("n=",n)),
                  fontface="bold", size=pt2mm(PT$base), color="grey30", inherit.aes=FALSE) +
        annotate("text", x=1.5, y=5.55, label=paste("Wilcoxon (RQI)",p_label),
                 hjust=0.5, fontface="italic", size=pt2mm(PT$base), color="grey30") +
        scale_y_continuous(limits=c(0,6.25), breaks=0:5, expand=expansion(mult=c(0.02,0.02))) +
        labs(title="RQI by publication status (bioinformatics)", x=NULL, y="RQI score (0–5)") +
        theme_supp() +
        theme(panel.grid.major.x=element_blank())
      
    } else if (input$cit_plot == "fig5a") {                            # Fig 4a (main text)
      base_data <- citations_raw %>%
        filter(tolower(type)=="bioinformatics", !is.na(final_score_1to5)) %>%
        mutate(
          pub_label    = factor(ifelse(pub_label=="Published","Published","Not published"),
                                levels=c("Published","Not published")),
          recency_1to5 = recency_score_1to5,
          issue_1to5   = issue_score_1to5,
          pop_1to5     = pop_score_1to5
        )
      invisible(require_two_groups(base_data$pub_label, "publication-status"))
      layered_violin(base_data, group="pub_label", title="Publication status")$plot
      
    } else if (input$cit_plot == "age_scatter") {                      # Fig S31
      model_data <- craw %>%
        filter(!is.na(final_score_1to5), !is.na(repo_age_days), !is.na(citation_count),
               published, citation_count>0) %>%
        mutate(log_citations=log1p(citation_count), age_years=repo_age_days/365)
      ggplot(model_data, aes(x=final_score_1to5,y=log_citations,color=age_years)) +
        geom_point(alpha=0.5,size=2*SK) +
        geom_smooth(method="lm", color="grey20", linewidth=1*SK, se=TRUE) +
        scale_color_viridis_c(name="Repo Age (years)", option="plasma") +
        labs(title=W("Citation Count Explained by RQI and Repository Age",58),
             x="RQI (0–5)", y="log(Citation Count + 1)") +
        theme_supp() +
        theme(legend.position="bottom") +
        guides(color=guide_colorbar(barwidth=10, barheight=0.6))
      
    } else if (input$cit_plot == "scatter_lm") {                       # Fig S33
      dat <- craw %>%
        filter(!is.na(final_score_1to5), published, citation_count>0) %>%
        mutate(log_citations=log1p(citation_count))
      ggplot(dat,aes(x=final_score_1to5,y=log_citations)) +
        geom_point(alpha=0.3,size=1.8*SK,color="#1F78B4") +
        geom_smooth(method="lm",color="#1F78B4",fill="#1F78B4",
                    alpha=0.15,linewidth=1.2*SK,se=TRUE) +
        labs(title=W("Citation Count vs Repository Quality Index",58),
             x="RQI (0–5)",y="log(Citation Count + 1)") +
        theme_supp() +
        theme(legend.position="none")
    }
  })
  
  output$citation_table_s2 <- renderTable({
    req(has_citations)
    citations_raw %>%
      filter(!is.na(citation_count), !is.na(final_score_1to5)) %>%
      arrange(desc(citation_count), final_score_1to5) %>%
      filter(final_score_1to5 <= 2.5) %>%
      select(repo, type, citation_count, final_score_1to5, days_since_last_commit, doi) %>%
      head(20)
  }, striped = TRUE, hover = TRUE, digits = 2)
  
  # ── 12. Funding (conditional) ────────────────────────────────────────────────
  fund_layout <- list(
    bar_bio   = c(4.2, 0.7), fig5b_bio = c(5,   0.5), lm_dev_bio = c(4.8, 1),
    lm_org_bio= c(4.8, 1),   fig_all   = c(5.2, 0.7), lm_dev_all = c(4.8, 1),
    lm_org_all= c(4.8, 1)
  )
  
  output$funding_ui <- renderUI({
    if (!has_funding) {
      p("Funding data not found (oa_publication_summary_gemma.csv).", style="color:gray;")
    } else {
      uiOutput("fund_plot_ui")
    }
  })
  
  output$fund_plot_ui <- renderUI({
    req(input$fund_plot)
    lay <- fund_layout[[input$fund_plot]]
    if (is.null(lay)) lay <- c(4.8, 1)
    plot_card("funding_plot_out", lay[1], lay[2])
  })
  
  # RQI vs log10(award) scatter with OLS fit (Figs S35–S38)
  funding_lm_plot <- function(dat, cost_col, xname, title) {
    lm_data <- dat %>%
      filter(!is.na(.data[[cost_col]]), !is.na(final_score_1to5), .data[[cost_col]] > 0) %>%
      mutate(log_award=log10(.data[[cost_col]]))
    validate(need(nrow(lm_data) >= 3, "Too few repositories with an award amount to fit a regression."))
    mod <- lm(final_score_1to5~log_award, data=lm_data); ms <- summary(mod)
    beta <- round(coef(mod)[["log_award"]],3); r2 <- round(ms$r.squared,3)
    pval <- round(coef(ms)[2,"Pr(>|t|)"],4)
    sub  <- paste0("β = ",beta," | R² = ",r2," | ",
                   ifelse(pval<0.0001,"p < 0.0001",paste("p =",pval))," | n = ",nrow(lm_data))
    ggplot(lm_data, aes(x=log_award,y=final_score_1to5)) +
      geom_point(alpha=0.55,size=1.5*SK,color="#1F78B4") +
      geom_smooth(method="lm", color="#1F78B4", fill="#1F78B4", alpha=0.15,
                  linewidth=1*SK, se=TRUE) +
      scale_x_continuous(name=xname,
                         labels=function(x) paste0("$",format(10^x,big.mark=",",scientific=FALSE))) +
      labs(title=W(title,58), subtitle=sub, y="RQI (0–5)") +
      theme_supp() +
      theme(axis.text.x=element_text(angle=30,hjust=1))
  }
  
  output$funding_plot_out <- renderPlot(res = APP_RES, {
    req(has_funding)
    
    if (input$fund_plot == "bar_bio") {
      bar_data <- funding_df %>%
        filter(tolower(type)=="bioinformatics",!is.na(final_score_1to5),!is.na(funding_group))
      bar_data$funding_group <- require_two_groups(bar_data$funding_group, "funding-status")
      w  <- wilcox.test(final_score_1to5~funding_group,data=bar_data,exact=FALSE)
      pl <- ifelse(w$p.value<0.0001,"p < 0.0001",paste("p =",round(w$p.value,4)))
      meds <- bar_data %>% group_by(funding_group) %>%
        summarise(median_rqi=median(final_score_1to5,na.rm=TRUE),n=n(),.groups="drop")
      # Bracket goes above the highest raw point (not just the median bars), so it
      # cannot collide with the jittered points or the two-line median labels.
      sig_y <- max(bar_data$final_score_1to5, na.rm=TRUE) * 1.04
      
      ggplot(bar_data,aes(x=funding_group,y=final_score_1to5,fill=funding_group)) +
        geom_bar(stat="summary",fun="median",alpha=0.85,width=0.5) +
        geom_jitter(alpha=0.12,width=0.18,size=1.2*SK,color="grey30") +
        geom_text(data=meds,aes(x=funding_group,y=median_rqi,
                                label=paste0(round(median_rqi,2),"\n(n=",n,")")),
                  vjust=-0.4,fontface="bold",size=TXT_M,inherit.aes=FALSE) +
        sig_bracket(1, 2, sig_y, pl, dy=sig_y*0.05, tick=sig_y*0.02, size=TXT_M, fontface="bold") +
        scale_fill_manual(values=c("Grant Funded"="#1F78B4","Not Grant Funded"="#A6CEE3")) +
        scale_y_continuous(breaks=0:5, expand=expansion(mult=c(0,0.02))) +
        coord_cartesian(ylim=c(0, sig_y*1.15)) +
        labs(title=W("Median RQI by Funding Status (Bioinformatics)",58),
             subtitle="Wilcoxon rank-sum test",x=NULL,y="Median RQI (0–5)") +
        theme_supp() +
        theme(legend.position="none")
      
    } else if (input$fund_plot == "lm_dev_bio") {                      # Fig S37
      funding_lm_plot(filter(funding_df, tolower(type)=="bioinformatics"), "developer_cost_usd",
                      "Developer Award Amount (log₁₀ USD)", "RQI vs Developer Award Amount (Bioinformatics)")
      
    } else if (input$fund_plot == "lm_org_bio") {                      # Fig S38
      funding_lm_plot(filter(funding_df, tolower(type)=="bioinformatics"), "total_cost_usd",
                      "Total Organization Award Amount (log₁₀ USD)", "RQI vs Organization Award Amount (Bioinformatics)")
      
    } else if (input$fund_plot == "lm_dev_all") {                      # Fig S35
      funding_lm_plot(funding_df, "developer_cost_usd",
                      "Developer Award Amount (log₁₀ USD)", "RQI vs Developer Award Amount (All Domains)")
      
    } else if (input$fund_plot == "lm_org_all") {                      # Fig S36
      funding_lm_plot(funding_df, "total_cost_usd",
                      "Total Organization Award Amount (log₁₀ USD)", "RQI vs Organization Award Amount (All Domains)")
      
    } else {
      # fig5b_bio = Fig 4b (bioinformatics); fig_all = Fig S34 (all domains)
      scope_bio <- (input$fund_plot == "fig5b_bio")
      
      base_data <- funding_df %>% filter(!is.na(final_score_1to5))
      if (scope_bio) base_data <- base_data %>% filter(tolower(type) == "bioinformatics")
      
      grp_lv <- if (scope_bio) c("Grant funded","Not grant funded") else c("Grant Funded","Not Grant Funded")
      base_data <- base_data %>%
        mutate(
          recency_1to5  = recency_score_1to5,
          issue_1to5    = issue_score_1to5,
          pop_1to5      = pop_score_1to5,
          funding_group = factor(ifelse(funding_group == "Grant Funded", grp_lv[1], grp_lv[2]),
                                 levels = grp_lv)
        )
      invisible(require_two_groups(base_data$funding_group, "funding-status"))
      
      title_str    <- if (scope_bio) "Grant funding status" else
        W("RQI and components by funding status (all domains)", 58)
      subtitle_str <- W(paste0("Components scaled 0–5  |  Grant sources: ",
                               paste(funded_sources_list, collapse=", ")), 90)
      
      layered_violin(base_data, group="funding_group", title=title_str, subtitle=subtitle_str)$plot
    }
  })
  
  # ── 13. Summary tables ───────────────────────────────────────────────────────
  output$table1_summary <- renderTable(summary_tbl_type, digits = 2)
  output$tableS1_languages <- renderTable(top_languages_tbl)
}


shinyApp(
  ui = ui,
  server = server,
  options = list(host = "0.0.0.0", port = 3838)
)