## ============================================================
## analysis.R
## Replication code for:
##   "Daily nature contact and mental health across 55 Chinese cities"
##
## Requires: the dataset file (available upon request) in the same directory.
## R >= 4.1.0 recommended.
## ============================================================

options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lme4)
  library(lmerTest)
  library(marginaleffects)
  library(ggplot2)
  library(patchwork)
  library(broom.mixed)
  library(tibble)
  library(writexl)
})

## ============================================================
## 0. Load data
## ============================================================

dat <- read_csv("ema_opendata.csv", show_col_types = FALSE) %>%
  mutate(participant_id = as.character(participant_id))

## ============================================================
## 1. Variable preparation
## ============================================================

tree_levels <- c("None", "Few", "Moderate", "Many", "Dense")

dat <- dat %>%
  mutate(
    ## Ordered factor for tree density (1 = None, 5 = Dense)
    tree_f   = factor(tree,   levels = tree_levels),
    tree_num = as.numeric(factor(tree, levels = tree_levels)),

    ## Binary indicators for wildlife and water presence
    wildlife_bin = case_when(
      wildlife %in% c("No", "Not sure") ~ 0L,
      wildlife == "Yes"                 ~ 1L
    ),
    water_bin = case_when(
      water %in% c("No", "Not sure") ~ 0L,
      water == "Yes"                 ~ 1L
    ),

    ## Categorical covariates
    gender             = factor(gender),
    education_3        = factor(education_3,
                               levels = c("Less than high school",
                                          "High school",
                                          "Higher education")),
    income_individual  = factor(income_individual),
    income_household   = factor(income_household),
    work_status        = factor(work_status),
    ever_rural         = factor(ever_rural),

    ## Numeric covariates (coerce, suppressing parse warnings)
    chronic_disease      = suppressWarnings(as.numeric(chronic_disease)),
    age                  = suppressWarnings(as.numeric(age)),
    bmi                  = suppressWarnings(as.numeric(bmi)),
    city_pop_density_log = suppressWarnings(as.numeric(city_pop_density_log)),
    city_gdp_log         = suppressWarnings(as.numeric(city_gdp_log)),
    work_hours_per_day   = suppressWarnings(as.numeric(work_hours_per_day))
  )

## Exclude participants with fewer than 11 valid EMA assessments
## (requires non-missing mh and tree to count as a valid response)
valid_ids <- dat |>
  filter(!is.na(mh), !is.na(tree_f)) |>
  group_by(participant_id) |>
  summarise(n_valid = n(), .groups = "drop") |>
  filter(n_valid >= 11) |>
  pull(participant_id)

dat <- dat |> filter(participant_id %in% valid_ids)

## Mundlak decomposition: within-person deviation and person mean
dat <- dat %>%
  group_by(participant_id) %>%
  mutate(
    tree_mean     = mean(tree_num,    na.rm = TRUE),
    tree_diff     = tree_num     - tree_mean,
    wildlife_mean = mean(wildlife_bin, na.rm = TRUE),
    wildlife_diff = wildlife_bin - wildlife_mean,
    water_mean    = mean(water_bin,   na.rm = TRUE),
    water_diff    = water_bin    - water_mean
  ) %>%
  ungroup()

## ============================================================
## 2. Adjusted-model covariates (right-hand side string)
## ============================================================

ctrl_vars <- paste(
  "gender + age + education_3",
  "income_individual + income_household",
  "chronic_disease + bmi",
  "ever_rural + city_gdp_log + city_pop_density_log",
  "work_status + work_hours_per_day",
  sep = " + "
)

## ============================================================
## 3. Table 1 — Baseline associations (random-intercept models)
## ============================================================

## Trees
m_tree_unadj <- lmer(mh ~ tree_f + (1 | participant_id), data = dat)
m_tree_adj   <- lmer(
  as.formula(paste("mh ~ tree_f +", ctrl_vars, "+ (1 | participant_id)")),
  data = dat
)

## Wildlife
m_wildlife_unadj <- lmer(mh ~ wildlife_bin + (1 | participant_id), data = dat)
m_wildlife_adj   <- lmer(
  as.formula(paste("mh ~ wildlife_bin +", ctrl_vars, "+ (1 | participant_id)")),
  data = dat
)

## Water
m_water_unadj <- lmer(mh ~ water_bin + (1 | participant_id), data = dat)
m_water_adj   <- lmer(
  as.formula(paste("mh ~ water_bin +", ctrl_vars, "+ (1 | participant_id)")),
  data = dat
)

## Export Table 1
tidy_model <- function(mod) {
  tidy(mod, effects = "fixed", conf.int = TRUE) %>%
    mutate(n_obs = nobs(mod))
}

write_xlsx(
  list(
    tree_unadj     = tidy_model(m_tree_unadj),
    tree_adj       = tidy_model(m_tree_adj),
    wildlife_unadj = tidy_model(m_wildlife_unadj),
    wildlife_adj   = tidy_model(m_wildlife_adj),
    water_unadj    = tidy_model(m_water_unadj),
    water_adj      = tidy_model(m_water_adj)
  ),
  path = "results_table1.xlsx"
)

## ============================================================
## 4. Table 2 — Mundlak within/between decomposition
##    (random intercept + random slope for within-person term)
## ============================================================

fit_mundlak <- function(diff_var, mean_var, data_full, data_adj) {
  rand <- sprintf("(1 + %s | participant_id)", diff_var)
  f_u  <- as.formula(sprintf("mh ~ %s + %s + %s", diff_var, mean_var, rand))
  f_a  <- as.formula(sprintf("mh ~ %s + %s + %s + %s",
                              diff_var, mean_var, ctrl_vars, rand))
  list(
    unadj = lmer(f_u, data = data_full, REML = TRUE),
    adj   = lmer(f_a, data = data_adj,  REML = TRUE)
  )
}

mnd_tree     <- fit_mundlak("tree_diff",     "tree_mean",     dat, dat)
mnd_wildlife <- fit_mundlak("wildlife_diff", "wildlife_mean", dat, dat)
mnd_water    <- fit_mundlak("water_diff",    "water_mean",    dat, dat)

write_xlsx(
  list(
    tree_unadj     = tidy(mnd_tree$unadj,     conf.int = TRUE) %>% mutate(n_obs = nobs(mnd_tree$unadj)),
    tree_adj       = tidy(mnd_tree$adj,        conf.int = TRUE) %>% mutate(n_obs = nobs(mnd_tree$adj)),
    wildlife_unadj = tidy(mnd_wildlife$unadj,  conf.int = TRUE) %>% mutate(n_obs = nobs(mnd_wildlife$unadj)),
    wildlife_adj   = tidy(mnd_wildlife$adj,    conf.int = TRUE) %>% mutate(n_obs = nobs(mnd_wildlife$adj)),
    water_unadj    = tidy(mnd_water$unadj,     conf.int = TRUE) %>% mutate(n_obs = nobs(mnd_water$unadj)),
    water_adj      = tidy(mnd_water$adj,       conf.int = TRUE) %>% mutate(n_obs = nobs(mnd_water$adj))
  ),
  path = "results_table2.xlsx"
)

## ============================================================
## 5. Figure 1 — Violin + prediction plots
## ============================================================

make_pred_df_factor <- function(mod, var) {
  predictions(mod, variables = var) %>%
    as.data.frame() %>%
    group_by(.data[[var]]) %>%
    summarise(
      estimate  = mean(estimate),
      conf.low  = mean(conf.low),
      conf.high = mean(conf.high),
      .groups   = "drop"
    )
}

make_pred_df_binary <- function(mod, var, labels) {
  predictions(mod, variables = var) %>%
    as.data.frame() %>%
    group_by(.data[[var]]) %>%
    summarise(
      estimate  = mean(estimate),
      conf.low  = mean(conf.low),
      conf.high = mean(conf.high),
      .groups   = "drop"
    ) %>%
    mutate(!!var := factor(.data[[var]], labels = labels))
}

pred_tree    <- make_pred_df_factor(m_tree_adj,    "tree_f")
pred_wildlife <- make_pred_df_binary(m_wildlife_adj, "wildlife_bin",
                                     c("No/Not sure", "Yes"))
pred_water   <- make_pred_df_binary(m_water_adj,   "water_bin",
                                    c("No/Not sure", "Yes"))

dat_adj <- dat %>%
  filter(!is.na(gender), !is.na(age), !is.na(education_3),
         !is.na(income_individual), !is.na(income_household),
         !is.na(chronic_disease), !is.na(bmi), !is.na(ever_rural),
         !is.na(city_pop_density_log), !is.na(city_gdp_log),
         !is.na(work_status), !is.na(work_hours_per_day))

diff_tree     <- round(pred_tree$estimate[5]     - pred_tree$estimate[1],    2)
diff_wildlife <- round(pred_wildlife$estimate[2]  - pred_wildlife$estimate[1], 2)
diff_water    <- round(pred_water$estimate[2]     - pred_water$estimate[1],   2)

p1 <- ggplot() +
  geom_violin(data = dat_adj,
              aes(x = tree_f, y = mh),
              fill = "#A5D6A7", color = NA, alpha = 0.3) +
  geom_jitter(data = dat_adj,
              aes(x = tree_f, y = mh),
              width = 0.2, size = 0.3, alpha = 0.02, color = "#2E7D32") +
  geom_line(data = pred_tree,
            aes(x = tree_f, y = estimate, group = 1),
            color = "#0B6623", linewidth = 1.2) +
  geom_errorbar(data = pred_tree,
                aes(x = tree_f, ymin = conf.low, ymax = conf.high),
                width = 0.08, linewidth = 0.7) +
  geom_point(data = pred_tree,
             aes(x = tree_f, y = estimate),
             size = 3.5, shape = 21, fill = "white",
             color = "#0B6623", stroke = 1.3) +
  annotate("segment",
           x = 1.1, xend = 4.9, y = 35, yend = 35,
           arrow = arrow(length = unit(0.15, "cm")), color = "grey30") +
  annotate("text", x = 3, y = 39,
           label = paste0("Total Increase: +", diff_tree),
           fontface = "bold", size = 3.5, color = "#0B6623") +
  scale_x_discrete(labels = c("None", "Few", "Moderate", "Many", "Dense")) +
  coord_cartesian(ylim = c(-15, 45)) +
  labs(title = "A. Seeing Trees", x = NULL, y = "Mental well-being (mh)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "sans"))

make_binary_plot <- function(dat_plot, pred_df, x_var, x_labels,
                             fill_col, point_col, title, diff_val) {
  dat_plot <- dat_plot %>%
    filter(!is.na(.data[[x_var]])) %>%
    mutate(x_plot = factor(.data[[x_var]], labels = x_labels))

  ggplot() +
    geom_violin(data = dat_plot,
                aes(x = x_plot, y = mh),
                fill = fill_col, color = NA, alpha = 0.3) +
    geom_jitter(data = dat_plot,
                aes(x = x_plot, y = mh),
                width = 0.2, size = 0.3, alpha = 0.02, color = point_col) +
    geom_line(data = pred_df,
              aes(x = .data[[x_var]], y = estimate, group = 1),
              color = point_col, linewidth = 1.2) +
    geom_errorbar(data = pred_df,
                  aes(x = .data[[x_var]], ymin = conf.low, ymax = conf.high),
                  width = 0.05, linewidth = 0.7) +
    geom_point(data = pred_df,
               aes(x = .data[[x_var]], y = estimate),
               size = 3.5, shape = 21, fill = "white",
               color = point_col, stroke = 1.3) +
    annotate("segment",
             x = 1.1, xend = 1.9, y = 35, yend = 35,
             arrow = arrow(length = unit(0.15, "cm")), color = "grey30") +
    annotate("text", x = 1.5, y = 39,
             label = paste0("Diff: +", diff_val),
             fontface = "bold", size = 3.5, color = point_col) +
    coord_cartesian(ylim = c(-15, 45)) +
    labs(title = title, x = NULL, y = "Mental well-being (mh)") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line = element_line(color = "black"),
          text = element_text(family = "sans"))
}

p2 <- make_binary_plot(
  dat_adj, pred_wildlife, "wildlife_bin",
  c("No/Not sure", "Yes"),
  "#FFCC80", "#B45309",
  "B. Seeing/Hearing Wildlife", diff_wildlife
)

p3 <- make_binary_plot(
  dat_adj, pred_water, "water_bin",
  c("No/Not sure", "Yes"),
  "#90CAF9", "#1E3A8A",
  "C. Seeing/Hearing Water", diff_water
)

fig1 <- p1 + p2 + p3 + plot_layout(ncol = 3)
ggsave("figure1.pdf", fig1, width = 12, height = 4.5)

cat("All analyses complete. Output files written to working directory.\n")
