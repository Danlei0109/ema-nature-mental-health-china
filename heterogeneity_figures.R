## generate_opendata_figures.R
## Produces figures 2–10 from the dataset (available upon request).
## Individual-moderator figures (2-4): 1598 × 1354 px
## City-scale figures (5-7):           1526 × 830 px
## Baseline mental health figures (8-10): 1824 × 628 px

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(lme4)
  library(lmerTest); library(broom.mixed); library(tibble); library(ggplot2)
})

out_dir <- "."

## ── Load & filter ────────────────────────────────────────────
dat <- read_csv("ema_opendata.csv", show_col_types = FALSE)

valid_ids <- dat |>
  filter(!is.na(mh), !is.na(tree)) |>
  group_by(participant_id) |>
  summarise(n_valid = n(), .groups = "drop") |>
  filter(n_valid >= 11) |>
  pull(participant_id)
dat <- dat |> filter(participant_id %in% valid_ids)

## ── Variable preparation ─────────────────────────────────────
dat <- dat |>
  mutate(
    gender            = factor(gender),
    education_3       = factor(education_3,
                               levels = c("Less than high school",
                                          "High school", "Higher education")),
    income_individual = factor(income_individual),
    income_household  = factor(income_household),
    work_status       = factor(work_status),
    ever_rural        = factor(ever_rural),
    chronic_disease   = suppressWarnings(as.numeric(chronic_disease)),
    age               = suppressWarnings(as.numeric(age)),
    bmi               = suppressWarnings(as.numeric(bmi)),
    work_hours_per_day = suppressWarnings(as.numeric(work_hours_per_day)),
    phq_score         = suppressWarnings(as.numeric(phq_score)),
    pss_score         = suppressWarnings(as.numeric(pss_score)),
    who_score         = suppressWarnings(as.numeric(who_score)),
    city_pop_density_log  = suppressWarnings(as.numeric(city_pop_density_log)),
    city_gdp_log          = suppressWarnings(as.numeric(city_gdp_log)),
    city_pop_density_2022 = suppressWarnings(as.numeric(city_pop_density_2022))
  )

## ── Mundlak decompositions ───────────────────────────────────
dat <- dat |>
  mutate(tree_num = c(None=0L, Few=1L, Moderate=2L, Many=3L, Dense=4L)[tree],
         wl_num   = as.integer(wildlife == "Yes"),
         wa_num   = as.integer(water    == "Yes")) |>
  group_by(participant_id) |>
  mutate(tree_mean = mean(tree_num, na.rm = TRUE),
         tree_diff = tree_num - tree_mean,
         wl_mean   = mean(wl_num,   na.rm = TRUE),
         wl_diff   = wl_num   - wl_mean,
         wa_mean   = mean(wa_num,   na.rm = TRUE),
         wa_diff   = wa_num   - wa_mean) |>
  ungroup()

## ── Quartile groups (city) ───────────────────────────────────
make_q <- function(x) {
  factor(dplyr::ntile(x, 4), 1:4,
         labels = c("Q1 (lowest)","Q2","Q3","Q4 (highest)"))
}
dat <- dat |>
  mutate(
    prp_q  = factor(ifelse(!is.na(city_pop_density_log),
                           as.character(make_q(city_pop_density_log)), NA_character_),
                    levels = c("Q1 (lowest)","Q2","Q3","Q4 (highest)")),
    gdp_q  = factor(ifelse(!is.na(city_gdp_log),
                           as.character(make_q(city_gdp_log)), NA_character_),
                    levels = c("Q1 (lowest)","Q2","Q3","Q4 (highest)")),
    dens_q = factor(ifelse(!is.na(city_pop_density_2022),
                           as.character(make_q(city_pop_density_2022)), NA_character_),
                    levels = c("Q1 (lowest)","Q2","Q3","Q4 (highest)"))
  )

## ── Mental health groups ─────────────────────────────────────
dat <- dat |>
  mutate(
    phq_group = factor(case_when(
      !is.na(phq_score) & phq_score >= 10 ~ "Moderate/severe depressive symptoms",
      !is.na(phq_score) & phq_score <  10 ~ "None/mild depressive symptoms",
      TRUE ~ NA_character_
    ), levels = c("Moderate/severe depressive symptoms",
                  "None/mild depressive symptoms")),

    pss_group = factor(case_when(
      !is.na(pss_score) & pss_score >= 14 ~ "Moderate/high perceived stress",
      !is.na(pss_score) & pss_score <= 13 ~ "Low perceived stress",
      TRUE ~ NA_character_
    ), levels = c("Moderate/high perceived stress", "Low perceived stress")),

    who_group = factor(case_when(
      !is.na(who_score) & who_score <  13 ~ "Low well-being",
      !is.na(who_score) & who_score >= 13 ~ "Higher well-being",
      TRUE ~ NA_character_
    ), levels = c("Low well-being", "Higher well-being"))
  )

## ── Controls ─────────────────────────────────────────────────
ctrl_rhs <- paste(
  "gender + age + education_3",
  "income_household + income_individual",
  "chronic_disease + bmi",
  "ever_rural + city_pop_density_log + city_gdp_log",
  "work_status + work_hours_per_day",
  sep = " + "
)

## ── Helpers ──────────────────────────────────────────────────
fmt_p <- function(p, digits = 3, thresh = 0.001) {
  ifelse(is.na(p), "",
         ifelse(p < thresh, "<0.001", formatC(p, format = "f", digits = digits)))
}

fit_subgroup <- function(data, group_var, diff_var, mean_var, ctrl, rand) {
  fml <- as.formula(paste0("mh ~ ", diff_var, " + ", mean_var,
                           " + ", ctrl, " + ", rand))
  data |>
    filter(!is.na(.data[[group_var]])) |>
    group_by(.data[[group_var]]) |>
    group_modify(~{
      d <- .x |> filter(complete.cases(across(all.vars(fml))))
      m <- lmer(fml, data = d, REML = TRUE)
      tidy(m, effects = "fixed", conf.int = TRUE) |>
        filter(term == diff_var) |>
        transmute(group = as.character(.y[[1]]),
                  estimate, std.error, conf.low, conf.high, p.value,
                  n_obs = nobs(m))
    }) |>
    ungroup()
}

## Wald P_int (for city moderators)
pint_wald <- function(data, diff_var, mean_var, mod_cont, ctrl, rand) {
  fml <- as.formula(paste0("mh ~ ", diff_var, " * ", mod_cont,
                           " + ", mean_var, " + ", ctrl, " + ", rand))
  d <- data |>
    filter(!is.na(.data[[mod_cont]])) |>
    filter(complete.cases(across(all.vars(fml))))
  if (nrow(d) == 0) return(NA_real_)
  m <- lmer(fml, data = d, REML = TRUE)
  sm <- coef(summary(m))
  tn <- paste0(diff_var, ":", mod_cont)
  if (!tn %in% rownames(sm)) return(NA_real_)
  as.numeric(sm[tn, "Pr(>|t|)"])
}

## LRT P_int (for mental health moderators)
pint_lrt <- function(data, diff_var, mean_var, mod_cont, ctrl, rand) {
  f0 <- as.formula(paste0("mh ~ ", diff_var, " + ", mean_var,
                          " + ", mod_cont, " + ", ctrl, " + ", rand))
  f1 <- as.formula(paste0("mh ~ ", diff_var, " * ", mod_cont,
                          " + ", mean_var, " + ", ctrl, " + ", rand))
  d <- data |>
    filter(!is.na(.data[[mod_cont]])) |>
    filter(complete.cases(across(all.vars(f1))))
  if (n_distinct(d[[mod_cont]]) < 2) return(NA_real_)
  lrt <- anova(lmer(f0, data = d, REML = FALSE),
               lmer(f1, data = d, REML = FALSE))
  lrt$`Pr(>Chisq)`[2]
}

## ── Forest plot renderer (shared style) ──────────────────────
## text_scale multiplies all geom_text sizes; base_sz controls axis/tick text
render_forest <- function(plot_df, point_colour, x_lim = c(-4, 6),
                          text_scale = 1.0, base_sz = 12) {
  lbl_sz  <- 3.2 * text_scale
  data_sz <- 3.0 * text_scale
  pt_sz   <- 2.7 * text_scale

  x_range      <- diff(x_lim)
  left_gutter  <- 0.70 * x_range
  right_gutter <- 0.75 * x_range
  ci_x <- x_lim[2] + 0.40 * right_gutter
  p_x  <- x_lim[2] + 0.62 * right_gutter
  n_x  <- x_lim[2] + 0.90 * right_gutter
  y_hdr_text <- min(plot_df$row_id) - 1.05
  y_hdr_line <- min(plot_df$row_id) - 0.70
  last_row   <- max(plot_df$row_id[!plot_df$is_header & !is.na(plot_df$conf.low)])
  y_bot      <- last_row + 0.35 * text_scale
  bot_line_y <- y_bot - 0.10 * text_scale

  ggplot(plot_df, aes(x = estimate, y = row_id)) +
    annotate("segment", x = 0, xend = 0,
             y = y_hdr_line + 0.25, yend = bot_line_y,
             linetype = 2, linewidth = 0.45, colour = "black") +
    geom_errorbar(data = subset(plot_df, !is_header),
                  aes(xmin = conf.low, xmax = conf.high),
                  width = 0, linewidth = 0.55, colour = "black",
                  orientation = "y") +
    geom_point(data = subset(plot_df, !is_header),
               shape = 15, size = pt_sz, colour = point_colour) +
    geom_text(aes(x = x_lim[1] - left_gutter + 0.02 * x_range,
                  label = label_show,
                  fontface = ifelse(is_header, "bold", "plain")),
              hjust = 0, size = lbl_sz) +
    geom_text(aes(x = ci_x, label = ci_text),   hjust = 1, size = data_sz) +
    geom_text(aes(x = p_x,  label = pcol_text), hjust = 1, size = data_sz) +
    geom_text(aes(x = n_x,  label = n_text),    hjust = 1, size = data_sz) +
    annotate("segment",
             x = x_lim[1] - left_gutter, xend = x_lim[2] + right_gutter,
             y = y_hdr_line, yend = y_hdr_line,
             linewidth = 0.6, colour = "black") +
    annotate("segment",
             x = x_lim[1] - left_gutter, xend = x_lim[2] + right_gutter,
             y = bot_line_y, yend = bot_line_y,
             linewidth = 0.6, colour = "black") +
    annotate("text", x = ci_x, y = y_hdr_text,
             label = "AME",   hjust = 1, size = lbl_sz, fontface = "bold") +
    annotate("text", x = p_x,  y = y_hdr_text,
             label = "P_int", hjust = 1, size = lbl_sz, fontface = "bold") +
    annotate("text", x = n_x,  y = y_hdr_text,
             label = "N",     hjust = 1, size = lbl_sz, fontface = "bold") +
    scale_y_reverse(breaks = NULL, expand = expansion(add = c(0, 0))) +
    scale_x_continuous(
      limits = c(x_lim[1] - left_gutter, x_lim[2] + right_gutter),
      breaks = seq(x_lim[1], x_lim[2], by = 2)) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(clip = "off",
                    ylim = c(y_hdr_text - 0.25, y_bot + 0.15)) +
    theme_classic(base_size = base_sz) +
    theme(axis.line.x  = element_blank(),
          axis.line.y  = element_blank(),
          plot.margin  = margin(35, 10, 6, 10))
}

## ── Forest df builder (shared) ────────────────────────────────
build_forest_df <- function(res_all, p_int_df, mod_order,
                            q_sort = FALSE, grp_levels_list = NULL) {
  res_all <- res_all |> left_join(
    p_int_df |> mutate(p_int_txt = formatC(p_int, format = "f", digits = 3)),
    by = "moderator")

  header_rows <- res_all |>
    distinct(moderator, p_int_txt) |>
    mutate(label_show = moderator,
           pcol_text  = {
             p_num <- sub(".*?([0-9]*\\.?[0-9]+(?:e-?[0-9]+)?)\\s*$",
                          "\\1", p_int_txt)
             fmt_p(suppressWarnings(as.numeric(p_num)))
           },
           is_header = TRUE,
           estimate = NA_real_, conf.low = NA_real_,
           conf.high = NA_real_, n_obs = NA_integer_)

  sub_rows <- res_all |>
    mutate(label_show = paste0("   ", group),
           pcol_text  = "", is_header = FALSE)

  bind_rows(lapply(mod_order, function(m) {
    hdr  <- header_rows |> filter(moderator == m)
    subs <- sub_rows    |> filter(moderator == m)
    if (q_sort) {
      subs <- subs |>
        mutate(q_ord = case_when(
          grepl("^Q1", group) ~ 1L, grepl("^Q2", group) ~ 2L,
          grepl("^Q3", group) ~ 3L, grepl("^Q4", group) ~ 4L,
          TRUE ~ 99L)) |>
        arrange(q_ord) |> select(-q_ord)
    } else if (!is.null(grp_levels_list[[m]])) {
      subs <- subs |> arrange(match(group, grp_levels_list[[m]]))
    }
    bind_rows(hdr, subs)
  })) |>
    mutate(
      ci_text = ifelse(is_header, "",
                       sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)),
      n_text  = ifelse(is_header, "", format(n_obs, big.mark = ",")),
      row_id  = row_number()
    )
}

## ============================================================
## PART A — CITY-SCALE HETEROGENEITY (Figures 5, 6, 7)
## 1526 × 830 px
## ============================================================

city_mod_order <- c("Permanent resident population (quartiles)",
                    "Gross regional product (quartiles)",
                    "Population density (quartiles)")

run_city_het <- function(diff_var, mean_var, rand_part, point_color, outfile) {
  res_prp  <- fit_subgroup(dat, "prp_q",  diff_var, mean_var, ctrl_rhs, rand_part) |>
    mutate(moderator = "Permanent resident population (quartiles)")
  res_gdp  <- fit_subgroup(dat, "gdp_q",  diff_var, mean_var, ctrl_rhs, rand_part) |>
    mutate(moderator = "Gross regional product (quartiles)")
  res_dens <- fit_subgroup(dat, "dens_q", diff_var, mean_var, ctrl_rhs, rand_part) |>
    mutate(moderator = "Population density (quartiles)")
  res_all  <- bind_rows(res_prp, res_gdp, res_dens)

  p_int_df <- tibble(
    moderator = city_mod_order,
    p_int = c(
      pint_wald(dat, diff_var, mean_var, "city_pop_density_log",  ctrl_rhs, rand_part),
      pint_wald(dat, diff_var, mean_var, "city_gdp_log",          ctrl_rhs, rand_part),
      pint_wald(dat, diff_var, mean_var, "city_pop_density_2022", ctrl_rhs, rand_part)
    )
  )

  plot_df <- build_forest_df(res_all, p_int_df, city_mod_order, q_sort = TRUE)
  fig     <- render_forest(plot_df, point_color, text_scale = 1.4, base_sz = 14)
  ggsave(outfile, fig, width = 1526, height = 830, units = "px", dpi = 150)
  cat("Saved:", outfile, "\n")
  invisible(fig)
}

run_city_het("tree_diff", "tree_mean", "(1 + tree_diff | participant_id)",
             "#0B6623",
             file.path(out_dir, "figure5_tree_city_heterogeneity_opendata.png"))

run_city_het("wl_diff", "wl_mean", "(1 + wl_diff | participant_id)",
             "#B45309",
             file.path(out_dir, "figure6_wildlife_city_heterogeneity_opendata.png"))

run_city_het("wa_diff", "wa_mean", "(1 + wa_diff | participant_id)",
             "#1E3A8A",
             file.path(out_dir, "figure7_water_city_heterogeneity_opendata.png"))


## ============================================================
## PART B — BASELINE MENTAL HEALTH HETEROGENEITY (Figures 8, 9, 10)
## 1824 × 628 px
## ============================================================

mh_mod_order    <- c("Depressive symptoms", "Perceived stress", "Well-being")
grp_levels_list <- list(
  "Depressive symptoms" = levels(dat$phq_group),
  "Perceived stress"    = levels(dat$pss_group),
  "Well-being"          = levels(dat$who_group)
)

run_mh_het <- function(diff_var, mean_var, rand_part, point_color, outfile) {
  res_phq <- fit_subgroup(dat, "phq_group", diff_var, mean_var, ctrl_rhs, rand_part) |>
    mutate(moderator = "Depressive symptoms")
  res_pss <- fit_subgroup(dat, "pss_group", diff_var, mean_var, ctrl_rhs, rand_part) |>
    mutate(moderator = "Perceived stress")
  res_who <- fit_subgroup(dat, "who_group", diff_var, mean_var, ctrl_rhs, rand_part) |>
    mutate(moderator = "Well-being")
  res_all <- bind_rows(res_phq, res_pss, res_who)

  p_int_df <- tibble(
    moderator = mh_mod_order,
    p_int = c(
      pint_lrt(dat, diff_var, mean_var, "phq_score", ctrl_rhs, rand_part),
      pint_lrt(dat, diff_var, mean_var, "pss_score", ctrl_rhs, rand_part),
      pint_lrt(dat, diff_var, mean_var, "who_score", ctrl_rhs, rand_part)
    )
  )

  plot_df <- build_forest_df(res_all, p_int_df, mh_mod_order,
                             grp_levels_list = grp_levels_list)
  fig     <- render_forest(plot_df, point_color, text_scale = 1.4, base_sz = 14)
  ggsave(outfile, fig, width = 1824, height = 628, units = "px", dpi = 150)
  cat("Saved:", outfile, "\n")
  invisible(fig)
}

run_mh_het("tree_diff", "tree_mean", "(1 + tree_diff | participant_id)",
           "#0B6623",
           file.path(out_dir, "figure8_tree_mentalhealth_heterogeneity_opendata.png"))

run_mh_het("wl_diff", "wl_mean", "(1 + wl_diff | participant_id)",
           "#B45309",
           file.path(out_dir, "figure9_wildlife_mentalhealth_heterogeneity_opendata.png"))

run_mh_het("wa_diff", "wa_mean", "(1 + wa_diff | participant_id)",
           "#1E3A8A",
           file.path(out_dir, "figure10_water_mentalhealth_heterogeneity_opendata.png"))


## ============================================================
## PART C — INDIVIDUAL-LEVEL HETEROGENEITY (Figures 2, 3, 4)
## 1598 × 1354 px
## ============================================================

## Helper: drop named terms from ctrl_rhs string
drop_terms <- function(rhs, drop = NULL) {
  if (is.null(drop)) return(rhs)
  terms <- trimws(unlist(strsplit(rhs, "\\+")))
  paste(terms[!terms %in% drop & terms != ""], collapse = " + ")
}

## fit_subgroup with optional drop_ctrl (wraps the generic fit_subgroup)
fit_subgroup_drop <- function(data, group_var, diff_var, mean_var,
                               ctrl, rand, drop_ctrl = NULL) {
  fit_subgroup(data, group_var, diff_var, mean_var,
               drop_terms(ctrl, drop_ctrl), rand)
}

## LRT interaction test with optional drop_ctrl
test_interaction_drop <- function(data, group_var, diff_var, mean_var,
                                   ctrl, rand, drop_ctrl = NULL) {
  rhs <- drop_terms(ctrl, drop_ctrl)
  f0  <- as.formula(paste0("mh ~ ", diff_var, " + ", mean_var,
                            " + ", rhs, " + ", rand))
  f1  <- as.formula(paste0("mh ~ ", diff_var, " * ", group_var,
                            " + ", mean_var, " + ", rhs, " + ", rand))
  d   <- data |> filter(!is.na(.data[[group_var]]))
  lrt <- anova(lmer(f0, data = d, REML = FALSE),
               lmer(f1, data = d, REML = FALSE))
  lrt$`Pr(>Chisq)`[2]
}

## Subgroup variables
dat <- dat |>
  mutate(
    gender_group = factor(gender),

    age_group = factor(case_when(
      age <  45 ~ "<45",
      age >= 45 ~ "≥45"
    ), levels = c("<45", "≥45")),

    rural_group = factor(trimws(as.character(ever_rural)),
                         levels = c("Never lived rural", "Lived rural")),

    student_status = factor(case_when(
      trimws(as.character(work_status)) == "Student" ~ "Student",
      trimws(as.character(work_status)) %in%
        c("Part-time","Freelance","Full-time","Unemployed") ~ "Non-student"
    ), levels = c("Student", "Non-student")),

    family_wealth = factor(case_when(
      grepl("<\\s*10k",           as.character(income_household), ignore.case=TRUE) ~ "<10k",
      grepl("10\\s*[-–]\\s*30k", as.character(income_household), ignore.case=TRUE) ~ "10–30k",
      grepl("30\\s*[-–]\\s*60k|60\\s*[-–]\\s*100k|>\\s*100k",
            as.character(income_household), ignore.case=TRUE)                       ~ "≥30k"
    ), levels = c("<10k", "10–30k", "≥30k")),

    indi_income_clean = case_when(
      income_individual %in% c("<3k")    ~ "<3k",
      income_individual %in% c("3–6k")  ~ "3-6k",
      income_individual %in% c("6–10k") ~ "6-10k",
      income_individual %in% c("10–50k") ~ "10-50k",
      income_individual %in% c(">50k")  ~ ">50k",
      .default = as.character(income_individual)
    )
  ) |>
  mutate(
    income_group_ns = factor(case_when(
      student_status == "Non-student" &
        indi_income_clean %in% c("<3k","3-6k")    ~ "Low income",
      student_status == "Non-student" &
        indi_income_clean == "6-10k"               ~ "Middle income",
      student_status == "Non-student" &
        indi_income_clean %in% c("10-50k",">50k") ~ "High income"
    ), levels = c("Low income","Middle income","High income")),

    worktime_group_ns = factor(case_when(
      student_status == "Non-student" &
        !is.na(work_hours_per_day) & work_hours_per_day <= 8 ~ "≤8 h/day",
      student_status == "Non-student" &
        !is.na(work_hours_per_day) & work_hours_per_day >  8 ~ ">8 h/day"
    ), levels = c("≤8 h/day", ">8 h/day"))
  )

indiv_mod_order <- c(
  "Gender", "Age", "Prior rural residence", "Student status",
  "Family income", "Individual income (non-students)", "Worktime (non-students)"
)

## Label rename map (matches analysis.R)
label_rename <- list(
  "FEMALE" = "Female", "MALE" = "Male",
  "PHQ_low" = "Low", "PHQ_high" = "High"
)

run_indiv_het <- function(diff_var, mean_var, rand_part, point_color, outfile) {
  dat_ns <- dat |> filter(student_status == "Non-student")

  res <- bind_rows(
    fit_subgroup_drop(dat |> filter(!is.na(gender_group)),
                      "gender_group",  diff_var, mean_var, ctrl_rhs, rand_part,
                      drop_ctrl = "gender") |>
      mutate(moderator = "Gender"),

    fit_subgroup_drop(dat, "age_group", diff_var, mean_var, ctrl_rhs, rand_part) |>
      mutate(moderator = "Age"),

    fit_subgroup_drop(dat |> filter(!is.na(rural_group)),
                      "rural_group",   diff_var, mean_var, ctrl_rhs, rand_part,
                      drop_ctrl = "ever_rural") |>
      mutate(moderator = "Prior rural residence"),

    fit_subgroup_drop(dat |> filter(!is.na(student_status)),
                      "student_status", diff_var, mean_var, ctrl_rhs, rand_part,
                      drop_ctrl = "work_status") |>
      mutate(moderator = "Student status"),

    fit_subgroup_drop(dat |> filter(!is.na(family_wealth)),
                      "family_wealth",  diff_var, mean_var, ctrl_rhs, rand_part,
                      drop_ctrl = "income_household") |>
      mutate(moderator = "Family income"),

    fit_subgroup_drop(dat_ns |> filter(!is.na(income_group_ns)),
                      "income_group_ns", diff_var, mean_var, ctrl_rhs, rand_part,
                      drop_ctrl = c("work_status","income_individual")) |>
      mutate(moderator = "Individual income (non-students)"),

    fit_subgroup_drop(dat_ns |> filter(!is.na(worktime_group_ns)),
                      "worktime_group_ns", diff_var, mean_var, ctrl_rhs, rand_part,
                      drop_ctrl = c("work_status","work_hours_per_day")) |>
      mutate(moderator = "Worktime (non-students)")
  )

  p_int_df <- tibble(
    moderator = indiv_mod_order,
    p_int = c(
      test_interaction_drop(dat |> filter(!is.na(gender_group)),
                            "gender_group",    diff_var, mean_var, ctrl_rhs, rand_part, "gender"),
      test_interaction_drop(dat, "age_group",  diff_var, mean_var, ctrl_rhs, rand_part),
      test_interaction_drop(dat |> filter(!is.na(rural_group)),
                            "rural_group",     diff_var, mean_var, ctrl_rhs, rand_part, "ever_rural"),
      test_interaction_drop(dat |> filter(!is.na(student_status)),
                            "student_status",  diff_var, mean_var, ctrl_rhs, rand_part, "work_status"),
      test_interaction_drop(dat |> filter(!is.na(family_wealth)),
                            "family_wealth",   diff_var, mean_var, ctrl_rhs, rand_part, "income_household"),
      test_interaction_drop(dat_ns |> filter(!is.na(income_group_ns)),
                            "income_group_ns", diff_var, mean_var, ctrl_rhs, rand_part,
                            c("work_status","income_individual")),
      test_interaction_drop(dat_ns |> filter(!is.na(worktime_group_ns)),
                            "worktime_group_ns", diff_var, mean_var, ctrl_rhs, rand_part,
                            c("work_status","work_hours_per_day"))
    )
  )

  ## Apply label renames to group column
  res <- res |>
    mutate(group = {
      v <- group
      for (nm in names(label_rename))
        v <- ifelse(grepl(nm, v, ignore.case = TRUE), label_rename[[nm]], v)
      v
    })

  plot_df <- build_forest_df(res, p_int_df, indiv_mod_order)
  fig     <- render_forest(plot_df, point_color, text_scale = 1.6, base_sz = 16)
  ggsave(outfile, fig, width = 1598, height = 1354, units = "px", dpi = 150)
  cat("Saved:", outfile, "\n")
  invisible(fig)
}

## Figure 2 — tree × individual moderators
run_indiv_het("tree_diff", "tree_mean", "(1 + tree_diff | participant_id)",
              "#0B6623",
              file.path(out_dir, "figure2_tree_heterogeneity_opendata.png"))

## Figure 3 — wildlife × individual moderators
run_indiv_het("wl_diff", "wl_mean", "(1 + wl_diff | participant_id)",
              "#B45309",
              file.path(out_dir, "figure3_wildlife_heterogeneity_opendata.png"))

## Figure 4 — water × individual moderators
run_indiv_het("wa_diff", "wa_mean", "(1 + wa_diff | participant_id)",
              "#1E3A8A",
              file.path(out_dir, "figure4_water_heterogeneity_opendata.png"))

cat("All figures complete.\n")

cat("All open-data figures complete.\n")
