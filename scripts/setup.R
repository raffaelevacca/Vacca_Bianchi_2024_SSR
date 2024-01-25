## INFO ----
##
## Project setup: packages, functions, objects
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Packages   ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

library(broom.mixed)
library(broom)
library(clusrank)
library(egor)
library(ggeffects)
library(ggh4x)
library(ggpubr)
library(ggraph)
library(igraph)
library(janitor)
library(kableExtra)
library(lme4)
library(patchwork)
library(pscl)
library(sessioninfo)
library(skimr)
library(tidyverse)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Functions                                                                ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Set appropriate vertex attribute values for ego node in igraph that includes ego
set_ego_attr <- function(gr, alter.data) {
  
  # Find ego node
  ind <- which(V(gr)$name == "ego")
  
  # If there's no ego node, stop with error
  if (length(ind) == 0) stop("No node called `ego` in graph")
  
  # Contact frequency: maximum + 1 
  V(gr)$freq.rec.num[ind] <- max(unique(alter.data$freq.rec.num), na.rm = TRUE) + 1
  V(gr)$week.daily[ind] <- "Yes"
  
  # Generation
  V(gr)$intgen[ind] <- "ego"
  V(gr)$intgen.fam.cat[ind] <- "ego"
  
  # Type
  V(gr)$type <- "alter"
  V(gr)$type[ind] <- "ego"
  
  # Rename ego ID
  gr$ego_id <- gr$.egoID
  gr <- delete_graph_attr(gr, ".egoID")
  
  gr  
}

# Plotting function for Fig. 1
plot.fun <- function(lay, 
                     # Vertex attribute to be used for node fill/size
                     node.var = "intgen",
                     cols = c(Yes = "darkblue", No = "darkblue", ego = "red"), 
                     circle.col = "grey70",
                     fills = c(Yes = "blue", No = pal[1]), 
                     sizes = c(Yes = 9, No = 7, ego = 9), 
                     shapes = c(ego = 15, alter = 21), # Note: ego's shape only has col, no fill
                     edge.cols = c(ego.alter = "red", alter.alter = "grey20"),
                     edge.width = 1,
                     margs = rep(5.5, 4),
                     leg.pos = "none",
                     bg.fill = "white",
                     bg.col = "white"
) {
  
  p <- ggraph(graph = lay) + 
    graphlayouts::draw_circle(col = circle.col, use = "cent") +
    geom_edge_link(aes(edge_color = tie.type), edge_width= edge.width, edge_alpha = 0.5) +  # color = "grey20"
    # Nodes with fill and shape
    geom_node_point(aes(fill= !!rlang::sym(node.var), size = !!rlang::sym(node.var), 
                        color = !!rlang::sym(node.var), shape = type)) +
    scale_fill_manual(values= fills, na.value = "grey80") +
    scale_edge_color_manual(values= edge.cols) +
    scale_color_manual(values= cols) +
    scale_shape_manual(values= shapes) +
    scale_size_manual(values= sizes) +
    theme_graph(base_family = 'Helvetica') +
    theme(legend.position = leg.pos,
          plot.margin = unit(margs, "points"),
          plot.background = element_rect(fill = bg.fill, colour = bg.col),
          panel.background = element_rect(fill = bg.fill, colour = bg.col)
    ) +
    coord_fixed()
  
  p
}

# Get pvalue from clustered Wilcoxon test on alter.data for a given variable 
# (var) on intgen.fam as groups
clusWilcox.test.pval <- function(df, var) {
  form <- paste0(var, "~ intgen.fam + cluster(ego_id)")
  df |> 
    mutate(intgen.fam = as.numeric(intgen.fam)) %>%
    clusrank::clusWilcox.test(as.formula(form), 
                              data = ., 
                              alternative = "two.sided") |> 
    pluck("p.value")
}

# Get p-value from Anova test
anova_pval <- function(var, groups, df) {
  form <- paste0(var, " ~ ", groups)
  aov(as.formula(form), 
      data = df) %>%
    broom::tidy() %>%
    filter(term == groups) %>%
    pull(p.value)
}

# Same as above, for t-test
ttest_pval <- function(df, var, groups) {
  form <- paste0(var, " ~ ", groups)
  t.test(as.formula(form), 
         data = df, 
         alternative = "two.sided") %>%
    pluck("p.value")
}

# Tidy method for zeroinfl object class
tidy.zeroinfl <- function(x, exponentiate = FALSE, coef.type = "both", 
                          conf.int = TRUE, ...) {
  
  stopifnot(class(x) == "zeroinfl")
  
  # Get zeroinfl coefficients from summary()
  ret <- x %>%
    summary %>%
    pluck("coefficients") %>%
    # Result is list of two data frames, one for count one for binary ("zero") 
    # coefficients
    map(~ as_tibble(.x, rownames = "term")) %>%
    # Bind the two data frames and clean
    bind_rows(.id = "coef.type") %>%
    # Unite coef.type to term. This avoids that functions like mice::pool()
    # inappropriately mix the count and zero coefficients
    mutate(term = paste0(coef.type, "_", term)) %>%
    dplyr::select(c(term, coef.type, Estimate, `Std. Error`, `Pr(>|z|)`)) %>%
    rename(estimate = Estimate, std.error = `Std. Error`, p.value = `Pr(>|z|)`)
  
  # If conf.int, add confidence intervals
  if (conf.int) {
    ret <- confint(x) %>%
      as_tibble(rownames = "term") %>%
      # Create coef.type column
      separate(term, into = c("coef.type", "term2"), sep = "\\_", remove = FALSE) %>%
      dplyr::select(-term2) %>%
      # Rename
      dplyr::rename(conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
      # Join with ret
      left_join(ret, ., by = c("term", "coef.type"))
  }
  
  # Coefficients to keep, based on argument coef.type
  if (coef.type == "count") {
    ret <- ret |> 
      filter(coef.type == "count")
  } else if (coef.type == "zero") {
    ret <- ret |> 
      filter(coef.type == "zero")
  }
  
  if (exponentiate) {
    ret <- exponentiate(ret)
  }
  
  # Return
  ret 
}

# Use zeroinfl tidier to get custom tidy table from zeroinfl model fit
tidy_zeroinfl <- function(mod, exponentiate = FALSE, which = "both", 
                          coef.type = "both", conf.int = TRUE, conf.int.paste = FALSE,
                          std.error = TRUE, p.value = TRUE, term.labels = NULL) {
  
  stopifnot(class(mod) == "zeroinfl")
  
  # Get table of estimates (se)
  est <- tidy(mod, exponentiate = exponentiate, conf.int = conf.int, coef.type = coef.type) %>%
    # P value stars
    mutate(p.stars = pval_stars(p.value)) %>%
    # Round numeric
    mutate_if(is.numeric, ~ round(.x, 2))
  
  # If conf.int, create conf.int cell
  if (conf.int.paste) {
    est <- est |> 
      mutate(conf.int = case_when(
        # Only if conf.low and conf.high are not NA
        !is.na(conf.low) ~ paste(conf.low, "-", conf.high))
      )
  }
  
  # Table of fit stats
  fit <- tibble(loglik = as.numeric(logLik(mod)),
                AIC = AIC(mod),
                nobs = summary(mod)$n) %>%
    pivot_longer(cols = everything(), names_to = "term", values_to = "estimate") %>%
    mutate(estimate = round(estimate, 2))
  
  # Bind together
  tab <- bind_rows(list(estimates = est, fit = fit), .id = "type")  
  
  # If term names exist, join with appropriate term names
  if (!is.null(term.labels)) {
    tab <- tab %>%
      full_join(term.labels, ., by = "term") 
  }
  
  # Replace NA with blank in p-stars to avoid problems when pasting with other cells
  tab <- tab |> 
    mutate(p.stars = tidyr::replace_na(p.stars, ""))
  
  # Rows to keep, based on argument which
  if (which != "both") {
    tab <- tab |> 
      filter(type == which)
  } 
  
  # Columns to keep based on arguments
  cols <- c("term", "estimate")
  cols <- c("term", "estimate")
  if (conf.int & conf.int.paste) cols <- c(cols, "conf.int")
  if (conf.int & !conf.int.paste) cols <- c(cols, "conf.low", "conf.high")
  if (std.error) cols <- c(cols, "std.error")
  if (p.value) cols <- c(cols, "p.value")
  if (!is.null(term.labels)) cols <- c("label", cols)
  cols <- c(cols, "p.stars")
  
  # Return
  tab |> 
    dplyr::select(!!cols)
}

# Get custom tidy table from glm model fit
tidy_glm <- function(mod, exponentiate = FALSE, which = "both", conf.int = TRUE,
                     conf.int.paste = FALSE, std.error = TRUE, p.value = TRUE, 
                     term.labels = NULL) {
  
  # Get table of estimates (se)
  est <- broom::tidy(mod, exponentiate = exponentiate, conf.int = conf.int)  |> 
    # P value stars
    mutate(p.stars = pval_stars(p.value)) |>
    # Round numeric
    mutate_if(is.numeric, ~ round(.x, 2))
  
  # If conf.int.paste, create conf.int cell
  if (conf.int.paste) {
    est <- est |> 
      mutate(conf.int = case_when(
        # Only if conf.low and conf.high are not NA
        !is.na(conf.low) ~ paste(conf.low, "-", conf.high))
      )
  }
  
  # Table of fit stats
  fit <- glance(mod) |>
    dplyr::select(deviance, AIC, nobs) |>
    pivot_longer(cols = everything(), names_to = "term", values_to = "estimate") |>
    mutate(estimate = round(estimate, 2))
  
  # Bind together
  tab <- bind_rows(list(estimates = est, fit = fit), .id = "type")  
  
  # If term names exist, join with appropriate term names
  if (!is.null(term.labels)) {
    tab <- tab %>%
      full_join(term.labels, ., by = "term")  
  }
  
  # Replace NA with blank in p-stars to avoid problems when pasting with other cells
  tab <- tab |> 
    mutate(p.stars = tidyr::replace_na(p.stars, ""))
  
  # Rows to keep, based on argument which
  if (which != "both") {
    tab <- tab |> 
      filter(type == which)
  } 
  
  # Columns to keep based on arguments
  cols <- c("term", "estimate")
  if (std.error) cols <- c(cols, "std.error")
  if (p.value) cols <- c(cols, "p.value")
  if (conf.int & conf.int.paste) cols <- c(cols, "conf.int")
  if (conf.int & !conf.int.paste) cols <- c(cols, "conf.low", "conf.high")
  # If multinom object, include y.level in final table
  if ("multinom" %in% class(mod)) {
    cols <- c("y.level", cols)
  }
  if (!is.null(term.labels)) cols <- c("label", cols)
  cols <- c(cols, "p.stars")
  
  # Return
  tab |>
    dplyr::select(!!cols)
}

# Get custom tidy table from glmer model fit
tidy_glmer <- function(mod, effects = c("ran_pars", "fixed"), exponentiate = FALSE, 
                       which = "both", conf.int = TRUE, conf.int.paste = FALSE,
                       std.error = TRUE, p.value = TRUE, term.labels = NULL) {
  
  stopifnot(class(mod)=="glmerMod")
  
  # Get table of estimates (se)
  est <- broom.mixed::tidy(mod, exponentiate = exponentiate, conf.int = conf.int,
                           effects = effects) 
  
  # If there is no p.value column, add one with NA (this happens when 
  # effects = "ran_pars")
  if (!("p.value" %in% names(est))) {
    est <- est |> 
      add_column(p.value = NA_real_)
  }
  
  # If there is no std.error column, add one with NA (this happens when 
  # effects = "ran_pars")
  if (!("std.error" %in% names(est))) {
    est <- est |> 
      add_column(std.error = NA_real_)
  }
  
  # P value stars
  est <- est |> 
    mutate(p.stars = pval_stars(p.value)) |>
    # Round numeric
    mutate_if(is.numeric, ~ round(.x, 2))
  
  # If conf.int.paste, create conf.int cell
  if (conf.int.paste) {
    est <- est |> 
      mutate(conf.int = case_when(
        # Only if conf.low and conf.high are not NA
        !is.na(conf.low) ~ paste(conf.low, "-", conf.high))
      )
  }
  
  # Table of fit stats
  fit <- glance(mod) |>
    dplyr::select(deviance, AIC) |>
    pivot_longer(cols = everything(), names_to = "term", values_to = "estimate") |>
    mutate(estimate = round(estimate, 2))
  
  # Number of level-1 observations
  nobs <- getME(mod, "n")
  
  # Number of observations for higher level groups
  nobs.groups <- getME(mod, "l_i") |>
    enframe(name = "group", value = "estimate") |>
    dplyr::transmute(group, term = "nobs", estimate)
  
  # All n obs together
  nobs.groups <- nobs.groups |> 
    add_row(term = "nobs", estimate = nobs, .before = 1)
  
  # Bind
  fit <- bind_rows(fit, nobs.groups)
  
  # Bind all together
  tab <- bind_rows(list(estimates = est, fit = fit), .id = "type")  
  
  # Paste group name in term when present
  tab <- tab |> 
    mutate(
    term = case_when(
      !is.na(group) ~ paste0(group, "_", term),
      TRUE ~ term
    )
  )
  
  # If term names exist, join with appropriate term names
  if (!is.null(term.labels)) {
    tab <- tab %>%
      full_join(term.labels, ., by = "term")  
  }
  
  # Replace NA with blank in p-stars to avoid problems when pasting with other 
  # cells
  tab <- tab |> 
    mutate(p.stars = tidyr::replace_na(p.stars, ""))
  
  # Rows to keep, based on which and effects argument
  if (which != "both") {
    tab <- tab |> 
      filter(type == which)
  } 
  
  # Columns to keep based on arguments
  cols <- c("term", "estimate")
  if (std.error) cols <- c(cols, "std.error")
  if (p.value) cols <- c(cols, "p.value")
  if (conf.int & conf.int.paste) cols <- c(cols, "conf.int")
  if (conf.int & !conf.int.paste) cols <- c(cols, "conf.low", "conf.high")
  if (!is.null(term.labels)) cols <- c("label", cols)
  cols <- c(cols, "p.stars")
  
  # Return
  tab |>
    dplyr::select(!!cols)
}

# Recode p-values into star symbols for tables
pval_stars <- function(pvals, thresholds = c(.001, .01, .05)) {
  stopifnot(is.numeric(pvals), is.numeric(thresholds))
  case_when(pvals <= thresholds[1] ~ "***",
            pvals > thresholds[1] & pvals <= thresholds[2] ~ "**",
            pvals > thresholds[2] & pvals <= thresholds[3] ~ "*",
            is.na(pvals) ~ NA_character_,
            TRUE ~ "")  
}


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Objects   ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Labels for Figure 3
term.labels.biv.fig <- c(`Married/co-living` = "Married",
                         `Prim` = "primary",
                         `Middle` = "middle",
                         `HS` = "high", 
                         `Uni+` = "uni or higher", 
                         `1-Elem` = "1. Elementary/menial",
                         `2-Skill` = "2. Skilled crafts/trades",
                         `3-Retail` = "3. Retail/office employee", 
                         `4-Prof` = "4. Professional/self-employed", 
                         `5-Manag` = "5. Manager/executive"
) 

# Term labels for ego-level models
term.labels.ego <- c(Sociodemographics = "",
                     Age = "age",
                     `Female` = "genderFemale",
                     `Marital status (ref: Single/widow)` = "",
                     `Married/co-living` = "maritalMarried",
                     `Education level (ref: Primary)` = "",
                     `Middle school` = "educmiddle",
                     `High school` = "educhigh", 
                     `University or more` = "educuni or higher", 
                     `Occupational status (ref: 1. Elementary/menial)` = "",
                     `2. Skilled crafts/trades` = "occ.typ2. Skilled crafts/trades",
                     `3. Retail/office employee` = "occ.typ3. Retail/office employee", 
                     `4. Professional/self-employed` = "occ.typ4. Professional/self-employed", 
                     `5. Manager/executive` = "occ.typ5. Manager/executive", 
                     `Health` = "",
                     `N physical diseases (ref: None)` = "",
                     `One` = "dis.phys.count.catOne",
                     `Multiple` = "dis.phys.count.catMultiple", 
                     `Recent health care events (ref: None)` = "",
                     `One` = "hcare.count.catOne",
                     `Multiple` = "hcare.count.catMultiple", 
                     `Limited ADL` = "adl.recYes",
                     `GDS (ref: 0)` = "",
                     `1` = "gds.rec1",
                     `>1` = "gds.rec>1",
                     `Dementia` = "demenYes",
                     `Other` = "",
                     `In ALF` = "rsaYes",
                     `Phone interview` = "tel.intYes",
                     `(Intercept)` = "(Intercept)",
                     `Model statistics` = "",
                     Deviance = "deviance", 
                     AIC = "AIC",
                     `N obs` = "nobs") |>
  # To tibble
  enframe(name = "label", value = "term") |>
  # When term is empty, replace with label.
  mutate(term = case_when(
    term == "" ~ label,
    TRUE ~ term
  )) %>%
  add_column(order = 1:nrow(.))

# Same as above, but only with estimates (no fit stats)
term.labels.ego.est <- term.labels.ego |>
  filter(!(label %in% c("Model statistics", "Deviance", "AIC", "N obs")))

# For zeroinfl models, add "count_" to term
term.labels.ego.est.zinfl <- term.labels.ego.est |>
  mutate(term = paste0("count_", term))
# Add zeroinfl fit stats and theta
temp <- c(`Log theta` = "count_Log(theta)",
          `Model statistics` = "",
          LogLik = "loglik", 
          AIC = "AIC",
          `N obs` = "nobs") |>
  enframe(name = "label", value = "term")
term.labels.ego.zinfl <- term.labels.ego.est.zinfl |>
  dplyr::select(label, term) |>
  bind_rows(temp) |>
  # When term is empty, replace with label
  mutate(term = case_when(
    term == "" ~ label,
    TRUE ~ term
  )) %>%
  add_column(order = 1:nrow(.)) 

# Term labels for tie-level models (table)
term.labels.tie <- c(`Ego characteristics` = "",
                     Age = "age.st",
                     `Female` = "genderFemale",
                     `Marital status (ref: Single/widow)` = "",
                     `Married/co-living` = "maritalMarried",
                     `Education level (ref: Primary)` = "",
                     `Middle school` = "educmiddle",
                     `High school` = "educhigh", 
                     `University or more` = "educuni or higher", 
                     `Occupational status (ref: 1. Elementary/menial)` = "",
                     `2. Skilled crafts/trades` = "occ.typ2. Skilled crafts/trades",
                     `3. Retail/office employee` = "occ.typ3. Retail/office employee", 
                     `4. Professional/self-employed` = "occ.typ4. Professional/self-employed", 
                     `5. Manager/executive` = "occ.typ5. Manager/executive", 
                     `Health` = "",
                     `N physical diseases (ref: None)` = "",
                     `One` = "dis.phys.count.catOne",
                     `Multiple` = "dis.phys.count.catMultiple", 
                     `Recent health care events (ref: None)` = "",
                     `One` = "hcare.count.catOne",
                     `Multiple` = "hcare.count.catMultiple", 
                     `Limited ADL` = "adl.recYes",
                     `GDS (ref: 0)` = "",
                     `1` = "gds.rec1",
                     `>1` = "gds.rec>1",
                     `Dementia` = "demenYes",
                     `Tie/alter characteristics` = "",
                     `Contact frequency (ref: Yearly)` = "",
                     `Monthly` = "freq.recmonthly",
                     `Weekly` = "freq.recweekly",
                     `Daily` = "freq.recdaily",
                     `Alter is female` = "alter.sexFemale",
                     `Alter is co-resident` = "alter.coresYes",
                     `Alter centrality` = "alter.deg.norm",
                     `Other` = "",
                     `Ego in ALF` = "rsaYes",
                     `Phone interview` = "tel.intYes",
                     `(Intercept)` = "(Intercept)",
                     `Variance components` = "",
                     `Ego-level SD` = "ego_id_sd__(Intercept)",
                     `Model statistics` = "",
                     Deviance = "deviance", 
                     AIC = "AIC",
                     `N obs (ties)` = "nobs",
                     `N obs (egos)` = "ego_id_nobs") |>
  # To tibble
  enframe(name = "label", value = "term") |>
  # When term is empty, replace with label (to avoid problems when joining tables 
  # by term)
  mutate(term = case_when(
    term == "" ~ label,
    TRUE ~ term
  )) %>%
  add_column(order = 1:nrow(.))

# Term labels for ego-level model coefficient figure
term.labels.ego.fig <- c(Age = "age",
                         `Female` = "genderFemale",
                         `Married/co-living` = "maritalMarried",
                         `Education*:\nMiddle school` = "educmiddle",
                         `Education:\nHigh school` = "educhigh", 
                         `Education:\nUniversity+` = "educuni or higher", 
                         `Occupation*:\n2-Skilled crafts/trades` = "occ.typ2. Skilled crafts/trades",
                         `Occupation:\n3-Retail/office` = "occ.typ3. Retail/office employee", 
                         `Occupation:\n4-Prof./self-employed` = "occ.typ4. Professional/self-employed", 
                         `Occupation:\n5-Manager/exec.` = "occ.typ5. Manager/executive", 
                         `N physical\ndiseases*: One` = "dis.phys.count.catOne",
                         `N physical\ndiseases: Multiple` = "dis.phys.count.catMultiple", 
                         `Health care\nevents*: One` = "hcare.count.catOne",
                         `Health care\nevents: Multiple` = "hcare.count.catMultiple",
                         `Limited ADL` = "adl.recYes",
                         `GDS*: 1` = "gds.rec1",
                         `GDS: >1` = "gds.rec>1",
                         `Dementia` = "demenYes"
) |> 
  # To tibble
  enframe(name = "label", value = "term")

# Term labels for tie-level models (figure)
term.labels.tie.fig <- term.labels.ego.fig |> 
  mutate(term = str_replace(term, "^age", "age.st")) |> 
  bind_rows(
    enframe(c(`Contact*:\nMonthly` = "freq.recmonthly",
              `Contact:\nWeekly` = "freq.recweekly",
              `Contact:\nDaily` = "freq.recdaily",
              `Alter:\nFemale` = "alter.sexFemale",
              `Alter:\nCo-resident` = "alter.coresYes",
              `Alter\ncentrality†` = "alter.deg.norm"
    ),
    name = "label", value = "term")
  )
