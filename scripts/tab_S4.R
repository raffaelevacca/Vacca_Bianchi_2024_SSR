# Load model estimation results
load("./results/model_fit.rda")

# Filter to model fit on full sample
mod.df.prop <- mod.df.prop |> 
  filter(sample == "full") |> 
  dplyr::select(-sample)

# Get tidy result tables from each model fit
tab.df <- mod.df.prop |> 
  rowwise() |> 
  # Get tidy summaries from model fit 
  mutate(
    mod.tab = list(tidy_glm(mod, conf.int = FALSE, p.value = TRUE, 
                            term.labels = term.labels.ego))
  ) 

# Modify each table
tab.df <- tab.df |> 
  # Round numeric values
  mutate(
    mod.tab = list(mutate(mod.tab,
                          across(where(is.numeric), ~ round(.x, 2)))
    )
  ) |> 
  # Create Estimate (SD) cell and only keep relevant columns
  mutate(
    mod.tab = list(transmute(mod.tab,
                             label, term,
                             est_sd = paste0(estimate, " (", std.error, ")"),
                             p.value = paste0(p.value, p.stars)
    )
    )
  ) |> 
  # Remove errors due to NA in cell
  mutate(
    mod.tab = list(
      mutate(mod.tab,
             est_sd = na_if(est_sd, "NA (NA)"),
             est_sd = str_remove_all(est_sd, " \\(NA\\)"))
    )
  ) |> 
  mutate(
    mod.tab = list(
      mutate(mod.tab,
             across(where(is.character),
                    ~ na_if(.x, "NA")))
    )
  )

# Final table
fin.tab <- tab.df |> 
  dplyr::select(prop.var, mod.tab) |> 
  unnest("mod.tab") |>  
  pivot_wider(names_from = prop.var, values_from = c(est_sd, p.value)) |> 
  dplyr::select(-term) |> 
  dplyr::select(label, ends_with(mod.df.prop$prop.var)) |> 
  rename(
    `Proportion IG ties: Est. (SE)` = "est_sd_ig.fam.prop",
    `Proportion IG ties: P-value` = "p.value_ig.fam.prop",
    `Proportion IG active ties: Est. (SE)` = "est_sd_ig.fam.act.prop",
    `Proportion IG active ties: P-value` = "p.value_ig.fam.act.prop",
    `Concentration children: Est. (SE)` = "est_sd_ig.fam.imm.prop",
    `Concentration children: P-value` = "p.value_ig.fam.imm.prop"
  )

# Knitr option to print NAs as blanks
options(knitr.kable.NA = "")
# Print to html
knitr::kable(fin.tab) %>%
  kable_styling() %>%
  print()