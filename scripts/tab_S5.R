# Load model estimation results
load("./results/model_fit.rda")

# Get tidy result tables from each model fit
tab.df <- mod.df.tie |> 
  rowwise() |> 
  # Get tidy summaries from model fit 
  mutate(
    mod.tab = list(
      tidy_glmer(mod, conf.int = FALSE, p.value = TRUE, 
                 term.labels = term.labels.tie))
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
  dplyr::select(DV, mod.tab) |> 
  unnest("mod.tab") |>  
  pivot_wider(names_from = DV, values_from = c(est_sd, p.value)) |> 
  dplyr::select(-term) |> 
  dplyr::select(label, ends_with(mod.df.tie$DV)) |> 
  rename(
    `Pr(Tie is IG): Est. (SE)` = "est_sd_intgen.fam",
    `Pr(Tie is IG): P-value` = "p.value_intgen.fam",
    `Pr(Tie is IG active): Est. (SE)` = "est_sd_intgen.fam.act",
    `Pr(Tie is IG active): P-value` = "p.value_intgen.fam.act",
    `Pr(Tie is child): Est. (SE)` = "est_sd_intgen.fam.imm",
    `Pr(Tie is child): P-value` = "p.value_intgen.fam.imm"
  )

# Knitr option to print NAs as blanks
options(knitr.kable.NA = "")
# Print to html
knitr::kable(fin.tab) %>%
  kable_styling() %>%
  print()