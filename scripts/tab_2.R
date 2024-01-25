# Load data
load("./data/data_clean.rda")

# Set data on full sample as alter data
alter.data <- alter.data.fs

# Reorder factor levels
alter.data <- alter.data |>
  mutate(across(c(intgen, intgen.fam, freq.rec), fct_rev)) |>
  mutate(role.rec.3 = fct_relevel(role.rec.3, "imm.fam", "ext.fam", "other"))

# Categorical variables 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Preassign lists
tab.list <- list()
pval.list <- list()

# First row with intgen.fam freq
tab.list[["intgen.fam"]] <- alter.data |> 
  tabyl(intgen.fam, show_na = FALSE) |> 
  adorn_totals("row") |> 
  # Rename column of categories
  rename_with(~ "var", 1) |> 
  adorn_pct_formatting(digits = 0) |> 
  as_tibble() |> 
  # Remove % sign
  mutate(percent = str_remove(percent, "%")) |> 
  # Create cell value
  transmute(var, 
            value = paste0(n, " (", percent, ")")) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  add_column(var = NA, .before = 1) |> 
  add_column(variable = "intgen.fam", .before = 1) |> 
  add_row(variable = "_", .after = 1) 

# For each relevant variable
vars <- c("alter.sex", "role.rec.3", "freq.rec", "alter.cores")

for (var in vars) {
  
  # Get the original two-way table
  tw.tab <- alter.data |> 
    tabyl(!!rlang::sym(var), intgen.fam, show_na = FALSE) 
  
  # Create final table to be exported for variable
  tab.list[[var]] <- tw.tab |> 
    # Add totals
    adorn_totals(c("row", "col")) |> 
    # Rename column of categories
    rename_with(~ "var", 1) %>%
    # Join with percent columns
    {left_join(., adorn_percentages(., "col"), by = "var")} |> 
    dplyr::select(var, starts_with("No"), starts_with("Yes"), starts_with("Total")) |> 
    adorn_pct_formatting(digits = 0, , , ends_with(".y")) |> 
    as_tibble() |> 
    # Remove % sign
    mutate(across(ends_with(".y"), ~ str_remove(.x, "%"))) |> 
    # Create cell value
    transmute(var, 
              No = paste0(No.x, " (", No.y, ")"),
              Yes = paste0(Yes.x, " (", Yes.y, ")"),
              Total = paste0(Total.x, " (", Total.y, ")")) |> 
    # Add column with variable name
    add_column(variable = var, .before = 1)
  
  # Get pvalue from chisquared test
  # (Only when this is an actual two-way table with >1 categories in rows)
  if (nrow(tw.tab) > 1) {
    p.val <- tw.tab |> 
      chisq.test() |> 
      pluck("p.value") |> 
      round(2)
  } else {
    p.val <- NA
  }
  
  # Add p value to table
  tab.list[[var]] <- tab.list[[var]] |> 
    add_column(pval = p.val)
}

fin.tab <- tab.list |> 
  # Add empty row after each variable
  map(~ add_row(.x, variable = "_")) |> 
  # Bind all rows together
  bind_rows()

# Continuous variables (alter centrality)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Relevant variables
vars <- c("alter.deg.norm", "alter.deg.prop", "alter.betw.prop")

# Stats in whole data set
stats.all <- alter.data |> 
  dplyr::select(all_of(vars)) |> 
  skim() |> 
  as_tibble() |> 
  dplyr::select(skim_variable, numeric.mean, numeric.sd) |> 
  mutate(across(where(is.numeric), ~ round(.x, 2))) |> 
  # Get value of final cell
  mutate(Total = paste0(numeric.mean, " (", numeric.sd, ")")) |> 
  dplyr::select(variable = skim_variable, Total)

# Stats for IG vs non-IG
stats.ig <- alter.data |> 
  dplyr::select(all_of(c("intgen.fam", vars))) |> 
  group_by(intgen.fam) |> 
  skim() |> 
  as_tibble() |> 
  dplyr::select(skim_variable, numeric.mean, numeric.sd, intgen.fam) |> 
  filter(!is.na(intgen.fam)) |> 
  mutate(across(where(is.numeric), ~ round(.x, 2))) |> 
  # Get value of final cell
  mutate(Total = paste0(numeric.mean, " (", numeric.sd, ")")) |> 
  # Select variables
  dplyr::select(variable = skim_variable, Total, intgen.fam) |> 
  # Pivot into form of final table
  pivot_wider(names_from = intgen.fam, values_from = Total) |> 
  dplyr::select(variable, Yes, No)

# Join into single table
stats.ig <- stats.ig |> 
  left_join(stats.all, by = "variable")

# Get p.values from Wilcoxon test
stats.ig  <- stats.ig |> 
  rowwise() |> 
  mutate(
    pval = clusWilcox.test.pval(df = alter.data, var = variable)
  )

# Bind into single df
fin.tab <- stats.ig %>%
  add_row(variable = "_", .before = 1) %>%
  bind_rows(fin.tab, .) 

# Most frequent roles among IG, non-IG, all ties
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 

## List of data frames
data.list <- alter.data |> 
  filter(!is.na(intgen.fam)) %>%
  {split(., f = .$intgen.fam)}
data.list[["Total"]] <- alter.data

## Get most frequent roles in each df
top.roles <- data.list |> 
  map(~ .x |> 
        tabyl(role, show_na = FALSE) |> 
        filter(n != 0) |> 
        arrange(desc(n)) |> 
        # Top 5
        slice(1:5) |> 
        pull(role) |> 
        as.vector() |> 
        paste(collapse = ", ")
  ) 

# Join to table
fin.tab <- top.roles |> 
  as_tibble() |> 
  add_column(variable = "top 5 roles", .before = 1) |> 
  add_row(variable = "_", .before = 1) %>%
  bind_rows(fin.tab, .) 


# Clean and export table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 

fin.tab <- fin.tab |> 
  # Remove Total rows
  filter(is.na(var) | var != "Total") 

fin.tab <- fin.tab |> 
  # Remove consecutive duplicated rows with "_"
  filter(!(variable == "_" & variable == lag(variable, default = "1"))) 

# Add blank rows before/after certain rows to replicate actual table
fin.tab <- fin.tab |> 
  # Find index
  with(which(variable == "intgen.fam")) %>%
  # Add blank
  add_row(fin.tab, variable = "_", .after = .)
fin.tab <- fin.tab |> 
  # Find index
  with(which(variable == "alter.deg.norm")) %>%
  # Add blank
  add_row(fin.tab, variable = "_", .before = .)
fin.tab <- fin.tab |> 
  # Find index
  with(which(variable == "top 5 roles")) %>%
  # Remove previous row
  {slice(fin.tab, -(.-1))}

# Table row labels
row.labels <- c(
  `N (row %)` = "intgen.fam",
  `Alter gender` = "alter.sex",
  `Alter role` = "role.rec.3",
  `Frequency of contact` = "freq.rec",
  Coresident = "alter.cores",
  `Proportion network known (p)` = "alter.deg.norm",
  `Relative degree (d)` = "alter.deg.prop",
  `Relative betweenness (b)` = "alter.betw.prop",
  `Top 5 alter roles` = "top 5 roles"
)
fin.tab <- fin.tab |> 
  mutate(variable = fct_recode(variable, !!!row.labels))
# Column labels
fin.tab <- fin.tab |> 
  rename(
    Variable = variable,
    Category = var, 
    `IG ties` = Yes, 
    `Same-generation ties` = No, 
    `All ties` = Total
  )

# Knitr option to print NAs as blanks
options(knitr.kable.NA = "")

# Print to html
fin.tab %>%
  knitr::kable() %>%
  kable_styling() %>%
  print()

