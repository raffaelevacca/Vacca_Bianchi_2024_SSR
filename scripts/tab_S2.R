# Load data
load("./data/data_clean.rda")
ego.data <- ego.data.fs

# Define relevant categorical variables
rel.cat.vars <- c(
  "gender",
  "marital",
  "educ",
  "occ.typ",
  "dis.phys.count.cat",
  "hcare.count.cat",
  "adl.rec",
  "gds.rec",
  "demen",
  "rsa" 
)

# Pre-assign lists of results
desc.list <- list()
freq.list <- list()

# Continuous IV: age
var = "age"
desc.list[[var]] <- ego.data |> 
  skim(!!sym(var)) |> 
  as_tibble() |> 
  dplyr::select(cat = skim_variable, Mean = numeric.mean, Std.Dev = numeric.sd) |> 
  # Round
  mutate_if(is.numeric, ~ round(., 2)) |> 
  # Create mean (sd)
  mutate(Mean = paste0(Mean, " (", Std.Dev, ")")) |>
  dplyr::select(Mean) |>
  # Variable name into table
  mutate(cat = NA, variable = var) |>
  rename(`Mean (sd) or N (%)` = Mean) |>
  # Blank row to separate variables in final table
  add_row(variable = "_")

# Categorical IVs
for (var in rel.cat.vars) {
  
  freq.list[[var]] <- tabyl(ego.data[[var]]) |> 
    # Proportion to percent
    mutate(percent = percent*100) |>
    # Rename first column
    dplyr::rename(cat = 1) |>
    # Round
    mutate(across(where(is.numeric), ~ round(., 1))) |>
    # Get table cell
    mutate(Freq = paste0(n, " (", percent, ")")) |>
    dplyr::select(cat, `Mean (sd) or N (%)` = Freq) |>
    # Add variable name
    add_column(variable = !!var, .before = 1) |>
    # Blank row to separate variables in final table
    add_row(variable = "_")
}

# Bind all together
fin.tab <- bind_rows(bind_rows(desc.list),
                     bind_rows(freq.list)
) |>
  dplyr::select(variable, cat, everything())

# Remove reference category rows for binary variables
fin.tab <- fin.tab |> 
  filter(variable != "_") |>
  filter(!(variable == "gender" & cat == "Male")) |>
  filter(!(variable == "marital" & cat == "Not married")) |>
  filter(!(variable == "adl.rec" & cat == "No")) |>
  filter(!(variable == "demen" & cat == "No")) |>
  filter(!(variable == "rsa" & cat == "No")) 

# Add blank rows before variables with multiple categories
fin.tab <- fin.tab |> 
  # Find index
  with(which(variable == "educ" & cat == "primary")) %>%
  # Add blank
  add_row(fin.tab, variable = "_", .before = .)

fin.tab <- fin.tab |> 
  # Find index
  with(which(variable == "occ.typ" & cat == "1. Elementary/menial")) %>%
  # Add blank
  add_row(fin.tab, variable = "_", .before = .)

fin.tab <- fin.tab |>  
  # Find index
  with(which(variable == "dis.phys.count.cat" & cat == "None")) %>%
  # Add blank
  add_row(fin.tab, variable = "_", .before = .)

fin.tab <- fin.tab |> 
  # Find index
  with(which(variable == "gds.rec" & cat == "0")) %>%
  # Add blank
  add_row(fin.tab, variable = "_", .before = .)

fin.tab <- fin.tab |> 
  # Find index
  with(which(variable == "hcare.count.cat" & cat == "None")) %>%
  # Add blank
  add_row(fin.tab, variable = "_", .before = .)

# Table row labels
row.labels.1 <- c(
  `Age` = "age",
  `Gender` = "gender",
  `Marital status` = "marital",
  `Education level` = "educ",
  `Occupation type` = "occ.typ",
  `N physical diseases` = "dis.phys.count.cat",
  `Health care events` = "hcare.count.cat",
  `Limited ADL` = "adl.rec",
  `GDS score` = "gds.rec",
  `Has dementia diagnosis` = "demen",
  `Lives in ALF` = "rsa"
)
fin.tab <- fin.tab |> 
  mutate(variable = fct_recode(variable, !!!row.labels.1))
row.labels.2 <- c(
  `Married/co-living` = "Married",
  `Primary school` = "primary",
  `Middle school` = "middle",
  `High school` = "high",
  `University or higher` = "uni or higher"
)
fin.tab <- fin.tab |> 
  mutate(cat = fct_recode(cat, !!!row.labels.2))

# Knitr option to print NAs as blanks
options(knitr.kable.NA = "")

# Print table to html
fin.tab |>
  knitr::kable() |>
  kable_styling() |>
  print()
