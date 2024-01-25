# Load data (random sample)
load("./data/data_clean.rda")
ego.data <- ego.data.rs
            
# Variables of interest: alter counts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

vars <- c("alt.count", "fam.count", "act.count", "ig.fam.count", "ig.fam.act.count", "ig.fam.imm.count")

desc.list <- list()

for (var in vars) {
  desc.list[[var]] <- ego.data |> 
    skim(!!sym(var)) |> 
    as_tibble() |> 
    dplyr::select(
      var = skim_variable,
      mean = numeric.mean,
      sd = numeric.sd,
      min = numeric.p0,
      max = numeric.p100
    ) |> 
    # Round
    mutate_if(is.numeric, ~ round(., 1)) |> 
    # Paste cells
    mutate(value = paste0(mean, " (", sd, ")"),
           range = paste0(min, "-", max)) |> 
    dplyr::select(var, value, range)
}

# Get table
desc.tab.counts <- bind_rows(desc.list)


# Variables of interest: alter proportions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

vars <- c("ig.fam.prop", "ig.fam.act.prop", "ig.fam.imm.prop")

desc.list <- list()

for (var in vars) {
  desc.list[[var]] <- ego.data |> 
    skim(!!sym(var)) |> 
    as_tibble() |> 
    dplyr::select(
      var = skim_variable,
      mean = numeric.mean,
      sd = numeric.sd,
      min = numeric.p0,
      max = numeric.p100
    ) |> 
    # Round
    mutate_if(is.numeric, ~ round(., 2)) |> 
    # Paste cells
    mutate(value = paste0(mean, " (", sd, ")"),
           range = paste0(min, "-", max)) |> 
    dplyr::select(var, value, range)
}

# Bind all together
fin.tab.prop <- bind_rows(desc.list) 


# Variables of interest: ego-level centrality variables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

vars <- c(
  "deg.norm", 
  "deg.prop",
  "betw.prop"
)

desc.list <- list()

for (var in vars) {
  desc.list[[var]] <- ego.data |> 
    skim(!!sym(var)) |> 
    as_tibble() |> 
    dplyr::select(
      var = skim_variable,
      mean = numeric.mean,
      sd = numeric.sd,
      min = numeric.p0,
      max = numeric.p100
    ) |> 
    # Round
    mutate_if(is.numeric, ~ round(., 2)) |> 
    # Paste cells
    mutate(value = paste0(mean, " (", sd, ")"),
           range = paste0(min, "-", max)) |> 
    dplyr::select(var, value, range)
}

# Get table
fin.tab.centr <- bind_rows(desc.list) 

# Variables of interest: binary measures of IG connectedness
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

vars <- c("has.ig.fam", "has.ig.fam.act", "has.ig.fam.imm", "deg.norm.cat", "deg.prop.cat", "betw.prop.cat")

freq.list <- list()

for (var in vars) {
  
  freq.list[[var]] <- ego.data |> 
    tabyl(!!rlang::sym(var), show_na = FALSE) |> 
    # Rename column of categories
    rename_with(~ "cat", 1) |> 
    # Add variable name
    add_column(var = var, .before = 1) |> 
    mutate(percent = round(percent*100, 1)) |> 
    as_tibble() |> 
    # Create cell value
    transmute(var, cat,
              value = paste0(n, " (", percent, ")")
    ) 
}

# Get table
freq.tab.bin <- bind_rows(freq.list)

# Clean table
freq.tab.bin <- freq.tab.bin |> 
  # Only keep "Yes" or "Max" rows
  filter(is.na(cat) | cat == "Yes" | cat == "Max") |> 
  dplyr::select(-cat)


# Print
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Bind all together
tab.all <- desc.tab.counts %>%
  add_row(var = "_") %>%
  bind_rows(fin.tab.prop) %>%
  add_row(var = "_") %>%
  bind_rows(fin.tab.centr) %>%
  add_row(var = "_") %>%
  bind_rows(freq.tab.bin) 

# Table row labels
row.labels <- c(
  `Count: All` = "alt.count",
  `Count: Family` = "fam.count",
  `Count: Active`  = "act.count",
  `Count: IG` = "ig.fam.count",
  `Count: IG active` = "ig.fam.act.count",
  `Count: Adult children` = "ig.fam.imm.count",
  `Prop: IG` = "ig.fam.prop",
  `Prop: IG active` = "ig.fam.act.prop",
  `Prop: Adult children` = "ig.fam.imm.prop",
  `Max proportion network known (p)` = "deg.norm",
  `Max relative degree (d)` = "deg.prop",
  `Max relative betweenness (b)` = "betw.prop",
  `Ego has IG ties` = "has.ig.fam",
  `Ego has IG active ties` = "has.ig.fam.act",
  `Ego has adult children in network` = "has.ig.fam.imm",
  `IG alter knows all network members` = "deg.norm.cat",
  `IG alter has highest degree` = "deg.prop.cat",
  `IG alter has highest betweenness` = "betw.prop.cat"
)
tab.all <- tab.all |> 
  mutate(var = fct_recode(var, !!!row.labels))

# Knitr option to print NAs as blanks
options(knitr.kable.NA = "")
# Print to html
knitr::kable(tab.all) %>%
  kable_styling() %>%
  print()
