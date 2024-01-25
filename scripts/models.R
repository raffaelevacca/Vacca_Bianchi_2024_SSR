## INFO ----
##
## Estimate all statistical models
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load data
load("./data/data_clean.rda")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Ego-level models for alter counts                                        ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Build model formulas
# - - - - - - - - - - - - - - - - - - - - 

## IV formula for binary part of zero inflated models
zero.form <- c("age", "gender", "marital") |> 
  paste(collapse = " + ") 

## Explanatory variables
ego.control.IV <- c("age", "gender", "marital", "educ", "occ.typ", "tel.int") |> 
  paste(collapse = " + ")
ego.health.IV <- c("dis.phys.count.cat", "hcare.count.cat", "adl.rec", "gds.rec", 
                   "demen", "rsa") |> 
  paste(collapse = " + ")

## Dependent variables
ego.count.DV <- c("ig.fam.count", "ig.fam.act.count")

## Data frame of formulas to estimate models
mod.df <- tibble(
  # Column of DVs
  DV = ego.count.DV
) |> 
  # Model formula for each DV
  mutate(
    form = paste(DV, "~", ego.control.IV, "+", ego.health.IV, " | ", zero.form)
  ) 

# Estimate the models on full sample data
# - - - - - - - - - - - - - - - - - - - - 
df <- ego.data.fs
mod.df.count.fs <- mod.df |> 
  rowwise() |> 
  mutate(
    mod = list(zeroinfl(as.formula(form), data = df, dist = "negbin")),
  )

# Add flag for sample type
mod.df.count.fs <- mod.df.count.fs |> 
  add_column(sample = "full")

# Estimate the models on random sample data
# - - - - - - - - - - - - - - - - - - - - 

df <- ego.data.rs

# Remove tel.int from formulas (this IV has no variation in random sample)
mod.df <- mod.df |> 
  mutate(
    form = str_remove(form, pattern = " \\+ tel.int")
    )

# Estimate
mod.df.count.rs <- mod.df |> 
  rowwise() |> 
  mutate(
    mod = list(zeroinfl(as.formula(form), data = df, dist = "negbin")),
  )

# Add flag for sample type
mod.df.count.rs <- mod.df.count.rs |> 
  add_column(sample = "random")

# Stack into single df
mod.df.count <- bind_rows(mod.df.count.fs, mod.df.count.rs)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Ego-level models for alter proportions   ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Build model formulas
# - - - - - - - - - - - - - - - - - - - - 

## Explanatory variables
ego.control.IV <- c("age", "gender", "marital", "educ", "occ.typ", "tel.int") |> 
  paste(collapse = " + ")
ego.health.IV <- c("dis.phys.count.cat", "hcare.count.cat", "adl.rec", "gds.rec", 
                   "demen", "rsa") |> 
  paste(collapse = " + ")

## Dependent variables
ego.prop.DV <- c(
  "cbind(ig.fam.count, alt.count - ig.fam.count)",
  "cbind(ig.fam.act.count, act.count - ig.fam.act.count)",
  "cbind(ig.fam.imm.count, alt.count - ig.fam.imm.count)"
)
prop.var <- c(
  "ig.fam.prop",
  "ig.fam.act.prop",
  "ig.fam.imm.prop"
)
count.var <- prop.var |> 
  str_replace("prop", "count")

## Data frame of formulas to estimate models
mod.df <- tibble(
  # Column of DVs
  DV = ego.prop.DV,
  prop.var = prop.var,
  count.var = count.var
) |> 
  # Model formula for each DV
  mutate(
    form = paste(DV, "~", ego.control.IV, "+", ego.health.IV)
  ) 

# Estimate the models on full sample
# - - - - - - - - - - - - - - - - - - - - 

df <- ego.data.fs
mod.df.prop.fs <- mod.df |> 
  rowwise() |> 
  mutate(
    mod = list(glm(as.formula(form), family= binomial(logit), data = df))
  )

# Add flag for sample type
mod.df.prop.fs <- mod.df.prop.fs |> 
  add_column(sample = "full")

# Estimate the models on random sample
# - - - - - - - - - - - - - - - - - - - - 

df <- ego.data.rs

# Remove tel.int from formulas (this IV has no variation in random sample)
mod.df <- mod.df |> 
  mutate(
    form = str_remove(form, pattern = " \\+ tel.int")
  )

# Estimate
mod.df.prop.rs <- mod.df |> 
  rowwise() |> 
  mutate(
    mod = list(glm(as.formula(form), family= binomial(logit), data = df))
  )

# Add flag for sample type
mod.df.prop.rs <- mod.df.prop.rs |> 
  add_column(sample = "random")

# Stack into single df
mod.df.prop <- bind_rows(mod.df.prop.fs, mod.df.prop.rs)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Tie-level models for probability of IG ties   ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Set the data
df <- alter.ego.data.fs

# Build model formulas
# - - - - - - - - - - - - - - - - - - - - 

## Explanatory variables
ego.control.IV <- c("age.st", "gender", "marital", "educ", "occ.typ", "tel.int") |> 
  paste(collapse = " + ")
ego.health.IV <- c("dis.phys.count.cat", "hcare.count.cat", "adl.rec", "gds.rec", 
                   "demen", "rsa") |> 
  paste(collapse = " + ")
alter.IV <- c("freq.rec", "alter.sex", "alter.cores", "alter.deg.norm") |> 
  paste(collapse = " + ")

## Dependent variables
alter.DV <- c(
  "intgen.fam",
  "intgen.fam.act",
  "intgen.fam.imm"
)

## Data frame of formulas to estimate models
mod.df <- tibble(DV = alter.DV) |> 
  mutate(
    form.full = paste(DV, "~", 
                      alter.IV, "+", 
                      ego.control.IV, "+", 
                      ego.health.IV, "+", 
                      "(1 | ego_id)")
  ) |> 
  # When DV is "intgen.fam.act", remove freq.rec because active ties have 
  # freq.rec = weekly or daily by definition.
  mutate(
    form.full = case_when(
      str_detect(DV, "\\.act") ~ str_remove(form.full, "freq.rec \\+ "),
      TRUE ~ form.full
    )
  ) 

# Estimate the models
# - - - - - - - - - - - - - - - - - - - - 

mod.df.tie <- mod.df |> 
  rowwise() |> 
  mutate(
    mod = list(glmer(as.formula(form.full), 
                                family = binomial("logit"), 
                                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                                data = df))
  )
    

mod.df.tie |> 
  filter(DV == "intgen.fam.act") |> 
  pull("mod") |> 
  pluck(1) |> 
  summary()

# Save all model fit objects.
save(mod.df.count, mod.df.prop, mod.df.tie, file = "./results/model_fit.rda")

