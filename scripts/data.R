## INFO ----
##
## Import and clean all data.
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load data
ego.data <- read_csv("./data/ego-data.csv")
alter.data <- read_csv("./data/alter-data.csv") 
alter.ties <- read_csv("./data/alter-ties.csv")
load("./data/data.rda")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Create alter attribute variables                                         ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Measures of intergenerational ties
alter.data <- alter.data |> 
  mutate(
    intgen = fct_relevel(intgen, "No"),
    # Intergenerational and family
    intgen.fam = (intgen == "Yes" & role.rec %in% c("imm.fam", "ext.fam")) %>%
      as.character() %>%
      fct_recode(Yes = "TRUE", No = "FALSE"),
    # Intergenerational and immediate family (i.e. adult child)
    intgen.fam.imm = (intgen == "Yes" & role.rec == "imm.fam") |> 
      as.character() |> 
      fct_recode(Yes = "TRUE", No = "FALSE"),
    # Intergenerational and family and active
    intgen.fam.act = (intgen.fam == "Yes" & week.daily == "Yes") |> 
      as.character() |> 
      fct_recode(Yes = "TRUE", No = "FALSE"),
    intgen.fam.cat = case_when(
      intgen.fam == "Yes" & intgen.fam.imm == "No" ~ "IG other fam",
      intgen.fam == "Yes" & intgen.fam.imm == "Yes" ~ "IG child",
      intgen.fam == "No" ~ "Other"
    ) %>% fct_relevel("IG child", "IG other fam")
  )

# Alter role
alter.data <- alter.data |> 
  mutate(
    role = factor(role),
    role.rec = fct_relevel(role.rec, "imm.fam", "ext.fam" , "friend", "neighbor",
                           "rel.vol", "work"),
    role.rec.3 = fct_collapse(role.rec, other = c("friend", "rel.vol", "work", "neighbor", "other")) |> 
      fct_relevel("other")
  )

# Replace with variable with imputed NA's
imp.var <- alter.data.imp |> 
  dplyr::select(alter_id, intgen.fam.act)
alter.data <- alter.data |> 
  dplyr::select(-intgen.fam.act) |> 
  left_join(imp.var, by = "alter_id") 

# Relevel factors
alter.data <- alter.data |> 
  mutate(
  alter.sex = fct_relevel(alter.sex, "Male"),
  freq.rec = fct_relevel(freq.rec, "yearly", "monthly", "weekly", "daily"),
  week.daily = fct_relevel(week.daily, "No"),
  alter.cores = fct_relevel(alter.cores, "No")
  )

# Interaction frequency: numeric version
alter.data <- alter.data |> 
  mutate(
    freq.rec.num = as.numeric(freq.rec)
  )

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Create and recode ego variables                                          ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Factors
ego.data <- ego.data |> 
  mutate(
    random = fct_relevel(random, "No"), 
    gender = fct_relevel(gender, "Male"), 
    marital =	fct_relevel(marital, "Not married"), 
    educ = fct_relevel(educ, "primary", "middle", "high"),
    occ.typ = fct_relevel(occ.typ, "1. Elementary/menial", "2. Skilled crafts/trades",
                          "3. Retail/office employee", "4. Professional/self-employed"),	
    dis.phys.count.cat = fct_relevel(dis.phys.count.cat, "None", "One"),
    hcare.count.cat = fct_relevel(hcare.count.cat, "None", "One"),
    adl.rec = fct_relevel(adl.rec, "No"),
    gds.rec = fct_relevel(gds.rec, "0", "1"),
    demen = fct_relevel(demen, "No"),
    rsa = fct_relevel(rsa, "No"),
    tel.int = fct_relevel(tel.int, "No")
  )
  
# Standardized age
ego.data <- ego.data |> 
  mutate(
    age.st = as.vector(scale(age))
  )

# Counts of different types of alters
alter.counts <- alter.data |> 
  group_by(ego_id) |> 
  dplyr::summarise(
    alt.count = n(),
    act.count = sum(week.daily == "Yes", na.rm = TRUE),
    fam.count = sum(role.rec %in% c("imm.fam", "ext.fam"), na.rm = TRUE),
    ig.fam.count = sum(intgen.fam == "Yes", na.rm = TRUE),
    ig.fam.imm.count = sum(intgen.fam.imm == "Yes", na.rm = TRUE),
    ig.fam.act.count = sum(intgen.fam.act == "Yes", na.rm = TRUE)
  )

# Ego ids 74 and 200 don't appear in alter.data because they didn't nominate any 
# alter.

# ego.data |> 
#   filter(ego_id %in% c(74, 200))
# alter.data |> 
#   filter(ego_id %in% c(74, 200))

# So set alter counts to 0 for ego ids 74 and 200.
count.names <- alter.counts |> 
  dplyr::select(ends_with("count")) |> 
  names()
zero.counts <- rep(0, length(count.names)) |> 
  set_names(count.names)
alter.counts <- tibble(ego_id = c(74, 200), 
                       !!!zero.counts) |> 
  bind_rows(alter.counts) |> 
  arrange(ego_id)

# Join with main ego.data
ego.data <- ego.data |> 
  left_join(alter.counts, by = "ego_id")

# Replace with variable with imputed NA's
imp.var <- ego.data.imp |> 
  dplyr::select(ego_id, ig.fam.count, ig.fam.act.count)
ego.data <- ego.data |> 
  dplyr::select(-c(ig.fam.count, ig.fam.act.count)) |> 
  left_join(imp.var, by = "ego_id")

# Binary versions
ego.data <- ego.data |> 
  mutate(
    has.ig.fam = case_when(
      ig.fam.count > 0 ~ "Yes",
      is.na(ig.fam.count) ~ NA_character_,
      ig.fam.count == 0 ~ "No") %>% fct_relevel("No"),
    has.ig.fam.act = case_when(
      ig.fam.act.count > 0 ~ "Yes",
      is.na(ig.fam.act.count) ~ NA_character_,
      ig.fam.act.count == 0 ~ "No") %>% fct_relevel("No"),
    has.ig.fam.imm = case_when(
      ig.fam.imm.count > 0 ~ "Yes",
      is.na(ig.fam.imm.count) ~ NA_character_,
      ig.fam.imm.count == 0 ~ "No") %>% fct_relevel("No")
  )

# Alter proportions
ego.data <- ego.data |> 
  mutate(
    ig.fam.prop = ig.fam.count/alt.count,
    ig.fam.act.prop = ig.fam.act.count/act.count,
    ig.fam.imm.prop = ig.fam.imm.count/alt.count
  )

# Replace with variable with imputed NA's
imp.var <- ego.data.imp |> 
  dplyr::select(ego_id, ig.fam.prop, ig.fam.imm.prop)
ego.data <- ego.data |> 
  dplyr::select(-c(ig.fam.prop, ig.fam.imm.prop)) |> 
  left_join(imp.var, by = "ego_id")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Variables from alter-alter ties                                          ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Create igraph list to calculate alter-level centrality
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Temporary copies of data used to create egor object and graph list
ego.data.copy <- ego.data
alter.ties.copy <- alter.ties
alter.data.copy <- alter.data

# Alter id 15519 appears in alter tie data but is missing in alter attribute data.

# alter.ties.copy |> 
#   filter(alttarg_id == 15519)
# alter.data.copy |> 
#   filter(alter_id == 15519)

# To avoid egor error, manually add alter 15519 to alter attribute data
# (will have all alter attributes NA).
alter.data.copy <- alter.data.copy |> 
  add_row(ego_id = 155, alter_id = 15519)

# Get egor object    
egor.ob <- egor(alters = alter.data.copy,
                egos = ego.data.copy,
                aaties = alter.ties.copy,
                ID.vars = list(
                  ego = "ego_id",
                  alter = "alter_id",
                  source = "altsource_id",
                  target = "alttarg_id"))

# Convert to list of igraphs
graph.list <- egor::as_igraph(egor.ob) |> 
  # Remove list elements (graphs) in which V(graph)$name is NULL, i.e. empty
  # graphs (the egos who have no alter)
  discard(~ is.null(V(.x)$name))


# Get alter centrality
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Preassign
cent.df <- list()

# Regular degree
cent.df$deg <- graph.list |> 
  map(~ igraph::degree(.x, normalize = FALSE) |> 
        enframe(name = "alter_id", value = "deg") |> 
        mutate(alter_id = as.numeric(alter_id))
  ) |> 
  bind_rows()

# Normalized degree: proportion of other alters known by alter i
cent.df$deg.norm <- graph.list |> 
  map(~ igraph::degree(.x, normalize = TRUE) |> 
        enframe(name = NULL, value = "deg.norm") 
  ) |> 
  bind_rows()

# Regular betweenness
cent.df$betw <- graph.list |> 
  map(~ igraph::betweenness(.x, normalize = FALSE) |> 
        enframe(name = NULL, value = "betw")
  ) |> 
  bind_rows()

# Bind all 3 measures in one df
cent.df <- bind_cols(cent.df)

# Join with alter_id, ego_id
cent.df <- alter.data |> 
  dplyr::select(alter_id, ego_id, intgen) |> 
  left_join(cent.df, by = "alter_id")

# Get alter-level degree/betweenness relative to max in ego-network
cent.df <- cent.df |> 
  group_by(ego_id) |> 
  summarise(
    ego_id = ego_id,
    alter_id = alter_id,
    intgen = intgen,
    deg = deg,
    deg.norm = deg.norm, 
    betw = betw,
    deg.prop = deg/max(deg),
    betw.prop = betw/max(betw),
    .groups = "drop"
  ) 

# Add centrality measures to alter data
cent.meas <- cent.df |> 
  dplyr::select(ego_id, alter_id, 
                alter.deg.norm = deg.norm, 
                alter.deg.prop = deg.prop, 
                alter.betw.prop = betw.prop)
alter.data <- alter.data |> 
  left_join(cent.meas, by = c("ego_id", "alter_id"))

# Replace with variable with imputed NA's
imp.var <- alter.data.imp |> 
  dplyr::select(alter_id, alter.deg.norm)
alter.data <- alter.data |> 
  dplyr::select(-alter.deg.norm) |> 
  left_join(imp.var, by = "alter_id")

# Remove egor object and graph list
rm(egor.ob, graph.list)

# Summarize into ego-level IG centrality measures
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Filter to IG alters only
cent.df.ego <- cent.df |> 
  filter(intgen == "Yes") |> 
  # For each ego_id, get max value of centrality measures among IG alters 
  group_by(ego_id) |> 
  summarise(
    deg.norm = max(deg.norm),
    deg.prop = max(deg.prop), 
    betw.prop = max(betw.prop)
  )

# Bring new measures into ego.data
ego.data <- ego.data |> 
  left_join(cent.df.ego, by = "ego_id")

# Categorical versions
ego.data <- ego.data |> 
  mutate(
    deg.norm.cat = case_when(
      deg.norm == 1 ~ "Max",
      deg.norm > 0 & deg.norm < 1 ~ "Positive",
      deg.norm == 0 ~ "Zero",
      ig.fam.count == 0 ~ "No IG"
    ) %>% fct_relevel("No IG", "Zero", "Positive", "Max"),
    deg.prop.cat = case_when(
      deg.prop == 1 ~ "Max",
      deg.prop > 0 & deg.prop < 1 ~ "Positive",
      deg.prop == 0 ~ "Zero",
      ig.fam.count == 0 ~ "No IG"
    ) %>% fct_relevel("No IG", "Zero", "Positive", "Max"),
    betw.prop.cat = case_when(
      betw.prop == 1 ~ "Max",
      betw.prop > 0 & betw.prop < 1 ~ "Positive",
      betw.prop == 0 ~ "Zero",
      ig.fam.count == 0 ~ "No IG"
    ) %>% fct_relevel("No IG", "Zero", "Positive", "Max")
  ) 

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Join ego-level and alter-level data                                      ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Merge alter and ego data
alter.ego.data <- left_join(alter.data, ego.data, by = "ego_id") 


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Create graph list                                                        ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Alter id 15519 appears in alter tie data but is missing in alter attribute data.

# alter.ties.copy |> 
#   filter(alttarg_id == 15519)
# alter.data.copy |> 
#   filter(alter_id == 15519)

# To avoid egor error, manually add alter 15519 to alter attribute data
# (will have all alter attributes NA).
alter.data <- alter.data |> 
  add_row(ego_id = 155, alter_id = 15519)

# Get egor object    
egor.ob <- egor(alters = alter.data,
                egos = ego.data,
                aaties = alter.ties,
                ID.vars = list(
                  ego = "ego_id",
                  alter = "alter_id",
                  source = "altsource_id",
                  target = "alttarg_id"))

# Convert to graph list with ego node included
graph.list.ego <- egor::as_igraph(egor.ob, include.ego = TRUE) %>%
  # Set appropriate attributes for ego node in each graph
  map(~ set_ego_attr(.x, alter.data = alter.data))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Filter to egos in full probability sample and save                       ====
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Filter all data objects to just random sample of egos

## Get ego IDs of random sample
ego.id.r <- ego.data %>%
  filter(random == "Yes") %>%
  pull(ego_id)

## Save data with all egos in different objects
ego.data.fs <- ego.data
alter.data.fs <- alter.data 
alter.ego.data.fs <- alter.ego.data 
graph.list.ego.fs <- graph.list.ego

## Filter data objects to random sample
ego.data.rs <- ego.data %>%
  filter(ego_id %in% ego.id.r)
alter.data.rs <- alter.data %>%
  filter(ego_id %in% ego.id.r)
alter.ego.data.rs <- alter.ego.data %>%
  filter(ego_id %in% ego.id.r)
graph.list.ego.rs <- graph.list.ego[as.character(ego.id.r)]

# Save data objects
save(ego.data.fs, alter.data.fs, alter.ego.data.fs, graph.list.ego.fs,
     ego.data.rs, alter.data.rs, alter.ego.data.rs, graph.list.ego.rs,
     file = "./data/data_clean.rda")

