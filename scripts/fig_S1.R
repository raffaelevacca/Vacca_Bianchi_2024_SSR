# Load model estimation results
load("./results/model_fit.rda")

# Preassign model fit list
mod.df.list <- list()

# Count models
mod.df.list[["counts"]] <- mod.df.count |> 
  # Limit to full sample results
  filter(sample == "full")

# Prop models
mod.df.list[["proportions"]] <- mod.df.prop |> 
  # Limit to full sample results
  filter(sample == "full")


## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure column 1: Age and gender                                           ====
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Type of IG tie: IG
DV <- "ig.fam.count"

# Terms for ggpredict
mod.terms <- c("age [all]", "gender")

# DV: IG count
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# DV type
DV.type <- "counts"

# Get appropriate model
mod <- mod.df.list |> 
  pluck(DV.type) |> 
  filter(DV == !!DV) |> 
  pull(mod) |> 
  pluck(1)

# Get predicted values
pred <- ggpredict(mod, terms = mod.terms) 

# Clean data for plotting
df.plot <- pred |> 
  as_tibble() |> 
  mutate(DV = paste0(DV, ", ", DV.type))

# DV: IG proportion
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# DV type
DV.type <- "proportions"

# Get appropriate model
mod <- mod.df.list |> 
  pluck(DV.type) |> 
  filter(count.var == !!DV) |> 
  pull(mod) |> 
  pluck(1)

# Get predicted values
pred <- ggpredict(mod, terms = mod.terms) 

# Clean data for plotting and bind with data for counts
df.plot <- pred |> 
  as_tibble() |> 
  mutate(DV = paste0(DV, ", ", DV.type)) %>%
  bind_rows(df.plot, .)

# Labels for plot facets
values <- df.plot |> 
  pull(DV) |> 
  unique()
labels <- c("(A) Count IG", "(D) Proportion IG")
facet.labs <- labels |> 
  set_names(values)

# Plot
p.col.1 <- ggplot(df.plot, aes(x = x, y = predicted, group = group)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .4) + 
  geom_line(aes(color = group), lwd = 1.5) + 
  facet_wrap(~ DV, nrow = 2, scales = "free_y", labeller = as_labeller(facet.labs)) + 
  scale_x_continuous(breaks = seq(75, 95, by = 5)) +
  # ggh4x function to set scales of specific facets (first facet here)
  ggh4x::facetted_pos_scales(y = list(scale_y_continuous(breaks = seq(2, 12, by = 2), limits = c(2,13)), NULL)) +
  coord_cartesian(xlim = c(75, 95)) +
  labs(x = "Age", y = "", fill = "Gender", color = "Gender") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = rel(0.9)))


## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure column 2: Education and occupation                                 ====
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Type of IG tie: IG active
DV <- "ig.fam.act.count"

# Terms for ggpredict
mod.terms <- c("occ.typ", "educ")

# Set labels for occupations
occ.labs <- c(`1. Elementary/menial` = "1-Elem./menial",
              `2. Skilled crafts/trades` = "2-Skilled trades",
              `4. Professional/self-employed` = "4-Prof./self-empl."
)

# Occupations to exclude from plot
occ.excl <- c(
  "3. Retail/office employee",
  "5. Manager/executive"
)

# Set labels for education
edu.labs <- c(primary = "Primary",
              middle = "Middle",
              high = "HS",
              `uni or higher` = "Uni+"
)

# DV: IG active count
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Count or proportion
DV.type <- "counts"

# Get appropriate model
mod <- mod.df.list |> 
  pluck(DV.type) |> 
  filter(DV == !!DV) |> 
  pull(mod) |> 
  pluck(1)

# Get predicted values
pred <- ggpredict(mod, terms = mod.terms) 

# Clean data for plotting
df.plot <- pred |> 
  as_tibble() |> 
  # Only keep relevant occ levels
  filter(!(x %in% !!occ.excl)) |> 
  mutate(DV = paste0(DV, ", ", DV.type))

# DV: IG active proportion
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DV.type <- "proportions"

# Get appropriate model
mod <- mod.df.list |> 
  pluck(DV.type) |> 
  filter(count.var == !!DV) |> 
  pull(mod) |> 
  pluck(1)

# Get predicted values
pred <- ggpredict(mod, terms = mod.terms) 

# Clean data for plotting and bind with data for counts
df.plot <- pred |> 
  as_tibble() |> 
  # Only keep relevant occ levels
  filter(!(x %in% !!occ.excl)) |> 
  mutate(DV = paste0(DV, ", ", DV.type)) %>%
  bind_rows(df.plot, .)

# Labels for facets
values <- df.plot |> 
  pull(DV) |> 
  unique()
labels <- c("(B) Count IG active", "(E) Proportion IG active")
facet.labs <- labels |> 
  set_names(values)

# Plot
p.col.2 <- ggplot(df.plot, aes(x = x, y = predicted, group = group, color = group)) + 
  geom_line() + 
  geom_point(size = 2) + 
  facet_wrap(~ DV, nrow = 2, scales = "free_y", labeller = as_labeller(facet.labs)) + 
  scale_x_discrete(labels = occ.labs) + 
  scale_color_discrete(labels = edu.labs) + 
  # ggh4x function to set scales of specific facets
  facetted_pos_scales(y = list(scale_y_continuous(breaks = 2:6, limits = c(2,6.5)), 
                               scale_y_continuous(breaks = seq(0.2, 0.6, by = .1), 
                                                  limits = c(0.25, 0.65))
  )
  ) +
  labs(x = "Occupation", y = "", color = "Education") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = rel(0.9)))


## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure column 3: Health care events and GDS                               ====
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Type of IG tie: IG active
DV <- "ig.fam.act.count"

# Terms for ggpredict
mod.terms <- c("hcare.count.cat", "gds.rec")

# DV: IG active count
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DV.type <- "counts"

# Get appropriate model
mod <- mod.df.list |> 
  pluck(DV.type) |> 
  filter(DV == !!DV) |> 
  pull(mod) |> 
  pluck(1)

# Get predicted values
pred <- ggpredict(mod, terms = mod.terms) 

# Clean data for plotting
df.plot <- pred |> 
  as_tibble() |> 
  mutate(DV = paste0(DV, ", ", DV.type))

# DV: IG active proportion
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DV.type <- "proportions"

# Get appropriate model
mod <- mod.df.list |> 
  pluck(DV.type) |> 
  filter(count.var == !!DV) |> 
  pull(mod) |> 
  pluck(1)

# Get predicted values
pred <- ggpredict(mod, terms = mod.terms) 

# Clean data for plotting and bind with data for counts
df.plot <- pred |> 
  as_tibble() |> 
  mutate(DV = paste0(DV, ", ", DV.type)) %>%
  bind_rows(df.plot, .)

# Labels for facets
values <- df.plot |> 
    pull(DV) |> 
    unique()
labels <- c("(C) Count IG active", "(F) Proportion IG active")
facet.labs <- labels |> 
  set_names(values)

# Plot
p.col.3 <- ggplot(df.plot, aes(x = x, y = predicted, group = group, color = group)) + 
  geom_line() + 
  geom_point(size = 2) + 
  facet_wrap(~ DV, nrow = 2, scales = "free_y", labeller = as_labeller(facet.labs)) +
  # ggh4x function to set scales of specific facets (first facet here)
  facetted_pos_scales(y = list(scale_y_continuous(breaks = 2:8, limits = c(2,8)), 
                               scale_y_continuous(breaks = seq(0.4, 0.7, by = .1), 
                                                  limits = c(0.4, 0.7))
  )
  ) +
  labs(x = "Recent health care events", y = "", color = "GDS") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = rel(0.9)))

# Make final plot
p <- ggarrange(p.col.1, p.col.2, p.col.3, ncol = 3)

