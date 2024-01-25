# Load data (full sample)
load("./data/data_clean.rda")
ego.data <- ego.data.fs

# Ego-level dependent variables (DVs)
ego.DVs <- c(
  "ig.fam.count",
  "ig.fam.act.count",
  "ig.fam.prop", 
  "ig.fam.act.prop", 
  "ig.fam.imm.prop"
)

# Ego-level independent variables (IVs)
ego.IVs.cat.df <- tribble(~ variable, ~ binary, ~ label,
  "gender", "Yes", "Gender",
  "marital", "Yes", "Marital status",
  "educ", "No", "Education",
  "occ.typ", "No", "Occupation",
  "dis.phys.count.cat", "No", "N physical dis.",
  "hcare.count.cat", "No", "Health care",
  "adl.rec", "Yes", "Limited ADL",
  "gds.rec", "No", "GDS",
  "demen", "Yes", "Dementia"
)

# Same as above, as vector
ego.IVs.cat <- ego.IVs.cat.df |> 
  pull(variable)

# Same as above, as named vector
ego.IV.labs <- ego.IVs.cat |> 
  set_names(ego.IVs.cat.df$label)

# Get mean (SD) of alter counts/proportions by ego IVs                      ----
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-assign list
res.table <- list()

# For each IV...
for (i in seq_along(ego.IVs.cat)) {
  
  res.table[[i]] <- ego.data |>
    # Select DVs and relevant IVs
    dplyr::select(!!ego.DVs, ego.IVs.cat[[i]]) |>
    # Group by relevant IV, then skim
    group_by(!!rlang::sym(ego.IVs.cat[[i]])) |>
    skim() |>
    as_tibble() |>
    # Select relevant variables
    dplyr::select(DV = skim_variable, mean = numeric.mean, sd = numeric.sd, 
                  cat = !!rlang::sym(ego.IVs.cat[[i]])) |>
    mutate(IV = ego.IVs.cat[[i]]) |>
    mutate(DV.type = map_chr(DV, ~ str_split(.x, "\\.") %>% 
                               unlist %>% 
                               pluck(length(.))))
  
}

# Join tables from all IVs
res.table.df <- bind_rows(res.table)

## Get p-values from relevant tests                                         ----
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# All combinations of relevant DVs and IVs
pvals <- tidyr::crossing(ego.DVs, ego.IVs.cat) |>
  # Binary IV?
  left_join(ego.IVs.cat.df, by = c("ego.IVs.cat" = "variable")) |> 
  dplyr::select(-label)

# For binary IVs  
pvals.bin <- pvals |>
  filter(binary == "Yes") |>
  # Get p-value from appropriate test
  rowwise() |>
  # Parametric tests for binary case (2 groups) 
  mutate(
    pval.par = ttest_pval(var=ego.DVs, groups=ego.IVs.cat, df=ego.data)
  )

# Non binary IVs
pvals.nbin <- pvals |>
  filter(binary == "No") |>
  # Get p-value from appropriate test
  rowwise() |>
  # Parametric tests for non binary case (>2 groups) 
  mutate(
    pval.par = anova_pval(var=ego.DVs, groups=ego.IVs.cat, df=ego.data),
  )

# All p values together
all.pvals <- bind_rows(pvals.bin, pvals.nbin) |>
  ungroup() |>
  dplyr::select(DV = ego.DVs, IV = ego.IVs.cat, pval = pval.par)


## Bar charts for mean (SD) of alter counts/proportions                     ----
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Join mean (SD) data with p-values
res.table.df <- res.table.df |> 
  left_join(all.pvals, by = c("DV", "IV"))


# Create var:cat variable
res.table.df <- res.table.df |> 
  mutate(var.cat = paste(IV, as.vector(cat), sep=":"), 
         IV = as_factor(IV),
         DV = as_factor(DV),
         DV.type = as_factor(DV.type),
         # Recode cat to short category labels
         cat = fct_recode(cat,
                          !!!term.labels.biv.fig)
  )

# Before each DV-IV combination, add row with IV name and appropriate values
# for other variables
res.table.df <- res.table.df |> 
  # Split data frame by DV-IV
  group_by(DV, IV) |>
  group_split() |>
  map(~ add_row(.x, 
                DV = .x$DV[[1]], 
                DV.type = .x$DV.type[[1]], 
                mean = 0,
                cat = .x$IV[[1]],
                IV = .x$IV[[1]], 
                var.cat = .x$IV[[1]],
                .before = 1)) |>
  # Re-join all data frame pieces by IV
  bind_rows()   

# Y axis labels for IVs
y.labs.df <- res.table.df |>
  dplyr::select(IV, var.cat, cat) |>
  mutate(cat = fct_recode(cat,
                          !!!ego.IV.labs)) |>
  unique()

# Labels for y axis in the plot
y.labs <- y.labs.df |>
  pull(cat) |>
  as.vector() |> 
  set_names(y.labs.df$var.cat)

# Set distance of text from bars
text.dist <- 0.03

# Reduction factors for length of horizontal bars
bar.red.factor.count <- 1/12
bar.red.factor.prop <- 1/1.1

# Create factors
res.table.df <- res.table.df |> 
  mutate(var.cat = as_factor(var.cat),
         var.cat = fct_rev(var.cat)
  ) |>
  # Aesthetics for errorbarh 
  # Note that bar reduction factor changes between counts and props
  mutate(xmax= case_when(
    DV.type == "count" ~ as.numeric(DV) + abs(mean*bar.red.factor.count),
    DV.type == "prop" ~ as.numeric(DV) + abs(mean*bar.red.factor.prop)
  )) |>
  # Aesthetics for geom_text
  mutate(xtext = as.numeric(DV)-text.dist, 
         label = case_when(
           # For rows with response categories
           !is.na(sd) ~ paste0(round(mean, 2), " (", round(sd, 2), ")"),
           # For rows with variable names
           is.na(sd) ~ ""
         )
  ) |>
  mutate(
    # Same as mean, but NA when pval < 0.5
    mean.pv = case_when(
      pval < 0.05 ~ mean,
      TRUE ~ NA_real_
    ),
    significant = case_when(
      pval < 0.05 ~ "yes",
      pval >= 0.05 ~ "no"
    )
  )

# Set other plot parameters
bar.size <- 3.5
text.size <- 2.5

# Color for non-significant bars
ns.col <- "grey60"

# Labels, titles
scale.x.lab <- "Mean (SD)"
plot.titles <- c("Counts", "Proportions")

# Theme options
rel.y = c(8.5, 8)
rel.x = 1
angle.x = 0
rel.leg.tit = 0.6
rel.leg.tex = 0.6
leg.size = 0.6

# Get the plot
plot.df <- res.table.df
p1 <- ggplot(plot.df) + 
  # Horizontal bars
  geom_errorbarh(aes(xmin=DV, xmax= xmax, y= var.cat, color= mean.pv), 
                 height= 0, size= bar.size) +
  # Labels with means
  geom_text(aes(x= xtext, y= var.cat, label= label, color= mean.pv), 
            size = text.size, hjust = "right") +
  # Scales
  scale_color_gradient2(low="white", high="darkblue", mid="darkblue", 
                        na.value = ns.col) + 
  scale_x_discrete(position = "top",
                   labels = rep(scale.x.lab, n_distinct(plot.df$DV))) + 
  scale_y_discrete(position = "left",
                   labels = as_labeller(y.labs)) + 
  labs(x="", y= "", color = "Mean") +
  # Theme
  theme_bw() +
  theme(panel.grid.major.y= element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_line(colour = "grey30"),
        # Greater size if label is variable name
        axis.text.y = element_text(size = ifelse(levels(plot.df$var.cat) %in% plot.df$IV, rel.y[1], rel.y[2]),
                                   # Bold if label is variable name
                                   face = ifelse(levels(plot.df$var.cat) %in% plot.df$IV, "bold", "plain")), 
        axis.ticks.y = element_line(color = ifelse(levels(plot.df$var.cat) %in% plot.df$IV, NA, "grey30")),
        axis.text.x = element_text(size=rel(rel.x), angle = angle.x, color = "black"), 
        legend.title = element_text(size= rel(rel.leg.tit)), 
        legend.text = element_text(size= rel(rel.leg.tex)), 
        legend.key.size = unit(leg.size, "line"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0, 0, 0.1, 0.1, "cm"),
        panel.border = element_blank()
  ) + 
  # Trick to keep bottom horizontal "axis:
  annotate(geom = 'segment', colour = "grey30", x= -Inf, xend = Inf, y = -Inf, yend = -Inf) 

# Save plot data for later
age.table.df <- plot.df


## Age: empty bar chart with cor values                                     ----
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Relevant variables
raw.data <- ego.data |>
  dplyr::select(age, all_of(ego.DVs)) |>
  # Pivot data frame
  pivot_longer(cols =  all_of(ego.DVs), 
               names_to = "DV", values_to = "value") |>
  mutate(DV = as_factor(DV))

# Pearson correlation values and p-values
cor.df <- raw.data |>
  group_by(DV) |>
  group_split() |>
  map_dfr(~ cor.test(.x[["age"]], .x[["value"]], method = "pearson") |>
            tidy()) |>
  # Add ego DV name
  add_column(DV = levels(raw.data$DV), .before = 1) |>
  # Only keep relevant cols
  dplyr::select(DV, cor = estimate, pval = p.value) |>
  # Add pvalue stars
  mutate(pval.star = case_when(
    pval < .05 ~ "*",
    pval >= .05 ~ ""
  )) |>
  # Create final value
  mutate(cor.num = round(cor, 2),
         cor = paste0(cor.num, pval.star),
         label = paste0(cor.num, "\n(", round(pval, 2), ")"),
         label = str_replace_all(label, "\\(0\\)", "(<.001)")
  ) 

# Amount to substract to xtext (x coordinate of text label)
xtext.offset <- 0.12

# Title for the DV columns
scale.x.labs <- c(
  "Count IG",
  "Count IG active",
  "Prop. IG",                            
  "Prop. IG active",
  "Network concentration\non adult children"
) |> 
  paste0("\n\nCor (p-value)")

# Max lenght of (white, invisible) horizontal bars
xmax <- age.table.df |>
  pull(xmax) |>
  max()

# Add 
cor.df <- cor.df |> 
  add_column(xmax = xmax, var.cat = "Age")

# Get df used for previous plot
cor.df <- age.table.df |>
  # Only keep column headers, no rows
  filter(var.cat == "Age") |>
  # Add rows from cor.df
  bind_rows(cor.df) |>
  # Cor value with NA when not significant
  mutate(cor.pv = case_when(
    pval < .05 ~ cor.num,
    pval >= .05 ~ NA_real_,
  )) |>
  # Remove xtext, to be added later
  dplyr::select(-xtext)

# Add xtext
xtext.df <- age.table.df |>
  dplyr::select(DV, xtext) |>
  unique() |>
  mutate(xtext = xtext - xtext.offset)
plot.df <- cor.df |>
  right_join(xtext.df, by = "DV")

# Create the plot
p2 <- ggplot(plot.df) + 
  # Horizontal bars
  geom_errorbarh(aes(xmin=DV, xmax= xmax, y= var.cat), 
                 color = NA, height= 0, size= bar.size) +
  # Labels with means
  geom_text(aes(x= xtext, y= var.cat, label= label, color= cor.pv), 
            size = text.size, hjust = "center") +
  # Scales
  scale_color_gradient2(low="white", high="darkblue", mid="lightblue", 
                        na.value = ns.col) + 
  scale_x_discrete(position = "top",
                   labels = scale.x.labs) + 
  scale_y_discrete(position = "left",
                   labels = "Age",
                   expand = expansion(add = 2)) + 
  labs(x="", y= "") +
  # Theme
  theme_bw() +
  theme(panel.grid.major.y= element_blank(), 
        panel.grid.major.x= element_blank(), 
        axis.line.x = element_line(colour = "grey30"),
        axis.line.y = element_line(colour = "grey30"),
        # Greater size if label is variable name
        axis.text.y = element_text(size = rel.y[1],
                                   # Bold if label is variable name
                                   face = "bold"), 
        axis.ticks.y = element_line(color = NA),
        axis.text.x = element_text(size=rel(rel.x), angle = angle.x, color = "black"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0, 0, 0, 1.5, "cm"),
        panel.border = element_blank()
  )  

## Age-DV scatterplots as inset plots                                       ----
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get list of relevant data frames for scatterplots, one for each DV
df.plot.list <- raw.data |>
  group_by(DV) |>
  group_split()

# Create scatterplot for each data frame
set.seed(287)
inset.plot.list <- df.plot.list |>
  map(~ .x |>
        ggplot(aes(x=age, y=value)) +
        geom_point(shape = 21, size = 0.3, color = "grey40", alpha = 0.5, position = position_jitter(width = 0.2)) +
        geom_smooth(method = "lm", se = FALSE, color = "darkblue") + 
        scale_x_continuous(breaks = seq(75, 100, by = 5)) + 
        coord_cartesian(xlim = c(73, 102)) +
        labs(y = "", x = "", color = "", title = "") +
        theme_bw() + 
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.minor.y= element_blank(),
              panel.grid.minor.x= element_blank(),
              panel.grid.major.y= element_blank(),
              panel.grid.major.x= element_blank(),
              panel.border = element_rect(color = "grey70", linewidth = 1),
              plot.margin = margin(-0.5, 0, 0, 0, "cm")
        )
  )

# Add inset scatterplot 1
x <- 0.11
y <- 0.2
xlen <- 0.11
ylen <- 0.68
pos <- c(x, y, x+xlen, y+ylen)

p2 <- p2 + inset_element(inset.plot.list[[1]],
                           pos[1], pos[2], pos[3], pos[4])

# Add inset scatterplot 2
x <- 0.3
y <- 0.2
pos <- c(x, y, x+xlen, y+ylen)

p2 <- p2 + inset_element(inset.plot.list[[2]],
                           pos[1], pos[2], pos[3], pos[4])

# Add inset scatterplot 3
x <- 0.5
y <- 0.2
pos <- c(x, y, x+xlen, y+ylen)

p2 <- p2 + inset_element(inset.plot.list[[3]],
                           pos[1], pos[2], pos[3], pos[4])


# Add inset scatterplot 4
x <- 0.69
y <- 0.2
pos <- c(x, y, x+xlen, y+ylen)

p2 <- p2 + inset_element(inset.plot.list[[4]],
                           pos[1], pos[2], pos[3], pos[4])


# Add inset scatterplot 5
x <- 0.88
y <- 0.2
pos <- c(x, y, x+xlen, y+ylen)

p2 <- p2 + inset_element(inset.plot.list[[5]],
                           pos[1], pos[2], pos[3], pos[4])


## Join all together in one figure                                          ----
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

p <- ggarrange(p2, p1,
                 ncol = 1, nrow = 2,
                 heights = c(0.22, 1)
)
