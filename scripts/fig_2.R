# Load model fit results
load("./results/model_fit.rda")

# Labels for DVs
mod.df.tie <- mod.df.tie |> 
  # Labels
  add_column(lab = c("IG", "IG active", "Child")) |> 
  # Shapes
  add_column(shape = c(19, 17, 15))

# Shapes vector
IG.type.shapes <- mod.df.tie |> 
  pull(shape) |> 
  set_names(mod.df.tie$DV)

# Get estimates, p values, conf intervals from model fit
mod.df.plot <- mod.df.tie |>
  rowwise() |>
  mutate(
    tab = list(
      tidy_glmer(mod, p.value = TRUE, conf.int = TRUE, 
                 term.labels = term.labels.tie))
  ) |>
  dplyr::select(DV, tab)

# Unnest df
mod.df.plot <- mod.df.plot |> 
  unnest(tab)

# Sign/significance of estimate
mod.df.plot <- mod.df.plot |> 
  mutate(est.sign = case_when(
    p.value >= 0.05 ~ "Not significant",
    estimate > 0 & p.value < 0.05 ~ "Positive",
    estimate < 0 & p.value < 0.05 ~ "Negative",
  ),
  # Factors
  term = factor(term, levels = term.labels.tie.fig$term) |> fct_rev(),
  DV = factor(DV, levels = mod.df.tie$DV) |> fct_rev()
  ) |> 
  # Filter to terms of interest for figure
  filter(str_detect(term, pattern = "alter|freq"))
  

# Plot
p <- ggplot(mod.df.plot, aes(x= term, group= DV)) + 
  geom_hline(yintercept = 0, color= "grey50", alpha=0.8) +
  geom_errorbar(aes(ymin= conf.low, ymax= conf.high, color=est.sign, size = est.sign), 
                width = 0, position = position_dodge(width = 0.2)) + # size = 0.3,
  geom_point(aes(y= estimate, shape= DV, color=est.sign), 
             size = 1.5, position = position_dodge(width = 0.2)) +
  scale_x_discrete(breaks = term.labels.tie.fig$term,
                   labels = term.labels.tie.fig$label) + # expand = expansion(add = 0, mult= 0.5)
  scale_color_manual(values= c(Negative = "red", Positive = "blue", 
                               `Not significant` = "grey60"), na.translate = FALSE) +
  scale_size_manual(values= c(Negative = 0.5, Positive = 0.5, 
                              `Not significant` = 0.3), na.translate = FALSE,
                    guide = "none") +
  scale_shape_manual(values= IG.type.shapes,
                     breaks = mod.df.tie$DV,
                     labels = mod.df.tie$lab,
                     na.translate = FALSE) +
  coord_flip(expand = TRUE) + 
  labs(y="Coefficient estimate", x="", shape = "P(Tie is...)", 
       color = "Sign/significance") +
  # For position of legend titles
  guides(color = guide_legend(title.position="top"),
         shape = guide_legend(title.position="top")) +
  theme_classic() + 
  theme(
    panel.grid.major.x = element_line(color="white"), 
    axis.line.y.left = element_blank(),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(face = "bold", size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.9)),
    panel.background = element_rect(color = "grey30", fill = scales::alpha("grey80", 0.25)),
    legend.justification = "center",
    legend.position="right"
  ) 