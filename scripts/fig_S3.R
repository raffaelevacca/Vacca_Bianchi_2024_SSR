# Load model estimation results
load("./results/model_fit.rda")

# Filter to model fit on random sample
mod.df.count <- mod.df.count |> 
  filter(sample == "random") |> 
  dplyr::select(-sample)
mod.df.prop <- mod.df.prop |> 
  filter(sample == "random") |> 
  dplyr::select(-sample)

# DVs in desired order
ego.DVs <- c(mod.df.count$DV, mod.df.prop$prop.var) |> 
  # DV labels
  set_names(
    "Count IG", "Count IG active", 
    "Prop. IG", "Prop. IG active",
    "Network concentration\non adult children"
    )

# Shape for each DV
ego.DVs.shapes <- c(19, 17, 1, 2, 0) |> 
  set_names(ego.DVs)

# Terms/effects not shown in the figure
no.show.terms <- c("rsaYes", "tel.intYes")

# Models for counts: get estimates, p values, conf intervals 
mod.df.count.fig <- mod.df.count |> 
  rowwise() |>
  mutate(
    tab.count = list(tidy_zeroinfl(mod, conf.int = TRUE, p.value = TRUE, 
                                        coef.type = "count", term.labels = term.labels.ego.est.zinfl))
  ) |> 
  # Remove "count_" from term string
  mutate(
    tab.count = list(mutate(tab.count, term = str_remove(term, 
                                            "count_")))
  ) |> 
  dplyr::select(DV, tab.count)

# Models for proportions: same as above
mod.df.prop.fig <- mod.df.prop |> 
  rowwise() |>
  mutate(
    tab.prop = list(tidy_glm(mod, conf.int = TRUE, p.value = TRUE, 
                                    term.labels = term.labels.ego.est))
  ) |>
  dplyr::select(DV = count.var, tab.prop)

# Join count and prop models in one df
mod.df.plot <- full_join(mod.df.count.fig, mod.df.prop.fig, by = "DV")

# Reshape df for plotting
mod.df.plot <- mod.df.plot |> 
  # Pivot to have model type as value of single variable (column) rather than 
  # different columns
  pivot_longer(cols = -DV, names_to = "mod.type", values_to = "tab") |>
  # Unnest
  unnest(tab)

mod.df.plot <- mod.df.plot |> 
  # Sign/significance of estimate
  mutate(est.sign = case_when(
    p.value > 0.05 ~ "Not significant",
    estimate > 0 & p.value <= 0.05 ~ "Positive",
    estimate < 0 & p.value <= 0.05 ~ "Negative"
  )
  ) |>  
  # Type of IV if need to split in two plot panels
  mutate(
    IV.type = case_when(
      str_detect(term, pattern = "age|gender|educ|occ|marit") ~ "Sociodemographics",
      str_detect(term, pattern = "dis.phys|adl|gds|demen|hcare|rsa|mmse") ~ "Health"
    ) |> fct_relevel("Sociodemographics"),
    # Type of DV if needed for aestethics/panels, can be derived from model type
    DV.type = str_remove_all(mod.type, "tab\\."), 
    # DV classification combining prop/count and IG type
    DV.2 = case_when(
      DV.type == "count" ~ DV,
      DV.type == "prop" ~ str_replace(DV, ".count", ".prop")
    )
  ) |> 
  # Clean up factors
  mutate(
    term = factor(term, levels = term.labels.ego.est$term) |> fct_rev(),
    mod.type = fct_relevel(mod.type, "tab.count", "tab.prop"),
    DV = factor(DV, levels = mod.df.count$DV) |> fct_rev(),
    DV.2 = factor(DV.2, levels = ego.DVs) |> fct_rev()
  ) |>
  # Remove intercept, log theta, irrelevant terms
  filter(!is.na(label)) |>
  filter(!(term %in% no.show.terms)) |>
  # Remove rows with variable headings like "sociodemographics"
  filter(term != label) 

# Create plot
p <- ggplot(mod.df.plot, aes(x= term, group= DV.2)) + 
  geom_hline(yintercept = 0, color= "grey50", alpha=0.8) +
  geom_errorbar(aes(ymin= conf.low, ymax= conf.high, color=est.sign, size = est.sign), 
                width = 0, position = position_dodge(width = 0.5)) + # size = 0.3,
  geom_point(aes(y= estimate, shape= DV.2, color=est.sign), 
             size = 1.5, position = position_dodge(width = 0.5)) +
  scale_x_discrete(breaks = term.labels.ego.fig$term,
                   labels = term.labels.ego.fig$label) + # expand = expansion(add = 1, mult=-0.3)
  scale_color_manual(values= c(Negative = "red", Positive = "blue", 
                               `Not significant` = "grey60"), na.translate = FALSE) +
  scale_size_manual(values= c(Negative = 0.5, Positive = 0.5, 
                              `Not significant` = 0.3), na.translate = FALSE,
                    guide = "none") +
  scale_shape_manual(values= ego.DVs.shapes,
                     breaks = ego.DVs,
                     labels = names(ego.DVs),
                     na.translate = FALSE) +
  coord_flip() +
  facet_wrap(. ~ IV.type, scales="free_y") + 
  labs(y="Coefficient estimate", x="", shape = "Dependent variable", 
       color = "Sign/significance") +
  theme_classic() +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.line.y.left = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(face = "bold", size = rel(0.8)),
        axis.title.x = element_text(size = rel(0.9)),
        panel.background = element_rect(color = NA, fill = scales::alpha("grey80", 0.25))
  )
