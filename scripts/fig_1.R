# Load data
load("./data/data_clean.rda")

# Select the four ego-networks to plot
graph.list.ego <- graph.list.ego.fs[c("72", "82", "76", "21")] 

# Create edge attribute to flag ego-alter ties
graph.list.ego <- graph.list.ego |> 
  map(function(x) {
  E(x)$tie.type <- "alter.alter"
  E(x)[.inc("ego")]$tie.type <- "ego.alter"
  x 
}
)

# Create network layouts
set.seed(709)
lay.list.con <- graph.list.ego |> 
  map(~ create_layout(.x, "centrality", cent = V(.x)$freq.rec.num))

# Create plots
plots <- lay.list.con |> 
  map(~ plot.fun(lay = .x,
           node.var = "intgen.fam.cat",
           fills = c(`IG child` = "darkblue", `IG other fam` = "#4DAF4A", Other = "#F7FBFF"),
           cols = c(`IG child` = "darkblue", `IG other fam` = "darkgreen", Other = "darkblue", 
                    ego = "#E41A1C"),
           sizes = c(`IG child` = 2.5, `IG other fam` = 2.5, Other = 2, ego = 4), 
           margs = rep(0, 4)
           )
  )

# Arrange plots in one figure
ggarrange(plotlist = plots, ncol = 2, nrow = 2) |> 
  print()

