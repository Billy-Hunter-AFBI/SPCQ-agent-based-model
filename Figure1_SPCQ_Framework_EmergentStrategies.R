# =========================================================
# FIGURE 1
# SPC-Q framework and emergent dietary strategies
# ---------------------------------------------------------
# Generates Figure 1 from SPC-Q model outputs.
#
# Panel A: Conceptual overview of the SPC-Q framework.
# Panel B: Latent sensory-predictability trait space.
# Panel C: Emergent dietary strategies.
# Panel D: Relationship between dietary entropy and
# microbiome dominance.
#
# Input:
# outputs/SPCQ_agent_summary.rds
#
# Output:
# outputs/figures/Figure1_EmergentBehaviour.png
# =========================================================

library(dplyr)
library(ggplot2)
library(patchwork)

# Load aggregated model outputs
agent_summary <- readRDS(
  "outputs/SPCQ_agent_summary.rds"
)

# =========================================================
# 0. PREP DATA
# =========================================================

# Mean behaviour across Monte Carlo replicates
agent_summary <- all_runs %>%
  group_by(agent) %>%
  summarise(
    S = first(S),
    P = first(P),
    C = first(C),
    entropy = mean(entropy),
    repertoire_size = mean(repertoire_size),
    .groups = "drop"
  )

# Classify emergent dietary strategies
agent_summary <- agent_summary %>%
  mutate(
    entropy_norm = (entropy - min(entropy)) /
      (max(entropy) - min(entropy)),
    strategy_class = case_when(
      entropy_norm < 0.33 ~ "Specialist",
      entropy_norm > 0.66 ~ "Explorer",
      TRUE ~ "Opportunist"
    )
  )

# Counts for figure annotation
n_specialist  <- sum(agent_summary$strategy_class == "Specialist")
n_opportunist <- sum(agent_summary$strategy_class == "Opportunist")
n_explorer    <- sum(agent_summary$strategy_class == "Explorer")

strategy_label <- paste0(
  "Specialists = ", n_specialist,
  "\nOpportunists = ", n_opportunist,
  "\nExplorers = ", n_explorer
)

agent_summary <- agent_summary %>%
  mutate(
    entropy_norm = (entropy - min(entropy)) /
      (max(entropy) - min(entropy)),
    strategy_class = case_when(
      entropy_norm < 0.33 ~ "Specialist",
      entropy_norm > 0.66 ~ "Explorer",
      TRUE ~ "Opportunist"
    )
  )

# =========================================================
# PANEL A — CONCEPT
# =========================================================

p_concept <- ggplot() +
  
  annotate("rect", xmin = 0.05, xmax = 0.95,
           ymin = 0.75, ymax = 0.9,
           fill = "grey90", color = "black") +
  
  annotate("text", x = 0.5, y = 0.825,
           label = "Sensory (S) | Predictability (P)  | Context (C)",
           size = 3.75, fontface = "bold") +
  
  annotate("rect", xmin = 0.2, xmax = 0.8,
           ymin = 0.55, ymax = 0.68,
           fill = "grey95", color = "black") +
  
  annotate("text", x = 0.5, y = 0.615,
           label = "State modulation\n(stress, sensory load)",
           size = 3.75) +
  
  annotate("segment", x = 0.5, xend = 0.5,
           y = 0.75, yend = 0.68,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  annotate("rect", xmin = 0.2, xmax = 0.8,
           ymin = 0.35, ymax = 0.48,
           fill = "grey90", color = "black") +
  
  annotate("text", x = 0.5, y = 0.415,
           label = "Probabilistic food choice\n(softmax + learning)",
           size = 3.75) +
  
  annotate("segment", x = 0.5, xend = 0.5,
           y = 0.55, yend = 0.48,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  annotate("rect", xmin = 0.25, xmax = 0.75,
           ymin = 0.12, ymax = 0.25,
           fill = "grey80", color = "black") +
  
  annotate("text", x = 0.5, y = 0.185,
           label = "Emergent dietary strategy\n(entropy, repertoire)",
           size = 3.75, fontface = "bold") +
  
  annotate("segment", x = 0.5, xend = 0.5,
           y = 0.35, yend = 0.25,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  theme_void()

# =========================================================
# PANEL B — LATENT TRAIT SPACE
# =========================================================
# Explorer - Red
# Opportunist - Green
# Specialist - Blue


p_latent <- ggplot(
  agent_summary,
  aes(
    x = S,
    y = P,
    colour = strategy_class
  )
) +
  
  geom_point(size = 3, alpha = 0.85) +
  
  scale_colour_manual(
    values = c(
      "Specialist"  = "#377EB8",
      "Opportunist" = "#4DAF4A",
      "Explorer"    = "#E41A1C"
    )
  ) +
  
  theme_minimal() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )+
  theme(legend.position = "none") +
  
  labs(
    x = "Sensory drive (S)",
    y = "Predictability preference (P)",
  )

# =========================================================
# PANEL C — EMERGENT DIETARY STRATEGIES
# =========================================================

p_behaviour <- ggplot(
  agent_summary,
  aes(
    x = repertoire_size,
    y = entropy,
    colour = strategy_class
  )
) +
  
  # individual agents
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  
  # strategy colours
  scale_colour_manual(
    values = c(
      "Specialist"  = "#377EB8",
      "Opportunist" = "#4DAF4A",
      "Explorer"    = "#E41A1C"
    )
  ) +

  theme_minimal() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )+
  theme(legend.position = "none")+
  
  labs(
    x = "Dietary repertoire size",
    y = "Dietary entropy",
    colour = "Strategy"
  )


# -----------------------------
# PANEL D
# Dietary entropy vs microbiome dominance
# -----------------------------

agent_summary <- all_runs %>%
  group_by(agent) %>%
  summarise(
    S = first(S),
    P = first(P),
    C = first(C),
    entropy = mean(entropy),
    repertoire_size = mean(repertoire_size),
    dominance = mean(dominance),
    micro_entropy = mean(micro_entropy),
    micro_richness = mean(micro_richness),
    .groups = "drop"
  ) %>%
  mutate(
    entropy_norm = (entropy - min(entropy)) /
      (max(entropy) - min(entropy)),
    strategy_class = case_when(
      entropy_norm < 0.33 ~ "Specialist",
      entropy_norm > 0.66 ~ "Explorer",
      TRUE ~ "Opportunist"
    )
  )

mod_dom <- lm(dominance ~ entropy, data = agent_summary)

eq <- paste0(
  "y = ",
  round(coef(mod_dom)[2], 3),
  "x + ",
  round(coef(mod_dom)[1], 3),
  "\nR² = ",
  round(summary(mod_dom)$r.squared, 3)
)

pD <- ggplot(
  agent_summary,
  aes(
    x = entropy,
    y = dominance,
    colour = strategy_class
  )
) +
  
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  
  geom_smooth(
    aes(group = 1),
    method = "lm",
    colour = "black",
    fill = "grey80",
    linewidth = 1,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = c(
      "Specialist"  = "#377EB8",
      "Opportunist" = "#4DAF4A",
      "Explorer"    = "#E41A1C"
    )
  ) +
  
  labs(
    x = "Dietary entropy",
    y = "Microbiome dominance"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  ) +
  
  theme(
    legend.position = "none"
  )

# =========================================================
# COMBINE FIGURE
# =========================================================

figure1 <-
  (p_concept + labs(tag = "A") |
     p_latent + labs(tag = "B")) /
  (p_behaviour + labs(tag = "C") |
     pD + labs(tag = "D"))

print(figure1)

# =========================================================
# SAVE
# =========================================================
# Output directory
out_dir <- "outputs/figures"

if(!dir.exists(out_dir))
  dir.create(out_dir, recursive = TRUE)


ggsave("Figure1_SPCQ_Framework.png",
       plot = figure1,
       width = 10,
       height = 8,
       dpi = 300)
