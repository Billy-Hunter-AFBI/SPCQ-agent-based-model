# =========================================================
# FIGURE 3
# Dietary filtering sensitivity analysis
# ---------------------------------------------------------
# Purpose:
# Assess the robustness of the dietary filtering
# mechanism identified in Figure 2.
#
# Outcome:
# Standardised regression coefficient for dietary
# entropy in the mediation model:
#
#   dominance ~ S + P + C + entropy
#
# Parameters varied:
#   λ = microbiome persistence
#   α = reinforcement learning rate
#   β = choice stochasticity
#
# Unlike Figures 1 and 2, which are generated from
# archived model outputs, this analysis re-runs the
# SPC-Q model under alternative parameterisations.
# For each parameter value, 30 Monte Carlo simulations
# are performed and agent-level responses are averaged
# before fitting the mediation model.
#
# Dependency:
#   SPCQ_Model_Core.R
#
# Outputs:
#   Figure3_Dietary_Filtering_Sensitivity_Analysis.png
#   Figure3_Dietary_Filtering_Sensitivity_Analysis.csv
# =========================================================

library(dplyr)
library(ggplot2)
library(broom)
library(purrr)

# Load SPC-Q model implementation
source("SPCQ_Model_Core.R")

# =========================================================
# HELPER FUNCTION
# =========================================================
#
# For a given parameter value:
#   1. Run 30 Monte Carlo replicates
#   2. Average outcomes across replicates
#   3. Fit mediation model
#   4. Extract entropy coefficient
#
# =========================================================

run_sensitivity <- function(
    world,
    param_name,
    param_value,
    n_sims = 30){
  
  sim_seeds <- 1000 + seq_len(n_sims)
  
  runs <- lapply(
    sim_seeds,
    function(s){
      
      args <- list(
        seed = s,
        world = world
      )
      
      args[[param_name]] <- param_value
      
      do.call(
        run_model,
        args
      )$agent_summary
      
    }
  )
  
  all_runs <- bind_rows(runs)
  
  # -------------------------------------
  # Mean behavioural phenotype
  # across Monte Carlo replicates
  # -------------------------------------
  
  agent_summary <- all_runs %>%
    group_by(agent) %>%
    summarise(
      S = first(S),
      P = first(P),
      C = first(C),
      
      entropy =
        mean(entropy),
      
      dominance =
        mean(dominance),
      
      .groups = "drop"
    )
  
  # -------------------------------------
  # Mediation model
  # -------------------------------------
  
  mod <- lm(
    scale(dominance) ~
      scale(S) +
      scale(P) +
      scale(C) +
      scale(entropy),
    data = agent_summary
  )
  
  tidy(mod) %>%
    filter(
      term == "scale(entropy)"
    ) %>%
    transmute(
      Estimate = estimate,
      Lower =
        estimate -
        1.96 * std.error,
      Upper =
        estimate +
        1.96 * std.error
    )
}

# =========================================================
# MICROBIOME MEMORY
# λ
# =========================================================

decay_values <- c(
  0.90,
  0.95,
  0.98,
  0.99
)

results_decay <- bind_rows(
  lapply(
    decay_values,
    function(d){
      
      run_sensitivity(
        world = world,
        param_name = "decay",
        param_value = d
      ) %>%
        
        mutate(
          Parameter = "Microbiome memory",
          Value = d
        )
      
    }
  )
)

# =========================================================
# LEARNING RATE
# α
# =========================================================

alpha_values <- c(
  0.01,
  0.05,
  0.10,
  0.20,
  0.40
)

results_alpha <- bind_rows(
  lapply(
    alpha_values,
    function(a){
      
      run_sensitivity(
        world = world,
        param_name = "alpha",
        param_value = a
      ) %>%
        
        mutate(
          Parameter = "Learning rate",
          Value = a
        )
      
    }
  )
)

# =========================================================
# CHOICE STOCHASTICITY
# β
# =========================================================

beta_values <- c(
  1,
  2,
  5,
  10,
  20
)

results_beta <- bind_rows(
  lapply(
    beta_values,
    function(b){
      
      run_sensitivity(
        world = world,
        param_name = "beta",
        param_value = b
      ) %>%
        
        mutate(
          Parameter = "Choice stochasticity",
          Value = b
        )
      
    }
  )
)

# =========================================================
# COMBINE RESULTS
# =========================================================

sensitivity_df <- bind_rows(
  results_decay,
  results_alpha,
  results_beta
)

# Label rows

sensitivity_df <- sensitivity_df %>%
  mutate(
    Label =
      paste0(
        Parameter,
        ": ",
        Value
      )
  )

# Order for display

sensitivity_df$Label <-
  factor(
    sensitivity_df$Label,
    levels =
      rev(sensitivity_df$Label)
  )

# =========================================================
# PREPARE LABELS
# =========================================================

sensitivity_df <- sensitivity_df %>%
  mutate(
    Label = case_when(
      
      Parameter == "Microbiome memory" ~
        paste0("\u03BB = ", Value),
      
      Parameter == "Learning rate" ~
        paste0("\u03B1 = ", Value),
      
      Parameter == "Choice stochasticity" ~
        paste0("\u03B2 = ", Value)
      
    )
  )

# =========================================================
# BASELINE MODEL
# Main manuscript parameterisation
# λ = 0.98
# α = 0.10
# β = 5
# =========================================================

baseline_beta <- sensitivity_df %>%
  filter(
    Parameter == "Microbiome memory",
    Value == 0.98
  ) %>%
  pull(Estimate)

# =========================================================
# FIGURE 3
# Sensitivity analysis of dietary filtering mechanism
# =========================================================

p_sensitivity <- ggplot(
  sensitivity_df,
  aes(
    Estimate,
    reorder(Label, Estimate),
    colour = Parameter
  )
) +
  
  # Baseline model reference
  geom_vline(
    xintercept = baseline_beta,
    colour = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  
  # Confidence intervals
  geom_errorbarh(
    aes(
      xmin = Lower,
      xmax = Upper
    ),
    height = 0.20,
    linewidth = 0.9
  ) +
  
  # Point estimates
  geom_point(
    size = 3.5
  ) +
  
  # Separate parameter classes
  facet_grid(
    Parameter ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  scale_colour_manual(
    values = c(
      "Microbiome memory" = "#377EB8",
      "Learning rate" = "#4DAF4A",
      "Choice stochasticity" = "#E41A1C"
    )
  ) +
  
  # Remove excessive empty space
  coord_cartesian(
    xlim = c(-1.02, -0.65)
  ) +
  
  labs(
    x = "Effect of dietary entropy on microbiome dominance\n(Standardised regression coefficient, \u03B2)",
    y = NULL,
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    
    legend.position = "none",
    
    strip.text.y = element_text(
      face = "bold",
      size = 11
    ),
    
    panel.grid.minor = element_blank(),
    
    axis.title.x = element_text(
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 10
    ),
    
    panel.spacing.y = unit(
      0.8,
      "lines"
    )
  )

print(p_sensitivity)

# =========================================================
# SAVE
# =========================================================

ggsave(
  "Figure3_Dietary_Filtering_Sensitivity_Analysis.png",
  p_sensitivity,
  width = 8,
  height = 7,
  dpi = 300
)
# =========================================================
# EXPORT TABLE
# =========================================================

write.csv(
  sensitivity_df,
  "Figure3_Sensitivity_Analysis.csv",
  row.names = FALSE
)
