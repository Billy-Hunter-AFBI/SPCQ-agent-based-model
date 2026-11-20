# =========================================================
# FIGURE 2
# Dietary behaviour mediates the relationship between
# latent SPC traits and microbiome structure
# ---------------------------------------------------------
# This script generates Figure 2 from aggregated SPC-Q
# model outputs.
#
# Panel A: Effects of latent SPC dimensions on dietary
# entropy.
#
# Panel B: Direct effects of SPC dimensions on microbiome
# dominance.
#
# Panel C: Changes in SPC coefficients after accounting
# for dietary entropy.
#
# Panel D: Comparison of model explanatory power with and
# without dietary entropy.
#
# Input:
# outputs/SPCQ_agent_summary.rds
#
# Output:
# outputs/figures/Figure2_Behavioural_Mediation.png
# =========================================================

library(dplyr)
library(ggplot2)
library(broom)
library(patchwork)

agent_summary <- readRDS(
  "outputs/SPCQ_agent_summary.rds"
)

# =========================================================
# DATA CHECK
# =========================================================

required_cols <- c(
  "S",
  "P",
  "C",
  "entropy",
  "repertoire_size",
  "dominance"
)

stopifnot(
  all(required_cols %in% names(agent_summary))
)

# =========================================================
# PANEL A
# Sensory phenotype predicts dietary entropy
#
# Standardised regression coefficients:
# Which SPC dimensions contribute most strongly to
# dietary diversity?
# =========================================================

mod_entropy <- lm(
  scale(entropy) ~
    scale(S) +
    scale(P) +
    scale(C),
  data = agent_summary
)

coef_entropy <- tidy(mod_entropy) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = c(
      "Sensory (S)",
      "Predictability (P)",
      "Context (C)"
    )
  )

pA <- ggplot(
  coef_entropy,
  aes(
    Predictor,
    estimate
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "grey50"
  ) +
  
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.10
  ) +
  
  geom_point(
    size = 4,
    colour = "#377EB8"
  ) +
  
  labs(
    x = NULL,
    y = "Effect on dietary entropy\n(standardised coefficient)"
  ) +
  
  theme_minimal()

# =========================================================
# PANEL B
# Direct effect model
#
# How strongly do SPC dimensions predict microbiome
# dominance if dietary behaviour is ignored?
# =========================================================

mod_direct <- lm(
  scale(dominance) ~
    scale(S) +
    scale(P) +
    scale(C),
  data = agent_summary
)

coef_direct <- tidy(mod_direct) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = c(
      "Sensory (S)",
      "Predictability (P)",
      "Context (C)"
    )
  )

pB <- ggplot(
  coef_direct,
  aes(
    Predictor,
    estimate
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "grey50"
  ) +
  
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.10
  ) +
  
  geom_point(
    size = 4,
    colour = "#E41A1C"
  ) +
  
  labs(
    x = NULL,
    y = "Direct effect on microbiome\ndominance"
  ) +
  
  theme_minimal()

# =========================================================
# PANEL C
# Mediation model
#
# Compare direct SPC effects before and after
# accounting for dietary behaviour.
#
# If the SPC coefficients collapse after entropy is
# added, dietary behaviour is acting as a mediator.
# =========================================================

mod_mediated <- lm(
  scale(dominance) ~
    scale(S) +
    scale(P) +
    scale(C) +
    scale(entropy),
  data = agent_summary
)

direct_coef <- tidy(mod_direct) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    model = "Direct"
  )

mediated_coef <- tidy(mod_mediated) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    model = "With entropy"
  )

coef_compare <- coef_compare %>%
  mutate(
    term = recode(
      term,
      "scale(S)" = "Sensory (S)",
      "scale(P)" = "Predictability (P)",
      "scale(C)" = "Context (C)",
      "scale(entropy)" = "Dietary entropy"
    )
  )

pC <- ggplot(
  coef_compare,
  aes(
    term,
    estimate,
    colour = model
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "grey50"
  ) +
  
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    position = position_dodge(0.4),
    width = 0.10
  ) +
  
  geom_point(
    position = position_dodge(0.4),
    size = 3
  ) +
  
  scale_colour_manual(
    values = c(
      "Direct" = "#377EB8",
      "With entropy" = "#E41A1C"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Effect on microbiome dominance",
    colour = NULL
  ) +
  
  theme_minimal()

# =========================================================
# PANEL D
# Variance explained by sensory phenotype alone
# versus sensory phenotype plus dietary behaviour
#
# Purpose:
# Test whether dietary entropy mediates the relationship
# between SPC traits (S, P, C) and microbiome dominance.
#
# Model 1:
#   microbiome dominance ~ S + P + C
#
# Model 2:
#   microbiome dominance ~ S + P + C + dietary entropy
#
# If dietary behaviour acts as a mediator, adding entropy
# should substantially increase the variance explained
# (R²) while reducing the apparent direct effects of
# SPC dimensions on microbiome structure.
# =========================================================

# Extract model R² values
r2_df <- data.frame(
  Model = factor(
    c(
      "SPC only",
      "SPC + entropy"
    ),
    levels = c(
      "SPC only",
      "SPC + entropy"
    )
  ),
  R2 = c(
    summary(mod_direct)$r.squared,
    summary(mod_mediated)$r.squared
  )
)

# Plot model explanatory power
pD <- ggplot(
  r2_df,
  aes(
    x = Model,
    y = R2,
    fill = Model
  )
) +
  
  # bar heights represent explained variance
  geom_col(
    width = 0.6,
    show.legend = FALSE
  ) +
  
  # print R² values above bars
  geom_text(
    aes(
      label = paste0(
        "R² = ",
        round(R2, 2)
      )
    ),
    vjust = -0.5,
    size = 5
  ) +
  
  scale_fill_manual(
    values = c(
      "SPC only"     = "#377EB8",
      "SPC + entropy" = "#E41A1C"
    )
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    x = NULL,
    y = expression(R^2),
    title = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )
# =========================================================
# COMBINE FIGURE
# =========================================================

figure2 <-
  
  (pA + labs(tag = "A") |
     pB + labs(tag = "B")) /
  
  (pC + labs(tag = "C") |
     pD + labs(tag = "D"))

print(figure2)

# =========================================================
# SAVE FIGURE
# =========================================================

ggsave(
  "Figure2_Behavioural_Mediation",
  figure2,
  width = 11,
  height = 8,
  dpi = 300
)

# =========================================================
# MODEL OUTPUTS
# ---------------------------------------------------------
# Report statistical models underpinning Figure 2.
#
# Model 1:
# Tests whether latent SPC traits (Sensory,
# Predictability and Context) explain variation in
# dietary entropy.
#
# Model 2:
# Tests whether latent SPC traits directly predict
# microbiome dominance when dietary behaviour is not
# considered.
#
# Model 3:
# Tests whether dietary entropy mediates the
# relationship between SPC traits and microbiome
# structure.
#
# ANOVA:
# Evaluates whether inclusion of dietary entropy
# significantly improves model performance relative
# to the SPC-only model.
# =========================================================


# ---------------------------------------------------------
# Model 1
# Dietary entropy ~ SPC traits
# ---------------------------------------------------------
cat("\n========================================\n")
cat("MODEL 1: DIETARY ENTROPY\n")
cat("Response: Dietary entropy\n")
cat("Predictors: Sensory (S), Predictability (P), Context (C)\n")
cat("========================================\n")
print(summary(mod_entropy))


# ---------------------------------------------------------
# Model 2
# Microbiome dominance ~ SPC traits
# ---------------------------------------------------------
cat("\n========================================\n")
cat("MODEL 2: DIRECT EFFECTS ON MICROBIOME\n")
cat("Response: Microbiome dominance\n")
cat("Predictors: Sensory (S), Predictability (P), Context (C)\n")
cat("========================================\n")
print(summary(mod_direct))


# ---------------------------------------------------------
# Model 3
# Microbiome dominance ~ SPC traits + behaviour
# ---------------------------------------------------------
cat("\n========================================\n")
cat("MODEL 3: BEHAVIOURAL MEDIATION MODEL\n")
cat("Response: Microbiome dominance\n")
cat("Predictors: S, P, C and Dietary entropy\n")
cat("Purpose: Test whether dietary behaviour mediates\n")
cat("the relationship between latent SPC traits and\n")
cat("microbiome structure.\n")
cat("========================================\n")
print(summary(mod_mediated))


# ---------------------------------------------------------
# Nested model comparison
# ---------------------------------------------------------
cat("\n========================================\n")
cat("MODEL COMPARISON\n")
cat("Direct model vs Behavioural mediation model\n")
cat("Likelihood-ratio test (ANOVA)\n")
cat("========================================\n")
print(anova(mod_direct, mod_mediated))
