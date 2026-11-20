# SPC-Q: An Agent-Based Model of Dietary Behaviour and Microbiome Assembly

This repository contains the code and outputs associated with the manuscript describing the Sensory-Predictability-Context (SPC-Q) framework, an agent-based model linking dietary behaviour to microbiome structure.

## Overview

The SPC-Q model simulates food choice as a stochastic, state-dependent process driven by:

- Sensory preference (S)
- Predictability preference (P)
- Context sensitivity (C)

Agents repeatedly select foods using reinforcement learning and probabilistic decision making. Dietary behaviour subsequently influences microbiome composition through a mechanistic diet-microbiome mapping.

Monte Carlo simulations are used to characterise stochastic variability among model realisations.

## Repository Structure

```text
.
├── SPCQ_Model_Core.R
├── Figure1_SPCQ_Framework_EmergentStrategies.R
├── Figure2_Behavioural_Mediation.R
├── Figure3_Sensitivity_Analysis.R
│
├── outputs/
│   ├── SPCQ_agent_runs.rds
│   └── SPCQ_agent_summary.rds
│
├── Figures/
│   ├── Figure1_SPCQ_Framework_EmergentStrategies.png
│   ├── Figure2_Behavioural_Mediation.png
│   └── Figure3_Dietary_Filtering_Sensitivity_Analysis.png
│
└── Summary_Table_Sensitivity_Analysis.csv
```

## Core Files

### SPCQ_Model_Core.R

Implements the SPC-Q agent-based model and Monte Carlo simulation framework.

Running this script generates:

- `SPCQ_agent_runs.rds` : agent-level outputs from all Monte Carlo simulations.
- `SPCQ_agent_summary.rds` : agent-level mean responses across Monte Carlo replicates.

### Figure1_SPCQ_Framework_EmergentStrategies.R

Generates Figure 1 illustrating:

- Conceptual structure of the SPC-Q framework.
- Latent sensory-predictability trait space.
- Emergent dietary strategies.
- Relationship between dietary entropy and microbiome dominance.

### Figure2_Behavioural_Mediation.R

Generates Figure 2 examining whether dietary behaviour mediates the relationship between latent SPC traits and microbiome structure.

### Figure3_Sensitivity_Analysis.R

Generates Figure 3 assessing the robustness of dietary filtering across alternative parameterisations of:

- Microbiome persistence (λ)
- Learning rate (α)
- Choice stochasticity (β)

## Reproducing the Analysis

### 1. Generate model outputs

Run:

```r
source("SPCQ_Model_Core.R")
```

This will create:

```text
outputs/SPCQ_agent_runs.rds
outputs/SPCQ_agent_summary.rds
```

### 2. Generate manuscript figures

Run:

```r
source("Figure1_SPCQ_Framework_EmergentStrategies.R")
source("Figure2_Behavioural_Mediation.R")
source("Figure3_Sensitivity_Analysis.R")
```

Publication-ready figures will be written to the `Figures` directory.

## Software Requirements

The model was developed in R.

Required packages include:

```r
dplyr
tidyr
ggplot2
broom
purrr
patchwork
proxy
ggtern
knitr
```

## Author

William Ross Hunter

## Use of Artificial Intelligence (AI)

The author used Microsoft Copilot to assist with language editing, code annotation, and refinement of reproducible R scripts. All scientific concepts, study design, model development, data analysis, interpretation, and editorial decisions were undertaken by the author, who reviewed and approved all content and retains full responsibility for the accuracy and integrity of the work.