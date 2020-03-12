#### ---- Task: Survival Analysis ----
#### ---- Subtask: Handling time dependent covariate and RE ----
#### ---- Tools: GAM models using pammtools ----
#### ---- By: Steve & BB ----
#### ---- Date: 2020 Mar 09 (Mon) ----

library(tidyr)
library(dplyr)

library(ggplot2); theme_set(theme_bw() + 
theme(panel.spacing = grid::unit(0,"lines"), legend.position = "bottom"))

library(survival)
library(mgcv)
library(pammtools)

load("timedependData.rda")
load("timedependCoxph.rda")
load("timedependPammtools.rda")

## Survival probabilities

### Cox ph
cox_survp <- survfit(cox_lagged)

### Pam

### Seem to require as_ped to use add_surv_prob
pam_survp <- (working_df_lagged
	%>% make_newdata(age = unique(age))
	%>% add_surv_prob(pam_lagged)
)

print(pam_survp, width = Inf)

