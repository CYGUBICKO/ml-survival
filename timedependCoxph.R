#### ---- Task: Survival Analysis ----
#### ---- Subtask: Handling time dependent covariate and RE ----
#### ---- Tools: GAM models using pammtools ----
#### ---- By: Steve & BB ----
#### ---- Date: 2020 Mar 09 (Mon) ----

library(survival)

load("timedependData.rda")

## Fit Cox PH

### Cox-PH
cox_unlagged <- coxph(Surv(start, stop, arrest) ~ fin + pspline(age) + race + wexp + mar + paro + pspline(prio)
	, data = working_df_unlagged
)

summary(cox_unlagged)

#### Lagged
cox_lagged <- coxph(Surv(start, stop, arrest) ~ fin + pspline(age) + race + wexp + mar + paro + pspline(prio) + employed.lag1
	, data = working_df_lagged
)
summary(cox_lagged)

save(file = "timedependCoxph.rda"
	, cox_lagged
	, cox_unlagged
)
