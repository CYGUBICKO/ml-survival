#### ---- Task: Survival Analysis ----
#### ---- Subtask: Handling time dependent covariate and RE ----
#### ---- Tools: GAM models using pammtools ----
#### ---- By: Steve & BB ----
#### ---- Date: 2020 Mar 09 (Mon) ----

library(tidyr)
library(dplyr)
library(purrr)
library(survival)
library(mgcv)
library(pammtools)


load("timedependData.rda")

## Fit models

### PAM: Now sure how it translates to using poisson() 

#### Unlagged
pam_unlagged <- gam(arrest ~ s(stop) + fin + s(age, bs="ps") + race + wexp + mar + paro + s(prio, bs="ps") + employed
	, data = working_df_unlagged
	, offset = offset
	, family = poisson()
)

summary(pam_unlagged)

#### Lagged
pam_lagged <- gam(arrest ~ s(stop) + fin + s(age, bs="ps") + race + wexp + mar + paro + s(prio, bs="ps") + employed.lag1
	, data = working_df_lagged
	, offset = offset
	, family = poisson()
)
summary(pam_lagged)

save(file = "timedependPammtools.rda"
	, pam_unlagged
	, pam_lagged
)
