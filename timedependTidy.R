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


load("timedependPammtools.rda")
load("timedependCoxph.rda")

## Tidy estimates
extract_coefs_df <- (map(
		list(pam_unlagged = pam_unlagged
			, pam_lagged = pam_lagged
			, cox_unlagged = cox_unlagged
			, cox_lagged = cox_lagged
		)
		, tidy_fixed
		, conf.int = TRUE
   )
	%>% bind_rows(.id = "model")
	%>% mutate(Method = gsub("\\_.*", "", model)
		, model = gsub(".*\\_", "", model)
		, variable = gsub(".*\\(|\\).*", "", variable)
	)
	%>% filter(!is.na(coef))
)

print(extract_coefs_df, n = Inf)

save(file = "timedependTidy.rda"
	, extract_coefs_df
)
