#### ---- Task: Survival Analysis ----
#### ---- Subtask: Handling time dependent covariate and RE ----
#### ---- Tools: GAM models using pammtools ----
#### ---- By: Steve & BB ----
#### ---- Date: 2020 Mar 09 (Mon) ----

library(tidyr)
library(dplyr)
library(ggplot2)
library(survival)
library(mgcv)
library(pammtools)

load("timedependData.rda")
load("timedependCoxph.rda")
load("timedependPammtools.rda")

## Plot effects for smoothed coefficients

addPreds <- function(df, model, term){
	if (inherits(model, "coxph")){
		df <- (df
			%>% mutate(coxfit = predict(object = model, ., type = "terms"))[, term]
		)
	}
	return(df)
}

#### Generate new age data
working_df_unlagged <- (working_df_unlagged
	%>% make_newdata(age = seq_range(age, n = 100))
)

#print(working_df_unlagged %>% add_term(pam_unlagged, term = "age"), width = Inf)
#addPreds(working_df_unlagged %>% add_term(pam_unlagged, term = "age"), cox_unlagged, "pspline(age)")
names(cox_unlagged$assign)

coxfit = predict(object = cox_unlagged, newdata = working_df_unlagged, type = "terms"
	, terms = names(cox_unlagged$assign)[grep("age|prio", names(cox_unlagged$assign))]
	, se.fit = TRUE
)
coxfit
