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

## Plot effects for smoothed coefficients: What are term predicitons?

## Function to predict smoothed predictors from new data
addPreds <- function(df, model, term, fitname = "fit"){
	if (inherits(model, "coxph")){
		term_label <- names(model[["assign"]])[grep(term, names(model[["assign"]]))]
		df <- (df
			%>% mutate(fit = as.vector(predict(object = model, ., type = "terms", terms = term_label)))
			%>% rename(setNames("fit", fitname))
		)
	} else {
		df <- (df
			%>% add_term(model, term = term)
			%>% rename(setNames("fit", fitname))
		)
	}
	return(df)
}

### Generate new data fro predictions

## Unlagged
npreds <- 100
new_df <- (working_df_unlagged
	%>% make_newdata(age = seq_range(age, n = npreds))
)

#### Age

pred_age_df <- (new_df 
	%>% addPreds(., model = cox_unlagged, term = "age", fitname = "coxu") 
	%>% addPreds(., model = pam_unlagged, term = "age", fitname = "pamu")
)

### Plot the predicitons
p1 <- (ggplot(pred_age_df, aes(x = age, y = pamu))
	+ geom_line(aes(colour = "pamu"))
	+ geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3)
	+ geom_line(aes(y = coxu, col = "coxu"))
	+ scale_colour_manual(name = "Model", values=c("#E41A1C", "#000000"))
	+ labs(x = "Age", y = "Predictions (terms?)")
)
print(p1)

## Lagged
new_df2 <- (working_df_lagged
	%>% make_newdata(age = seq_range(age, n = npreds))
)

#### Age

pred_age_df2 <- (new_df2 
	%>% addPreds(., model = cox_lagged, term = "age", fitname = "coxl") 
	%>% addPreds(., model = pam_lagged, term = "age", fitname = "paml")
)

### Plot the predicitons
p2 <- (ggplot(pred_age_df2, aes(x = age, y = paml))
	+ geom_line(aes(colour = "paml"))
	+ geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3)
	+ geom_line(aes(y = coxl, col = "coxl"))
	+ scale_colour_manual(name = "Model", values=c("#E41A1C", "#000000"))
	+ labs(x = "Age", y = "Predictions (terms?)")
)
print(p2)
