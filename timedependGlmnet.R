#### ---- Task: Survival Analysis ----
#### ---- Subtask: Handling time dependent covariate and RE ----
#### ---- Tools: GAM models using pammtools ----
#### ---- By: Steve & BB ----
#### ---- Date: 2020 Mar 09 (Mon) ----

library(dplyr)
library(survival)
library(glmnet)

load("timedependData.rda")


# This does not work in glmet
#### Lagged
#X <- (working_df_lagged
#	%>% select(fin, age, race, wexp, mar, paro, prio, employed.lag1)
#	%>% mutate_at(names(.), function(x)drop(scale(x)))
#)
#
#start <- working_df_lagged$start
#stop <- working_df_lagged$stop
#arrest <- working_df_lagged$arrest
#
#baseline <- Surv(start, stop, arrest)
#
#glmnet_lagged <- glmnet(x = X, y = baseline, family = "cox", alpha = 0, lambda = 0, standardize = FALSE)
#coef(glmnet_lagged)

## Using veteran data from survival package

df <- survival::veteran
df <- df[order(df$time),]
df <- df[!duplicated(df$time),]
delta <- df$status
time <- df$time
baseline <- Surv(time=time,event=delta)
X <- as.matrix(df[,c('karno','diagtime','age')])
X <- scale(X)

glmnet_lagged <- glmnet(x = X, y = baseline, family = "cox", alpha = 0, lambda = 0, standardize = FALSE)
coef(glmnet_lagged)


save(file = "timedependGlmnet.rda"
	, glmnet_lagged
)
