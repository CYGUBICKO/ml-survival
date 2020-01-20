#### ---- Random codes for survival analysis ----

library("pec")
library("survival")
library("rms")
library("randomSurvivalForest")
library("party")
library(dplyr)

source("funs/balPartition.R")

set.seed(7902)

#### ---- Some key functions ----

## Stepwise variable selection in cox
selectCox <- function(formula, data, rule = "aic") {
	require("rms")
	require("prodlim")
	fit <- cph(formula, data, surv = TRUE)
	bwfit <- fastbw(fit, rule = rule)
	if (length(bwfit$names.kept) == 0) {
		newform <- reformulate("1", formula[[2]])
		newfit <- prodlim(newform, data = data)
	} else{
		newform <- reformulate(bwfit$names.kept, formula[[2]])
		newfit <- cph(newform, data, surv = TRUE)
	}
	out <- list(fit = newfit,In = bwfit$names.kept)
	out$call <- match.call()
	class(out) <- "selectCox"
	out
}


predictSurvProb.selectCox <- function(object, newdata, times, ...) {
	predictSurvProb(object[[1]], newdata = newdata, times = times, ...)
}

predictSurvProb.rsf <- function (object, newdata, times, ...) {
	N <- NROW(newdata)
	class(object) <- c("rsf", "grow")
	S <- exp(-predict.rsf(object, test = newdata)$ensemble)
	if(N == 1) S <- matrix(S, nrow = 1)
	Time <- object$timeInterest
	p <- cbind(1, S)[, 1 + sindex(Time, times),drop = FALSE]
	if(NROW(p) != NROW(newdata) || NCOL(p) != length(times))
		stop("Prediction failed")
	p
}

pecCforest <- function(formula, data, ...) {
	require("party")
	out <- list(forest = cforest(formula, data, ...))
	class(out) <- "pecCforest"
	out$call <- match.call()
	out
}

predictSurvProb.pecCforest <- function (object, newdata, times, ...) {
	survObj <- treeresponse(object$forest, newdata = newdata)
	p <- do.call("rbind", lapply(survObj, function(x) {
			predictSurvProb(x, newdata = newdata[1, , drop = FALSE], times = times)
		})
	)
	if(NROW(p) != NROW(newdata) || NCOL(p) != length(times))
		stop("Prediction failed")
	p
}

#### ---- Using randomSurvivalForest package

data("cost")
df <- cost
df_parts <- (df
	%>% balPartition("status", 0.994)
)

train_df <- df_parts[["train_df"]]
test_df <- df_parts[["test_df"]]

head(df)

fitform <- (Surv(time,status) ~ age 
	+ sex 
	+ hypTen 
	+ ihd 
	+ prevStroke 
	+ othDisease 
	+ alcohol 
	+ diabetes 
	+ smoke 
	+ atrialFib 
	+ hemor 
	+ strokeScore 
	+ cholest
)

## Full fit
fullfit <- cph(fitform, data = train_df)
fullfit

bestfit <- selectCox(fitform, data = train_df, rule = "aic")
bestfit

#### Fit random forest

fitrsf <- rsf(fitform, data = train_df, forest = TRUE, ntree = 1000)
fitcforest <- pecCforest(fitform, data = train_df, controls = cforest_classical(ntree = 1000))

#### Test
pcox <- predictSurvProb(bestfit, newdata = test_df, times = 10 * 365.25)
prsf <- predictSurvProb(fitrsf, newdata = test_df, times = 10 * 365.25)
extends <- function(...) TRUE
pcf <- predictSurvProb(fitcforest, newdata = test_df, times = 10 * 365.25)

### Plot the predictions
par(mfrow = c(1, 3))
lapply(1:3, function(x) {
	plotPredictSurvProb(fitcox, newdata = test_df[x, ], lty = 1)
	plotPredictSurvProb(fitrsf, newdata = test_df[x, ], add = TRUE, lty = 2)
	plotPredictSurvProb(fitcforest, newdata = test_df[x, ], add = TRUE,lty = 3)
})
