#### ---- Random codes for survival analysis ----

## BMB: be consistent about quotes/no quotes for loading pkgs?
library(ggplot2)
library(pec)
library(survival)
library(rms)
library(party)
library(dplyr)
library(doMC)
registerDoMC()

library(randomSurvivalForest)
## ??
## Package ‘randomSurvivalForest’ was removed from the CRAN repository.
## Formerly available versions can be obtained from the archive.
## Archived on 2014-12-04 after 20 months of deprecation in favour of randomForestSRC, by the maintainer.
## Consider using package ‘randomForestSRC’ instead. 

source("funs/balPartition.R")
## s <- 7902
s <- 801234
set.seed(s)

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

# ## Full fit
# fullfit <- cph(fitform, data = train_df)
# fullfit
# 
# bestfit <- selectCox(fitform, data = train_df, rule = "aic")
# bestfit
# 
# #### Fit random forest
# 
# fitrsf <- rsf(fitform, data = train_df, forest = TRUE, ntree = 1000)
# fitcforest <- pecCforest(fitform, data = train_df, controls = cforest_classical(ntree = 1000))
# 
# #### Test
# pcox <- predictSurvProb(bestfit, newdata = test_df, times = 10 * 365.25)
# prsf <- predictSurvProb(fitrsf, newdata = test_df, times = 10 * 365.25)
# extends <- function(...) TRUE
# pcf <- predictSurvProb(fitcforest, newdata = test_df, times = 10 * 365.25)
# 
# ### Plot the predictions
# par(mfrow = c(1, 3))
# lapply(1:3, function(x) {
# 	plotPredictSurvProb(bestfit, newdata = test_df[x, ], lty = 1)
# 	plotPredictSurvProb(fitrsf, newdata = test_df[x, ], add = TRUE, lty = 2)
# 	plotPredictSurvProb(fitcforest, newdata = test_df[x, ], add = TRUE,lty = 3)
# })


### Integrated Brier score
# extends <- function(...) TRUE
# set.seed(13)
# library("doMC")
# registerDoMC()
# 
# fitpec <- pec(list("selectcox" = bestfit
#     , "rsf" = fitrsf
#     , "cforest" = fitcforest
#   )
#   , data = cost
#   , formula = Surv(time, status) ~ 1
#   , splitMethod = "Boot632plus"
#   , B = 1000
#   , M = 350
#   , keep.index = TRUE
#   , keep.matrix = TRUE
# )

#### ---- CROSS VALIDATION ----
## ntree <- c(50, 100, 150, 200, 300)
## mtry <- seq(2, 8, 2)
ntree <- c(50,100)
mtry <- c(2,4)

## BMB: write one function that uses either %do% or %dopar% depending
## on an argument (so that you repeat less code)
### In sample cross validation
cvFunc <- function(df, ntree, mtry, nfolds = 5){
  n_folds <- nfolds
  n_train <- nrow(df)
  tuneGrid <- expand.grid(folds = 1:n_folds, ntree = ntree, mtry = mtry)
  cv_errors <- expand.grid(folds = 1:n_folds, ntree = ntree, mtry = mtry, error_rate = 0)
  tuneGrid$seed <- -1*sample(100000,size=nrow(tuneGrid))
  folds_i <- sample(rep(1:n_folds, length.out = n_train))
  for (p in 1:nrow(tuneGrid)){
    index <- which(folds_i == tuneGrid[["folds"]][p])
    train <- df[-index, ]
    test <- df[index, ]
    fit <- rsf(fitform, data = train,
               ntree = tuneGrid[["ntree"]][p],
               mtry = tuneGrid[["mtry"]][p],
               seed = tuneGrid[["seed"]][p]
               )
    cv_errors[p, "error_rate"] <- mean(fit$err.rate)
  }
  return(cv_errors)
}


genFunc <- function(df, ntree, mtry, nfolds = 5, type="serial",
                    seed=7902){
  if (!is.null(seed)) set.seed(seed)
  n_folds <- nfolds
  n_train <- nrow(df)
  tuneGrid <- expand.grid(folds = 1:n_folds, ntree = ntree, mtry = mtry)
  cv_errors <- expand.grid(folds = 1:n_folds, ntree = ntree, mtry = mtry, error_rate = 0)
  tuneGrid$seed <- -1*sample(100000,size=nrow(tuneGrid))
  folds_i <- sample(rep(1:n_folds, length.out = n_train))
  fitfun <- function(p) {
      index <- which(folds_i == tuneGrid[["folds"]][p])
      train <- df[-index, ]
      test <- df[index, ]
      fit <- rsf(fitform, data = train,
                 ntree = tuneGrid[["ntree"]][p],
                 mtry = tuneGrid[["mtry"]][p],
                 seed = tuneGrid[["seed"]][p]
                 )
      ret <- list(folds = tuneGrid[["folds"]][p],
                   ntree = tuneGrid[["ntree"]][p],
                   mtry = tuneGrid[["mtry"]][p],
                   error_rate =  mean(fit$err.rate))
      return(ret)
  }
  cv_errors <- if (type=="serial") {
      foreach (p = 1:nrow(tuneGrid), .combine=bind_rows) %do%
          fitfun(p)
  } else {
      foreach (p = 1:nrow(tuneGrid), .combine=bind_rows) %dopar%
          fitfun(p)
  }      
  return(cv_errors)
}

starttime <- Sys.time()
tt_serial <- genFunc(train_df, ntree = ntree, mtry = mtry)
endtime <- Sys.time()
cv_time <- difftime(endtime, starttime)

t2 <- (tt_serial
  %>% group_by(ntree, mtry)
  %>% summarise(mean_error = mean(error_rate))
  %>% mutate(type = paste0("N (", as.numeric(round(cv_time, 2)), ")"))
)

#### Parallelised cross-validation

# Random Forests ####
pcvFunc <- function(df, ntree, mtry, nfolds = 5){
  n_folds <- nfolds
  n_train <- nrow(df)
  tuneGrid <- expand.grid(folds = 1:n_folds, ntree = ntree, mtry = mtry)
  folds_i <- sample(rep(1:n_folds, length.out = n_train))
  
  ## Parallel
  foreach (p = 1:nrow(tuneGrid), .combine=bind_rows) %dopar% {
    index <- which(folds_i == tuneGrid[["folds"]][p])
    train <- df[-index, ]
    test <- df[index, ]
    fit <- rsf(fitform, data = train, ntree = tuneGrid[["ntree"]][p], mtry = tuneGrid[["mtry"]][p])
    #cv_errors[p, "error_rate"] <- mean(fit$err.rate)
    list(folds = tuneGrid[["folds"]][p], ntree = tuneGrid[["ntree"]][p], mtry = tuneGrid[["mtry"]][p], error_rate =  mean(fit$err.rate))
  }
}

set.seed(s)

starttime <- Sys.time()
tt_parallel <- genFunc(train_df, ntree = ntree, mtry = mtry,
                       type="parallel")
endtime <- Sys.time()
pcv_time <- difftime(endtime, starttime)

t3 <- (tt_parallel
  %>% group_by(ntree, mtry)
  %>% summarise(mean_error = mean(error_rate))
  %>% mutate(type = paste0("P (", as.numeric(round(pcv_time, 2)), ")"))
)

#### Compare the two
dd <- bind_rows(t2, t3)

p1 <- (ggplot(dd, aes(x = as.factor(ntree), y = mean_error, group = as.factor(mtry), colour = as.factor(mtry)))
  + geom_point()
  + geom_line()
  + facet_wrap(~type)
  + labs(x = "No. of trees", y = "Mean error rate", colour = "No. predictors\n in each split")
  + theme_bw()
  + theme(legend.position = "bottom")
)
print(p1)
ggsave("compare_cv_parallel_cv.pdf", p1)

## BMB plotting code
tt_comb <- bind_rows(serial=tt_serial, parallel=tt_parallel, .id="type")
p2 <- (ggplot(tt_comb, aes(x = as.factor(ntree),
                      y = error_rate, colour = type))
  + stat_summary(fun.data = "mean_cl_normal",position=position_dodge(width=0.25))
    ## + geom_line()
  + facet_wrap(~mtry)
  + labs(x = "No. of trees", y = "Mean error rate", colour = "No. predictors\n in each split")
  + theme_bw()
  + theme(legend.position = "bottom")
)
print(p2)
