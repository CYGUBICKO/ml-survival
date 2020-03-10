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

source("funs/downloadDf.R")

#### ---- 1. Time-dependent covariates

## Uses recidivism dataset

df_url <- "http://math.unm.edu/~james/Rossi.txt"
working_df <- downloadDf(filename = "recidivism"
	, filetype = "csv"
	, df_url = df_url
	, sep = ""
)

head(working_df)

## The trick is to take the time varying covariates
## and convert them to long format
working_df <- (working_df
	%>% mutate(sid = row_number())
	%>% gather(calendar.week, employed, emp1:emp52)
)

head(working_df)


